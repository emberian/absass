import random
import os
import traceback
import sys
import ast
from random import randrange

from myhdl import block, always_comb, always_seq, always, \
        Signal, ResetSignal, intbv, modbv, delay, instance, \
        now, enum, bin

SUBDIR = 'verilog'
os.makedirs(SUBDIR, exist_ok = True)

@block
def logic_unit(p, q, op, out):
    assert len(p) == len(q) == len(out)
    assert len(op) == 4

    width = len(out)

    @always_comb
    def comb():
        res = intbv(0)[width:]
        for bit in range(width):
            res[bit] = op[
                    q(bit) | (p(bit) << 1)
            ]
        out.next = res

    return comb

# FIXME: MyHDL can't yet seem to transpile dynamic indexing
# NB: carefully escape Verilog functions ($) with ($$) due to the use of a
# template string
logic_unit.verilog_code = '''
integer bit;
always @(p, q, op) begin: LOGIC_UNIT_COMB
    for(bit = 0; bit < $$size(out); bit = bit + 1) begin
        out[bit] = op[q[bit] + (p[bit] <<< 1)];
    end
end
'''

ARITH = enum('ADD', 'SUB', 'SHL', 'SHR', 'ASR', 'MUL', 'DIV', 'MOD')

@block
def arith_unit(d, s, op, out, has_mul=True):
    assert len(d) == len(s) == len(out)
    assert len(op) == 3

    @always_comb
    def comb():
        if op == ARITH.ADD:
            out.next = d + s
        elif op == ARITH.SUB:
            out.next = d - s
        elif op == ARITH.SHL:
            out.next = d << s
        elif op == ARITH.SHR:
            out.next = d >> s
        elif op == ARITH.ASR:
            out.next = d.signed() >> s
        elif op == ARITH.MUL and has_mul:
            out.next = d * s
        elif op == ARITH.DIV and has_mul:
            out.next = d // s
        elif op == ARITH.MOD and has_mul:
            out.next = d % s
        else:
            out.next = 0

    return comb

@block
def comp_unit(d, s, eq, gt, sn, iv, out):
    assert len(d) == len(s)

    @always_comb
    def comb():
        nx = False
        if eq:
            nx = nx or d == s
        if gt:
            if sn:
                nx = nx or d.signed() > s.signed()
            else:
                nx = nx or d > s
        if iv:
            nx = not nx
        out.next = nx

    return comb

CPU_STAGE = enum('IDLE', 'FETCH', 'EXEC', 'WRITEBACK')

@block
def mem_xlate_rd():
    pass

@block
def cpu(addr, data, ready, valid, clk, halt, reset):
    width = len(addr)

    state = Signal(CPU_STAGE.IDLE)
    d = Signal(modbv(0)[width:])
    s = Signal(modbv(0)[width:])

    l_op, l_out = Signal(intbv(0)[4:]), Signal(modbv(0)[width:])
    logic = logic_unit(d, s, l_op, l_out)

    a_op, a_out = Signal(ARITH.ADD), Signal(modbv(0)[width:])
    arith = arith_unit(d, s, a_op, a_out)

    c_eq, c_gt, c_sn, c_iv, c_out = map(Signal, [False] * 5)
    comp = comp_unit(d, s, c_eq, c_gt, c_sn, c_iv, c_out)

    inst = Signal(intbv(0)[16:])
    npc = Signal(intbv(0)[width:])

    reg = [Signal(modbv(0)[width:]) for _ in range(16)]
    
    # Temporaries used below
    pc = intbv(0)[width:]
    dl = intbv(0)[4:]
    sp = intbv(0)[4:]
    opcode = intbv(0)[4:]
    a_code = intbv(0)[3:]

    @always_seq(clk.posedge, reset=reset)
    def tick():
        # Keep the branches of this if unmerged; it helps MyHDL realize that
        # it's actually a case analysis
        if state == CPU_STAGE.IDLE:
            if not halt:
                state.next = CPU_STAGE.FETCH
        elif state == CPU_STAGE.FETCH:
            if valid:
                ready.next = False
                inst.next = data
                state.next = CPU_STAGE.EXEC
            else:
                pc[:] = reg[0]  # PC
                addr.next = pc
                npc.next = pc + 2
                ready.next = True
        elif state == CPU_STAGE.EXEC:
            dl[:] = inst[4:0]
            sp[:] = inst[8:4]
            if inst[16:14] == 0b01:
                pass  # todo: data transfer
            else:
                opcode[:] = inst[16:12]
                if opcode == 0b0001:
                    d.next = reg[dl]
                    s.next = reg[sp]
                    l_op.next = inst[12:8]
                elif opcode == 0b0010:
                    d.next = reg[dl]
                    s.next = reg[sp]
                    # FIXME: the Verilog synth can't deal with this swizzle
                    #a_op.next = getattr(ARITH, ARITH._names[int(inst[11:8])])
                    a_code[:] = inst[11:8]
                    if a_code == 0:
                        a_op.next = ARITH.ADD
                    elif a_code == 1:
                        a_op.next = ARITH.SUB
                    elif a_code == 2:
                        a_op.next = ARITH.SHL
                    elif a_code == 3:
                        a_op.next = ARITH.SHR
                    elif a_code == 4:
                        a_op.next = ARITH.ASR
                    elif a_code == 5:
                        a_op.next = ARITH.MUL
                    elif a_code == 6:
                        a_op.next = ARITH.DIV
                    elif a_code == 7:
                        a_op.next = ARITH.MOD
                elif opcode == 0b0011:
                    d.next = reg[dl]
                    s.next = reg[sp]
                    c_eq.next = inst[8]
                    c_gt.next = inst[9]
                    c_sn.next = inst[10]
                    c_iv.next = inst[11]
                elif opcode == 0b1000:
                    offset = inst[8:0]
                    cmp = inst[12:8]
                    if reg[cmp] != 0:
                        npc.next = npc.signed() + offset.signed()
                elif opcode == 0b1001:
                    reg[dl].next = npc
                    npc.next = reg[sp]
            state.next = CPU_STAGE.WRITEBACK
        elif state == CPU_STAGE.WRITEBACK:
            dl[:] = inst[4:0]
            if inst[16:14] != 0b01:
                opcode[:] = inst[16:12]
                if opcode == 0b0001:
                    reg[dl].next = l_out
                elif opcode == 0b0010:
                    reg[dl].next = a_out
                elif opcode == 0b0011:
                    if c_out:
                        reg[dl].next = 1
                    else:
                        reg[dl].next = 0
            reg[0].next = npc  # PC
            if halt:
                state.next = CPU_STAGE.IDLE
            else:
                pc[:] = npc
                addr.next = pc
                npc.next = pc + 2
                ready.next = True
                state.next = CPU_STAGE.FETCH

    return logic, arith, comp, tick

def try_convert(block):
    try:
        block.convert(path = SUBDIR)
    except Exception:
        traceback.print_exc()
        _, _, tb = sys.exc_info()

        print('-'*70)
        print('This appears to have been during the processing of this/these AST node(s) (the innermost one(s) found):')

        frames = []
        cur = tb
        while cur is not None:
            frames.append(cur.tb_frame)
            cur = cur.tb_next

        for frame in reversed(frames):
            lc = frame.f_locals
            found = False
            for name, val in lc.items():
                if isinstance(val, ast.AST):
                    found = True
                    print(f'{name}:')
                    print(ast.dump(val))
                    print(f'(line {val.lineno})')
            if found:
                break

        print('Continuing to your test anyway...')

@block
def logic_test(w, tries=10000):
    p = Signal(intbv(0)[w:])
    q = Signal(intbv(0)[w:])
    op = Signal(intbv(0)[4:])
    out = Signal(intbv(0)[w:])

    unit = logic_unit(p, q, op, out)
    try_convert(unit)

    @instance
    def temporal():
        for oval in range(16):
            op.next = oval
            otbl = list(reversed(list(intbv(oval)[w:])))
            #print(f'table = {otbl}')
            yield delay(1)
            for _try in range(tries):
                p.next, q.next = randrange(2**w), randrange(2**w)
                yield delay(1)
                print(f'{now()}:\t{p}\t{q}\t{op}({bin(op, 4)})\t={out}')
                for bit in range(w):
                    bp = (p >> bit) & 1
                    bq = (q >> bit) & 1
                    assert otbl[bq + 2*bp] == (out >> bit) & 1

    return unit, temporal

ARITH_TEST_FNS = [
        lambda d, s: d + s,
        lambda d, s: d - s,
        lambda d, s: d << s,
        lambda d, s: d >> s,
        None,
        lambda d, s: d * s,
        lambda d, s: d // s,
        lambda d, s: d % s,
]

@block 
def arith_test(w, tries=10000):
    d = Signal(modbv(0)[w:])
    s = Signal(modbv(0)[w:])
    op = Signal(ARITH.ADD)
    out = Signal(modbv(0)[w:])

    unit = arith_unit(d, s, op, out)
    try_convert(unit)

    @instance
    def temporal():
        for oval, fnc in enumerate(ARITH_TEST_FNS):
            if fnc is None:
                continue
            # XXX convert int to enum
            op.next = getattr(ARITH, ARITH._names[oval])
            yield delay(1)

            for _try in range(tries):
                d.next, s.next = randrange(2**w), randrange(2**w)
                yield delay(1)
                print(f'{now()}:\t{d}\t{s}\t{op}\t={out}')
                assert out == fnc(d, s) % 2**w

    return unit, temporal

cu = lambda c, b: c.upper() if b else c.lower()
COMP_NAMES = ['F1', 'EQ1', 'AB', 'ABE',
        'F2', 'EQ2', 'GT', 'GTE',
        'T1', 'NEQ1', 'BLE', 'BL',
        'T2', 'NEQ2', 'LT', 'LTE',
]

@block
def comp_test(w, tries=10000):
    d = Signal(intbv(0)[w:])
    s = Signal(intbv(0)[w:])
    eq = Signal(False)
    gt = Signal(False)
    sn = Signal(False)
    iv = Signal(False)
    out = Signal(False)

    unit = comp_unit(d, s, eq, gt, sn, iv, out)
    try_convert(unit)

    @instance
    def temporal():
        for cp in range(16):
            eq.next = cp & 1 != 0
            gt.next = cp & 2 != 0
            sn.next = cp & 4 != 0
            iv.next = cp & 8 != 0
            yield delay(1)
            for _try in range(tries):
                d.next, s.next = randrange(2**w), randrange(2**w)
                yield delay(1)
                res = False
                if eq and d == s:
                    res = True
                if gt and (not sn) and d > s:
                    res = True
                if gt and sn and d.signed() > s.signed():
                    res = True
                if iv:
                    res = not res
                print(f'{now()}:\t{d}\t{s}\t{cu("E", eq)}{cu("G", gt)}{cu("S", sn)}{cu("I", iv)} ({COMP_NAMES[cp]})\t={out}\t(={res})')
                assert out == res

    return unit, temporal

@block
def cpu_ram(words, addr, data, ready, valid):
    @always_comb
    def comb():
        if ready:
            data.next = words[int(addr) >> 1]
            valid.next = True
        else:
            valid.next = False
    return comb

@block
def cpu_test(w, _ignored=0):
    addr = Signal(modbv(0)[w:])
    data = Signal(modbv(0)[16:])
    ready = Signal(False)
    valid = Signal(False)
    clk = Signal(False)
    halt = Signal(False)
    reset = ResetSignal(True, active=True, isasync=True)

    cpu_unit = cpu(addr, data, ready, valid, clk, halt, reset)
    try_convert(cpu_unit)
    mem = cpu_ram(
            [0x1322, 0x1a21, 0x82fc],
            addr, data, ready, valid,
    )

    @instance
    def temporal():
        # Allow a reset to occur
        yield delay(1)
        reset.next = False
        for step in range(48):
            print(f'{now()}:\t{"tick" if clk else "tock"}\taddr={addr}\tdata={data}\tvalid={valid}\tready={ready}\tstate={cpu_unit.sigdict["state"]}\tinst={cpu_unit.sigdict["inst"]}\tregs={" ".join(bin(i.val, w) for i in cpu_unit.symdict["reg"])}')
            clk.next = not clk
            yield delay(1)

    return cpu_unit, mem, temporal

if __name__ == '__main__':
    import sys
    
    if len(sys.argv) > 1:
        for test in sys.argv[1:]:
            dut = globals()[test](4, 1)
            dut.run_sim()
