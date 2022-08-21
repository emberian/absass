import random
import os
import traceback
import sys
import ast
from random import randrange

from myhdl import block, always_comb, always_seq, always, \
        Signal, ResetSignal, intbv, modbv, delay, instance, \
        now, enum, bin, traceSignals

SUBDIR = 'verilog'
os.makedirs(SUBDIR, exist_ok = True)

STEPPING = 0
CLOCKRATE = 1024

CAP_HAS_MUL = 1

@block
def logic_unit(p, q, op, out):
    assert len(p) == len(q) == len(out)
    assert len(op) == 4

    width = len(out)

    @always_comb
    def comb():
        for bit in range(width):
            out.next[bit] = op[q[bit] | (p[bit] << 1)]

    return comb

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

CPU_STAGE = enum('IDLE', 'FETCH', 'EXEC', 'WRITEBACK', 'HALT')
XF_STAGE = enum('WB_PC', 'FETCH_SRC', 'WB_SRC', 'WRITE_DST', 'WB_DST')

@block
def mem_xlate_rd():
    pass

@block
def cpu(addr, data_in, data_out, mode, ready, valid, clk, halt, reset, d_ready, d_out, addr_skip = 2):
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
    bits_valid = Signal(intbv(0, min=0, max=17))

    regs = [Signal(modbv(0)[width:]) for _ in range(16)]
    xf = Signal(modbv(0)[width:])
    xf_state = Signal(XF_STAGE.WB_PC)

    ivt = Signal(intbv(0)[width:])  # Unused as of yet
    cycles = Signal(modbv(0)[width:])
    ins_retired = Signal(modbv(0)[width:])
    
    @always_seq(clk.posedge, reset=reset)
    def tick():
        # Keep the branches of this if unmerged; it helps MyHDL realize that
        # it's actually a case analysis
        cycles.next = cycles + 1
        if state == CPU_STAGE.IDLE:
            if not halt:
                state.next = CPU_STAGE.FETCH
                bits_valid.next[:] = 0
        elif state == CPU_STAGE.FETCH:
            if valid:
                ready.next = False
                if width >= 16:
                    inst.next[:] = data_in[16:0]
                    state.next = CPU_STAGE.EXEC
                else:
                    access_width = width
                    if bits_valid + access_width > 16:
                        access_width = 16 - bits_valid
                    inst.next[16 - bits_valid:16 - bits_valid - access_width] = data_in[access_width:]
                    if bits_valid + access_width >= 16:
                        state.next = CPU_STAGE.EXEC
                    else:
                        regs[0].next[:] = regs[0] + 1
                        bits_valid.next[:] = bits_valid + access_width
            else:
                pc = regs[0]  # PC
                addr.next[:] = pc
                npc.next[:] = pc + addr_skip
                mode.next = False
                ready.next = True
        elif state == CPU_STAGE.EXEC:
            dl = inst[4:0]
            sp = inst[8:4]
            if inst[16:14] == 0b01:
                dmode = inst[10:8]
                dind = inst[10]
                smode = inst[13:11]
                sind = inst[13]
                if xf_state == XF_STAGE.WB_PC:
                    # This only needs to be done in case src or dst is the
                    # PC, but nonetheless...
                    regs[0].next = npc
                    xf_state.next = XF_STAGE.FETCH_SRC
                elif xf_state == XF_STAGE.FETCH_SRC:
                    sval = regs[sp]
                    if sind:
                        if smode == 2:
                            sval -= width // 8
                        if valid:
                            ready.next = False
                            xf.next[:] = data_in
                            xf_state.next = XF_STAGE.WB_SRC
                        else:
                            mode.next = False
                            addr.next[:] = sval
                            ready.next = True
                    else:
                        xf.next[:] = sval
                        xf_state.next = XF_STAGE.WB_SRC
                elif xf_state == XF_STAGE.WB_SRC:
                    sval = regs[sp]
                    if smode == 1:
                        regs[sp].next[:] = sval + width // 8
                    elif smode == 2 or smode == 3:
                        regs[sp].next[:] = sval - width // 8
                    xf_state.next = XF_STAGE.WRITE_DST
                elif xf_state == XF_STAGE.WRITE_DST:
                    dval = regs[dl]
                    if dind:
                        if dmode == 2:
                            dval -= width // 8
                        if valid:
                            ready.next = False
                            xf_state.next = XF_STAGE.WB_DST
                        else:
                            mode.next = True
                            addr.next[:] = dval
                            data_out.next[:] = xf
                            ready.next = True
                    else:
                        regs[dl].next[:] = xf
                        xf_state.next = XF_STAGE.WB_DST
                elif xf_state == XF_STAGE.WB_DST:
                    dval = regs[dl]
                    if dmode == 2 or dmode == 3:
                        regs[dl].next[:] = dval - width / 8
                    elif dmode == 1:
                        regs[dl].next[:] = dval + width / 8
                    xf_state.next = XF_STAGE.WB_PC
                    state.next = CPU_STAGE.FETCH
                    ins_retired.next = ins_retired + 1
                    if halt:
                        state.next = CPU_STAGE.IDLE
                    else:
                        # Don't writeback PC, it could have been a source
                        mode.next = False
                        pc = regs[0]
                        addr.next[:] = pc
                        npc.next[:] = pc + 2
                        ready.next = True
            else:
                opcode = inst[16:12]
                if opcode == 0b0001:
                    d.next = regs[dl]
                    s.next = regs[sp]
                    l_op.next = inst[12:8]
                elif opcode == 0b0010:
                    d.next = regs[dl]
                    if inst[11] != 0:  # SI
                        s.next = sp
                    else:
                        s.next = regs[sp]
                    # FIXME: the Verilog synth can't deal with this swizzle
                    #a_op.next = getattr(ARITH, ARITH._names[int(inst[11:8])])
                    a_code = inst[11:8]
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
                    d.next = regs[dl]
                    s.next = regs[sp]
                    c_eq.next = inst[8]
                    c_gt.next = inst[9]
                    c_sn.next = inst[10]
                    c_iv.next = inst[11]
                elif opcode == 0b1000:
                    offset = inst[8:0]
                    cmp = inst[12:8]
                    if regs[cmp] != 0:
                        npc.next = npc.signed() + offset.signed()
                elif opcode == 0b1001:
                    regs[dl].next = npc
                    npc.next = regs[sp]
                elif opcode == 0b1010 or opcode == 0b1011:  # Don't rewrite this to in for Verilog's sake
                    write = inst[12]
                    r = inst[12:8]
                    sr = inst[8:0]
                    if sr == 0:
                        if not write:
                            regs[r].next = STEPPING
                    elif sr == 1:
                        if not write:
                            regs[r].next = width
                    elif sr == 2:
                        if write:
                            ivt.next = regs[r]
                        else:
                            regs[r].next = ivt
                    elif sr == 3:
                        if write:
                            v = regs[r]
                            if v & 0x1:
                                state.next = CPU_STAGE.HALT
                                return
                    elif sr == 4:
                        if write:
                            ins_retired.next = regs[r]
                        else:
                            regs[r].next = ins_retired
                    elif sr == 5:
                        if write:
                            cycles.next = regs[r]
                        else:
                            regs[r].next = cycles
                    elif sr == 6:
                        if not write:
                            regs[r].next = CLOCKRATE
                    elif sr == 7:
                        if not write:
                            regs[r].next = CAP_HAS_MUL
                    elif sr == 10:
                        if write:
                            d_ready.next = True
                            d_out.next = regs[r]
                elif opcode == 0b1100:
                    r = inst[4:0]
                    val = inst[12:4]
                    regs[r].next = val
                state.next = CPU_STAGE.WRITEBACK
        elif state == CPU_STAGE.WRITEBACK:
            dl = inst[4:0]
            if inst[16:14] != 0b01:
                opcode = inst[16:12]
                if opcode == 0b0001:
                    regs[dl].next = l_out
                elif opcode == 0b0010:
                    regs[dl].next = a_out
                elif opcode == 0b0011:
                    if c_out:
                        regs[dl].next = 1
                    else:
                        regs[dl].next = 0
            regs[0].next = npc  # PC
            if halt:
                state.next = CPU_STAGE.IDLE
            else:
                pc = npc
                addr.next = pc
                npc.next = pc + addr_skip
                mode.next = False
                ready.next = True
                state.next = CPU_STAGE.FETCH
                ins_retired.next = ins_retired + 1
                bits_valid.next[:] = 0

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
            low = 0
            if op.next in (ARITH.DIV, ARITH.MOD):
                low = 1
                if s == 0:  # Fix a hold-over bug from MUL before we tick
                    s.next = 1
            yield delay(1)

            for _try in range(tries):
                d.next, s.next = randrange(2**w), randrange(low, 2**w)
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

class bitarray:
    def __init__(self, bs):
        self.bs = bytearray(bs)

    @staticmethod
    def to_index(n):
        return divmod(n, 8)

    def __repr__(self):
        return f'bitarray({self.bs!r})'

    def __getitem__(self, v):
        try:
            addr = int(v)
        except TypeError:
            pass
        else:
            bt, bi = self.to_index(addr)
            return (self.bs[bt] >> (7 - bi)) & 1
        if isinstance(v, slice):
            # XXX unsatisfying, but exceedingly difficult to do correctly.
            res = 0
            for bidx in range(v.start, v.stop, v.step if v.step is not None else 1):
                res = (res << 1) | self[bidx]
            return res
        #    if v.step is not None:
        #        raise NotImplementedError('bitstring stride')
        #    start_bt, start_bi = self.to_index(v.start)
        #    stop_bt, stop_bi = self.to_index(v.stop)
        #    print(f'get {v.start}:{v.stop} = bytes {start_bt},{start_bi}:{stop_bt},{stop_bi}')
        #    bitwidth = v.stop - v.start
        #    range_bt = stop_bt if stop_bi == 0 else stop_bt + 1
        #    bts = int.from_bytes(self.bs[start_bt:range_bt], 'big')
        #    if start_bi > 0:
        #        bts >>= start_bi
        #    bts &= (1 << bitwidth) - 1
        #    return bts
        raise NotImplementedError(type(v))

    def __setitem__(self, v, val):
        try:
            addr = int(v)
        except TypeError:
            pass
        else:
            bt, bi = self.to_index(addr)
            if val & 1:
                self.bs[bt] |= (1 << (7 - bi))
            else:
                self.bs[bt] &= ~(1 << (7 - bi))
            return val
        if isinstance(v, slice):
            # XXX idem
            width = v.stop - v.start
            for bidx in range(v.start, v.stop, v.step if v.step is not None else 1):
                self[bidx] = (val >> (width - bidx + v.start - 1)) & 1
            return val
        #    if v.step is not None:
        #        raise NotImplementedError('bitstring stride')
        #    start_bt, start_bi = self.to_index(v.start)
        #    stop_bt, stop_bi = self.to_index(v.stop)
        #    bitwidth = v.stop - v.start
        #    bytewidth = (bitwidth + 7 // 8)
        #    val_bits = val
        #    if start_bi > 0:
        #        val_bits <<= start_bi
        #        val_bits |= self.bs[start_bt] & ((1 << start_bi) - 1)
        #    range_bt = stop_bt if stop_bi == 0 else stop_bt + 1
        #    if stop_bi > 0:
        #        val_bits |= (self.bs[range_bt] & ~((1 << stop_bi) - 1)) << v.stop
        #    bts = val_bits.to_bytes(bytewidth, 'big')
        #    self.bs[start_bt:range_bt] = bts
        #    return val
        raise NotImplementedError(type(v))

@block
def cpu_ram(bs, addr, data_in, data_out, mode, ready, valid, addr_gran = 8):
    bs = bitarray(bytearray(bs))
    bitsize = len(addr)
    upper_ws = (bitsize + 7) // 8
    @always_comb
    def comb():
        if ready:
            bitaddr = addr * addr_gran
            if mode:
                bs[bitaddr:bitaddr + bitsize] = int(data_in)
            data_out.next[bitsize:] = bs[bitaddr:bitaddr + bitsize]
            valid.next = True
        else:
            valid.next = False
    return comb

@block
def cpu_test(w, _ignored=0):
    addr = Signal(modbv(0)[w:])
    data_to_ram = Signal(modbv(0)[w:])
    data_to_cpu = Signal(modbv(0)[w:])
    mode = Signal(False)
    ready = Signal(False)
    valid = Signal(False)
    clk = Signal(False)
    halt = Signal(False)
    reset = ResetSignal(True, active=True, isasync=True)
    debug_ready = Signal(False)
    debug_out = Signal(modbv(0)[w:])

    cpu_unit = traceSignals(cpu(addr, data_to_cpu, data_to_ram, mode, ready, valid, clk, halt, reset, debug_ready, debug_out, 2))
    try_convert(cpu_unit)
    mem = cpu_ram(
            open('../assembler/hello16.bin', 'rb').read(),
            addr, data_to_ram, data_to_cpu, mode, ready, valid,
            min((8, w))
    )

    @instance
    def temporal():
        # Allow a reset to occur
        yield delay(1)
        reset.next = False
        while cpu_unit.symdict['state'] != CPU_STAGE.HALT:
            #print(f'{now()}:\t{"tick" if clk else "tock"}\t@{addr}/O={data_to_ram}/I={data_to_cpu}:{"W" if mode else "R"},{"V" if valid else " "}{"R" if ready else " "}\tnpc={cpu_unit.sigdict["npc"]}\tstate={cpu_unit.sigdict["state"]}\tinst={cpu_unit.sigdict["inst"]}/{cpu_unit.sigdict["bits_valid"]}\txf={cpu_unit.sigdict["xf"]},{cpu_unit.sigdict["xf_state"]}\n\tregs={" ".join(hex(i.val)[2:].rjust(4, "0") for i in cpu_unit.symdict["regs"])}')
            clk.next = not clk
            yield delay(1)
            if debug_ready:
                debug_ready.next = False
                try:
                    out = chr(debug_out)
                except Exception:
                    out = f'{{{debug_out!r}}}'
                print(out, end='', flush=True)

    return cpu_unit, mem, temporal

if __name__ == '__main__':
    import sys
    
    if len(sys.argv) > 1:
        for test in sys.argv[1:]:
            dut = globals()[test](16, 1)
            dut.run_sim()
