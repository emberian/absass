import random
import os
from random import randrange

from myhdl import block, always_comb, \
        Signal, intbv, modbv, delay, instance, \
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

# FIXME: MyHDL can't yet transpile dynamic bit indexing
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

@block
def logic_test(w, tries=10000):
    p = Signal(intbv(0)[w:])
    q = Signal(intbv(0)[w:])
    op = Signal(intbv(0)[4:])
    out = Signal(intbv(0)[w:])

    unit = logic_unit(p, q, op, out)
    unit.convert(path = SUBDIR)

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
    unit.convert(path = SUBDIR)

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
    unit.convert(path = SUBDIR)

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

if __name__ == '__main__':
    import sys
    
    if len(sys.argv) > 1:
        for test in sys.argv[1:]:
            dut = globals()[test](4, 1)
            dut.run_sim()
