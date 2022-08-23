PROJ=TopLvl
DEVICE=LIFCL-40-8MG289
PDC=vvml.pdc

SYNTH_ARGS=-flatten

YOSYS?=yosys
NEXTPNR?=nextpnr-nexus
PRJOXIDE?=prjoxide
ECPPROG?=ecpprog

all: $(PROJ).bit

vvml.pdc: mkpdc
	./mkpdc > vvml.pdc

$(PROJ).json: ../spinal_ass/$(PROJ).v $(PROJ).v $(EXTRA_VERILOG) $(MEM_INIT_FILES) vvml.pdc
	#(cd ../spinal_ass && sbt 'runMain absass.Nexass')
	cp ../spinal_ass/$(PROJ).v ../spinal_ass/TopLvl.v_toplevel_top_nx_cpu_regs_regs.bin .
	sed -e '/input\s*clk/d' -e '/input\s*reset/d' -e '/assign core_rst/a wire clk;assign clk = osc_HFCLKOUT;' -i $(PROJ).v
	$(YOSYS) -ql $(PROJ)_syn.log -p "synth_nexus $(SYNTH_ARGS) -top $(PROJ) -json $(PROJ).json" $(PROJ).v $(EXTRA_VERILOG)

$(PROJ).fasm: $(PROJ).json $(PDC)
	$(NEXTPNR) --detailed-timing-report -r --placed-svg placed.svg --routed-svg routed.svg --report report.json --device $(DEVICE) --pdc $(PDC) --json $(PROJ).json --fasm $(PROJ).fasm

$(PROJ).bit: $(PROJ).fasm
	$(PRJOXIDE) pack $(PROJ).fasm $(PROJ).bit

prog: $(PROJ).bit
	$(ECPPROG) -S $(PROJ).bit

prog-flash: $(PROJ).bit
	$(ECPPROG) $(PROJ).bit

clean:
	rm -f $(PROJ).json $(PROJ).fasm $(PROJ)_syn.log $(PROJ).bit

.SECONDARY:
.PHONY: prog prog-flash clean $(PROJ).fasm
