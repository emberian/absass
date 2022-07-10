class Assembler
	new: (out, wordsize = 64) =>
		if type(out) == "string"
			out = io.open out, "wb"
		@out = out
		@wordsize = wordsize
		@wordbytes = math.floor wordsize / 8
		@labels = {}
		@cur_global = nil
		@origin = 0
		@fixups = {}

	trim: (s) =>
		start = s\find "%S"
		s = s\sub start if start
		while true
			comi, come = s\find "^#.-\n"
			break unless comi
			s = s\sub come + 1
		s

	optcomma: (s) =>
		s = s\sub 2 if s\sub(1, 1) == ","
		s

	word: (s) =>
		space = s\find "%s"
		if space
			return {
				word: s\sub 1, space - 1
				rest: s\sub space
			}
		word: s, rest: ""

	ok: (val, rest) => {:val, :rest}
	fail: => false
	die: (msg, rest) => error msg .. " (at " .. @ellipsize(rest) .. ")"
	@ELLIPSIS: 50
	ellipsize: (s) =>
		s\sub(1, @@ELLIPSIS) .. "..." if #s > @@ELLIPSIS
		s

	bit: (bl) =>
		return 1 if bl
		0

	emit_byte: (by) =>
		@out\write string.char by
		@origin += 1

	emit: (ins) =>
		@emit_byte (ins >> 8) & 255
		@emit_byte ins & 255

	emit_word: (wd) =>
		for btind = @wordbytes - 1, 0, -1
			@emit_byte (wd >> (8 * btind)) & 0xff

	emit_maybe_byte: (v) =>
		if type(v) == "number"
			@emit_byte v
			return
		if v.val
			@emit_byte v.val
			return
		table.insert @fixups, {emit: 'byte', where: @origin, ref: v}
		@emit_byte 0

	emit_maybe_word: (v) =>
		if type(v) == "number"
			@emit_word v
			return
		if v.val
			@emit_word v.val
			return
		table.insert @fixups, {emit: 'word', where: @origin, ref: v}
		@emit_word 0

	@OPCODE:
		LOGIC: 0x1000
		ARITH: 0x2000
		COMP: 0x3000
		XFER: 0x4000
		COND: 0x8000
		JAL: 0x9000
		SR: 0xa000
		SMALLI: 0xc000

	@ARITH:
		ADD: 0
		SUB: 1
		SHL: 2
		SHR: 3
		ASR: 4
		MUL: 5
		DIV: 6
		MOD: 7

	e_logic: (data) =>
		{:tbl, :src, :dst} = data
		@emit (@@OPCODE.LOGIC | ((tbl&0xf)<<8) | ((src&0xf)<<4) | (dst&0xf))

	e_arith: (data) =>
		{:op, :src, :dst} = data
		@emit (@@OPCODE.ARITH | ((op&0x7)<<8) | ((src&0xf)<<4) | (dst&0xf))

	e_smalli: (data) =>
		{:dst, :val} = data
		@emit (@@OPCODE.SMALLI | ((val & 0xff)<<4) | (dst & 0xf))

	e_comp: (data) =>
		{:eq, :gt, :sn, :iv, :src, :dst} = data
		eq, gt, sn, iv = @bit(eq), @bit(gt), @bit(sn), @bit(iv)
		@emit (
			@@OPCODE.COMP |
			(iv << 11) |
			(sn << 10) |
			(gt << 9) |
			(eq << 8) |
			((src&0xf)<<4) |
			(dst&0xf)
		)

	e_jc: (data) =>
		{:cmp, :offset} = data
		@emit_byte (@@OPCODE.COND | ((cmp&0xf)<<8)) >> 8
		@emit_maybe_byte offset

	e_jal: (data) =>
		{:prog, :link} = data
		@emit (@@OPCODE.JAL | ((prog&0xf)<<4) | (link&0xf))

	e_xfer: (data) =>
		{:src, :dst} = data
		{ind: s_ind, mode: s_mode, reg: s_reg} = src
		{ind: d_ind, mode: d_mode, reg: d_reg} = dst
		s_ind, d_ind = @bit(s_ind), @bit(d_ind)
		@emit (
			@@OPCODE.XFER |
			(d_reg & 0xf) |
			((s_reg & 0xf) << 4) |
			((d_mode & 0x3) << 8) |
			(d_ind << 10) |
			((s_mode & 0x3) << 11) |
			(s_ind << 13)
		)

	e_sr: (data) =>
		{:mode, :reg, :sr} = data
		write = switch mode
			when "read" then 0
			when "write" then 1
			else error "Bad mode " .. mode
		@emit_byte (
			@@OPCODE.SR |
			(write << 12) |
			(reg << 8)
		) >> 8
		@emit_maybe_byte sr

	p_num: (s) =>
		s = @trim s
		-- The current origin
		return @ok @origin, s\sub 2 if s\sub(1, 1) == "$"
		-- A character constant
		if s\sub(1, 1) == "'"
			return @die "expected closing quote", s unless s\sub(3, 3) == "'"
			return @ok s\byte(2), s\sub(4)
		ixs, ixe = s\find "^[%da-fA-FxX]+"
		if ixs
			num = tonumber s\sub ixs, ixe
			return @ok num, s\sub ixe + 1 if num
		-- Try a label name instead
		lbl = @p_labelname s
		return @fail! unless lbl
		{global: glob, local: loc} = lbl.val
		addr = @get_label glob, loc
		return @die "No address for label " .. glob .. "." .. loc, s unless addr
		@ok addr, lbl.rest

	p_expr: (s) => @p_additive s

	p_additive: (s) =>
		term = @p_term s
		return @fail! unless term
		{val: ex, rest: s} = term
		s = @trim s
		c = s\sub 1, 1
		while c == "+" or c == "-"
			rhs = @p_term s\sub 2
			return @die "expected term", s unless rhs
			ex = {op: c, lhs: ex, rhs: rhs.val}
			s = @trim rhs.rest
			c = s\sub 1, 1
		@ok ex, s

	p_term: (s) =>
		atom = @p_atom s
		return @fail! unless atom
		{val: ex, rest: s} = atom
		s = @trim s
		c = s\sub 1, 1
		while c == "*" or c == "/"
			rhs = @p_atom s\sub 2
			return @die "expected atom", s unless rhs
			ex = {op: c, lhs: ex, rhs: rhs.val}
			s = @trim rhs.rest
			c = s\sub 1, 1
		@ok ex, s

	p_atom: (s) =>
		s = @trim s
		if s\sub(1, 1) == "("
			ex = @p_expr s\sub 2
			return @die "expected expression", s unless ex
			s = @trim ex.rest
			return @die "expected ')'", s unless s\sub(1, 1) == ")"
			return @ok ex.val, s\sub 2
		@p_num s

	p_reg: (s) =>
		s = @trim s

		return @ok 0, s\sub 3 if s\sub(1, 2) == "PC"

		if s\sub(1, 1) == "R"
			numpart = @trim s\sub 2
			ix, ixf = numpart\find "^%d+"
			return @fail! unless ix
			num = tonumber numpart\sub ix, ixf
			if num and num >= 0 and num < 16
				return @ok num, numpart\sub ixf + 1

		@fail!

	p_ident: (s) =>
		ix, last = s\find "^%a[%a%d_]*"
		return @fail! if ix == nil
		@ok s\sub(ix, last), s\sub(last + 1)

	p_labelname: (s) =>
		if s\sub(1, 1) == "."
			loc = @p_ident s\sub 2
			return @ok {local: loc.val}, loc.rest if loc
		global = @p_ident s
		return @fail! unless global
		s = global.rest
		return @ok {global: global.val}, s if s\sub(1, 1) ~= "."
		loc = @p_ident s\sub 2
		return @ok {global: global.val}, s unless loc
		@ok {global: global.val, local: loc.val}, loc.rest

	p_assn: (s) =>
		s = @trim s
		lbl = @p_labelname s
		return @fail! unless lbl
		s = @trim lbl.rest
		return @fail! unless s\sub(1, 1) == "="
		s = @trim s\sub 2
		ex = @p_expr s
		return @die "expected expression", s unless ex
		@ok {
			lbl: lbl.val
			ex: ex.val
		}, ex.rest

	p_label: (s) =>
		s = @trim s
		lbl = @p_labelname s
		return @fail! unless lbl
		return @fail! if lbl.rest\sub(1, 1) ~= ":"
		@ok lbl.val, lbl.rest\sub 2

	@MODES:
		normal: 0
		autoinc: 1
		autodec: 2
		ones: 3

	p_xft: (s) =>
		s = @trim s
		ind, mode = false, @@MODES.normal
		if s\sub(1, 1) == "*"
			ind, s = true, @trim s\sub 2
		if s\sub(1, 1) == "-"
			mode, s = @@MODES.autodec, @trim s\sub 2
		reg = @p_reg s
		return @fail! unless reg
		s = @trim reg.rest
		if s\sub(1, 1) == "+"
			return @die "conflicting modes for transfer", s if mode ~= @@MODES.normal
			mode, s = @@MODES.autoinc, @trim s\sub 2
		@ok {:ind, :mode, reg: reg.val}, s

	p_seq: (s) =>
		nums = {}
		while true
			break if s\find "^%s*\n"
			s = @trim s
			if s\sub(1, 1) == ";"
				s = s\sub 2
				break
			num = @p_expr s
			break unless num
			table.insert nums, num.val
			s = @optcomma num.rest
		@ok nums, s

	@INSNS: {}
	DECL_INSN = (nm, parse) -> @INSNS[nm] = {
		:parse
	}

	p_insn: (s) =>
		s = @trim s
		-- Maybe it's an assignment?
		assn = @p_assn s
		return @ok {assn: assn.val}, assn.rest if assn
		-- Maybe it's a label?
		lbl = @p_label s
		return @ok {label: lbl.val}, lbl.rest if lbl
		-- Go ahead and check which insn word it is
		{:word, rest: s} = @word s
		if iinfo = @@INSNS[word]
			iinfo.parse @, s
		else
			@die "unknown instruction " .. word, s

	DECL_INSN "MOV", (s) =>
		dst = @p_reg s
		return @die "expected dest reg", s unless dst
		s = @optcomma dst.rest
		src = @p_reg s
		return @die "expected source reg", s unless src
		s = src.rest
		@ok {
			insn: "logic"
			data:
				tbl: 0xa  -- The "source" table
				src: src.val
				dst: dst.val
		}, s

	DECL_INSN "ZERO", (s) =>
		reg = @p_reg s
		return @die "expected register", s unless reg
		@ok {
			insn: "logic"
			data:
				tbl: 0  -- the "false" table
				src: 0  -- doesn't matter
				dst: reg.val
		}, reg.rest

	DECL_INSN "ONE", (s) =>
		reg = @p_reg s
		return @die "expected register", s unless reg
		@ok {
			insn: "logic"
			data:
				tbl: 0xf  -- the "true" table
				src: 0  -- doesn't matter
				dst: reg.val
		}, reg.rest

	DECL_INSN ".BYTE", (s) =>
		seq = @p_seq @trim s
		return @die "expected number seq", s unless seq
		@ok {
			insn: "raw"
			data:
				bytes: seq.val
		}, seq.rest

	DECL_INSN ".WORD", (s) =>
		seq = @p_seq @trim s
		return @die "expected number seq", s unless seq
		@ok {
			insn: "raw"
			data:
				words: seq.val
		}, seq.rest

	for name, op in pairs @ARITH
		DECL_INSN name, (s) =>
			dst = @p_reg s
			return @die "expected dest register", s unless dst
			s = @trim @optcomma dst.rest
			src = @p_reg s
			return @die "expected source register", s unless src
			s = @trim @optcomma src.rest
			@ok {
				insn: "arith"
				data:
					:op
					src: src.val
					dst: dst.val
			}, s

	DECL_INSN "XF", (s) =>
		dst = @p_xft s
		return @die "expected dest transfer target", s unless dst
		s = @optcomma dst.rest
		src = @p_xft s
		return @die "expected source transfer target", s unless src
		s = src.rest
		@ok {
			insn: "xfer"
			data:
				dst: dst.val
				src: src.val
		}, s

	DECL_INSN "LI", (s) =>
		dst = @p_reg s
		return @die "expected dest register", s unless dst
		s = @optcomma dst.rest
		num = @p_expr s
		return @die "expected immediate", s unless num
		@ok {
			insn: "seq"
			data: {
				{insn: "xfer", data:
					dst:
						reg: dst.val
						mode: @@MODES.normal
						ind: false
					src:
						reg: 0  -- PC
						mode: @@MODES.autoinc
						ind: true
				},
				{insn: "word", data: num.val}
			}
		}, num.rest

	DECL_INSN "SI", (s) =>
		dst = @p_reg s
		return @die "expected dest register", s unless dst
		s = @optcomma dst.rest
		num = @p_expr s
		return @die "expected immediate", s unless num
		ex = @fold num.val
		return @die "sorry: non-const small immediates are not yet implemented", s unless type(ex) == "number"
		return @die "small immediate is too big (" .. tonumber(ex) .. ">255)", s if ex > 255
		@ok {
			insn: "smalli"
			data:
				dst: dst.val
				val: ex
		}, num.rest

	DECL_INSN "BIT", (s) =>
		dst = @p_reg @trim s
		return @die "expected dest register", s unless dst
		s = @trim @optcomma dst.rest
		src = @p_reg s
		return @die "expected source register", s unless src
		s = @trim @optcomma src.rest
		op = @p_expr s
		return @die "expected operation", s unless op
		@ok {
			insn: "logic"
			data:
				tbl: op.val
				src: src.val
				dst: dst.val
		}, op.rest

	DECL_INSN "SR", (s) =>
		s = @trim s
		local mode
		switch s\sub 1, 1
			when "R" then mode = "read"
			when "W" then mode = "write"
			else return @die "Bad mode " .. s\sub(1, 1) .. " (expected R/W)", s
		s = @trim @optcomma s\sub 2
		reg = @p_reg s
		return @die "Expected reg", s unless reg
		s = @trim @optcomma reg.rest
		sr = @p_expr s
		return @die "Expected SR number", s unless sr
		s = sr.rest
		@ok {
			insn: "sr"
			data:
				:mode
				reg: reg.val
				sr: sr.val
		}, s

	DECL_INSN "JC", (s) =>
		cmp = @p_reg s
		return @die "expected comp reg", s unless cmp
		s = @trim @optcomma cmp.rest
		off = @p_expr s
		return @die "expected offset", s unless off
		s = @trim @optcomma off.rest
		@ok {
			insn: "jc"
			data:
				cmp: cmp.val
				offset: off.val
		}, s

	DECL_INSN "JAL", (s) =>
		link = @p_reg s
		return @die "expected link reg", s unless link
		s = @trim @optcomma link.rest
		prog = @p_reg s
		return @die "expected prog reg", s unless prog
		s = @trim @optcomma prog.rest
		@ok {
			insn: "jal"
			data:
				link: link.val
				prog: prog.val
		}, s

	charif = (b, ch) ->
		return ch if b
		""

	for cmpcode = 0, 15
		eq, gt, sn, iv = (cmpcode&1~=0), (cmpcode&2~=0), (cmpcode&4~=0), (cmpcode&8~=0)
		iname = "CMP." .. charif(eq, "E") .. charif(gt, "G") .. charif(sn, "S") .. charif(iv, "I")
		DECL_INSN iname, (s) =>
			dst = @p_reg s
			return @die "expected dest reg", s unless dst
			s = @trim @optcomma dst.rest
			src = @p_reg s
			return @die "expected source reg", s unless src
			s = @trim @optcomma src.rest
			@ok {
				insn: "comp"
				data:
					:eq, :gt, :sn, :iv
					src: src.val
					dst: dst.val
			}, s

	@EMITTERS: {}
	DECL_EMIT = (ins, emit) -> @EMITTERS[ins] = {
		:emit
	}

	DECL_EMIT "logic", (ins) => @e_logic ins.data

	DECL_EMIT "arith", (ins) => @e_arith ins.data

	DECL_EMIT "comp", (ins) => @e_comp ins.data

	DECL_EMIT "jc", (ins) => @e_jc ins.data

	DECL_EMIT "jal", (ins) => @e_jal ins.data

	DECL_EMIT "xfer", (ins) =>
		@e_xfer ins.data

	DECL_EMIT "raw", (ins) =>
		data = ins.data
		local iter, emit
		if data.bytes
			iter = data.bytes
			emit = @\emit_maybe_byte
		else
			iter = data.words
			emit = @\emit_maybe_word
		for val in *iter
			emit val

	DECL_EMIT "word", (ins) => @emit_maybe_word ins.data

	DECL_EMIT "sr", (ins) => @e_sr ins.data

	DECL_EMIT "smalli", (ins) => @e_smalli ins.data

	DECL_EMIT "seq", (ins) =>
		for insn in *ins.data
			einfo = @@EMITTERS[insn.insn]
			error "No emitter for " .. insn.insn unless einfo
			einfo.emit @, insn

	set_label: (glob, loc, addr) =>
		unless glob
			unless @cur_global
				error "Can't define a local-only label before first global label"
			glob = @cur_global
		else
			@cur_global = glob
		loc = '' unless loc
		@labels[glob] = {} unless @labels[glob]
		g = @labels[glob]
		print 'WARN: Redefinition of label ' .. glob .. '.' .. loc if type(g[loc]) == 'number' or g.val
		if type(g[loc]) == 'table'
			g[loc].val = addr
		else
			g[loc] = addr

	get_label: (glob, loc) =>
		unless glob
			unless @cur_global
				error "Can't reference local label before first global label"
			glob = @cur_global
		loc = '' unless loc
		@labels[glob] = {} unless @labels[glob]
		g = @labels[glob]
		g[loc] = {forward: true, global: glob, local: loc} unless g[loc]
		g[loc]

	@FUNCS:
		"+": (a, b) -> a + b
		"-": (a, b) -> a - b
		"*": (a, b) -> a * b
		"/": (a, b) -> math.floor(a / b)

	fold: (ex) =>
		return ex if type(ex) == "number"
		return ex.val if ex.val
		if ex.op
			{:op, :lhs, :rhs} = ex
			l, r = @fold(lhs), @fold(rhs)
			if type(l) == "number" and type(r) == "number"
				return @@FUNCS[op](l, r)
		ex

	format_ex: (ex, depth = 0) =>
		s = "  "
		s = s\rep depth
		if type(ex) == "number"
			s = s .. tostring(ex) .. "\n"
		elseif ex.op
			s = s .. ex.op .. "\n" .. @format_ex(ex.lhs, depth + 1) .. @format_ex(ex.rhs, depth + 1)
		elseif ex.global
			{global: glob, local: loc} = ex
			s = s .. "ref: " .. glob .. "." .. loc
			if ex.val
				s = s .. " @" .. ex.val
			s = s .. "\n"
		else
			s = s .. "something: "
			for k, v in pairs ex
				s = s .. tostring(k) .. "=" .. tostring(v) .. " "
			s = s .. "\n"
		s

	force: (v) =>
		v = @fold v
		return v if type(v) == "number"
		if type(v) == "table"
			return v.val if v.val
			error "Value unresolvable at emission time:\n" .. @format_ex(v)
		error "Unknown type of value " .. type(v)

	one: (s) =>
		{:val, :rest} = @p_insn s
		-- Labels
		if val.label
			{global: glob, local: loc} = val.label
			@set_label glob, loc, @origin
		-- Assignments
		elseif val.assn
			{:lbl, :ex} = val.assn
			{global: glob, local: loc} = lbl
			ex = @fold ex
			@set_label glob, loc, ex
		-- Instructions
		else
			einfo = @@EMITTERS[val.insn]
			error "No emitter for " .. val.insn unless einfo
			einfo.emit @, val
		rest

	run: (s) =>
		while #s > 0
			break if s\find("%S") == nil
			s = @one s
		@fixup!

	fixup: =>
		print 'fixing'
		for fix in *@fixups
			@origin = fix.where
			@out\seek("set", @origin)
			val = @force fix.ref
			print 'fix ' .. fix.emit .. ' at ' .. @origin .. ' to ' .. val
			switch fix.emit
				when "word" then @emit_word val
				when "byte" then @emit_byte val
				else error "Unknown fix type " .. fix.emit
	
	info: =>
		print 'Labels:'
		for glob, sub in pairs @labels
			print '- ' .. glob .. ':'
			for loc, addr in pairs sub
				if type(addr) == "number"
					print '  - ' .. loc .. ' = ' .. tostring(addr)
				else
					print '  - ' .. loc .. ' = (forward) ' .. tostring(addr.val)

if arg
	unless arg[1]
		print 'Usage: lua (this script) out.bin < in.asm'
		return

	source = io.read "*a"
	assembler = Assembler arg[1], 16
	assembler\run source
	assembler\info!

{:Assembler}
