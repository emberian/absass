class Assembler
	new: (wordsize = 64) =>
		@set_word_size wordsize
		@labels = {}
		@cur_global = nil
		@origin = 0
		@byteoff = 0
		@fixups = {}
		@buf = {}

	set_word_size: (wordsize) =>
		@wordsize = wordsize
		@wordbytes = math.ceil wordsize / 8

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
		if @byteoff+1 > #@buf
			table.insert @buf, by
		else
			@buf[@byteoff+1] = by
		@origin += 1
		@byteoff += 1

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
		table.insert @fixups, {emit: 'byte', where: @byteoff, org: @origin, ref: v}
		@emit_byte 0

	emit_maybe_word: (v) =>
		if type(v) == "number"
			@emit_word v
			return
		if v.val
			@emit_word v.val
			return
		table.insert @fixups, {emit: 'word', where: @byteoff, org: @origin, ref: v}
		@emit_word 0

	@OPCODE:
		EXC: 0x0
		LOGIC: 0x1000
		ARITH: 0x2000
		COMP: 0x3000
		XFER: 0x4000
		COND: 0x8000
		MISC: 0xe000
		SBR: 0xe800
		SR: 0xa000
		SMALLI: 0xc000

	@ARITH:
		ADD: 0
		SUB: 1
		SHL: 2
		SHR: 3
		ASR: 4
		ROL: 5
		ROR: 6
		NEG: 7

	@LOGIC:
		AND: 8
		OR: 14
		XOR: 6
		NAND: 7
		NOR: 1
		XNOR: 9

	@MISC:
		SWAP: 0
		MUL: 1
		DIV: 2
		MOD: 3
		LOADR: 4
		STORER: 5
		LOOP: 6
		LOOPI: 7

	@SBR:
		Z: 0
		NZ: 1
		GZ: 2
		LZ: 3

	e_logic: (data) =>
		{:tbl, :src, :dst} = data
		@emit (@@OPCODE.LOGIC | ((tbl&0xf)<<8) | ((src&0xf)<<4) | (dst&0xf))

	e_arith: (data) =>
		{:op, :src, :dst, :imm} = data
		srcnum = 0
		if type(src) == "number"
			@die "arith immediate too big: " .. src, "byte offset " .. @byteoff if src < 0 or src >= 16
			srcnum = src
		else
			table.insert @fixups, {emit: 'arith', where: @byteoff, org: @origin, ref: src, :data}
		@emit (@@OPCODE.ARITH | (if imm then 0x800 else 0) | ((op&0x7)<<8) | ((srcnum&0xf)<<4) | (dst&0xf))

	e_smalli: (data) =>
		{:dst, :val} = data
		num = 0
		if type(val) == "number"
			num = val
		else
			table.insert @fixups, {emit: 'smalli', where: @byteoff, org: @origin, ref: val, :dst}
		@emit (@@OPCODE.SMALLI | ((num & 0xff)<<4) | (dst & 0xf))

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

	e_jnz: (data) =>
		{:cmp, :offset} = data
		@emit_byte (@@OPCODE.COND | ((cmp&0xf)<<8)) >> 8
		@emit_maybe_byte offset

	e_misc: (data) =>
	    {:a, :b, :op} = data
		@emit_byte (@@OPCODE.MISC | op << 8 | a << 4 | b)

	e_sbr: (data) =>
		{:v,:c,:s,:op} = data
		if s
			s = 1
		else
			s = 0
		@emit (@@OPCODE.SBR | op << 9 | s << 8 | c << 4 | v)

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
		-- A hex number
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

	@REG_ALIAS:  -- Taken from the calling convention standard
		PC: 0
		SP: 1
		FP: 2
		RA: 3
	for tnum = 0, 5
		@REG_ALIAS["A" .. tnum] = tnum + 4
	for tnum = 0, 5
		@REG_ALIAS["T" .. tnum] = tnum + 10

	p_reg: (s) =>
		s = @trim s

		-- Compat with assc, whenever that happens
		return @die "found an unallocated temporary, did assc fail?", s if s\sub(1, 1) == '%'

		-- Yes, I know it's not Aho-Corasick, hopefully it works for now
		for name, num in pairs @@REG_ALIAS
			namelen = #name
			return @ok num, s\sub namelen + 1 if s\sub(1, namelen) == name

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
		autopostdec: 3

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
		c = s\sub(1, 1)
		if c == "+" or c == "-"
			return @die "conflicting modes for transfer", s if mode ~= @@MODES.normal
			mode = if c == "+" then @@MODES.autoinc else @@MODES.autopostdec
			s = @trim s\sub 2
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
			if num
				table.insert nums, num.val
				s = @optcomma num.rest
				continue
			str = @p_str s
			if str
				v = str.val
				for ix = 1, #v
					table.insert nums, v\byte ix
				s = @optcomma str.rest
				continue
			break
		@ok nums, s

	p_imm: (s) =>
		s = @trim s
		return @fail! unless s\sub(1, 1) == "$"
		@p_expr s\sub 2

	@ESCAPES:
		n: '\n'
		r: '\r'
		t: '\t'
		v: '\v'
		b: '\b'
		f: '\f'
		["'"]: "'"
		['"']: '"'
		['\\']: '\\'

	@ESCAPE_CHAR: "\\"

	p_str: (s) =>
		return @fail! unless s\sub(1, 1) == '"'
		ix = 2
		seq = {}
		while true
			return @die "EOF in string", s if ix > #s
			c = s\sub ix, ix
			break if c == '"'
			if c == @@ESCAPE_CHAR
				ix += 1
				c = s\sub ix, ix
				if esc = @@ESCAPES[c]
					table.insert seq, esc
				else
					table.insert seq, @@ESCAPE_CHAR
					table.insert seq, c
			else
				table.insert seq, c
			ix += 1
		@ok table.concat(seq), s\sub ix + 1

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

	DECL_INSN "EXC", (s) =>
		@ok { insn: "exc" }, s

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

	DECL_INSN "NOT", (s) =>
		reg = @p_reg s
		return @die "expected register", s unless reg
		@ok {
			insn: "logic"
			data:
				tbl: 0x3  -- the "not D" table
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

	DECL_INSN ".WS", (s) =>
		num = @p_num @trim s
		return @die "expected word size number", s unless num
		@set_word_size num.val
		@ok {insn: "nop"}, num.rest

	DECL_INSN ".ORG", (s) =>
		num = @p_num @trim s
		return @die "expected origin number", s unless num
		@origin = num.val
		@ok {insn: "nop"}, num.rest

	for name, op in pairs @ARITH
		DECL_INSN name, (s) =>
			dst = @p_reg s
			return @die "expected dest register", s unless dst
			s = @trim @optcomma dst.rest
			if src = @p_reg s
				s = @trim @optcomma src.rest
				return @ok {
					insn: "arith"
					data:
						:op
						src: src.val
						dst: dst.val
				}, s
			if srcimm = @p_imm s
				s = @trim @optcomma srcimm.rest
				return @ok {
					insn: "arith"
					data:
						:op
						src: srcimm.val
						dst: dst.val
						imm: true
				}, s
			@die "expected source register or immediate", s unless src

	for name, tbl in pairs @LOGIC
		DECL_INSN name, (s) =>
			dst = @p_reg s
			return @die "expected dest register", s unless dst
			s = @trim @optcomma dst.rest
			src = @p_reg s
			return @die "expected source register", s unless src
			s = @trim @optcomma src.rest
			@ok {
				insn: "logic"
				data:
					:tbl
					src: src.val
					dst: dst.val
			}, s

	for name, op in pairs @MISC
		DECL_INSN name, (s) =>
			dst = @p_reg s
			return @die "expected A register", s unless dst
			s = @trim @optcomma dst.rest
			src = @p_reg s
			return @die "expected B register", s unless src
			s = @trim @optcomma src.rest
			@ok {
				insn: "misc"
				data:
					:op
					src: src.val
					dst: dst.val
			}, s

	for name, op in pairs @SBR
		iname = "SBR." .. name
		DECL_INSN iname, (s) =>
			val = @p_reg s
			return @die "expected Value register", s unless val
			s = @trim @optcomma val.rest
			if cond = @p_reg s
				s = @trim @optcomma cond.rest
				return @ok {
					insn: "sbr"
					data:
						:op
						s: false
						c: cond.val
						v: val.val
				}, s
			if condimm = @p_imm s
				s = @trim @optcomma condimm.rest
				return @ok {
					insn: "sbr"
					data:
						:op
						s: true
						c: condimm.val
						v: val.val
				}, s
			@die "expected offset register or immediate", s unless src

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

	DECL_INSN "JNZ", (s) =>
		cmp = @p_reg s
		return @die "expected comp reg", s unless cmp
		s = @trim @optcomma cmp.rest
		off = @p_expr s
		return @die "expected offset", s unless off
		s = @trim @optcomma off.rest
		@ok {
			insn: "jnz"
			data:
				cmp: cmp.val
				offset: off.val
		}, s

	charif = (b, ch) ->
		return ch if b
		""

	charif2 = (b, ch1, ch2) ->
		ch1 if b else ch2

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

	DECL_EMIT "nop", (ins) =>

	DECL_EMIT "exc", (ins) => @emit 0

	DECL_EMIT "logic", (ins) => @e_logic ins.data

	DECL_EMIT "arith", (ins) => @e_arith ins.data

	DECL_EMIT "comp", (ins) => @e_comp ins.data

	DECL_EMIT "jnz", (ins) => @e_jnz ins.data

	DECL_EMIT "misc", (ins) => @e_misc ins.data

	DECL_EMIT "sbr", (ins) => @e_sbr ins.data

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
			@byteoff = fix.where
			@origin = fix.org
			val = @force fix.ref
			print 'fix ' .. fix.emit .. ' at ' .. @byteoff .. ' to ' .. val
			switch fix.emit
				when "word" then @emit_word val
				when "byte" then @emit_byte val
				when "smalli" then @e_smalli {dst: fix.dst, :val}
				when "arith" then @e_arith with fix.data
					.src = val
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
	assembler = Assembler 16
	assembler\run source
	assembler\info!
	f = io.open arg[1], "wb"
	for b in *assembler.buf
		print b
		f\write string.char b

{:Assembler}
