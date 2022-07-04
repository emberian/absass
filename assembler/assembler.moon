class Assembler
	new: (out) =>
		if type(out) == "string"
			out = io.open out, "wb"
		@out = out
		@labels = {}
		@cur_global = nil
		@origin = 0

	trim: (s) =>
		start = s\find "%S"
		return s\sub start if start
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

	emit: (ins) =>
		@out\write string.char((ins >> 8) & 255) .. string.char(ins & 255)
		@origin += 2

	@OPCODE:
		LOGIC: 0x1000
		ARITH: 0x2000
		COMP: 0x3000
		XFER: 0x4000
		COND: 0x8000
		JAL: 0x9000


	e_logic: (data) =>
		{:tbl, :src, :dst} = data
		@emit (@@OPCODE.LOGIC | ((tbl&0xf)<<8) | ((src&0xf)<<4) | (dst&0xf))

	p_reg: (s) =>
		s = @trim s
		{:word, :rest} = @word s

		return @ok 0, rest if word == "PC"

		if word\sub(1, 1) == "R"
			num = tonumber word\sub 2
			if num and num >= 0 and num < 16
				return @ok num, rest

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

	p_label: (s) =>
		s = @trim s
		lbl = @p_labelname s
		return @fail! unless lbl
		return @fail! if lbl.rest\sub(1, 1) ~= ":"
		@ok lbl.val, lbl.rest\sub 2

	@INSNS: {}
	DECL_INSN = (nm, parse) -> @INSNS[nm] = {
		:parse
	}

	p_insn: (s) =>
		s = @trim s
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
		print 'MOV src', src.val, 'dst', dst.val
		s = src.rest
		@ok {
			insn: "logic"
			data:
				tbl: 0xa  -- The "source" table
				src: src.val
				dst: dst.val
		}, s

	@EMITTERS: {}
	DECL_EMIT = (ins, emit) -> @EMITTERS[ins] = {
		:emit
	}

	DECL_EMIT "logic", (ins) => @e_logic ins.data

	set_label: (glob, loc, addr) =>
		unless glob
			unless @cur_global
				error "Can't define a local-only label before first global label"
			glob = @cur_global
		else
			@cur_global = glob
		loc = '' unless loc
		@labels[glob] = {} unless @labels[glob]
		@labels[glob][loc] = addr

	get_label: (glob, loc) =>
		unless glob
			unless @cur_global
				error "Can't reference local label before first global label"
			glob = @cur_global
		loc = '' unless loc
		@labels[glob][loc]

	one: (s) =>
		{:val, :rest} = @p_insn s
		print 'One val:', val
		-- Labels
		if val.label
			{global: glob, local: loc} = val.label
			@set_label glob, loc, @origin
		else
			einfo = @@EMITTERS[val.insn]
			error "No emitter for " .. val.insn unless einfo
			einfo.emit @, val
		rest

	run: (s) =>
		while #s > 0
			break if s\find("%S") == nil
			s = @one s
	
	info: =>
		print 'Labels:'
		for glob, sub in pairs @labels
			print '- ' .. glob .. ':'
			for loc, addr in pairs sub
				print '  - ' .. loc .. ' = ' .. tostring(addr)

source = io.read "*a"
assembler = Assembler "test.bin"
assembler\run source
assembler\info!

{:Assembler}
