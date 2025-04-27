BEGIN {
    CWD = ENVIRON["PWD"]

    LM = "#{"
    RM = "}#"
    re_magic = "^#%[\t ]*"
    re_skip = re_magic "(#|$)"
    re_empty = "^([+-]?0)?$"
    re_expand = LM "[^}]*" RM
    re_trim = "^[\t ]*|[\t ]*$"
    re_trimq = "^\"|\"$"
    re_qstr  = "\"([^\"]*(\"\")?[^\"]*)*\""
    re_ref = "@[{][0-9]+}"
    re_op = "[!=]=|[<>]=?|[!()]|&&|[|][|]"
    re_num = "[+-]?[0-9]+"
    re_var = "([A-Za-z]|[A-Za-z_][A-Za-z0-9_]+)"
    re_varname = "^" re_var "$"
    re_legal = re_op "|" re_num "|" re_var
    re_token = re_ref "|" re_legal

    rc = system("test -c /dev/stderr")
    if ( rc == 0 )	{ stderr = 1 ; errout = "/dev/stderr" }
    else		{ stderr = 0 ; errout = "cat >&2" }
    realpath = exec_cmd("command -v realpath")
}

BEGIN {
    M_N = N_M++ ; M_D = N_M++ ; M_E = N_M++ ; M_I = N_M++

    T_M["call"]		= M_I ; T_S["call"]	= -1
    T_M["data"]		= M_D ; T_S["data"]	= 1
    T_M["datablk"]	= M_D ; T_S["datablk"]	= -1
    T_M["define"]	= M_I ; T_S["define"]	= 1
    T_M["elif"]		= M_N ; T_S["elif"]	= 1
    T_M["else"]		= M_N ; T_S["else"]	= 1
    T_M["end"]		= M_N ; T_S["end"]	= 1
    T_M["endif"]	= M_N ; T_S["endif"]	= 1
    T_M["error"]	= M_I ; T_S["error"]	= 1
    T_M["eval"]		= M_N ; T_S["eval"]	= 1
    T_M["expand"]	= M_E ; T_S["expand"]	= 1
    T_M["expblk"]	= M_E ; T_S["expblk"]	= -1
    T_M["if"]		= M_N ; T_S["if"]	= 1
    T_M["include"]	= M_I ; T_S["include"]	= 1
    T_M["nop"]		= M_I ; T_S["nop"]	= 1
    T_M["ret"]		= M_N ; T_S["ret"]	= 0
    T_M["undef"]	= M_I ; T_S["undef"]	= 1
}

BEGIN {
    if ( ARGC > 2 ) errx("too many arguments")
    f = ARGV[1] != "" ? ARGV[1] : "-"
    if ( f != "-" && !check_file(f) ) errx("can't read file '" f "'")

    set_incpath()
    def_vars()
    undef_vars()

    process_file(f)
}

####

function def_vars(_, i, j, n, t, v, x)
{
    n = split(DEFLIST, t, "\n")
    for ( i = 1 ; i <= n ; i++ ) {
	j = index(t[i], "=")
	v = substr(t[i], 1, j-1) ; x = substr(t[i], j+1)
	gsub(re_trimq, "", x)
	if ( x !~ /^[+-]?[0-9]+$/ ) { x = defstring(x) }
	VARS[v] = x ; CVARS[v]++
    }
}

function undef_vars(_, i, n, t)
{
    n = split(UNDLIST, t, "\n")
    for ( i = 1 ; i <= n ; i++ ) { delete VARS[t[i]] }
}

function set_incpath(_, i, n, p, t)
{
    n = split(INCLIST, t, "\n")
    for ( i = 1 ; i <= n ; i++ ) {
	p = abs_path(t[i])
	if ( ! (p in X_INC) ) { T_INC[X_INC[p]=++N_INC] = p }
    }
}

####

function search_file(f, _, i, n, t)
{
    if ( FILE == "" ) { return abs_path(f) }
    if ( f ~ /^\// ) { t[n++] = f }
    else {
	if ( FILE == "<stdin>" ) { t[n++] = CWD "/" f }
	else { t[n++] = substr(FILE, 1, match(FILE, /\/[^\/]*$/)-1) "/" f }
	for ( i = 1 ; i <= N_INC ; i++ ) t[n++] = T_INC[i] "/" f
    }
    for ( i = 0 ; i < n ; i++ ) {
	t[i] = abs_path(t[i])
	if ( t[i] in BF ) { continue }
	if ( t[i] in AF || check_file(t[i]) ) { break } else { BF[t[i]]=1 }
    }
    if ( i == n ) { err("can't find include file '" f "'") }
    return t[i]
}

function process_file(f, af, cf)
{
    FILE = af = ( f == "-" ) ? "<stdin>" : search_file(f)
    if ( ! (af in AF) ) {
	cf = ( f == "-" ) ? af : canon_path(af)
	if ( ! (cf in CF) ) { CF[cf] = -1 }
	AF[af] = cf
    }
    cf = AF[af]
    if ( cf in XF ) { err("cyclic include of file '" af "'") } else { XF[cf]=1 }
    if ( CF[cf] == -1 ) {
	CF[cf] = N_P + 1
	read_file(cf)
    }
    execute(CF[cf])
    delete XF[cf]
}

function check_file(f)
{
    return !system("test -f '" f "' && test -r '" f "'")
}

function abs_path(p)
{
    if ( p !~ /^\// ) { p = CWD "/" p }
    if ( p ~ /\/($|\/|\.\.?(\/|$))/ ) {
	gsub(/\/(\/|(\.(\/|$)))+/, "/", p)
	while ( match(p, /(^|\/)\.\.(\/|$)/) ) {
	    sub(/^\/?(\.\.\/)+/, "/", p)
	    sub(/[^\/]*\/\.\.(\/|$)/, "", p)
	}
	sub(/\/$/, "", p)
    }
    return p ? p : "/"
}

function canon_path(p, _, cmd, rc, ret)
{
    if ( realpath == "" ) { return p }
    ret = exec_cmd(realpath " '" p "'")
    if ( ret == "" ) { errx("can't resolve path '" p "'") }
    return ret
}

function exec_cmd(cmd, _, ret)
{
    cmd = "2>/dev/null " cmd
    cmd | getline ret ; close(cmd)
    return ret
}

####

function errx(msg)
{
    if ( stderr )	{ print msg > errout }
    else		{ print msg | errout }
    exit 1
}

function err(msg)
{
    errx(FILE ":" LN ": " msg)
}

function err_empty(d)
{
    err("empty arguments for directive '" d "'")
}

function err_extra(d)
{
    err("extra arguments for directive '" d "'")
}

####

function _defstring(s, _, ret)
{
    if ( s in XSTR ) { ret = XSTR[s] }
    else { ret = "@{" N_STR++ "}" ; XSTR[s] = ret ; STR[ret] = s }
    return ret
}

function defstring(s)
{
    gsub(re_trimq, "", s)
    return _defstring(s)
}

function getvar(v)
{
    if ( v in VARS ) { v = VARS[v] ~ /@/ ? STR[VARS[v]] : VARS[v] } else { v = "" }
    return v
}

####

function read_file(f, _, ferr)
{
    if ( f == "<stdin>" ) { f = "-" }
    LN = 0 ; IFP = 0 ; mode = M_N
    while ( ++LN && (ferr = getline LINE < f) > 0 ) {
	if ( ! (LINE in X_D) ) { create_op() }
	proc_op(X_D[LINE])
    }
    if ( ferr < 0 ) { err("error reading file '" f "'") }
    close(f)
    if ( IFP > 0 ) {
	LN = J_LN[IFP]
	err("unterminated directive '" J_I[IFP] "'")
    }
    if ( mode != M_N ) { create_db() }
    if ( ! ("end" in X_I) ) { create_instr("end") }
    P_I[++N_P] = X_I["end"]
}

####

function check_expand(s)
{
    while ( match(s, re_expand) ) {
	if ( substr(s, RSTART+2, RLENGTH-4) !~ re_varname ) {
	    err("expand error '" substr(s, RSTART, RLENGTH) "'")
	}
	sub(re_expand, "", s)
    }
}

####

function create_instr(h, _, d, op, t, x)
{
    split(h, t, ":")
    op = t[1] ; d = t[2] ; x = t[3]
    I_OP[++N_I] = op ; I_D[N_I] = d ; I_M[N_I] = T_M[op]
    if ( T_S[op] < 0 ) { I_S[N_I] = x }
    else { I_X[N_I] = x ; I_S[N_I] = T_S[op] }
    X_I[h] = N_I
}

####

function create_op(_, h)
{
    if ( LINE ~ re_magic ) { h = proc_directive(LINE) }
    else {
	D[++N_D] = LINE
	h = ((LINE ~ re_expand) ? "expand" : "data") ":" N_D
    }
    if ( ! (h in X_I) ) { create_instr(h) }
    X_D[LINE] = X_I[h]
}

####

function proc_directive(s, _, d, n, t)
{
    if ( s ~ re_skip ) { return "nop" }

    sub(re_magic, "", s)
    match(s, /[\t ]|$/)
    d = substr(s, 1, RSTART-1)
    s = tokenize(substr(s, RSTART))

    if ( s == "" && d !~ /^(else|endif)$/ )	{ err_empty(d) }
    if ( d !~ /^(el)?if$/ ) { n = split(s, t, " ") }

    if ( d ~ /^(el)?if(n?def)?$/ ) {
	if ( d ~ /^(el)?ifn?def$/ ) {
	    if ( n > 1 )			{ err_extra(d) }
	    if ( s !~ re_varname )		{ err("illegal variable name '" s "'") }
	    s = (d ~ /ifndef/ ? "! " : "") "defined " s
	    d = (d ~ /^el/) ? "elif" : "if"
	}
	if ( ! (s in X_X) ) { parse(s) }
	s = X_X[s]
    } else if ( d ~ /^(else|endif)$/ ) {
	if ( n > 0 )				{ err_extra(d) }
    } else if ( d == "define" ) {
	if ( n > 2 )				{ err_extra(d) }
	if ( t[1] !~ re_varname )		{ err("illegal variable name '" t[1] "'") }
	if ( n == 1 ) { t[2] = 1 }
	else if ( t[2] !~ re_num && t[2] !~ re_ref )	{ err("illegal argument in directive '" d "'") }
	s = t[1] ":" t[2]
    } else if ( d == "error" ) {
	if ( n > 1 )				{ err_extra(d) }
	if ( s !~ re_ref )			{ err("illegal argument in directive '" d "'") }
    } else if ( d == "include" ) {
	if ( n > 1 )				{ err_extra(d) }
	if ( s !~ re_ref && s !~ re_varname )	{ err("illegal variable name '" s "'") }
    } else if ( d == "undef" ) {
	if ( n > 1 )				{ err_extra(d) }
	if ( s !~ re_varname )			{ err("illegal variable name '" s "'") }
    } else					{ err("unknown directive '" d "'") }

    return d ":" s
}

####

function create_db(_, d, h, op, s)
{
    if ( C_DB > 1 ) {
	if ( ! (H_DB in X_DB) ) {
	    if ( mode == M_D || mode == M_E ) {
		DB[++N_DB] = H_DB
		d = (mode == M_D) ? "datablk" : "expblk" ; s = N_DB ":" C_DB
	    } else {
		d = "call" ; s = compile_sub(H_DB)
	    }
	    h = d ":" s
	    if ( ! (h in X_I) ) { create_instr(h) }
	    X_DB[H_DB] = X_I[h]
	}
    }
    op = (C_DB == 1) ? H_DB : X_DB[H_DB]
    if ( IFP == 0 ) { P_I[++N_P] = op }
    else { H_IF = H_IF "@" op }
}

####

function compile_sub(h, _, i, ln, n, op, pl, pp, ret, t)
{
    ret = N_PP + 1 ; pp = ln = 0
    n = split(h, t, "@")
    for ( i = 1 ; i <= n ; i++ ) {
	op = t[i]
	if ( op ~ /^[0-9]/ ) {
	    PP_I[++N_PP] = op
	    ln += I_S[op]
	} else {
	    if ( op ~ /^(else|endif)/ ) { op = "nop" }
	    if ( ! (op in X_I) ) { create_instr(op) }
	    PP_I[++N_PP] = X_I[op]
	    if ( op ~ /^(eval|nop)/ ) {
		if ( pp != 0 ) { PP_X[pp] = N_PP - pp ; PP_L[pp] = ln - pl }
		if ( op != "nop" ) { pp = N_PP ; pl = ln } else { pp = 0 }
	    }
	    ln += I_S[X_I[op]]
	}
    }
    if ( ! ("ret" in X_I) ) { create_instr("ret") }
    PP_I[++N_PP] = X_I["ret"]
    return ret ":" ln
}

####

function proc_op(op, _, d, h, m)
{
    m = I_M[op]
    if ( m == mode && m != M_N ) {
	H_DB = H_DB (C_DB++ ? "@" : "") op
    } else {
	if ( mode != M_N ) { create_db() }
	mode = m ; C_DB = 0 ; H_DB = ""
	if ( mode != M_N ) {
	    H_DB = H_DB (C_DB++ ? "@" : "") op
	} else {
	    d = I_OP[op]
	    if ( d ~ /^e/ ) {
		if ( IFP == 0 )				{ err("'" d "' without 'if'") }
		if ( d ~ /^el/ && J_I[IFP] ~ /else/ )	{ err("'" d "' after 'else'") }
	    }
	    if ( d == "if" ) {
		J_I[++IFP] = d ; J_LN[IFP] = LN ; J_HIF[IFP] = H_IF
		H_IF = "eval:" I_D[op]
	    } else if ( d == "elif" ) {
		J_I[IFP] = d ; J_LN[IFP] = LN
		H_IF = H_IF "@ret@eval:" I_D[op]
	    } else if ( d == "else" ) {
		J_I[IFP] = d ; J_LN[IFP] = LN
		H_IF = H_IF "@ret@else"
	    } else if ( d == "endif" ) {
		H_IF = H_IF "@endif"
		if ( ! (H_IF in X_DB) ) {
		    h = "call:" compile_sub(H_IF)
		    if ( ! (h in X_I) ) { create_instr(h) }
		    X_DB[H_IF] = X_I[h]
		}
		op = X_DB[H_IF]
		if ( IFP > 1 ) { H_IF = J_HIF[IFP] "@" op }
		else { P_I[++N_P] = op }
		IFP--
	    }
	}
    }
}

####

function expand(s, _, val, var, xvars)
{
    while ( match(s, re_expand) ) {
	var = substr(s, RSTART+2, RLENGTH-4)
	if ( var !~ re_varname ) {
	    err("expand error: illegal variable name '" var "'")
	} else if ( var in xvars ) {
	    err("expand error: recursion for variable '" var "'")
	}
	val = getvar(var)
	if ( val ~ re_expand ) { xvars[var]=1 }
	gsub(LM var RM, val, s)
    }
    return s
}

####

function execute(pc, _, d, ev, i, iop, n, op, pp, t)
{
    LN = 1 ; pp = 0
    while ( 1 ) {
	if ( pp == 0 ) { iop = P_I[pc] }
	else { iop = PP_I[pc] }
	op = I_OP[iop] ; d = I_D[iop]
	if ( op == "data" ) { print D[d] }
	else if ( op == "datablk" ) {
	    n = split(DB[d], t, "@")
	    for ( i = 1 ; i <= n ; i++ ) { print D[I_D[t[i]]] }
	}
	else if ( op == "call" ) {
	    S_D[++DSP] = iop ; S_D[++DSP] = LN
	    S_R[++RSP] = pc ; pc = d ; pp++
	    continue
	}
	else if ( op == "ret" ) {
	    LN = S_D[DSP--] ; iop = S_D[DSP--]
	    pc = S_R[RSP--] ; pp--
	}
	else if ( op == "eval" ) {
	    ev = 0
	    if ( XV[d] == 0 && XR[d] == -1 ) { ev = 1 }
	    else {
		n = 0
		for ( i = 1 ; i <= XV[d] ; i++ ) { n += CVARS[XV[d,i]] }
		if ( n != XV[d,0] ) { ev = 1 ; XV[d,0] = n }
	    }
	    if ( ev ) { XR[d] = (evaluate(d) ~ re_empty) ? 0 : 1 }
	    if ( XR[d] == 0 ) {
		LN += PP_L[pc] ; pc += PP_X[pc]
		continue
	    }
	}
	else if ( op == "include" ) {
	    if ( d ~ /@/ ) { d = STR[d] } else { d = expand(LM d RM) }
	    S_D[++DSP] = pc ; S_D[++DSP] = FILE ; S_D[++DSP] = LN ; S_D[++DSP] = pp
	    process_file(d)
	    pp = S_D[DSP--] ; LN = S_D[DSP--] ; FILE = S_D[DSP--] ; pc = S_D[DSP--]
	}
	else if ( op == "expand" ) { print expand(D[d]) }
	else if ( op == "expblk" ) {
	    n = split(DB[d], t, "@")
	    for ( i = 1 ; i <= n ; i++ ) { print expand(D[I_D[t[i]]]) }
	}
	else if ( op == "define" ) {
	    if ( VARS[d] != I_X[iop] ) { VARS[d] = I_X[iop] ; CVARS[d]++ }
	}
	else if ( op == "error" ) { err("'error' directive: '" STR[d] "'") }
	else if ( op == "undef" ) {
	    if ( d in VARS ) { delete VARS[d] ; CVARS[d]++ }
	}
	else if ( op == "nop" ) { }
	else if ( op == "end" ) { break }
	else { errx("unknown operation '" op "'") }
	pc++ ; LN += I_S[iop]
    }
}

####

function tokenize(e, _, s)
{
    s = e ; gsub(re_qstr, "", s) ; gsub(re_legal, " ", s)
    if ( s ~ /"/ )		{ err("unterminated string token") }
    if ( match(s, /[^\t ]+/) )	{ err("unrecognized token '" substr(s, RSTART, RLENGTH) "'") }
    while ( match(e, re_qstr) ) {
	s = defstring(substr(e, RSTART, RLENGTH))
	sub(re_qstr, s, e)
    }
    gsub(re_token, " &", e)
    gsub(/[\t ]+/, " ",  e)
    gsub(re_trim,  "",   e)
    return e
}

####

function categorize(t, n, _, ct, par, pt)
{
    par = 0 ; pt = ""
    for ( i = 1 ; i <= n ; i++ ) {
	if      ( t[i] == "defined" )		{ t[i,"type"] = "D" }
	else if ( t[i] == "!" )			{ t[i,"type"] = "B" }
	else if ( t[i] == "&&" )		{ t[i,"type"] = "A" }
	else if ( t[i] == "||" )		{ t[i,"type"] = "O" }
	else if ( t[i] ~ /[!=]=|[<>]=?/ )	{ t[i,"type"] = "C" }
	else if ( t[i] ~ /@[{][0-9]+}/ )	{ t[i,"type"] = "S" }
	else if ( t[i] ~ /^[+-]?[0-9]+$/ )	{ t[i,"type"] = "N" }
	else if ( t[i] ~ /[A-Za-z0-9_]+/ )	{ t[i,"type"] = "V" }
	else if ( t[i] == "(" )			{ t[i,"type"] = "l" ; par++ }
	else if ( t[i] == ")" )			{ t[i,"type"] = "r" ; par-- }
	else					{ err("unrecognized token '" t[i] "'") }
	ct = t[i,"type"]
	if ( par < 0 )				{ err("missing '('") }
	if ( ct == "V" && t[i] !~ re_varname )	{ err("illegal variable name '" t[i] "'") }
	if ( pt ct ~ /lr$/ )			{ err("missing expression in '()'") }
	if ( pt ct ~ /[NSVr][NSVBDl]$/ )	{ err("missing operator before token '" t[i] "'") }
	if ( pt ct ~ /(^|l)[ACO]$/ )		{ err("no left operand for '" t[i] "'") }
	if ( pt ct ~ /[ABCDO][ACOr]$/ )		{ err("no right operand for '" t[i-1] "'") }
	if ( pt ct ~ /D[^Vl]$/ )		{ err("operator 'defined' requires variable") }
	pt = pt ct
    }
    if ( pt ~ /D/ && pt !~ /DV|DlVr/ )		{ err("operator 'defined' requires variable") }
    if ( pt ~ /[ABCDO]$/ )			{ err("no right operand for '" t[n] "'") }
    if ( par > 0 )				{ err("missing ')'") }
}

####

function parse(e, _, c, i, l, n, r, t, xvars)
{
    n = split(e, t, " ")
    categorize(t, n)
    c = LXT+0 ; r = l = 0
    for ( i = 1 ; i <= n ; i++ ) {
	if ( t[i,"type"] == "l" ) {
	    S_D[++DSP] = r ; S_D[++DSP] = l ; r = l = 0 ; continue
	} else if ( t[i,"type"] == "r" ) {
	    l = S_D[DSP--]
	    if ( l != 0 ) { XT[l,"r"] = r ; r = S_D[DSP] ; l = 0 }
	    DSP-- ; continue
	}
	XT[++c,"t"] = t[i,"type"] ; XT[c,"v"] = t[i] ; XT[c,"l"] = XT[c,"r"] = 0
	if ( r == 0 )					{ r = c }
	else if ( XT[c,"t"] ~ /[NSVBD]/ )		{ XT[l,"r"] = c }
	else if ( XT[r,"t"] XT[c,"t"] ~ /OA|[AO]C/ )	{ XT[c,"l"] = XT[r,"r"] ; XT[r,"r"] = c }
	else if ( XT[r,"t"] XT[c,"t"] ~ /[^O]A|[CO]$/ )	{ XT[c,"l"] = r ; r = c }
	if ( XT[c,"t"] == "V" && ! ( t[i] in xvars ) )	{
	    xvars[t[i]] = 1
	    if ( ! (t[i] in CVARS) ) { CVARS[t[i]]++ }
	}
	l = c
    }
    XV[r] = 0 ; XR[r] = -1
    for ( i in xvars ) { XV[r,++XV[r]] = i }
    LXT = c ; X_X[e] = r
}

####

function evaluate(n, _, t, v, l, r, res1, res2, ret)
{
    t = XT[n,"t"] ; v = XT[n,"v"] ; l = XT[n,"l"] ; r = XT[n,"r"]
    if ( t ~ /[NS]/ ) {
	ret = (t == "N") ? v : STR[v]
    } else if ( t == "V" ) {
	ret = getvar(v)
    } else if ( t == "B" ) {
	res2 = evaluate(r)
	ret = (res2 ~ re_empty) ? 1 : 0
    } else if ( t == "D" ) {
	ret = (XT[r,"v"] in VARS) ? 1 : 0
    } else if ( t ~ /[AO]/ ) {
	res1 = evaluate(l) ; ret = (res1 ~ re_empty) ? 0 : 1
	if ( (t == "A" && ret == 1) || (t == "O" && ret == 0) ) {
	    res2 = evaluate(r) ; ret = (res2 ~ re_empty) ? 0 : 1
	}
    } else if ( t == "C" ) {
	res1 = evaluate(l) ; res2 = evaluate(r)
	if      ( v == "==" ) { ret = (res1 == res2) ? 1 : 0 }
	else if ( v == "!=" ) { ret = (res1 != res2) ? 1 : 0 }
	else if ( v == ">"  ) { ret = (res1 >  res2) ? 1 : 0 }
	else if ( v == "<"  ) { ret = (res1 <  res2) ? 1 : 0 }
	else if ( v == ">=" ) { ret = (res1 >= res2) ? 1 : 0 }
	else if ( v == "<=" ) { ret = (res1 <= res2) ? 1 : 0 }
    }
    return ret
}
