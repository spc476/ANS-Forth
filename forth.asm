;**********************************************************************
;   ANS Forth 2012 for the 6809 CPU
;   Copyright (C) 2025 by Sean Conner.
;
;   This program is free software: you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation, either version 3 of the License, or
;   (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   You should have received a copy of the GNU General Public License
;   along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;   Comments, questions and criticisms can be sent to: sean@conman.org
;
;**********************************************************************
;
; Forth word:
;	.next		pointer to next word in dictionary
;	.length		16b lenth of word
;				bit 15 - immediate word
;				bit 14 - hidden word
;				bit 13 - no interpretation semantics
;				bit 12 - double variable (required for TO)
;				bit 11 - local variable (required for TO)
;	.text		array of characters, length in size
;	.xt		code pointer to execute when running word
;	.body		code/xt array of word
;
; colon-sys	xt of word being compiled
; do-sys	u-dest c-orig (on stack)
; case-sys
; of-sys
; orig
; dest
; loop-sys	max-value index-value
; nest-sys	return IP within .body
; nt		points to the .next field of a word (15.3.1)
; wid		pointer to last word in dictionary (16.3.1)
;
; CPU register usage:
;	D	free for use
;	X	xt of word being executed/free for use
;	Y	Forth IP
;	S	return stack
;	U	data stack
;
; Wordlists implemented:
;	CORE		CORE-EXT
;	DOUBLE		DOUBLE-EXT
;	EXCEPTION	EXCEPTION-EXT
;	LOCAL		LOCAL-EXT
;	TOOLS		TOOLS-EXT (except ;CODE ASSEMBLER CODE EDITOR FORGET SYNONYM)
;	SEARCH		SEARCH-EXT
;	STRING		STRING-EXT
;
; Wordlists not implemented:
;	BLOCK		BLOCK-EXT	; system/OS dependent
;	FACILITY	FACILITY-EXT	; system/OS dependent
;	FILE		FILE-EXT	; system/OS dependent
;	FLOATING	FLOATING-EXT	; CPU/software dependent
;	MEMORY				; not worth it with 64K
;	XCHAR		XCHAR-EXT	; not worth it with 64k
;
; Minimum size of terminal-input buffer is 80 (3.3.3.5)
; Maximum size of definition name is 31 (3.3.1.2)
; Double cells are big endian (3.1.4.1)
; Numeric conversion (3.4.1.3)
;	#[-]<decdigit>1*
;	$[-]<hexdigit>1*
;	%[-]<bindigit>1*
;	'<char>'
;
; Continous Regions
;	HERE	- ALLOT , C, COMPILE, ALIGN CREATE 
;	SOURCE	- minimum for terminal input 80 characters
; Transient regions - may change after : :NONAME ALLOT , C, ALIGN
;	PAD		at least 84				( HERE )
;	WORD		at least 33 (can overlap with #>)	( HERE + /PAD )
;	#>		2*n + 2 (n = #bits in cell)		( HERE + /PAD + WORD_MAX )
;		locals	16 * (2 + 2 + 31 + 2 + 2)		( here_top - size )
;
; Also: https://forth-standard.org/proposals/clarify-find
;	What is Non-Default Compilation Semantics (NDCS)?
;
; THROW values defined by this system:
;	-256	Bad values given to ACCEPT
;
; Misc notes:
;	Code can be placed in ROM
;
;	Do nothing words
;		CHARS
;		ALIGNED
;		ALIGN
;
;	Synonyms
;		ALIGN	CHARS
;		ALIGNED	CHARS
;		CHAR+	1+
;		CS-PICK	PICK
;		CS-ROLL	ROLL
;		D>S	DROP
;
;**********************************************************************

INPUT_SIZE	equ	80		; per 3.3.3.5
SLASH_HOLD	equ	(16 * 2) + 2	; per 3.3.3.6
SLASH_PAD	equ	84		; per 3.3.3.6
NUMBER_LOCALS	equ	16		; per 13.4.2.1
NUMBER_LISTS	equ	8		; per 16.6.1.2197
DEFINITION_MAX	equ	31		; per 3.3.1.2
WORD_MAX	equ	33		; per 3.3.3.6
NL		equ	10		; system dependent NL character

_IMMED		equ	$80		; words executed during compilation
_HIDDEN		equ	$40		; hide ':' defs until ';'
_NOINTERP	equ	$20		; word sans interpretation semantics
_DOUBLE		equ	$10		; TO double
_LOCAL		equ	$08		; TO local_var

	;------------------------------------------------------------------
	; The following should be initialized by the system to appropriate
	; values.  Once set, these should never change---they are read only
	; values.
	;------------------------------------------------------------------

forth__vector_bye	fdb	0	; return to operating system
forth__vector_getchar	fdb	0	; read a character  - Exit:  D - character, all others saved
forth__vector_putchar	fdb	0	; write a character - Entry: D - character, all others saved
forth__ds_bottom	fdb	0	; data stack bottom
forth__ds_top		fdb	0	; data stack top
forth__rs_bottom	fdb	0	; return stack bottom
forth__rs_top		fdb	0	; return stack top
forth__here_top		fdb	0	; here area top

	;------------------------------------------
	; Variables used by the Forth system
	;------------------------------------------

forth__here		fdb	forth__free
forth__forth_wid	fdb	forth_string_ext_unescape
forth__string_wid	fdb	0	; REPLACES SUBSTITUTE
forth__current_wid	fdb	forth__forth_wid
forth__state		fdb	0
forth__in		fdb	0
forth__base		fdb	10
forth__source_id	fdb	0
forth__source		fdb	0	; SOURCE buffer
forth__source_len	fdb	0	; SOURCE length
forth__create_link	fdb	0	; CREATE SEE
forth__create_name	fdb	0	; CREATE SEE
forth__create_xt	fdb	0	; CREATE SEE
forth__hold		fdb	0	; <# HOLD #>	#> transient region
forth__local_link	fdb	0	; (LOCAL)	copy of forth__create_link
forth__local_name	fdb	0	; (LOCAL)	copy of forth__create_name
forth__local_xt		fdb	0	; (LOCAL)	copy of forth__create_xt
forth__local_fp		fdb	0	; (LOCAL)	ptr to local data
forth__local_fps	fdb	0	; (LOCAL)	# of bytes of local data
forth__local_current	fdb	0	; (LOCAL)	saved compile wordset
forth__local_wid	fdb	0	; (LOCAL)	local wordset (private)
forth__local_e_cnt	fcb	0	; (LOCAL)	# bytes on entry
forth__local_l_cnt	fcb	0	; (LOCAL)	# bytes on leaving
forth__local_here	fdb	0	; (LOCAL)	saved HERE location
forth__nr_storage	fdb	0	; N>R NR>
forth__handler		fdb	0	; THROW CATCH
forth__abortq		fdb	0	; THROW CATCH ABORT"
forth__abortql		fdb	0	; THROW CATCH ABORT"
forth__widnum		fdb	1	; SEARCH wordset
forth__widlist		fdb	forth__forth_wid
			rmb	(NUMBER_LISTS - 1) * 2
forth__leave_sp		fdb	0
forth__leave_stack	fdb	0,0,0,0,0,0,0,0

;**********************************************************************
;	forth__math_neg32	negate a 32 bit value
;Entry:	0,X - 32 bit value
;Exit:	0,X - negated 32 bit value
;**********************************************************************

forth__math_neg32
		com	3,x
		com	2,x
		com	1,x
		com	0,x
		ldd	2,x
		addd	#1
		std	2,x
		ldd	0,x
		adcb	#0
		adca	#0
		std	0,x
		rts

;**********************************************************************
;	forth__math_mul16	unsigned 16 bit multiple, 32 bit result
;Entry:	2,X - n1
;	0,X - n2
;Exit:	2,X - LSW
;	0,X - MSW
;**********************************************************************

Pd		set	3
Pc		set	2
Pb		set	1
Pa		set	0

forth__math_mul16
		leas	-8,s
		lda	Pb,x
		ldb	Pd,x
		mul
		std	6,s
		lda	Pa,x
		ldb	Pd,x
		mul
		std	4,s
		lda	Pb,x
		ldb	Pc,x
		mul
		std	2,s
		lda	Pa,x
		ldb	Pc,x
		mul
		std	,s
		clr	1,x
		clr	,x
		ldd	6,s
		std	2,x
		ldd	4,s
		addd	1,x
		std	1,x
		ldd	2,s
		addd	1,x
		std	1,x
		bcc	.next
		inc	,x
.next		ldd	,s
		addd	,x
		std	,x
		leas	8,s
		rts

	;----------------------------

	.opt	test	stack $E000
	.opt	test	stacksize 1024
	.opt	test	org $E000

	.test	"16 * 16"
		ldx	#.stack1
		jsr	forth__math_mul16
	.assert	/x     = .stack1 , "X"
	.assert	@@/0,x = $121C   , "MSW"
	.assert	@@/2,x = $3C5D   , "LSW"
		rts

.stack1		fdb	12097
		fdb	25117
	.endtst

	;---------------------------

	.test	"16 * 16 max"
		ldx	#.stack2
		clra
		clrb
		jsr	forth__math_mul16
		ldd	,x
		ldd	2,x
	.assert	/x     = .stack2 , "X"
	.assert @@/0,x = $FFFE   , "MSW"
	.assert	@@/2,x = $0001   , "LSW"
		rts

.stack2		fdb	$FFFF
		fdb	$FFFF
	.endtst

;**********************************************************************
;	forth__math_div32	unsigned 32 bit / unsiged 16 bit
;Entry:	2,X - 32 bit numerator
;	0,X - 16 bit demoninator
;Exit:	4,x - remainder
;	2,x - quotient
;	0,x - trashed
;**********************************************************************

forth__math_div32
		lda	#32
		pshs	a
		clra
		clrb
		pshs	d
.10		lsl	5,x
		rol	4,x
		rol	3,x
		rol	2,x
		rolb
		rola
		bcs	.15
		cmpd	,x
		blo	.20
.15		subd	,x
		orcc	{c}
		bra	.30
.20		andcc	{c}
.30		rol	1,s
		rol	,s
		dec	2,s
		bne	.10
		std	4,x	; save remainder
		puls	d	; quotient
		std	2,x
		leas	1,s
		rts

	;-----------------------------------

	.test	"DIV32"
		ldx	#.parms
		jsr	forth__math_div32
	.assert	/x     = .parms , "X"
	.assert	@@/4,x = 5      , "r"
	.assert	@@/2,x = 6553   , "q"
		rts

.parms		fdb	10
		fdb	0
		fdb	65535
	.endtst

	;---------------------------------

	.test	"DIV32 max-int"
		ldx	#.parms2
		jsr	forth__math_div32
	.assert	/x     = .parms2 , "X"
	.assert	@@/4,x = 0       , "r"
	.assert	@@/2,x = $7FFF   , "q"
		rts

.parms2		fdb	$7FFF
		fdb	$3FFF
		fdb	$0001
	.endtst

	;---------------------------------

	.test	"DIV32 max"
		ldx	#.parms3
		jsr	forth__math_div32
	.assert	/x     = .parms3 , "X"
	.assert	@@/4,x = 0       , "r"
	.assert	@@/2,x = $FFFF   , "q"
		rts

.parms3		fdb	$FFFF
		fdb	$FFFE
		fdb	$0001
	.endtst

;**********************************************************************
;	forth__util_xt_to_name	Return the nt from an xt
;Entry:	X - xt
;Exit:	X - nt
;	A - flags
;
; Return the name of a word based on the xt.  All names in Forth are ASCII
; graphic characters, and all are less than 31 characters.  There are no
; spaces in any Forth word, so once we get to a byte less than an ASCII
; SPACE, we've found the length byte.  The flag byte is in the previous
; byte.
;**********************************************************************

forth__util_xt_to_name
		lda	,-x
		cmpa	#' '
		bls	.found_name
		bra	forth__util_xt_to_name
.found_name	lda	,-x		; grab flags, X points to lengt field
		rts

;**********************************************************************

forth__util_check_ds
		cmpu	forth__ds_bottom
		blo	.throw_low
		cmpu	forth__ds_top
		bhi	.throw_high
		rts
.throw_low	ldd	#-3
		lbra	forth_exception_throw.asm
.throw_high	ldd	#-4
		lbra	forth_exception_throw.asm

;**********************************************************************

forth__util_check_rs
		cmps	forth__rs_bottom
		blo	.throw_low
		cmps	forth__rs_top
		bhi	.throw_high
		rts
.throw_low	ldd	#-5
		lbra	forth_exception_throw.asm
.throw_high	ldd	#-6
		lbra	forth_exception_throw.asm

;**********************************************************************

forth__private_check_stacks_xt
		fdb	.body
.body		bsr	forth__util_check_ds
		bsr	forth__util_check_rs
		ldx	,y++
		jmp	[,x]

;**********************************************************************
		
forth__private_create_quote_xt		; ( c-addr u -- )
		fdb	.body
.body		ldd	,u			; check length
		bmi	.throw_0name		; < 1 throw error
		beq	.throw_0name
		cmpd	#DEFINITION_MAX		; > 31 throw error
		bgt	.throw_bigname
		ldx	forth__here
		stx	forth__create_link	; save link field
		ldd	[forth__current_wid]	; get previous word
		stx	[forth__current_wid]	; save new word into current wordlist
		std	,x++			; link to previous word
		stx	forth__create_name	; save pointer to text
		pshs	y
		pulu	y,d			; get c-addr u
		std	,x++			; save length
.copy		lda	,y+			; copy text into definition
		sta	,x+
		decb
		bne	.copy
		stx	forth__create_xt	; save pointer to xt
		ldd	#forth_core_create.runtime ; intialize xt
		std	,x++
		stx	forth__here
		puls	y
		ldx	,y++
		jmp	[,x]

.throw_0name	ldd	#-16
		lbra	forth_exception_throw.asm
.throw_bigname	ldd	#-19
		lbra	forth_exception_throw.asm

;**********************************************************************

forth__private_eval_xt		; ( i*x -- j*x )
		fdb	forth_core_colon.runtime
	;====================================================
	; : eval
	;	BEGIN BL WORD DUP C@ WHILE
	;		FIND STATE @ IF
	;			eval-compile
	;		ELSE
	;			eval-interpret
	;		THEN check_stacks
	;	REPEAT DROP ;
	;====================================================
.L1		fdb	forth_core_b_l.xt
		fdb	forth_core_word.xt
		fdb	forth_core_dupe.xt
		fdb	forth_core_c_fetch.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L8
		fdb	forth_search_find.xt
		fdb	forth_tools_ext_state.xt
		fdb	forth_core_fetch.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L4
		fdb	forth__private_eval_compile_xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L7
.L4		fdb	forth__private_eval_interpret_xt
.L7		fdb	forth__private_check_stacks_xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L1
.L8		fdb	forth_core_drop.xt	
		fdb	forth_core_exit.xt

	;-----------------------------------------------

	.test	"eval 1 2 +"
	.opt	test	prot	rw , $6000 , $6100
	.opt	test	prot	n , .nu11
	.opt	test	prot	n , .nu12
	.opt	test	pokew	forth__source     , .buffer1
	.opt	test	pokew	forth__source_len , .len1
	.opt	test	pokew	forth__in         , 0
	.opt	test	pokew	forth__state      , 0
	.opt	test	pokew	forth__here       , $6000
	.opt	test	pokew	forth__ds_bottom  , .dsbot1
	.opt	test	pokew	forth__ds_top     , .datastack1 + 2
		leax	2,s
		stx	forth__rs_top
		leax	-256,x
		stx	forth__rs_bottom
		ldu	#.datastack1
		ldx	#forth__private_eval_xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result1 , "U"
	.assert	@@/0,u = 3        , "result"
		rts

.dsbot1		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.result1	fdb	0
.datastack1	fdb	0

.nu11		fcb	0
.buffer1	fcc	'1 2 +'
.len1		equ	* - .buffer1
.nu12		fcb	0
	.endtst

	;------------------------------------------------

	.test	"eval BYE"
	.opt	test	prot	rw , $6000 , $6100
	.opt	test	prot	n , .nu21
	.opt	test	prot	n , .nu22
	.opt	test	pokew	forth__source     , .buffer2
	.opt	test	pokew	forth__source_len , .len2
	.opt	test	pokew	forth__in         , 0
	.opt	test	pokew	forth__state      , 0
	.opt	test	pokew	forth__here       , $6000
	.opt	test	pokew	forth__vector_bye , .bye
		sts	.ret
		ldu	#.datastack2
		ldx	#forth__private_eval_xt
		jsr	forth_core_execute.asm
	.assert	1 = 2 , "wrong return"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack2	fdb	0

.nu21		fcb	0
.buffer2	fcc	'BYE '
.len2		equ	* - .buffer2
.nu22		fcb	0

.bye		lds	.ret
	.assert	/u = .datastack2 , "U ( bye )"
		rts
.ret		fdb	0

	.endtst

	;------------------------------------------------

	.test	"eval ( spaces )"
	.opt	test	prot	rw , $6000 , $6100
	.opt	test	prot	n , .nu31
	.opt	test	prot	n , .nu32
	.opt	test	pokew	forth__source     , .buffer3
	.opt	test	pokew	forth__source_len , .len3
	.opt	test	pokew	forth__in         , 0
	.opt	test	pokew	forth__state      , 0
	.opt	test	pokew	forth__here       , $6000
		ldu	#.datastack3
		ldx	#forth__private_eval_xt
		jsr	forth_core_execute.asm
	.assert	/u = .datastack3 , "U"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack3	fdb	0

.nu31		fcb	0
.buffer3	fcc	'    '
.len3		equ	* - .buffer3
.nu32		fcb	0
	.endtst

	;------------------------------------------------

	.test	"eval ( empty string )"
	.opt	test	prot	rw , $6000 , $6100
	.opt	test	prot	n , .nu41
	.opt	test	prot	n , .nu42
	.opt	test	pokew	forth__source     , .buffer4
	.opt	test	pokew	forth__source_len , .len4
	.opt	test	pokew	forth__in         , 0
	.opt	test	pokew	forth__state      , 0
	.opt	test	pokew	forth__here       , $6000
		ldu	#.datastack4
		ldx	#forth__private_eval_xt
		jsr	forth_core_execute.asm
	.assert	/u = .datastack4 , "U"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack4	fdb	0

.nu41		fcb	0
.buffer4	fcb	-1
.len4		equ	0
.nu42		fcb	0
	.endtst

;**********************************************************************

forth__private_eval_compile_xt
		fdb	forth_core_colon.runtime
	;=================================================
	; : eval_compile
	; ( 1 )	IF
	; ( 2 )		immediate? IF EXECUTE ELSE COMPILE, THEN
	; ( 3 )	ELSE
	; ( 4 )		number? IF CASE
	; ( 5 )			1 OF POSTPONE LITERAL ENDOF
	; ( 6 )			2 OF POSTPONE 2LITERAL ENDOF
	; ( 7 )			ENDCASE
	; ( 8 )		ELSE
	; ( 9 )			-13 THROW
	; ( 10 )	THEN
	; ( 11 ) THEN ;
	;==================================================
		fdb	forth_core_if.runtime_xt
		fdb	.numq
		fdb	forth__private_immediate_q_xt
		fdb	forth_core_if.runtime_xt
		fdb	.comp
		fdb	forth_core_execute.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.exit
.comp		fdb	forth_core_ext_compile_comma.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.exit
.numq		fdb	forth__private_number_q_xt
		fdb	forth_core_if.runtime_xt
		fdb	.throw
		fdb	forth_core_literal.runtime_xt
		fdb	1
		fdb	forth_core_ext_of.runtime_xt
		fdb	.compd
		fdb	forth_core_literal.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.exit
.compd		fdb	forth_core_literal.runtime_xt
		fdb	2
		fdb	forth_core_ext_of.runtime_xt
		fdb	.default
		fdb	forth_double_two_literal.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.exit
.default	fdb	forth_core_drop.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.exit
.throw		fdb	forth_core_literal.runtime_xt
		fdb	-13
		fdb	forth_exception_throw.xt
.exit		fdb	forth_core_exit.xt

;**********************************************************************

forth__private_eval_interpret_xt
		fdb	forth_core_colon.runtime
	;===========================================================
	; : eval-interpret
	; ( 1 )	IF
	; ( 2 )		interpret? IF EXECUTE ELSE -14 THROW THEN
	; ( 3 )	ELSE
	; ( 4 )		number? IF DROP ELSE -13 THROW THEN
	; ( 5 )	THEN ;
	;===========================================================
		fdb	forth_core_if.runtime_xt
		fdb	.L3
		fdb	forth__private_interpret_q_xt
		fdb	forth_core_if.runtime_xt
		fdb	.L2
		fdb	forth_core_execute.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L5
.L2		fdb	forth_core_literal.runtime_xt
		fdb	-14
		fdb	forth_exception_throw.xt	; doesn't return
.L3		fdb	forth__private_number_q_xt
		fdb	forth_core_if.runtime_xt
		fdb	.L4
		fdb	forth_core_drop.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L5
.L4		fdb	forth_core_literal.runtime_xt
		fdb	-13
		fdb	forth_exception_throw.xt
.L5		fdb	forth_core_exit.xt

	;---------------------------------------------------

	.test	"eval-interpret 42"
	.opt	test	pokew	forth__state , 0
		ldu	#.datastack1
		ldx	#forth__private_eval_interpret_xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result1 , "U"
	.assert	@@/0,u = 42       , "result"
		rts

		fdb	0
		fdb	0
		fdb	0
.datastack1	fdb	0
.result1	fdb	.caddr1

.caddr1		ascii	'42'c
	.endtst

	;--------------------------------------------------

	.test	"eval-interpret +"
	.opt	test	pokew	forth__state , 0
		ldu	#.datastack2
		ldx	#forth__private_eval_interpret_xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result2 , "U"
	.assert	@@/0,u = 150      , "result"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack2	fdb	-1
		fdb	forth_core_plus.xt
		fdb	100
.result2	fdb	50
	.endtst

;**********************************************************************

forth__private_find_nt_cb_xt
		fdb	forth_core_colon.runtime
	;=============================================================
	; : find-nt-wid ( c-addr u false nt -- c-addr u [ nt false | false true ] )
	;	>R 2 PICK 2 PICK R@ NAME>STRING strcmp IF
	;		DROP R> FALSE
	;	ELSE
	;		R> DROP TRUE
	;	THEN ;
	;=============================================================
		fdb	forth_core_to_r.xt
		fdb	forth_core_literal.runtime_xt
		fdb	2
		fdb	forth_core_ext_pick.xt
		fdb	forth_core_literal.runtime_xt
		fdb	2
		fdb	forth_core_ext_pick.xt
		fdb	forth_core_r_fetch.xt
		fdb	forth_tools_ext_name_to_string.xt
		fdb	forth__private_strcmp_xt
		fdb	forth_core_if.runtime_xt
		fdb	.L1
		fdb	forth_core_drop.xt
		fdb	forth_core_r_from.xt
		fdb	forth_core_ext_false.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L2
.L1		fdb	forth_core_r_from.xt
		fdb	forth_core_drop.xt
		fdb	forth_core_ext_true.xt
.L2		fdb	forth_core_exit.xt

	;-----------------------------------------------

	.test	"find_nt_wid found"
		ldu	#.datastack1
		ldx	#forth__private_find_nt_cb_xt
		jsr	forth_core_execute.asm
	.assert	/u     = .datastack1               , "U"
	.assert	@@/0,u = 0                         , "false"
	.assert	@@/2,u = forth_core_star_slash_mod , "nt"
	.assert	@@/4,u = .text1_len                , "len"
	.assert	@@/6,u = .text1                    , "c-addr"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack1	fdb	forth_core_star_slash_mod
		fdb	0
		fdb	.text1_len
		fdb	.text1

.text1		fcc	'*/MOD'
.text1_len	equ	* - .text1
	.endtst

	;-----------------------------------------------

	.test	"find_nt_wid not-found"
		ldu	#.datastack2
		ldx	#forth__private_find_nt_cb_xt
		jsr	forth_core_execute.asm
	.assert	/u     = .datastack2 , "U"
	.assert	@@/0,u = -1          , "true"
	.assert	@@/2,u =  0          , "false"
	.assert	@@/4,u = .text2_len  , "len"
	.assert	@@/6,u = .text2      , "c-addr"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack2	fdb	forth_core_star_slash_mod
		fdb	0
		fdb	.text2_len
		fdb	.text2

.text2		fcc	'+'
.text2_len	equ	* - .text2
	.endtst

;**********************************************************************

forth__private_immediate_q_xt		; ( xt -- xt flag )
		fdb	.body
.body		ldx	,u
		lbsr	forth__util_xt_to_name
		anda	#_IMMED
		bne	.true
		clra
		clrb
.done		pshu	d
		ldx	,y++
		jmp	[,x]
.true		ldd	#-1
		bra	.done		

;**********************************************************************

forth__private_interpret_q_xt		; ( xt -- xt flag )
		fdb	.body
.body		ldx	,u
		lbsr	forth__util_xt_to_name
		anda	#_NOINTERP
		bne	.false
		ldd	#-1
.done		pshu	d
		ldx	,y++
		jmp	[,x]
.false		clra
		clrb
		bra	.done

	;--------------------------------------

	.test	"interpret? TRUE"
		ldu	#.datastack1
		ldx	#forth__private_interpret_q_xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result1 , "U"
	.assert @@/0,u = -1       , "flag"
		rts

.result1	fdb	0
.datastack1	fdb	forth_core_dupe.xt
	.endtst

	;---------------------------------------

	.test	"interpret? FALSE"
		ldu	#.datastack2
		ldx	#forth__private_interpret_q_xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result2 , "U"
	.assert	@@/0,u = 0        , "flag"
		rts

.result2	fdb	0
.datastack2	fdb	forth_core_s_quote.xt
	.endtst

;**********************************************************************

forth__private_number_q_xt		; ( caddr -- n 1 true | d 2 true | false )
		fdb	.body
.body		ldd	forth__base	; save BASE just in case
		pshs	d
		clr	,-s		; default to not negative
		pulu	x		; get caddr
		clra
		clrb
		pshu	d		; push 0.
		pshu	d
		ldb	,x+		; get length
		lda	,x		; check first character
		cmpa	#39		; '?
		bne	.check_dec
		cmpb	#3		; should be three
		lbne	.error_ret
		lda	2,x		; check 3rd character
		cmpa	#39		; '?
		lbne	.error_ret	; if not, it's not a "number"
		ldb	1,x		; get character
		clra
		leau	2,u		; adjust data stack
		std	,u		; save onto stack
		ldd	#1		; 1 cell
		bra	.return_okay
.check_dec	cmpa	#'#'		; decimal?
		bne	.check_hex
		decb			; adjust length
		lda	#10		; set decimal
		sta	forth__base + 1
		bra	.check_negchar
.check_hex	cmpa	#'$'		; hex?
		bne	.check_bin
		decb
		lda	#16		; set hexadecimal
		sta	forth__base + 1
		bra	.check_negchar
.check_bin	cmpa	#'%'		; binary?
		bne	.check_neg
		decb
		lda	#2		; set binary
		sta	forth__base + 1
		bra	.check_negchar
.okay_done	ldd	#-1		; return true
.push_done	pshu	d
		puls	x,a		; remove sign flag and base
		stx	forth__base	; restore base
		ldx	,y++
		jmp	[,x]
.check_negchar	leax	1,x		; adjust character pointer
		lda	,x		; get character
.check_neg	cmpa	#'-'		; minus?
		bne	.to_number	; if not, convert to number
		sta	,s		; set sign flag
		decb			; adjust length
		leax	1,x		; and adjust character pointer
.to_number	clra
		pshu	x,d		; push c-addr u
		ldx	#forth_core_to_number.xt
		lbsr	forth_core_execute.asm
		pulu	x,d		; remove c-addr u
		subd	#0		; any more text?
		beq	.single		; if not, treat as a single cell
		subd	#1		; one more character?
		bne	.error_ret	; if more, error
		lda	,x+		; check to see if period
		cmpa	#'.'
		bne	.error_ret	; error if not
		tst	,s		; negative?
		beq	.dpos
		leax	,u		; negate double cell number
		lbsr	forth__math_neg32
.dpos		ldd	#2		; return two cells
		bra	.return_okay
.single		tst	,s		; negate?
		beq	.spos
		ldd	2,u		; negate it
		coma
		comb
		addd	#1
		std	2,u
.spos		ldd	#1		; return one cell
		leau	2,u		; adjust data stack
.return_okay	pshu	d		; save length
		bra	.okay_done
.error_ret	leau	4,u		; clean stack
		clra			; return false
		clrb
		bra	.push_done	

	;------------------------------------------

	.test	"number? g00"
	.opt	test	pokew	forth__base , 10
	.opt	test	prot	n , .nu1
		ldd	#10
		std	forth__base
		ldu	#.datastack1
		ldx	#forth__private_number_q_xt
		jsr	forth_core_execute.asm
	.assert	/u            = .datastack1 , "U"
	.assert	@@/0,u        = 0           , "false"
	.assert	@@forth__base = 10          , "base"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack1	fdb	.number1
.nu1		fcb	0
.number1	ascii	'g00'c
	.endtst

	;------------------------------------------

	.test	"number? 00g"
	.opt	pokew	forth__base , 10
	.opt	test	prot	n , .nu2
		ldd	#10
		std	forth__base
		ldu	#.datastack2
		ldx	#forth__private_number_q_xt
		jsr	forth_core_execute.asm
	.assert	/u	      = .datastack2 , "U"
	.assert	@@/0,u        = 0           , "false"
	.assert	@@forth__base = 10          , "base"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack2	fdb	.number2
.nu2		fcb	0
.number2	ascii	'00g'c
	.endtst

	;------------------------------------------

	.test	"number? 23456"
	.opt	pokew	forth__base , 10
	.opt	test	prot	n , .nu3
		ldd	#10
		std	forth__base
		ldu	#.datastack3
		ldx	#forth__private_number_q_xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result3 , "U"
	.assert	@@/0,u = -1       , "true"
	.assert @@/2,u = 1        , "one cell"
	.assert @@/4,u = 23456    , "result"
	.assert	@@forth__base = 10 , "base"
		rts

		fdb	0
		fdb	0
.result3	fdb	0
		fdb	0
.datastack3	fdb	.number3
.nu3		fcb	0
.number3	ascii	'23456'c
	.endtst

	;------------------------------------------

	.test	"number? -23456"
	.opt	pokew	forth__base , 10
	.opt	test	prot	n , .nu4
		ldd	#10
		std	forth__base
		ldu	#.datastack4
		ldx	#forth__private_number_q_xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result4 , "U"
	.assert	@@/0,u = -1       , "true"
	.assert @@/2,u = 1        , "one cell"
	.assert @@/4,u = -23456   , "result"
	.assert	@@forth__base = 10 , "base"
		rts

		fdb	0
		fdb	0
.result4	fdb	0
		fdb	0
.datastack4	fdb	.number4
.nu4		fcb	0
.number4	ascii	'-23456'c
	.endtst

	;------------------------------------------

	.test	"number? 305419896"
	.opt	pokew	forth__base , 10
	.opt	test	prot	n , .nu5
		ldd	#10
		std	forth__base
		ldu	#.datastack5
		ldx	#forth__private_number_q_xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result5 , "U"
	.assert	@@/0,u = -1       , "true"
	.assert @@/2,u = 1        , "one cell"
	.assert @@/4,u = 22136    , "result"
	.assert	@@forth__base = 10 , "base"
		rts

		fdb	0
		fdb	0
.result5	fdb	0
		fdb	0
.datastack5	fdb	.number5
.nu5		fcb	0
.number5	ascii	'305419896'c
	.endtst

	;------------------------------------------

	.test	"number? 305419896."
	.opt	pokew	forth__base , 10
	.opt	test	prot	n , .nu6
		ldd	#10
		std	forth__base
		ldu	#.datastack6
		ldx	#forth__private_number_q_xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result6 , "U"
	.assert	@@/0,u = -1       , "true"
	.assert @@/2,u = 2        , "two cell"
	.assert @@/4,u = $1234    , "MSW"
	.assert	@@/6,u = $5678    , "LSW"
	.assert	@@forth__base = 10 , "base"
		rts

		fdb	0
.result6	fdb	0
		fdb	0
		fdb	0
.datastack6	fdb	.number6
.nu6		fcb	0
.number6	ascii	'305419896.'c
	.endtst

	;------------------------------------------

	.test	"number? -305419896."
	.opt	pokew	forth__base , 10
	.opt	test	prot	n , .nu7
		ldd	#10
		std	forth__base
		ldu	#.datastack7
		ldx	#forth__private_number_q_xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result7 , "U"
	.assert	@@/0,u = -1       , "true"
	.assert @@/2,u = 2        , "two cell"
	.assert @@/4,u = $EDCB    , "MSW"
	.assert	@@/6,u = $A988    , "LSW"
	.assert	@@forth__base = 10 , "base"
		rts

		fdb	0
.result7	fdb	0
		fdb	0
		fdb	0
.datastack7	fdb	.number7
.nu7		fcb	0
.number7	ascii	'-305419896.'c
	.endtst

	;------------------------------------------

	.test	"number? $1234"
	.opt	pokew	forth__base , 10
	.opt	test	prot	n , .nu8
		ldd	#10
		std	forth__base
		ldu	#.datastack8
		ldx	#forth__private_number_q_xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result8 , "U"
	.assert	@@/0,u = -1       , "true"
	.assert @@/2,u = 1        , "one cell"
	.assert	@@/4,u = $1234    , "LSW"
	.assert	@@forth__base = 10 , "base"
		rts

		fdb	0
		fdb	0
.result8	fdb	0
		fdb	0
.datastack8	fdb	.number8
.nu8		fcb	0
.number8	ascii	'$1234'c
	.endtst

	;------------------------------------------

	.test	"number? $-1234"
	.opt	pokew	forth__base , 10
	.opt	test	prot	n , .nu9
		ldd	#10
		std	forth__base
		ldu	#.datastack9
		ldx	#forth__private_number_q_xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result9 , "U"
	.assert	@@/0,u = -1       , "true"
	.assert @@/2,u = 1        , "one cell"
	.assert	@@/4,u = $EDCC    , "LSW"
	.assert	@@forth__base = 10 , "base"
		rts

		fdb	0
		fdb	0
.result9	fdb	0
		fdb	0
.datastack9	fdb	.number9
.nu9		fcb	0
.number9	ascii	'$-1234'c
	.endtst

	;------------------------------------------

	.test	"number? 'x'"
	.opt	pokew	forth__base , 10
	.opt	test	prot	n , .nuA
		ldu	#.datastackA
		ldx	#forth__private_number_q_xt
		jsr	forth_core_execute.asm
	.assert	/u     = .resultA , "U"
	.assert	@@/0,u = -1       , "true"
	.assert @@/2,u = 1        , "one cell"
	.assert	@@/4,u = 120      , "LSW"
	.assert	@@forth__base = 10 , "base"
		rts
		fdb	0
		fdb	0
.resultA	fdb	0
		fdb	0
.datastackA	fdb	.numberA
.nuA		fcb	0
.numberA	ascii	"'x'"c
	.endtst

	;------------------------------------------

	.test	"number? 'x"
	.opt	pokew	forth__base , 10
	.opt	test	prot	n , .nuB
		ldd	#10
		std	forth__base
		ldu	#.datastackB
		ldx	#forth__private_number_q_xt
		jsr	forth_core_execute.asm
	.assert	/u     = .datastackB , "U"
	.assert	@@/0,u = 0       , "false"
	.assert	@@forth__base = 10 , "base"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastackB	fdb	.numberB
.nuB		fcb	0
.numberB	ascii	"'x"c
	.endtst

;**********************************************************************

forth__private_reset_dsp_xt		; ( i*x -- )
		fdb	.body
.body		ldu	forth__ds_top
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth__private_reset_rsp_xt		; ( -- ) ( R: i*x -- )
		fdb	.body
.body		lds	forth__rs_top
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth__private_set_source		; ( c-addr n -- )
		fdb	.body
.body		pulu	x,d
		stx	forth__source
		std	forth__source_len
		clra
		clrb
		std	forth__in
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth__private_set_source_i_d
		fdb	.body
.body		pulu	d
		std	forth__source_id
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth__private_source_restore_xt	; ( c-addr n -- )
		fdb	.body
.body		pulu	x,d
		stx	forth__source
		std	forth__source_len
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth__private_strcmp_xt
		fdb	forth_core_colon.runtime
	;=========================================
	; : strcmp	COMPARE 0= ;
	;=========================================
		fdb	forth_string_compare.xt
		fdb	forth_core_zero_equals.xt
		fdb	forth_core_exit.xt

;**********************************************************************
;		CORE
;**********************************************************************

forth_core_store		; ( x a-addr -- )
		fdb	0
		fdb	.xt - .name
.name		fcc	"!"
.xt		fdb	.body
.body		ldx	,u++
		ldd	,u++
		std	,x
		ldx	,y++		; NEXT
		jmp	[,x]

;**********************************************************************

forth_core_number_sign		; ( ud1 -- ud2 )
		fdb	forth_core_store
		fdb	.xt - .name
.name		fcc	"#"
.xt		fdb	.body
.body		ldd	forth__base
		pshu	d
		lda	#32
		pshs	a
		clra
		clrb
		pshs	d
		pshs	d
.10		lsl	5,u
		rol	4,u
		rol	3,u
		rol	2,u
		rolb
		rola
		bcs	.15
		cmpd	,u
		blo	.20
.15		subd	,u
		orcc	{c}
		bra	.30
.20		andcc	{c}
.30		rol	3,s
		rol	2,s
		rol	1,s
		rol	,s
		dec	4,s
		bne	.10
		cmpd	#9
		bls	.40
		addb	#7
.40		addb	#'0'
		ldx	forth__hold
		stb	,-x
		stx	forth__hold
		leau	2,u
		puls	x,d
		stx	2,u
		std	0,u
		leas	1,s
		ldx	,y++
		jmp	[,x]

	;----------------------------------------------

	.test	"#"
	.opt	test	pokew	forth__hold , .here
	.opt	test	pokew	forth__base , 10
		ldu	#.datastack
		ldx	#forth_core_number_sign.xt
		jsr	forth_core_execute.asm
	.assert	/u            = .datastack , "U"
	.assert	@@forth__hold = .digit     , "hold"
	.assert @.digit       = $36        , "digit"
	.assert	@@/0,u        = $01D2      , "MSW"
	.assert	@@/2,u        = $08A5      , "LSW"
		rts

		fdb	0
		fdb	0
.datastack	fdb	$1234
		fdb	$5678

.digit		fcb	0
.here		fcb	0
	.endtst

	;----------------------------------------------

	.test	"# max"
	.opt	test	pokew	forth__hold , .here1
	.opt	test	pokew	forth__base , 10
		ldu	#.datastack1
		ldx	#forth_core_number_sign.xt
		jsr	forth_core_execute.asm
	.assert	/u            = .datastack1 , "U"
	.assert	@@forth__hold = .digit1     , "hold"
	.assert @.digit1      = $35         , "digit"
	.assert	@@/0,u        = $1999       , "MSW"
	.assert	@@/2,u        = $9999       , "LSW"
		rts

		fdb	0
		fdb	0
.datastack1	fdb	$FFFF
		fdb	$FFFF

.digit1		fcb	0
.here1		fcb	0
	.endtst

	;----------------------------------------------

	.test	"# maxint"
	.opt	test	pokew	forth__hold , .here2
	.opt	test	pokew	forth__base , 10
		ldu	#.datastack2
		ldx	#forth_core_number_sign.xt
		jsr	forth_core_execute.asm
	.assert	/u            = .datastack2 , "U"
	.assert	@@forth__hold = .digit2     , "hold"
	.assert @.digit2      = $37         , "digit"
	.assert	@@/0,u        = $0CCC       , "MSW"
	.assert	@@/2,u        = $CCCC       , "LSW"
		rts

		fdb	0
		fdb	0
.datastack2	fdb	$7FFF
		fdb	$FFFF

.digit2		fcb	0
.here2		fcb	0
	.endtst

;**********************************************************************

forth_core_number_sign_greater	; ( xd -- c-addr u )
		fdb	forth_core_number_sign
		fdb	.xt - .name
.name		fcc	"#>"
.xt		fdb	.body
.body		ldd	forth__here
		addd	#SLASH_PAD + WORD_MAX + SLASH_HOLD
		subd	forth__hold
		ldx	forth__hold
		stx	2,u
		std	,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_number_sign_s	; ( ud1 -- ud2 )
		fdb	forth_core_number_sign_greater
		fdb	.xt - .name
.name		fcc	"#S"
.xt		fdb	forth_core_colon.runtime
	;=================================
	; : #S 	BEGIN # 2DUP D0= UNTIL ;
	;=================================
.L1		fdb	forth_core_number_sign.xt
		fdb	forth_core_two_dupe.xt
		fdb	forth_double_d_zero_equal.xt
		fdb	forth_core_until.runtime_xt
		fdb	.L1
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_tick			; ( "<spaces>name" -- xt )
		fdb	forth_core_number_sign_s
		fdb	.xt - .name
.name		fcc	"'"
.xt		fdb	forth_core_colon.runtime
	;===============================================
	; : '	BL WORD FIND 0= IF -13 THROW THEN ;
	;===============================================
		fdb	forth_core_b_l.xt
		fdb	forth_core_word.xt
		fdb	forth_search_find.xt
		fdb	forth_core_zero_equals.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L1
		fdb	forth_core_literal.runtime_xt
		fdb	-13
		fdb	forth_exception_throw.xt
.L1		fdb	forth_core_exit.xt

	;----------------------------------------------

	.test	"' */MOD"
	.opt	test	prot	rw , $6000 , $6100
	.opt	test	pokew	forth__source     , .buffer
	.opt	test	pokew	forth__source_len , .len
	.opt	test	pokew	forth__in         , 0
	.opt	test	pokew	forth__here       , $6000
		ldu	#.datastack
		ldx	#forth_core_tick.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result , "U"
	.assert	@@/0,u = forth_core_star_slash_mod.xt , "xt"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.result		fdb	0
.datastack	fdb	0

.buffer		fcc	' */MOD '
.len		equ	* - .buffer
	.endtst

;**********************************************************************

forth_core_paren		; ( "ccc<paren>" -- )
		fdb	forth_core_tick
		fdb	_IMMED :: .xt - .name
.name		fcc	"("
.xt		fdb	forth_core_colon.runtime
	;==========================================
	; : (	[CHAR] ) PARSE 2DROP ; IMMEDIATE
	;==========================================
		fdb	forth_core_literal.runtime_xt
		fdb	')'
		fdb	forth_core_ext_parse.xt
		fdb	forth_core_two_drop.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_star			; ( n1|u2 n2|u2 -- n3|u3 )
		fdb	forth_core_paren
		fdb	.xt - .name
.name		fcc	"*"
.xt		fdb	forth_core_colon.runtime
	;=========================================
	; : *	M* DROP ;
	;=========================================
		fdb	forth_core_m_star.xt
		fdb	forth_core_drop.xt
		fdb	forth_core_exit.xt
	
;**********************************************************************

forth_core_star_slash		; ( n1 n2 n3 -- n4 )
		fdb	forth_core_star
		fdb	.xt - .name
.name		fcc	"*/"
.xt		fdb	forth_core_colon.runtime
	;===========================================
	; : */	*/MOD SWAP DROP ;
	;===========================================
		fdb	forth_core_star_slash_mod.xt
		fdb	forth_core_swap.xt
		fdb	forth_core_drop.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_star_slash_mod	; ( n1 n2 n3 -- n4 n5 )
		fdb	forth_core_star_slash
		fdb	.xt - .name
.name		fcc	"*/MOD"
.xt		fdb	forth_core_colon.runtime
	;=========================================
	; : */MOD	>R M* R> SM/REM ;
	;=========================================
		fdb	forth_core_to_r.xt
		fdb	forth_core_m_star.xt
		fdb	forth_core_r_from.xt
		fdb	forth_core_s_m_slash_rem.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_plus			;  n1|u2 n2|u2 -- n3|u3 )
		fdb	forth_core_star_slash_mod
		fdb	.xt - .name
.name		fcc	"+"
.xt		fdb	.body
.body		ldd	,u++
		addd	,u
		std	,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_plus_store		; ( n|u a-addr -- )
		fdb	forth_core_plus
		fdb	.xt - .name
.name		fcc	"+!"
.xt		fdb	forth_core_colon.runtime
	;===============================
	; : +!	DUP @ ROT + SWAP ! ;
	;===============================
		fdb	forth_core_dupe.xt
		fdb	forth_core_fetch.xt
		fdb	forth_core_rote.xt
		fdb	forth_core_plus.xt
		fdb	forth_core_swap.xt
		fdb	forth_core_store.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_plus_loop		; C ( C: do-sys -- ) R ( n -- ) ( R: loop-sys1 -- | loop-sys2 )
		fdb	forth_core_plus_store
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"+LOOP"
.xt		fdb	.body
.body		ldd	#.runtime_xt	; xt to compile
.rest		ldx	forth__here	; get compile location
		std	,x++
		ldd	2,u		; get u-dest
		std	,x++		; compile it in
		stx	forth__here
		tfr	x,d		; cache current location in D
		ldx	,u
		beq	.checkleave
		std	,x
.checkleave	pshs	u
		ldx	forth__leave_sp
		ldu	,--x
		stx	forth__leave_sp
.fixup		tfr	u,x
		cmpx	#0
		beq	.xtdone
		ldu	,x
		std	,x
		bra	.fixup

.xtdone		puls	u
		leau	4,u		; clean parameter stack
		ldx	,y++		; NEXT
		jmp	[,x]

; https://forth-standard.org/standard/core/PlusLOOP#reply-214

.runtime_xt	fdb	.runtime
.runtime	ldd	,s
		subd	2,s
		addd	#$8000
		addd	,u
		bvs	.done
		ldd	,s
		addd	,u++
		std	,s
		ldy	,y
		ldx	,y++
		jmp	[,x]
.done		leas	4,s
		leay	2,y
		leau	2,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_comma		; ( x -- )
		fdb	forth_core_plus_loop
		fdb	.xt - .name
.name		fcc	","
.xt		fdb	.body
.body		ldx	forth__here
		pulu	d
		std	,x++
		stx	forth__here
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_minus		; ( n1|u1 n2|u2 -- n3|u3 )
		fdb	forth_core_comma
		fdb	.xt - .name
.name		fcc	"-"
.xt		fdb	.body
.body		ldd	2,u
		subd	,u++
		std	,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_dot			; ( n -- )
		fdb	forth_core_minus
		fdb	.xt - .name
.name		fcc	"."
.xt		fdb	forth_core_colon.runtime
	;==========================================================
	; : .	DUP >R ABS 0 <# #S R> SIGN #> TYPE SPACE ;
	;==========================================================
		fdb	forth_core_dupe.xt
		fdb	forth_core_to_r.xt
		fdb	forth_core_abs.xt
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_less_number_sign.xt
		fdb	forth_core_number_sign_s.xt
		fdb	forth_core_r_from.xt
		fdb	forth_core_sign.xt
		fdb	forth_core_number_sign_greater.xt
		fdb	forth_core_type.xt
		fdb	forth_core_space.xt
		fdb	forth_core_exit.xt

	;----------------------------------------------

	.test	"."
	.opt	test	prot	rw , $6000 , $6100
	.opt	test	pokew	forth__here           , $6000
	.opt	test	pokew	forth__vector_putchar , .putchar
		ldu	#.datastack
		ldx	#forth_core_dot.xt
		jsr	forth_core_execute.asm
	.assert	/u         = .result , "U"
	.assert	.outputbuf = "-32456"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack	fdb	-32456
.result		fdb	0

.putchar	pshs	x
		ldx	.output
		stb	,x+
		stx	.output
		puls	x,pc
.output		fdb	.outputbuf
.outputbuf	rmb	8
	.endtst

;**********************************************************************

forth_core_dot_quote		; ( -- )
		fdb	forth_core_dot
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	'."'
.xt		fdb	forth_core_colon.runtime
	;=================================================
	; ."	POSTPONE S" ['] TYPE COMPILE, ; IMMEDIATE
	;=================================================
		fdb	forth_core_s_quote.xt
		fdb	forth_core_literal.runtime_xt
		fdb	forth_core_type.xt
		fdb	forth_core_ext_compile_comma.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_slash		; ( n1 n2 -- n3 )
		fdb	forth_core_dot_quote
		fdb	.xt - .name
.name		fcc	"/"
.xt		fdb	forth_core_colon.runtime
	;=====================================
	; : /	/MOD SWAP DROP ;
	;=====================================
		fdb	forth_core_slash_mod.xt
		fdb	forth_core_swap.xt
		fdb	forth_core_drop.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_slash_mod		; ( n1 n2 -- n3 n4 )
		fdb	forth_core_slash
		fdb	.xt - .name
.name		fcc	"/MOD"
.xt		fdb	forth_core_colon.runtime
	;====================================
	; : /MOD	>R S>D R> SM/REM ; 
	;====================================
		fdb	forth_core_to_r.xt
		fdb	forth_core_s_to_d.xt
		fdb	forth_core_r_from.xt
		fdb	forth_core_s_m_slash_rem.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_zero_less		; ( n -- flag )
		fdb	forth_core_slash_mod
		fdb	.xt - .name
.name		fcc	"0<"
.xt		fdb	forth_core_colon.runtime
	;==============================================
	; : 0< 0 < ;
	;==============================================		
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_less_than.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_zero_equals		; ( n -- flag )
		fdb	forth_core_zero_less
		fdb	.xt - .name
.name		fcc	"0="
.xt		fdb	forth_core_colon.runtime
	;==============================================
	; : 0= 0 = ;
	;==============================================
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_equals.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_one_plus		; ( n1|u1 -- n2|u2 )
		fdb	forth_core_zero_equals
		fdb	.xt - .name
.name		fcc	"1+"
.xt		fdb	forth_core_colon.runtime
	;=========================================
	; : 1+	1 + ;
	;=========================================
		fdb	forth_core_literal.runtime_xt
		fdb	1
		fdb	forth_core_plus.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_one_minus		; ( n1 -- n2 )
		fdb	forth_core_one_plus
		fdb	.xt - .name
.name		fcc	"1-"
.xt		fdb	forth_core_colon.runtime
	;========================================
	; : 1-	1 - ;
	;========================================
		fdb	forth_core_literal.runtime_xt
		fdb	1
		fdb	forth_core_minus.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_two_store		; ( x1 x2 a-addr -- )
		fdb	forth_core_one_minus
		fdb	.xt - .name
.name		fcc	"2!"
.xt		fdb	forth_core_colon.runtime
	;==========================================
	; : 2!	SWAP OVER ! CELL+ ! ;
	;==========================================
		fdb	forth_core_swap.xt
		fdb	forth_core_over.xt
		fdb	forth_core_store.xt
		fdb	forth_core_cell_plus.xt
		fdb	forth_core_store.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_two_star		; ( x1 -- x2 )
		fdb	forth_core_two_store
		fdb	.xt - .name
.name		fcc	"2*"
.xt		fdb	forth_core_colon.runtime
	;======================================
	; : 2*	1 LSHIFT ;
	;======================================
		fdb	forth_core_literal.runtime_xt
		fdb	1
		fdb	forth_core_l_shift.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_two_slash		; ( x1 -- x2 )
		fdb	forth_core_two_star
		fdb	.xt - .name
.name		fcc	"2/"
.xt		fdb	.body
.body		ldd	,u
		asra
		rorb
		std	,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_two_fetch		; ( a-addr -- x1 x2 )
		fdb	forth_core_two_slash
		fdb	.xt - .name
.name		fcc	"2@"
.xt		fdb	forth_core_colon.runtime
	;=====================================
	; : 2@	DUP @ SWAP CELL+ @ SWAP ;
	;=====================================
		fdb	forth_core_dupe.xt
		fdb	forth_core_fetch.xt
		fdb	forth_core_swap.xt
		fdb	forth_core_cell_plus.xt
		fdb	forth_core_fetch.xt
		fdb	forth_core_swap.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_two_drop		; ( x1 x2 -- )
		fdb	forth_core_two_fetch
		fdb	.xt - .name
.name		fcc	"2DROP"
.xt		fdb	forth_core_colon.runtime
	;===================================
	; : 2DROP	DROP DROP ;
	;===================================
		fdb	forth_core_drop.xt
		fdb	forth_core_drop.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_two_dupe		; ( x1 x2 -- x1 x2 x1 x2 )
		fdb	forth_core_two_drop
		fdb	.xt - .name
.name		fcc	"2DUP"
.xt		fdb	forth_core_colon.runtime
	;=======================================
	; : 2DUP	OVER OVER ;
	;=======================================
		fdb	forth_core_over.xt
		fdb	forth_core_over.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_two_over		; ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
		fdb	forth_core_two_dupe
		fdb	.xt - .name
.name		fcc	"2OVER"
.xt		fdb	.body
.body		ldd	4,u
		ldx	6,u
		pshu	x,d
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_two_swap		; ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
		fdb	forth_core_two_over
		fdb	.xt - .name
.name		fcc	"2SWAP"
.xt		fdb	.body
.body		ldx	,u
		ldd	4,u
		exg	d,x
		std	4,u
		stx	,u
		ldx	2,u
		ldd	6,u
		exg	d,x
		std	6,u
		stx	2,u
		ldx	,y++
		jmp	[,x]

	;---------------------------------------

	.test	"2SWAP"
		ldu	#.datastack
		ldx	#forth_core_two_swap.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .datastack
	.assert	@@/,u  = 3
	.assert	@@/2,u = 4
	.assert	@@/4,u = 1
	.assert	@@/6,u = 2
		rts

		fdb	0
.datastack	fdb	1
		fdb	2
		fdb	3
		fdb	4
	.endtst

;**********************************************************************

forth_core_colon		; ( C: "<spaces>name" -- colon-sys ) E ( i*x -- j*x )
		fdb	forth_core_two_swap
		fdb	.xt - .name
.name		fcc	":"
.xt		fdb	.body
.body		ldd	#forth__leave_stack
		std	forth__leave_sp
		ldx	#forth_core_create.xt	; execute CREATE
		lbsr	forth_core_execute.asm
		lda	[forth__create_name]	; hide word from search
		ora	#_HIDDEN
		sta	[forth__create_name]
		ldx	forth__create_xt	; create colon-sys
		pshu	x
		ldd	#.runtime		; set xt for new word
		std	,x
		inc	forth__state + 1	; set STATE to compile
		ldx	,y++			; NEXT
		jmp	[,x]
.runtime	pshs	y
		leay	2,x
		ldx	,y++
		jmp	[,x]

	;-------------------------------------

	.test	": BAR "
	.opt	test	pokew forth__source      , .buffer
	.opt	test	pokew forth__source_len  , .len
	.opt	test	pokew forth__in          , 0
	.opt	test	pokew forth__current_wid , .wid
	.opt	test	pokew forth__here        , .bar_link
		ldu	#.datastack
		ldx	#forth_core_colon.xt
		jsr	forth_core_execute.asm
	.assert	@@.bar_xt     = forth_core_colon.runtime , "xt"
	.assert	@@.bar_name   = _HIDDEN :: 3             , "hidden"
	.assert .bar_name + 2 = "bar"                    , "name"
		rts

		fdb	-6
		fdb	-4
		fdb	-2
.datastack	fdb	0

.buffer		fcc	'bar '
.len		equ	* - .buffer

.wid		fdb	.foo
.foo		fdb	.foo_xt - .foo_name
.foo_name	fcc	'foo'
.foo_xt		fdb	forth_core_colon.runtime
		fdb	forth_core_exit.xt

.bar_link	fdb	0
.bar_name	fdb	0		; length + flags
		fcb	0,0,0		; text
.bar_xt		fdb	0
		fdb	0
	.endtst

;**********************************************************************

forth_core_semicolon		; C ( C: colon-sys -- ) R ( -- ) ( R: nest-sys -- )
		fdb	forth_core_colon
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	";"
.xt		fdb	.body
.body		ldx	forth__here	; compile in EXIT
		ldd	#forth_core_exit.xt
		std	,x++
		stx	forth__here
		ldx	forth__create_name	; get name
		beq	.no_name		; no name to unhide
		lda	,x			; unhide word
		anda	#~_HIDDEN
		sta	,x
		leau	2,u			; remove colon-sys
.no_name	clra
		clrb
		std	forth__state
		std	forth__leave_sp
		ldx	,y++			; NEXT
		jmp	[,x]

;**********************************************************************

forth_core_less_than		; ( n1 n2 -- flag )
		fdb	forth_core_semicolon
		fdb	.xt - .name
.name		fcc	"<"
.xt		fdb	.body
.body		ldd	2,u
		cmpd	,u++
		blt	.lessthan
		clra
		clrb
		bra	.done
.lessthan	ldd	#-1
.done		std	,u
		ldx	,y++	; NEXT
		jmp	[,x]

	;---------------------------------------

	.test	"1 2 < ( TRUE )"
		ldu	#.datastack1
		ldx	#forth_core_less_than.xt
		jsr	forth_core_execute.asm
	.assert	/u    = .results1 , "U"
	.assert	@@/,u = -1        , "flag"
		rts

		fdb	0
.datastack1	fdb	2
.results1	fdb	1
	.endtst

	;---------------------------------------

	.test	"2 1 < ( FALSE )"
		ldu	#.datastack2
		ldx	#forth_core_less_than.xt
		jsr	forth_core_execute.asm
	.assert	/u    = .results2 , "U"
	.assert	@@/,u = 0         , "flag"
		rts

		fdb	0
.datastack2	fdb	1
.results2	fdb	2
	.endtst

;**********************************************************************

forth_core_less_number_sign	; ( -- )
		fdb	forth_core_less_than
		fdb	.xt - .name
.name		fcc	"<#"
.xt		fdb	.body
.body		ldd	forth__here	; space for HOLD
		addd	#SLASH_PAD + WORD_MAX + SLASH_HOLD
		std	forth__hold
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_equals		; ( x1 x2 -- flag )
		fdb	forth_core_less_number_sign
		fdb	.xt - .name
.name		fcc	"="
.xt		fdb	.body
.body		ldd	,u++
		cmpd	,u
		beq	.equal
		clra
		clrb
		bra	.done
.equal		ldd	#-1
.done		std	,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_greater_than		; ( n1 n2 -- flag )
		fdb	forth_core_equals
		fdb	.xt - .name
.name		fcc	">"
.xt		fdb	.body
.body		ldd	2,u
		cmpd	,u++
		bgt	.greaterthan
		clra
		clrb
		bra	.done
.greaterthan	ldd	#-1
.done		std	,u
		ldx	,y++
		jmp	[,x]

	;---------------------------------------

	.test	"1 2 > ( FALSE )"
		ldu	#.datastack1
		ldx	#forth_core_greater_than.xt
		jsr	forth_core_execute.asm
	.assert	/u    = .results1 , "U"
	.assert	@@/,u = 0         , "flag"
		rts

		fdb	0
.datastack1	fdb	2
.results1	fdb	1
	.endtst

	;---------------------------------------

	.test	"2 1 > ( TRUE )"
		ldu	#.datastack2
		ldx	#forth_core_greater_than.xt
		jsr	forth_core_execute.asm
	.assert	/u    = .results2 , "U"
	.assert	@@/,u = -1        , "flag"
		rts

		fdb	0
.datastack2	fdb	1
.results2	fdb	2
	.endtst

;**********************************************************************

forth_core_to_body		; ( ( xt -- a-addr )
		fdb	forth_core_greater_than
		fdb	.xt - .name
.name		fcc	">BODY"
.xt		fdb	.body
.body		ldx	,u
		leax	2,x
		stx	,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_to_in		; ( -- a-addr )
		fdb	forth_core_to_body
		fdb	.xt - .name
.name		fcc	">IN"
.xt		fdb	.body
.body		ldd	#forth__in
		pshu	d
		ldx	,y++
		jmp	[,x]

;**********************************************************************

Pd		set	5
Pc		set	4
Pb		set	3
Pa		set	2
Pchar		set	1
Pbase		set	0

Lde		set	2
Lce		set	1
Lbe		set	0
Le		set	0

forth_core_to_number		; ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
		fdb	forth_core_to_in
		fdb	.xt - .name
.name		fcc	">NUMBER"
.xt		fdb	.body
.body		ldd	,u		; check for < 1 length
		beq	.return		; if so, skip
		bmi	.return		; 
		pshs	y		; save register
		leas	-4,s		; clear some tmp space
		pulu	y,x		; get c-addr u
		lda	forth__base + 1
		pshu	d		; save base and space for character
.read_char	clr	Lbe,s		; clear tmp space
		clr	Lce,s
		ldb	,y		; get character
		subb	#'0'		; convert to digit
		bmi	.done		; if not possible, done
		cmpb	#9
		bls	.no_adjust
		subb	#7		; adjust for letters
		cmpb	#36		; where they upper case?
		blo	.no_adjust	; if so, we're fine
		subb	#32		; check for lower case
		cmpb	#36		; if not so, we're done
		bhs	.done
.no_adjust	cmpb	Pbase,u		; > base?
		bhs	.done		; if so, done
		stb	Pchar,u		; save character
		lda	Pbase,u		; base * D
		ldb	Pd,u
		mul
		std	Lde,s
		lda	Pbase,u		; base * C
		ldb	Pc,u
		mul
		addd	Lce,s
		std	Lce,s
		lda	Pbase,u		; base * B
		ldb	Pb,u
		mul
		addd	Lbe,s
		std	Lbe,s
		lda	Pbase,u		; base * A
		ldb	Pa,u
		mul
		addb	Le,s
		stb	Le,s
		ldd	0,s		; move result from temporary area
		std	Pa,u
		ldd	2,s
		std	Pc,u
		clra			; add in character
		ldb	Pchar,u
		addd	Pc,u
		std	Pc,u
		ldd	Pa,u
		adcb	#0
		adca	#0
		std	Pa,u
		leay	1,y
		leax	-1,x
		bne	.read_char
.done		leau	2,u
		leas	4,s
		pshu	y,x
		puls	y
.return		ldx	,y++
		jmp	[,x]

	;----------------------------------------------

	.test	">NUMBER 0"
	.opt	test	prot	n , .nu0
	.opt	test	pokew	forth__base , 10
		ldu	#.datastack0
		ldx	#forth_core_to_number.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .datastack0 , "U"
	.assert	@@/0,u = .len0 - 1   , "len"
	.assert	@@/2,u = .text0 + 1  , "text"
	.assert	@@/4,u = 0           , "MSW"
	.assert	@@/6,u = 0           , "LSW"
		rts

		fdb	0
.datastack0	fdb	.len0
		fdb	.text0
		fdb	0
		fdb	0

.nu0		fcb	0
.text0		fcc	'0'
.len0		equ	* - .text0
	.endtst

	;-------------------------------------------

	.test	">NUMBER 1"
	.opt	test	prot	n , .nu1
	.opt	test	pokew	forth__base , 10
		ldu	#.datastack1
		ldx	#forth_core_to_number.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .datastack1 , "U"
	.assert	@@/0,u = .len1 - 1   , "len"
	.assert	@@/2,u = .text1 + 1  , "text"
	.assert	@@/4,u = 0           , "MSW"
	.assert	@@/6,u = 1           , "LSW"
		rts

		fdb	0
.datastack1	fdb	.len1
		fdb	.text1
		fdb	0
		fdb	0

.nu1		fcb	0
.text1		fcc	'1      '
.len1		equ	* - .text1
	.endtst

	;-------------------------------------------

	.test	">NUMBER 65535"
	.opt	test	prot	n , .nu2
	.opt	test	pokew	forth__base , 10
		ldu	#.datastack2
		ldx	#forth_core_to_number.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .datastack2 , "U"
	.assert	@@/0,u = .len2 - 5   , "len"
	.assert @@/2,u = .text2 + 5  , "text"
	.assert	@@/4,u = 0           , "MSW"
	.assert	@@/6,u = $FFFF       , "LSW"
		rts

		fdb	0
.datastack2	fdb	.len2
		fdb	.text2
		fdb	0
		fdb	0

.nu2		fcb	0
.text2		fcc	'65535 '
.len2		equ	* - .text2
	.endtst

	;-------------------------------------------

	.test	">NUMBER 17373113"
	.opt	test	prot	n , .nu3
	.opt	test	pokew	forth__base , 10
		ldu	#.datastack3
		ldx	#forth_core_to_number.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .datastack3 , "U"
	.assert	@@/0,u = .len3 - 8   , "len"
	.assert	@@/2,u = .text3 + 8  , "text"
	.assert	@@/4,u = $0109       , "MSW"
	.assert	@@/6,u = $17B9       , "LSW"
		rts

		fdb	0
.datastack3	fdb	.len3
		fdb	.text3
		fdb	0
		fdb	0

.nu3		fcb	0
.text3		fcc	'17373113.'
.len3		equ	* - .text3
	.endtst

	;--------------------------------------------

	.test	">NUMBER 89ABCDEF ( HEX )"
	.opt	test	prot	n , .nu4
	.opt	test	pokew	forth__base , 16
		ldu	#.datastack4
		ldx	#forth_core_to_number.xt
		jsr	forth_core_execute.asm
	.assert	/u
	.assert @@/0,u = .len4 - 8 , "len"
	.assert	@@/2,u = .text4 + 8 , "text"
	.assert	@@/4,u = $89AB , "MSW"
	.assert	@@/6,u = $CDEF , "LSW"
		rts

		fdb	0
.datastack4	fdb	.len4
		fdb	.text4
		fdb	0
		fdb	0

.nu4		fcb	0
.text4		fcc	'89abcdef '
.len4		equ	* - .text4
	.endtst

	;--------------------------------------------

	.test	">NUMBER g00 (fails)"
	.opt	test	prot	n , .nu5
	.opt	test	pokew	forth__base , 10
		ldu	#.datastack5
		ldx	#forth_core_to_number.xt
		jsr	forth_core_execute.asm
	.assert	/u
	.assert @@/0,u = .len5  , "len"
	.assert	@@/2,u = .text5 , "text"
	.assert	@@/4,u = 0      , "MSW"
	.assert	@@/6,u = 0      , "LSW"
		rts

		fdb	0
.datastack5	fdb	.len5
		fdb	.text5
		fdb	0
		fdb	0

.nu5		fcb	0
.text5		fcc	'g00 '
.len5		equ	* - .text5
	.endtst

;**********************************************************************

forth_core_to_r			; E ( x -- ) ( R: -- x )
		fdb	forth_core_to_number
		fdb	.xt - .name
.name		fcc	">R"
.xt		fdb	.body
.body		pulu	d
		pshs	d
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_question_dupe	; ( x -- 0 | x x )
		fdb	forth_core_to_r
		fdb	.xt - .name
.name		fcc	"?DUP"
.xt		fdb	.body
.body		ldd	,u
		beq	.done
		pshu	d
.done		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_fetch		; ( a-addr -- x )
		fdb	forth_core_question_dupe
		fdb	.xt - .name
.name		fcc	"@"
.xt		fdb	.body
.body		ldd	[,u]
		std	,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************
;
;forth_core_abort		; EXCEPTION EXT ABORT
;		fdb	forth_core_fetch
;		fdb	.xt - .name
;.name		fcc	"ABORT"
;.xt		fdb	forth_core_colon.runtime
;		fdb	forth_core_literal.runtime_xt
;		fdb	-13
;		fdb	forth_exception_throw.xt
;
;**********************************************************************
;
;forth_core_abort_quote		; EXCEPTION EXT ABORT"
;		fdb	forth_core_abort
;		fdb	_IMMED | _NOINTERP :: .xt - .name
;.name		fcc	'ABORT"'
;.xt		fdb	forth_core_colon.runtime
;		fdb	forth_core_literal.runtime_xt
;		fdb	-13
;		fdb	forth_exception_throw.xt
;
;**********************************************************************

forth_core_abs			; ( n -- u )
		fdb	forth_core_fetch
		fdb	.xt - .name
.name		fcc	"ABS"
.xt		fdb	forth_core_colon.runtime
	;============================================
	; : ABS	DUP 0< IF NEGATE THEN ;
	;======================================
		fdb	forth_core_dupe.xt
		fdb	forth_core_zero_less.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L1
		fdb	forth_core_negate.xt
.L1		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_accept		; ( c-addr +n1 -- +n2 )
		fdb	forth_core_abs
		fdb	.xt - .name
.name		fcc	"ACCEPT"
.xt		fdb	forth_core_colon.runtime
	;===============================================
	; : ACCEPT
	;	DUP 1 < IF -256 THROW THEN
	;	DUP >R 0 DO
	;		KEY DUP 10 = IF
	;			DROP I UNLOOP R> DROP NIP EXIT
	;		THEN OVER C! CHAR+
	;	LOOP DROP R> ;
	;==============================================
		fdb	forth_core_dupe.xt
		fdb	forth_core_literal.runtime_xt
		fdb	1
		fdb	forth_core_less_than.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L1
		fdb	forth_core_literal.runtime_xt
		fdb	-256
		fdb	forth_exception_throw.xt
.L1		fdb	forth_core_dupe.xt
		fdb	forth_core_to_r.xt
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_do.runtime_xt
.L2		fdb	forth_core_key.xt
		fdb	forth_core_dupe.xt
		fdb	forth_core_literal.runtime_xt
		fdb	10
		fdb	forth_core_equals.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L3
		fdb	forth_core_drop.xt
		fdb	forth_core_i.xt
		fdb	forth_core_unloop.xt
		fdb	forth_core_r_from.xt
		fdb	forth_core_drop.xt
		fdb	forth_core_ext_nip.xt
		fdb	forth_core_exit.xt
.L3		fdb	forth_core_over.xt
		fdb	forth_core_c_store.xt
		fdb	forth_core_char_plus.xt
		fdb	forth_core_loop.runtime_xt
		fdb	.L2
		fdb	forth_core_drop.xt
		fdb	forth_core_r_from.xt
		fdb	forth_core_exit.xt

	;-----------------------------------------------

	.test	"ACCEPT"
	.opt	test	prot	n , .nu1
	.opt	test	prot	n , .nu2
	.opt	test	pokew	forth__vector_getchar , .getchar
		ldu	#.datastack
		ldx	#forth_core_accept.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result , "U"
	.assert	@@/0,u = 3       , "len"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack	fdb	.len
.result		fdb	.buffer

.nu1		fcb	0
.buffer		rmb	10
.len		equ	* - .buffer

.getchar	pshs	x
		ldx	.input
		clra
		ldb	,x+
		stx	.input
		puls	x,pc
.input		fdb	.inputbuf
.inputbuf	ascii	'BYE\n'
.nu2		fcb	0
	.endtst

;**********************************************************************

forth_core_align		; ( -- )
		fdb	forth_core_accept
		fdb	.xt - .name
.name		fcc	"ALIGN"
.xt		fdb	forth_core_colon.runtime
	;=============================================
	; : ALIGN ; 
	;=============================================
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_aligned		; ( addr -- a-addr )
		fdb	forth_core_align
		fdb	.xt - .name
.name		fcc	"ALIGNED"
.xt		fdb	forth_core_colon.runtime
	;===========================================
	; : ALIGNED ;
	;===========================================
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_allot		; ( n -- )
		fdb	forth_core_aligned
		fdb	.xt - .name
.name		fcc	"ALLOT"
.xt		fdb	.body
.body		ldd	,u++
		beq	.done
		addd	forth__here
		std	forth__here
.done		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_and			; ( x1 x2 -- x3 )
		fdb	forth_core_allot
		fdb	.xt - .name
.name		fcc	"AND"
.xt		fdb	.body
.body		pulu	d
		anda	,u
		andb	1,u
		std	,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_base			; ( -- a-addr )
		fdb	forth_core_and
		fdb	.xt - .name
.name		fcc	"BASE"
.xt		fdb	.body
.body		ldd	#forth__base
		pshu	d
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_begin		; C ( C: -- dest ) R ( -- )
		fdb	forth_core_base
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"BEGIN"
.xt		fdb	.body
.body		ldd	forth__here	; jump back here
		pshu	d
		ldx	,y++	; NEXT
		jmp	[,x]

;**********************************************************************

forth_core_b_l			; ( -- char )
		fdb	forth_core_begin
		fdb	.xt - .name
.name		fcc	"BL"
.xt		fdb	forth_core_constant.does
	;=======================================
	; 32 CONSTANT BL
	;=======================================
		fdb	32

	;--------------------------------------

	.test	"BL"
		ldu	#.datastack
		ldx	#forth_core_b_l.xt
		jsr	forth_core_execute.asm
	.assert	/u    = .result
	.assert	@@/,u = 32
		rts

.result		fdb	0
.datastack	fdb	0
	.endtst

;**********************************************************************

forth_core_c_store		; ( c c-addr -- )
		fdb	forth_core_b_l
		fdb	.xt - .name
.name		fcc	"C!"
.xt		fdb	.body
.body		ldx	,u++
		ldd	,u++
		stb	,x
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_c_comma		; ( char -- )
		fdb	forth_core_c_store
		fdb	.xt - .name
.name		fcc	"C,"
.xt		fdb	.body
.body		pulu	d
		ldx	forth__here
		stb	,x+
		stx	forth__here
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_c_fetch		; ( c-addr -- char )
		fdb	forth_core_c_comma
		fdb	.xt - .name
.name		fcc	"C@"
.xt		fdb	.body
.body		ldx	,u
		clra
		ldb	,x
		std	,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_cell_plus		; ( a-addr1 -- a-addr2 )
		fdb	forth_core_c_fetch
		fdb	.xt - .name
.name		fcc	"CELL+"
.xt		fdb	.body
.body		ldx	,u
		leax	2,x
		stx	,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_cells		; ( n1 -- n2)
		fdb	forth_core_cell_plus
		fdb	.xt - .name
.name		fcc	"CELLS"
.xt		fdb	.body
.body		ldd	,u
		addd	,u
		std	,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_char			; ( "<spaces>name" -- )
		fdb	forth_core_cells
		fdb	.xt - .name
.name		fcc	"CHAR"
.xt		fdb	forth_core_colon.runtime
	;=======================================
	; : CHAR	BL WORD CHAR+ C@ ;
	;=======================================
		fdb	forth_core_b_l.xt
		fdb	forth_core_word.xt
		fdb	forth_core_char_plus.xt
		fdb	forth_core_c_fetch.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_char_plus		; ( c-addr1 -- c-addr2 )
		fdb	forth_core_char
		fdb	.xt - .name
.name		fcc	"CHAR+"
.xt		fdb	.body
.body		ldx	,u
		leax	1,x
		stx	,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_chars		; ( n1 -- n2 )
		fdb	forth_core_char_plus
		fdb	.xt - .name
.name		fcc	"CHARS"
.xt		fdb	forth_core_colon.runtime
	;===================================
	; : CHARS	;
	;===================================
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_constant		; ( x "<spaces>name" -- ) E ( -- )
		fdb	forth_core_chars
		fdb	.xt - .name
.name		fcc	"CONSTANT"
.xt		fdb	forth_core_colon.runtime
	;=======================================
	; : CONSTANT	CREATE , DOES> @ ;
	;=======================================
		fdb	forth_core_create.xt
		fdb	forth_core_comma.xt
		fdb	forth_core_does.runtime_xt	; also EXIT is here
.does		jsr	forth_core_create.does_hook
		fdb	forth_core_fetch.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_count		; ( c-addr1 -- c-addr2 u )
		fdb	forth_core_constant
		fdb	.xt - .name
.name		fcc	"COUNT"
.xt		fdb	forth_core_colon.runtime
	;============================================
	; : COUNT	DUP C@ SWAP CHAR+ SWAP ;
	;============================================
		fdb	forth_core_dupe.xt
		fdb	forth_core_c_fetch.xt
		fdb	forth_core_swap.xt
		fdb	forth_core_char_plus.xt
		fdb	forth_core_swap.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_c_r			; ( -- )
		fdb	forth_core_count
		fdb	.xt - .name
.name		fcc	"CR"
.xt		fdb	forth_core_colon.runtime
	;======================================
	; : CR	10 EMIT ;
	;======================================
		fdb	forth_core_literal.runtime_xt
		fdb	NL
		fdb	forth_core_emit.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_create		; ( "<spaces>name" -- ) E ( -- a-addr )
		fdb	forth_core_c_r
		fdb	.xt - .name
.name		fcc	"CREATE"
.xt		fdb	.body

.body		ldx	#forth_core_ext_parse_name.xt ; PARSE-NAME
		lbsr	forth_core_execute.asm
		lbra	forth__private_create_quote_xt.body

.does_hook	puls	d		; see DOES> for an explanation
		pshs	y		; of this code.
		tfr	d,y
.runtime	leax	2,x
		pshu	x
		ldx	,y++		; NEXT
		jmp	[,x]

	;---------------------------------------

	.test	"CREATE"
	.opt	test	pokew forth__source      , .buffer
	.opt	test	pokew forth__source_len  , .len
	.opt	test	pokew forth__in          , 0
	.opt	test	pokew forth__current_wid , .wid
	.opt	test	pokew forth__here        , .bar_link
		ldu	#.datastack
		ldx	#forth_core_create.xt
		jsr	forth_core_execute.asm
	.assert @@.wid               = .bar_link , "wid"
	.assert	@@forth__create_name = .bar_len  , "forth__create_name"
	.assert @@forth__create_xt   = .bar_xt   , "forth__create_xt"
	.assert @@.bar_link          = .foo      , "bar_link"
	.assert @@.bar_xt            = forth_core_create.runtime , "xt"
	.assert @@.bar_body          = 3         , "bar_body"
	.assert @@.bar_len           = 3         , "name length"
	.assert .bar_name            = "bar"     , "name"
		rts

		fdb	-6
		fdb	-4
		fdb	-2
.datastack	fdb	0

.buffer		fcc	'bar '
.len		equ	* - .buffer

.wid		fdb	.foo
.foo		fdb	0
		fdb	.foo_xt - .foo_name
.foo_name	fcc	'foo'
.foo_xt		fdb	forth_core_colon.runtime
		fdb	forth_core_exit.xt

.bar_link	fdb	0
.bar_len	fdb	0
.bar_name	fcb	0,0,0
.bar_xt		fdb	0
.bar_body	fdb	3
	.endtst

;**********************************************************************

forth_core_decimal		; ( -- )
		fdb	forth_core_create
		fdb	.xt - .name
.name		fcc	"DECIMAL"
.xt		fdb	forth_core_colon.runtime
	;=========================================
	; : DECIMAL	10 BASE ! ;
	;=========================================
		fdb	forth_core_literal.runtime_xt
		fdb	10
		fdb	forth_core_base.xt
		fdb	forth_core_store.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_depth		; ( -- +n )
		fdb	forth_core_decimal
		fdb	.xt - .name
.name		fcc	"DEPTH"
.xt		fdb	.body
.body		lbsr	forth__util_check_ds
		pshs	u		; save current address
		ldd	forth__ds_top	; get top
		subd	,s++		; subtract current address, getting length
		lsra
		rorb
		pshu	d		; return to user
		ldx	,y++		; NEXT
		jmp	[,x]

;**********************************************************************

forth_core_do			; ( C: -- do-sys ) ( n1|u1 n2|u2 -- ) ( R: -- loop-sys )
		fdb	forth_core_depth
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"DO"
.xt		fdb	.body
.body		ldx	forth__leave_sp
		cmpx	#forth__leave_stack + 16
		beq	.throw_toodeep
		clra
		clrb
		std	,x++
		stx	forth__leave_sp
		ldx	forth__here		; lay down runtime xt
		ldd	#.runtime_xt
		std	,x++
		stx	forth__here		; update compile location
		clra				; no c-orig
		clrb
		pshu	x,d			; push u-dest c-orig
		ldx	,y++			; NEXT
		jmp	[,x]
.throw_toodeep	ldd	#-7
		lbra	forth_exception_throw.asm

.runtime_xt	fdb	.runtime
.runtime	jmp	forth_core_ext_two_to_r.body

;**********************************************************************
;	DOES>
;
; This functionality of this word happens at three different times:
;
;	: FOO CREATE ... DOES> ( time 1 ) ... ;
;	... FOO BAR ( time 2 ) ...
;	BAR ( time 3 )
;
; Time 1: DOES> is called.  It will compile the xt to its runtime action,
;	forth_core_does.runtime_xt, followed by a direct JSR call to
;	forth_core_create.does_hook.  DOES> then returns, and since we are
;	still in compile mode, the code following DOES> is compiled into
;	FOO.  It looks like:
;
;	[...]
;	[forth_core_does.runtime_xt]
;	JSR forth_core_create.does_hook
;	[...]
;
; Time 2: FOO is called.  At some point, we hit forth_core_does.runtime_xt,
;	which then pulls the next address off the return stack (it's the
;	address of the "JSR forth_core_create.does_hook" instruction).  This
;	is used to update the xt field of the newly created word, and
;	then causes FOO to return, since we're now done with FOO.  
;
;	Before time 2:
;			link header of BAR
;			[forth_core_create.runtime]
;			[...]
;	After time 2:
;			link header of BAR
;			[address of "JSR forth_core_create.does_hook" in FOO]
;			[...]
;
; Time 3: BAR is called.  This runs forth_core_create.does_hook, which
;	pushes the address of BAR's >BODY, then run the code that follows
;	DOES> in FOO's definition.
;
; Yes, this is mind bending.  You're welcome.
;
;**********************************************************************

forth_core_does			; C ( C: colon-sys1 -- colon-sys2 ) R ( -- ) ( R: nest-sys1 -- )
		fdb	forth_core_do
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"DOES>"
.xt		fdb	.body
.body		ldx	forth__here		; get current comp location
		ldd	#.runtime_xt		; set up for time 2.
		std	,x++
		lda	#$BD			; compile JSR
		sta	,x+
		ldd	#forth_core_create.does_hook ; to here
		std	,x++
		stx	forth__here	; update HERE location
		ldx	,y++			; NEXT
		jmp	[,x]

.runtime_xt	fdb	.runtime
.runtime	ldx	forth__create_xt	; get xt
		sty	,x			; point xt to JSR 
		puls	y			; effectively EXIT
		ldx	,y++			; NEXT
		jmp	[,x]

	;-------------------------------------------------

	.test	"DOES> : FOO CREATE ... DOES> ... ;"
	.opt	test	pokew	forth__here , .foo1_does_cxt
	.opt	test	prot	n , .datastack1 - 4 , .datastack1 - 3
	.opt	test	prot	n , .datastack1 + 2 , .datastack1 + 3
	.opt	test	prot	n , .foo1_xt , .foo1_xt + 1

		ldu	#.datastack1
		ldx	#forth_core_does.xt
		jsr	forth_core_execute.asm
	.assert	/u               = .datastack1                 , "U"
	.assert	@@/,u            = .foo1_xt                    , ",U"
	.assert @@.foo1_does_cxt = forth_core_does.runtime_xt     , "cxt"
	.assert	@.foo1_does_jsr  = $BD                         , "JSR"
	.assert @@.foo1_does_a   = forth_core_create.does_hook , "does_hook"
		rts

		fdb	-4	; no permissions
		fdb	-2
.datastack1	fdb	.foo1_xt
		fdb	0

.foo1_xt	fdb	forth_core_colon.runtime
.foo1_does_cxt	fdb	0	; forth_core_does.runtime_xt
.foo1_does_jsr	fcb	0	; JSR
.foo1_does_a	fdb	0	; forth_core_create.does_hook
	.endtst

	;---------------------------------------------------

	.test	"DOES> ... FOO BAR ... "
	.opt	test	pokew	forth__create_xt , .bar2_xt

		ldu	#.datastack2
		ldx	#.call2_foo_xt
		jsr	forth_core_execute.asm
	.assert	/u              = .datastack2 , "U"
	.assert	@@/,u           = 0           , ",U"
	.assert	@@.bar2_xt      = .foo2_jsr   , "JSR"
	.assert	@@.bar2_body    = 42          , "body"
		rts

		fdb	0
.datastack2	fdb	0

.call2_foo_xt	fdb	forth_core_colon.runtime
		fdb	.foo2_xt
		fdb	forth_core_exit.xt

.foo2_xt	fdb	forth_core_colon.runtime
		fdb	forth_core_does.runtime_xt
.foo2_jsr	jsr	forth_core_create.does_hook

.bar2_xt	fdb	forth_core_create.runtime; .foo2_does_jsr
.bar2_body	fdb	42
	.endtst

	;-----------------------------------------------------

	.test	"DOES> ... BAR ... "
		ldu	#.datastack3
		ldx	#.call3_bar_xt
		jsr	forth_core_execute.asm
	.assert	/u     = .datastack3 - 2 , "U"
	.assert	@@/,u  = 42              , ",U"
	.assert	@@/2,u = 0               , "2,U"
		rts

		fdb	0
.datastack3	fdb	0

.call3_bar_xt	fdb	forth_core_colon.runtime
		fdb	.bar3_xt
		fdb	forth_core_exit.xt

.foo3_does_jsr	jsr	forth_core_create.does_hook
		fdb	forth_core_fetch.xt
		fdb	forth_core_exit.xt

.bar3_xt	fdb	.foo3_does_jsr
		fdb	42
	.endtst

;**********************************************************************

forth_core_drop			; ( x -- )
		fdb	forth_core_does
		fdb	.xt - .name
.name		fcc	"DROP"
.xt		fdb	.body
.body		leau	2,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_dupe			; ( x -- x x )
		fdb	forth_core_drop
		fdb	.xt - .name
.name		fcc	"DUP"
.xt		fdb	.body
.body		ldd	,u
		pshu	d
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_else			; C ( C: orig1 -- orig2 ) R ( -- )
		fdb	forth_core_dupe
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"ELSE"
.xt		fdb	forth_core_colon.runtime
	;====================================================
	; : ELSE	POSTPONE AHEAD 1 CS-ROLL POSTPONE THEN ; IMMEDIATE
	;===================================================
		fdb	forth_tools_ext_ahead.xt
		fdb	forth_core_literal.runtime_xt
		fdb	1
		fdb	forth_tools_ext_c_s_roll.xt
		fdb	forth_core_then.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_emit			; ( x -- )
		fdb	forth_core_else
		fdb	.xt - .name
.name		fcc	"EMIT"
.xt		fdb	.body
.body		pulu	d		; char caracter
		jsr	[forth__vector_putchar]	; do that thing
		ldx	,y++		; NEXT
		jmp	[,x]

;**********************************************************************
;	ENVIRONMENT
;**********************************************************************

forth__env_wid	fdb	forth__env_number_sign_locals

forth__env_slash_counted_string
		fdb	0
		fdb	.xt - .name
.name		fcc	"/COUNTED-STRING"
.xt		fdb	forth_core_constant.does
		fdb	255
forth__env_slash_hold
		fdb	forth__env_slash_counted_string
		fdb	.xt - .name
.name		fcc	"/HOLD"
.xt		fdb	forth_core_constant.does
		fdb	SLASH_HOLD
forth__env_slash_pad
		fdb	forth__env_slash_hold
		fdb	.xt - .name
.name		fcc	"/PAD"
.xt		fdb	forth_core_constant.does
		fdb	SLASH_PAD
forth__env_address_unit_bits
		fdb	forth__env_slash_pad
		fdb	.xt - .name
.name		fcc	"ADDRESS-UNIT-BITS"
.xt		fdb	forth_core_constant.does
		fdb	8
forth__env_floored
		fdb	forth__env_address_unit_bits
		fdb	.xt - .name
.name		fcc	"FLOORED"
.xt		fdb	forth_core_constant.does
		fdb	0
forth__env_max_char
		fdb	forth__env_floored
		fdb	.xt - .name
.name		fcc	"MAX-CHAR"
.xt		fdb	forth_core_constant.does
		fdb	127
forth__env_max_d
		fdb	forth__env_max_char
		fdb	.xt - .name
.name		fcc	"MAX-D"
.xt		fdb	forth_double_two_constant.does
		fdb	$7FFF
		fdb	$FFFF
forth__env_max_n
		fdb	forth__env_max_d
		fdb	.xt - .name
.name		fcc	"MAX-N"
.xt		fdb	forth_core_constant.does
		fdb	$7FFF
forth__env_max_u
		fdb	forth__env_max_n
		fdb	.xt - .name
.name		fcc	"MAX-U"
.xt		fdb	forth_core_constant.does
		fdb	$FFFF
forth__env_max_u_d
		fdb	forth__env_max_u
		fdb	.xt - .name
.name		fcc	"MAX-UD"
.xt		fdb	forth_double_two_constant.does
		fdb	$FFFF
		fdb	$FFFF
forth__env_return_stack_cells
		fdb	forth__env_max_u_d
		fdb	.xt - .name
.name		fcc	"RETURN-STACK-CELLS"
.xt		fdb	.body
.body		ldd	forth__rs_top
		subd	forth__rs_bottom
		pshu	d
		ldx	,y++
		jmp	[,x]
forth__env_stack_cells
		fdb	forth__env_return_stack_cells
		fdb	.xt - .name
.name		fcc	"STACK-CELLS"
.xt		fdb	.body
.body		ldd	forth__ds_top
		subd	forth__ds_bottom
		pshu	d
		ldx	,y++
		jmp	[,x]
forth__env_wordlists
		fdb	forth__env_stack_cells
		fdb	.xt - .name
.name		fcc	"WORDLIST"
.xt		fdb	forth_core_constant.does
.body		fdb	NUMBER_LISTS
forth__env_number_sign_locals
		fdb	forth__env_wordlists
		fdb	.xt - .name
.name		fcc	"#LOCALS"
.xt		fdb	forth_core_constant.does
.body		fdb	NUMBER_LOCALS

forth_core_environment_query	; ( c-addr u -- false | i*x true )
		fdb	forth_core_emit
		fdb	.xt - .name
.name		fcc	"ENVIRONMENT?"
.xt		fdb	forth_core_colon.runtime
	;=====================================================
	; : ENVIRONMENT?
	;	forth__env_wid
	;	SEARCH-WORDLIST IF 
	;		EXECUTE TRUE 
	;	ELSE 
	;		FALSE 
	;	THEN ;
	;=====================================================
		fdb	forth_core_literal.runtime_xt
		fdb	forth__env_wid
		fdb	forth_search_search_wordlist.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L1
		fdb	forth_core_execute.xt
		fdb	forth_core_ext_true.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L2
.L1		fdb	forth_core_ext_false.xt
.L2		fdb	forth_core_exit.xt

	;-------------------------------------------------

	.test	'S" /COUNTED-STRING" ENVIRONMENT?'
	.opt	test	prot	n , ._nu
		ldu	#.datastack
		ldx	#forth_core_environment_query.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .datastack , "U"
	.assert	@@/,u  =  -1        , ",U = true"
	.assert	@@/2,u = 255        , ",U = 255"
		rts

.c_addr		fcc	'/COUNTED-STRING'
.u		equ	* - .c_addr
._nu		fcb	0

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack	fdb	.u
		fdb	.c_addr
	.endtst

;**********************************************************************

forth_core_evaluate		; ( i*x c-addr u -- j*x )
		fdb	forth_core_environment_query
		fdb	.xt - .name
.name		fcc	"EVALUATE"
.xt		fdb	forth_core_colon.runtime
	;====================================================================
	; : EVALUATE
	;	SAVE-INPUT N>R -1 set_source_id
	;	set-source private-evaluate NR>
	;	RESTORE-INPUT DROP ;
	;====================================================================
		fdb	forth_core_ext_save_input.xt
		fdb	forth_tools_ext_n_to_r.xt
		fdb	forth_core_literal.runtime_xt
		fdb	-1
		fdb	forth__private_set_source_i_d
		fdb	forth__private_set_source
		fdb	forth__private_eval_xt
		fdb	forth_tools_ext_n_r_from.xt
		fdb	forth_core_ext_restore_input.xt
		fdb	forth_core_drop.xt
		fdb	forth_core_exit.xt

	;--------------------------------------------

	.test	"EVALUATE"
	.opt	test	pokew	forth__source     , .dummy1
	.opt	test	pokew	forth__source_len , .dummy1len
	.opt	test	pokew	forth__in         , 5
	.opt	test	pokew	forth__base       , 10
	.opt	test	pokew	forth__state      , 0
	.opt	test	pokew	forth__here       , $6000
	.opt	test	pokew	forth__ds_bottom  , .rs_bot
	.opt	test	pokew	forth__ds_top     , .result1
	.opt	test	prot	rw , $6000 , $6100
	.opt	test	prot	rw , $DE00 , $DEFF
		ldu	#.datastack1
		ldx	#forth_core_evaluate.xt
		jsr	forth_core_execute.asm
	.assert	/u                      = .result1   , "U"
	.assert @@/0,u                  = 123        , "result"
	.assert	@@forth__source         = .dummy1    , "source"
	.assert @@forth__source_len     = .dummy1len , "source-len"
	.assert	@@forth__in             = 5          , ">IN"
		rts

.rs_bot		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack1	fdb	.len1
.result1	fdb	.text1
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0

		nop
.text1		fcc	'123'
.len1		equ	* - .text1
.dummy1		fcc	'dummy'
.dummy1len	equ	* - .dummy1
	.endtst

;**********************************************************************

forth_core_execute		; ( i*x xt -- j*x )
		fdb	forth_core_evaluate
		fdb	.xt - .name
.name		fcc	"EXECUTE"
.xt		fdb	.body
.body		pulu	x
		jmp	[,x]

	;********************************************
	;	forth_core_execute.asm	Allow assembly code to call xt
	;Entry:	X - xt
	;	U - datastack
	;Exit:	D - trashed
	;	X - trashed
	;	Y - saved
	;NOTE:	at least two bytes of stack space on U needs to be free
	;********************************************

.asm		pshs	y		; save Y
		pshu	x		; push xt onto data stack
		ldy	#.asm_body	; point to anonymous xt
		bra	.body

.asm_body	fdb	.asm_exit_xt
.asm_exit_xt	fdb	.asm_exit_code
.asm_exit_code	puls	y,pc		; restore Y and return

	;-----------------------------------------------

	.test	"EXECUTE"
		ldu	#.datastack1
		ldx	#.call_xt
		jsr	forth_core_execute.asm
	.assert	/u    = .result , "U"
	.assert	@@/,u = 5       , ",U"
		rts

		fdb	0
.datastack1	fdb	forth_core_plus.xt
		fdb	2
.result		fdb	3

.call_xt	fdb	forth_core_colon.runtime
		fdb	forth_core_execute.xt
		fdb	forth_core_exit.xt
	.endtst

	;-------------------------------------------------

	.test	"EXECUTE from assembly"
		sts	.stack
		ldu	#.datastack2
		ldy	#0
		ldx	#forth_core_dupe.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .datastack2 - 2 , "U"
	.assert	@@/,u  = 13              , ",U"
	.assert @@/2,u = 13              , "2,U"
	.assert /y     = 0               , "Y == 0"
	.assert	/s     = @@.stack        , "S == stack on entry"
		rts

.stack		fdb	0
		fdb	0
.datastack2	fdb	13
	.endtst

;**********************************************************************

forth_core_exit			; E ( -- ) ( R: nest-sys -- )
		fdb	forth_core_execute
		fdb	_NOINTERP :: .xt - .name
.name		fcc	"EXIT"
.xt		fdb	.body
.body		puls	y
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_fill			; ( c-addr u char -- )
		fdb	forth_core_exit
		fdb	.xt - .name
.name		fcc	"FILL"
.xt		fdb	.body
.body		ldx	2,u
		bne	.doit
		leau	6,u
		bra	.done
.doit		pshs	y
		pulu	y,x,d
.fill		stb	,y+
		leax	-1,x
		bne	.fill
		puls	y
.done		ldx	,y++
		jmp	[,x]

	;-----------------------------------------------

	.test	"FILL 0"
		ldu	#.datastack0
		ldx	#forth_core_fill.xt
		jsr	forth_core_execute.asm
	.assert	/u       = .result0 , "U"
	.assert	@.buff0  = 0        , "buffer"
	.assert @.buff01 = 0        , "2buf"	; XXX - @.buff0+1 is buggy
	.assert @.buff02 = 0        , "3buf"
		rts

		fdb	0
		fdb	0
.datastack0	fdb	' '
		fdb	0
		fdb	.buff0
.result0	fdb	0
		nop
.buff0		fcb	0
.buff01		fcb	0
.buff02		fcb	0
	.endtst

	;------------------------------------------------

	.test	"FILL 1"
		ldu	#.datastack1
		ldx	#forth_core_fill.xt
		jsr	forth_core_execute.asm
	.assert	/u       = .result1 , "U"
	.assert	@.buff1  = 32       , "buffer"
	.assert @.buff11 = 0        , "2buf"
        .assert @.buff12 = 0        , "3buf"
		rts

		fdb	0
		fdb	0
.datastack1	fdb	' '
		fdb	1
		fdb	.buff1
.result1	fdb	0
		nop
.buff1		fcb	0
.buff11		fcb	0
.buff12		fcb	0
	.endtst

	;------------------------------------------------

	.test	"FILL 2"
		ldu	#.datastack2
		ldx	#forth_core_fill.xt
		jsr	forth_core_execute.asm
	.assert	/u       = .result2 , "U"
	.assert	@.buff2  = 32       , "buffer"
	.assert @.buff21 = 32       , "2buf"
        .assert @.buff22 = 0        , "3buf"
		rts

		fdb	0
		fdb	0
.datastack2	fdb	' '
		fdb	2
		fdb	.buff2
.result2	fdb	0
		nop
.buff2		fcb	0
.buff21		fcb	0
.buff22		fcb	0
	.endtst

	;------------------------------------------------

	.test	"FILL 3"
		ldu	#.datastack3
		ldx	#forth_core_fill.xt
		jsr	forth_core_execute.asm
	.assert	/u       = .result3 , "U"
	.assert	@.buff3  = 32       , "buffer"
	.assert @.buff31 = 32       , "2buf"
        .assert @.buff32 = 32       , "3buf"
		rts

		fdb	0
		fdb	0
.datastack3	fdb	' '
		fdb	3
		fdb	.buff3
.result3	fdb	0
		nop
.buff3		fcb	0
.buff31		fcb	0
.buff32		fcb	0
	.endtst

;**********************************************************************
;
;forth_core_find		; SEARCH FIND
;		fdb	forth_core_fill
;		fdb	.xt - .name
;.name		fcc	"FIND"
;.xt		fdb	forth_core_colon.runtime
;		fdb	forth_core_literal.runtime_xt
;		fdb	-13
;		fdb	forth_exception_throw.xt
;
;**********************************************************************

quotient	set	0
remainder	set	2

forth_core_f_m_slash_mod	; ( d1 n1 -- n2 n3 )
		fdb	forth_core_fill
		fdb	.xt - .name
.name		fcc	"FM/MOD"
.xt		fdb	.body
.body		ldd	,u
		pshs	d		; save denominator
		ldx	#forth_core_s_m_slash_rem.xt
		bsr	forth_core_execute.asm
		ldx	,s		; check demoninator
		cmpx	#0		; if (demon > 0)
		ble	.neg_denom
		ldx	remainder,u	; if (remainder < 0)
		bpl	.done
.adjust		ldx	quotient,u
		leax	-1,x
		stx	quotient,u
		ldd	remainder,u
		addd	,s
		std	remainder,u
		bra	.done
.neg_denom	ldx	remainder,u
		cmpx	#0
		bgt	.adjust
.done		leas	2,s
		ldx	,y++
		jmp	[,x]

	;---------------------------------------

	.test	"FM/MOD +numerator +demoninator"
		ldu	#.datastack1
		ldx	#forth_core_f_m_slash_mod.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result1 , "U"
	.assert	@@/0,u = 1        , "q"
	.assert	@@/2,u = 3        , "r"
		rts

		fdb	0
.datastack1	fdb	7
.result1	fdb	0
		fdb	10
	.endtst

	;--------------------------------------

	.test	"FM/MOD -numerator +demoninator"
		ldu	#.datastack2
		ldx	#forth_core_f_m_slash_mod.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result2 , "U"
	.assert	@@/0,u = -2       , "q"
	.assert	@@/2,u = 4        , "r"
		rts

		fdb	0
		fdb	0
		fdb	0
.datastack2	fdb	7
.result2	fdb	$FFFF
		fdb	-10
	.endtst

	;--------------------------------------

	.test	"FM/MOD +numerator -demoninator"
		ldu	#.datastack3
		ldx	#forth_core_f_m_slash_mod.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result3 , "U"
	.assert	@@/0,u = -2       , "q"
	.assert	@@/2,u = -4       , "r"
		rts

		fdb	0
		fdb	0
		fdb	0
.datastack3	fdb	-7
.result3	fdb	0
		fdb	10
	.endtst

	;--------------------------------------

	.test	"FM/MOD -numerator -demoninator"
		ldu	#.datastack4
		ldx	#forth_core_f_m_slash_mod.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result4 , "U"
	.assert	@@/0,u = 1        , "q"
	.assert	@@/2,u = -3       , "r"
		rts

		fdb	0
		fdb	0
		fdb	0
.datastack4	fdb	-7
.result4	fdb	$FFFF
		fdb	-10
	.endtst

;**********************************************************************

forth_core_here			; ( -- addr )
		fdb	forth_core_f_m_slash_mod
		fdb	.xt - .name
.name		fcc	"HERE"
.xt		fdb	.bodyx
.bodyx		ldd	forth__here
		pshu	d
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_hold			; ( char -- )
		fdb	forth_core_here
		fdb	.xt - .name
.name		fcc	"HOLD"
.xt		fdb	.body
.body		pulu	d
		ldx	forth__hold
		stb	,-x
		stx	forth__hold
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_i			; E ( -- n|u ) ( R: loop-sys -- loop-sys )
		fdb	forth_core_hold
		fdb	_NOINTERP :: .xt - .name
.name		fcc	"I"
.xt		fdb	.body
.body		ldd	,s
		pshu	d
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_if			; C ( C: -- orig ) R ( x -- )
		fdb	forth_core_i
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"IF"
.xt		fdb	.body
.body		ldx	forth__here
		ldd	#.runtime_xt	; compile jump
		std	,x++
		pshu	x		; push orig
		clr	,x+
		clr	,x+
		stx	forth__here
		ldx	,y++		; NEXT
		jmp	[,x]

.runtime_xt	fdb	.runtime
.runtime	ldd	,u++		; to set CC
		bne	.true
		ldy	,y		; take branch
		ldx	,y++		; NEXT
		jmp	[,x]
.true		leay	2,y		; skip branch dest
		ldx	,y++		; NEXT
		jmp	[,x]

;**********************************************************************

forth_core_immediate		; ( -- )
		fdb	forth_core_if
		fdb	.xt - .name
.name		fcc	"IMMEDIATE"
.xt		fdb	.body
.body		lda	[forth__create_name]
		ora	#_IMMED
		sta	[forth__create_name]
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_invert		; ( x1 -- x2 )
		fdb	forth_core_immediate
		fdb	.xt - .name
.name		fcc	"INVERT"
.xt		fdb	.body
.body		com	,u
		com	1,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_j			; ( -- n|u ) ( R: loop-sys1 loop-sys2 -- loop-sys1 loop-sys2 )
		fdb	forth_core_invert
		fdb	_NOINTERP :: .xt - .name
.name		fcc	"J"
.xt		fdb	.body
.body		ldd	4,s
		pshu	d
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_key			; ( -- char )
		fdb	forth_core_j
		fdb	.xt - .name
.name		fcc	"KEY"
.xt		fdb	.body
.body		jsr	[forth__vector_getchar]
		pshu	d
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_leave		; E ( -- ) ( R: loop-sys -- )
		fdb	forth_core_key
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"LEAVE"
.xt		fdb	.body
.body		pshs	u		; save register
		ldu	forth__leave_sp	; to use as LEAVE stack ptr
		ldd	#.runtime_xt	; laydown runtime behavior
		ldx	forth__here	; get compile location
		std	,x++		; store
		ldd	-2,u		; get previous LEAVE patch
		stx	-2,u		; store HERE at LEAVE patch
		std	,x++		; save previous LEAVE patch in link list
		stx	forth__here	; update HERE
		puls	u		; restore and exit
		ldx	,y++
		jmp	[,x]

.runtime_xt	fdb	.runtime
.runtime	leas	4,s		; remove loop-sys
		ldy	,y		; GOTO
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_literal		; C ( x -- ) R ( -- x )
		fdb	forth_core_leave
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"LITERAL"
.xt		fdb	.body
.body		ldx	forth__here
		ldd	#.runtime_xt
		std	,x++
		pulu	d
		std	,x++
		stx	forth__here
		ldx	,y++	; NEXT
		jmp	[,x]

.runtime_xt	fdb	.runtime
.runtime	ldd	,y++
		pshu	d
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_loop			; C ( C: do-sys -- ) R ( -- ) ( R: loop-sys1 -- loop-sys2 )
		fdb	forth_core_literal
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"LOOP"
.xt		fdb	.body
.body		ldd	#.runtime_xt			; xt to compile
		lbra	forth_core_plus_loop.rest	; finish loop semantics

.runtime_xt	fdb	.runtime
.runtime	ldx	,s
		leax	1,x
		cmpx	2,s
		bge	.done
		stx	,s
		ldy	,y
		ldx	,y++
		jmp	[,x]
.done		leas	4,s
		leay	2,y
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_l_shift		; ( x1 u -- x2 )
		fdb	forth_core_loop
		fdb	.xt - .name
.name		fcc	"LSHIFT"
.xt		fdb	.body
.body		ldx	,u++		; get count
		beq	.done		; if 0, leave (no shift)
		ldd	,u		; get value to shift
.loop		lslb			; left shift
		rola
		leax	-1,x		; do more bits
		bne	.loop
		std	,u		; save result
.done		ldx	,y++		; NEXT
		jmp	[,x]		

;**********************************************************************

forth_core_m_star		; ( n1 n2 -- d )
		fdb	forth_core_l_shift
		fdb	.xt - .name
.name		fcc	"M*"
.xt		fdb	.body
.body		tfr	u,x
		ldd	2,x
		bsr	.chksign
		std	2,x
		ldd	,x
		bsr	.chksign
		std	,x
		lbsr	forth__math_mul16
		lda	,u+
		eora	,u+
		bpl	.done
		lbsr	forth__math_neg32
.done		ldx	,y++
		jmp	[,x]
.chksign	sta	,-u
		bpl	.skip
		coma
		comb
		addd	#1
.skip		rts

	;------------------------------

	.test	"M* positive positive"
		ldu	#.datastack1
		ldx	#forth_core_m_star.xt
		jsr	forth_core_execute.asm
	.assert	/u = .datastack1
	.assert	@@/0,u = 0
	.assert	@@/2,u = 50*50
		rts

		fdb	0
		fdb	0
.datastack1	fdb	50
		fdb	50
	.endtst

	;------------------------------

	.test	"M* positive negative"
		ldu	#.datastack2
		ldx	#forth_core_m_star.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .datastack2
	.assert	@@/0,u = -1
	.assert	@@/2,u = 50*-50
		rts

		fdb	0
		fdb	0
.datastack2	fdb	50
		fdb	-50
	.endtst

;**********************************************************************

forth_core_max			; ( n1 n2 -- n3 )
		fdb	forth_core_m_star
		fdb	.xt - .name
.name		fcc	"MAX"
.xt		fdb	forth_core_colon.runtime
	;===========================================
	; : MAX	2DUP > IF DROP ELSE NIP THEN ;
	;===========================================
		fdb	forth_core_two_dupe.xt
		fdb	forth_core_greater_than.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L1
		fdb	forth_core_drop.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L2
.L1		fdb	forth_core_ext_nip.xt
.L2		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_min			; ( n1 n2 -- n3 )
		fdb	forth_core_max
		fdb	.xt - .name
.name		fcc	"MIN"
.xt		fdb	forth_core_colon.runtime
	;========================================
	; : MIN	2DUP < IF DROP ELSE NIP THEN ;
	;========================================
		fdb	forth_core_two_dupe.xt
		fdb	forth_core_less_than.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L1
		fdb	forth_core_drop.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L2
.L1		fdb	forth_core_ext_nip.xt
.L2		fdb	forth_core_exit.xt

	;--------------------------------------

	.test	"1 2 MIN"
		ldu	#.datastack1
		ldx	#forth_core_min.xt
		jsr	forth_core_execute.asm
	.assert	/u    = .results1 , "U"
	.assert	@@/,u = 1         , "result"
		rts

		fdb	0
		fdb	0
.datastack1	fdb	2
.results1	fdb	1
	.endtst

	;--------------------------------------

	.test	"2 1 MIN"
		ldu	#.datastack2
		ldx	#forth_core_min.xt
		jsr	forth_core_execute.asm
	.assert	/u = .results2 , "U"
	.assert	@@/,u = 1 , "result"
		rts

		fdb	0
		fdb	0
.datastack2	fdb	1
.results2	fdb	2
	.endtst

;**********************************************************************

forth_core_mod			; ( n1 n2 -- n3 )
		fdb	forth_core_min
		fdb	.xt - .name
.name		fcc	"MOD"
.xt		fdb	forth_core_colon.runtime
	;========================================
	; : MOD		/MOD DROP ;
	;========================================
		fdb	forth_core_slash_mod.xt
		fdb	forth_core_drop.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_move			; ( addr1 addr2 u -- )
		fdb	forth_core_mod
		fdb	.xt - .name
.name		fcc	"MOVE"
.xt		fdb	.body
.body		ldd	4,u	; addr1 >= addr2
		cmpd	2,u
		bhs	.forward
		addd	,u	; addr1[n] < addr2
		cmpd	2,u
		blo	.forward
		ldx	#forth_string_c_move_up.xt
		bra	.done
.forward	ldx	#forth_string_c_move.xt
.done		lbsr	forth_core_execute.asm
		ldx	,y++
		jmp	[,x]

	;----------------------------------

	.test	"MOVE buf buf+1 2"
		ldu	#.datastack1
		ldx	#forth_core_move.xt
		jsr	forth_core_execute.asm
	.assert	/u      = .result1
	.assert	@.buf10 = 12
	.assert	@.buf11 = 12
	.assert	@.buf12 = 34
		rts

		fdb	0
		fdb	0
.datastack1	fdb	2
		fdb	.buf11
		fdb	.buf10
.result1		nop
.buf10		fcb	12
.buf11		fcb	34
.buf12		fcb	56
	.endtst

;**********************************************************************

forth_core_negate		; ( n1 -- n2 )
		fdb	forth_core_move
		fdb	.xt - .name
.name		fcc	"NEGATE"
.xt		fdb	.body
.body		clra
		clrb
		subd	,u
		std	,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_or			; ( x1 x2 -- x3 )
		fdb	forth_core_negate
		fdb	.xt - .name
.name		fcc	"OR"
.xt		fdb	.body
.body		pulu	d
		ora	,u
		orb	1,u
		std	,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_over			; ( x1 x2 -- x1 x2 x1 )
		fdb	forth_core_or
		fdb	.xt - .name
.name		fcc	"OVER"
.xt		fdb	.body
.body		ldd	2,u
		pshu	d
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_postpone		; ( "<spaces>name" -- )
		fdb	forth_core_over
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"POSTPONE"
.xt		fdb	forth_core_colon.runtime
	;=================================================
	; : POSTPONE
	; ( 1 )	BL WORD FIND CASE
	; ( 2 )		 0 OF -48 THROW ENDOF
	; ( 3 )		 1 OF COMPILE,  ENDOF
	; ( 4 )		-1 OF LITERAL ['] COMPILE, COMPILE, ENDOF
	; ( 5 )	ENDCASE 
	; ( 6 )	; IMMEDIATE
	;=================================================
		fdb	forth_core_b_l.xt		; ( 1 )
		fdb	forth_core_word.xt
		fdb	forth_search_find.xt
		fdb	forth_core_literal.runtime_xt	; ( 2 )
		fdb	0
		fdb	forth_core_ext_of.runtime_xt
		fdb	.L3
		fdb	forth_core_literal.runtime_xt
		fdb	-13
		fdb	forth_exception_throw.xt
.L3		fdb	forth_core_literal.runtime_xt
		fdb	1
		fdb	forth_core_ext_of.runtime_xt
		fdb	.L4
		fdb	forth_core_ext_compile_comma.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L6
.L4		fdb	forth_core_literal.runtime_xt
		fdb	-1
		fdb	forth_core_ext_of.runtime_xt
		fdb	.L5
		fdb	forth_core_literal.xt
		fdb	forth_core_literal.runtime_xt
		fdb	forth_core_ext_compile_comma.xt
		fdb	forth_core_ext_compile_comma.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L6
.L5		fdb	forth_core_drop.xt
.L6		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_quit			; ( -- ) ( R: i*x -- )
		fdb	forth_core_postpone
		fdb	.xt - .name
.name		fcc	"QUIT"
.xt		fdb	forth_core_colon.runtime
	;===================================================================
	; : (abort) ." ABORT!" CR reset-dsp QUIT ;
	; : QUIT
	;	reset-rsp
	;	0 set-source-id POSTPONE [
	;	BEGIN REFILL WHILE
	;		['] eval CATCH CASE
	;			 0 OF STATE @ 0= IF ." OK" CR THEN ENDOF
	;			-1 OF (abort)                      ENDOF
	;			-2 OF (abort") (abort)             ENDOF
	;			DUP ." EXCEPTION #" . CR
	;		ENDCASE
	;	REPEAT BYE ;
	;=====================================================================
		fdb	forth__private_reset_rsp_xt
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth__private_set_source_i_d
		fdb	forth_core_left_bracket.xt
.L3		fdb	forth_core_ext_refill.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L11
		fdb	forth_core_literal.runtime_xt
		fdb	forth__private_eval_xt
		fdb	forth_exception_catch.xt
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_ext_of.runtime_xt
		fdb	.L6
		fdb	forth_tools_ext_state.xt
		fdb	forth_core_fetch.xt
		fdb	forth_core_zero_equals.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L5
	;	fdb	forth_string_sliteral.runtime_xt	; XXX
	;	fdb	3
	;	fcc	' OK'
	;	fdb	forth_core_type.xt
	;	fdb	forth_core_c_r.xt			; XXX
.L5		fdb	forth_core_ext_again.runtime_xt
		fdb	.L10
.L6		fdb	forth_core_literal.runtime_xt
		fdb	-1
		fdb	forth_core_ext_of.runtime_xt
		fdb	.L7
		fdb	.abort_xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L10
.L7		fdb	forth_core_literal.runtime_xt
		fdb	-2
		fdb	forth_core_ext_of.runtime_xt
		fdb	.L8
		fdb	.abortq_xt
		fdb	.abort_xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L10
.L8		fdb	forth_core_dupe.xt
		fdb	forth_string_sliteral.runtime_xt
		fdb	.expmsg_len
.expmsg		fcc	'EXCEPTION #'
.expmsg_len	equ	* - .expmsg
		fdb	forth_core_type.xt
		fdb	forth_core_dot.xt
		fdb	forth_core_c_r.xt
		fdb	forth_core_drop.xt
.L10		fdb	forth_core_ext_again.runtime_xt
		fdb	.L3
.L11		fdb	forth_tools_ext_bye.xt
		fdb	forth_core_exit.xt

.abortq_xt	fdb	.abortq_body
.abortq_body	ldx	forth__abortq
		ldd	forth__abortql
		pshu	x,d
		ldx	#forth_core_type.xt
		lbsr	forth_core_execute.asm
		ldb	#' '
		jsr	[forth__vector_putchar]
		ldx	,y++
		jmp	[,x]

.abort_xt	fdb	forth_core_colon.runtime
		fdb	forth_string_sliteral.runtime_xt
		fdb	.abort_msg_len
.abort_msg	fcc	"ABORT!"
.abort_msg_len	equ	* - .abort_msg
		fdb	forth_core_type.xt
		fdb	forth_core_c_r.xt
		fdb	forth__private_reset_dsp_xt
		fdb	forth_core_quit.xt
		fdb	forth_core_exit.xt

	;--------------------------------------------------

	.test	"QUIT ( 123 987 + . BYE )"
	.opt	test	prot	rw , forth__free , $DFFF
	.opt	test	prot	n  , .nu1
	.opt	test	pokew	forth__vector_bye     , .bye
	.opt	test	pokew	forth__vector_getchar , .getchar
	.opt	test	pokew	forth__vector_putchar , .putchar
	.opt	test	pokew	forth__here_top       , $DB00
	.opt	test	pokew	forth__ds_bottom      , $DB00
	.opt	test	pokew	forth__ds_top         , $DC00
	.opt	test	pokew	forth__source         , .source
	.opt	test	pokew	forth__source_len     , 0
	.opt	test	pokew	forth__base           , 10
	.opt	test	pokew	forth__here           , forth__free
		leax	2,s
		stx	forth__rs_top
		leax	-256,x
		stx	forth__rs_bottom
		sts	.ret
		ldu	forth__ds_top
		ldx	#forth_core_quit.xt
		jsr	forth_core_execute.asm
	.assert	1 = 0 , "wrong bye"
		rts

.getchar	pshs	x
		ldx	.input
		clra
		ldb	,x+
		stx	.input
		puls	x,pc
.input		fdb	.inputbuf
.inputbuf	ascii	'123 987 + .\nBYE\n'
.nu1		fcb	0

.putchar	pshs	x
		ldx	.output
		stb	,x+
		stx	.output
		puls	x,pc
.output		fdb	.outputbuf
.outputbuf	rmb	32		; "1110 \nOK\n"

.bye		lds	.ret
	.assert	/u         = @@forth__ds_top , "U (bye)"
	.assert	.source    = "BYE"           , "source"
	.assert .outputbuf = "1110  OK\n"    , "output"
		rts

.ret		fdb	0

.source		rmb	INPUT_SIZE

	.endtst

;**********************************************************************

forth_core_r_from		; E ( -- x ) ( R: x -- )
		fdb	forth_core_quit
		fdb	_NOINTERP :: .xt - .name
.name		fcc	"R>"
.xt		fdb	.body
.body		puls	d
		pshu	d
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_r_fetch		; E ( -- x ) ( R: x -- x )
		fdb	forth_core_r_from
		fdb	_NOINTERP :: .xt - .name
.name		fcc	"R@"
.xt		fdb	.body
.body		ldd	,s
		pshu	d
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_recurse		; ( -- )
		fdb	forth_core_r_fetch
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"RECURSE"
.xt		fdb	.body
.body		ldx	forth__here		; get current comp location
		ldd	forth__create_xt	; get xt of current word
		std	,x++			; recurse
		stx	forth__here
		ldx	,y++			; NEXT
		jmp	[,x]

	;----------------------------------------------------

	.test	"RECURSE runtime"
	.opt	test	prot n , ._nw2
		ldu	#.datastack
		ldx	#.recurse_xt
		jmp	forth_core_execute.asm

		fdb	0
		fdb	0
.datastack	fdb	7
._nw2		fcb	0

.recurse_xt	fdb	forth_core_colon.runtime
		fdb	forth_core_dupe.xt
		fdb	forth_core_zero_equals.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L1
		fdb	forth_core_exit.xt
.L1		fdb	forth_core_one_minus.xt
		fdb	.recurse_xt
		fdb	forth_core_exit.xt
	.endtst

;**********************************************************************

forth_core_repeat		; C ( C: orig dest -- ) R ( -- )
		fdb	forth_core_recurse
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"REPEAT"
.xt		fdb	forth_core_colon.runtime
	;===============================================
	; : REPEAT	POSTPONE AGAIN POSTPONE THEN ;
	;===============================================
		fdb	forth_core_ext_again.xt
		fdb	forth_core_then.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_rote			; ( x1 x2 x3 -- x2 x3 x1 )
		fdb	forth_core_repeat
		fdb	.xt - .name
.name		fcc	"ROT"
.xt		fdb	.body
.body		pshs	x,y
		pulu	y,x,d
		pshu	x,d
		pshu	y
		puls	x,y
		ldx	,y++
		jmp	[,x]

	;---------------------------------

	.test	"ROT"
		ldu	#.datastack
		ldx	#forth_core_rote.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .datastack , "U"
	.assert	@@/,u  = 1          , ",U"
	.assert	@@/2,u = 3          , "2,U"
	.assert	@@/4,u = 2          , "4,U"
		rts

		fdb	0
.datastack	fdb	3
		fdb	2
		fdb	1
	.endtst

;**********************************************************************

forth_core_r_shift		; ( x1 u -- x2 )
		fdb	forth_core_rote
		fdb	.xt - .name
.name		fcc	"RSHIFT"
.xt		fdb	.body
.body		ldx	,u++		; get count
		beq	.done		; if 0, leave (no shift)
		ldd	,u		; get value to shift
.loop		lsra			; right shift
		rorb
		leax	-1,x		; do more bits
		bne	.loop
		std	,u		; save result
.done		ldx	,y++		; NEXT
		jmp	[,x]		

;**********************************************************************

forth_core_s_quote		; ( "ccc<quote>" -- ) ( -- c-addr u )
		fdb	forth_core_r_shift
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	'S"'
.xt		fdb	forth_core_colon.runtime
	;===================================================
	; : S"	[CHAR] " PARSE POSTPONE SLITERAL ; IMMEDIATE
	;===================================================
		fdb	forth_core_literal.runtime_xt
		fdb	'"'
		fdb	forth_core_ext_parse.xt
		fdb	forth_string_sliteral.xt
		fdb	forth_core_exit.xt

	;--------------------------------------------

	.test	'S" compile'
	.opt	test	pokew forth__source     , .buffer1
	.opt	test	pokew forth__source_len , .len1
	.opt	test	pokew forth__in         , 0
	.opt	test	pokew forth__here       , .foo_body
		ldu	#.datastack1
		ldx	#forth_core_s_quote.xt
		jsr	forth_core_execute.asm
	.assert	/u          = .datastack1                , "U"
	.assert	@@.foo_body = forth_string_sliteral.runtime_xt , "xt"
	.assert	@@.foo_len  = .len1                      , "len"
	.assert	.foo_addr   = 'This is a message'        , "text"
	.assert @@.stop     = -1                         , "no-write"
		rts

		fdb	0
		fdb	0
.datastack1	fdb	0

.buffer1	fcc	'This is a message'
.len1		equ	* - .buffer1

		fdb	forth_core_colon.runtime
.foo_body	fdb	0
.foo_len	fdb	0
.foo_addr	rmb	.len1
.stop		fdb	-1
	.endtst

	;---------------------------------------------

	.test	'S" output test'
		ldu	#.datastack2
		ldx	#.foo_xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result2 , "U"
	.assert	@@/0,u = .len2    , "len"
	.assert @@/2,u = .text2   , "text"
		rts

.result2	fdb	0
		fdb	0
.datastack2	fdb	0

.foo_xt		fdb	forth_core_colon.runtime
		fdb	forth_string_sliteral.runtime_xt
		fdb	.len2
.text2		fcc	'test'
.len2		equ	* - .text2
		fdb	forth_core_exit.xt
	.endtst

;**********************************************************************

forth_core_s_to_d		; ( n -- d )
		fdb	forth_core_s_quote
		fdb	.xt - .name
.name		fcc	"S>D"
.xt		fdb	forth_core_colon.runtime
	;=======================================
	; : S>D	DUP 0< ;
	;=======================================
		fdb	forth_core_dupe.xt
		fdb	forth_core_zero_less.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_sign			; ( n -- )
		fdb	forth_core_s_to_d
		fdb	.xt - .name
.name		fcc	"SIGN"
.xt		fdb	forth_core_colon.runtime
	;=============================================
	; : SIGN	0< IF [CHAR] - HOLD THEN ;
	;=============================================
		fdb	forth_core_zero_less.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L1
		fdb	forth_core_literal.runtime_xt
		fdb	'-'
		fdb	forth_core_hold.xt
.L1		fdb	forth_core_exit.xt

;**********************************************************************

Pnumerator	set	4
Pdemoninator	set	2
Psnum		set	1
Psdemon		set	0

Rremainder	set	6
Rquotient	set	4

forth_core_s_m_slash_rem	; ( d1 n2 -- n2 n3 )
		fdb	forth_core_sign
		fdb	.xt - .name
.name		fcc	"SM/REM"
.xt		fdb	.body
.body		ldd	,u		; check for d1/0
		beq	.throw_div0
		leau	-2,u		; space for sign flags
		ldd	Pdemoninator,u	; check sign of demoninator
		sta	Psdemon,u	; save sign for demoninator
		bpl	.10
		coma			; negate demoninator
		comb
		addd	#1
		std	Pdemoninator,u
.10		lda	Pnumerator,u	; check sign of numerator
		sta	Psnum,u	; save sign for numerator
		bpl	.20		
		leax	Pnumerator,u	; negate numerator
		lbsr	forth__math_neg32
.20		leax	Pdemoninator,u	; do that divide thang
		lbsr	forth__math_div32
		lda	Psnum,u		; adjust sign of quotient
		eora	Psdemon,u
		bpl	.30
		ldd	Rquotient,u
		coma
		comb
		addd	#1
		std	Rquotient,u
.30		tst	Psnum,u		; adjust sign of remainder
		bpl	.40
		ldd	Rremainder,u	; if matches sign of numerator
		coma
		comb
		addd	#1
		std	Rremainder,u
.40		leau	4,u		; adjust data stack
		ldx	,y++
		jmp	[,x]
.throw_div0	ldd	#-10
		lbra	forth_exception_throw.asm

	;--------------------------------------

	.test	"SM/REM +numerator +demoninator"
		ldu	#.datastack1
		ldx	#forth_core_s_m_slash_rem.xt
		jsr	forth_core_execute.asm
	.assert	/u = .result1
	.assert	@@/0,u = 1
	.assert	@@/2,u = 3
		rts

		fdb	0
.datastack1	fdb	7
.result1	fdb	0
		fdb	10
	.endtst

	;--------------------------------------

	.test	"SM/REM -numerator +demoninator"
		ldu	#.datastack2
		ldx	#forth_core_s_m_slash_rem.xt
		jsr	forth_core_execute.asm
	.assert	/u = .result2
	.assert	@@/0,u = -1
	.assert	@@/2,u = -3
		rts

		fdb	0
		fdb	0
		fdb	0
.datastack2	fdb	7
.result2	fdb	$FFFF
		fdb	-10
	.endtst

	;--------------------------------------

	.test	"SM/REM +numerator -demoninator"
		ldu	#.datastack3
		ldx	#forth_core_s_m_slash_rem.xt
		jsr	forth_core_execute.asm
	.assert	/u = .result3
	.assert	@@/0,u = -1
	.assert	@@/2,u = 3
		rts

		fdb	0
		fdb	0
		fdb	0
.datastack3	fdb	-7
.result3	fdb	0
		fdb	10
	.endtst

	;--------------------------------------

	.test	"SM/REM -numerator -demoninator"
		ldu	#.datastack4
		ldx	#forth_core_s_m_slash_rem.xt
		jsr	forth_core_execute.asm
	.assert	/u = .result4
	.assert	@@/0,u = 1
	.assert	@@/2,u = -3
		rts

		fdb	0
		fdb	0
		fdb	0
.datastack4	fdb	-7
.result4	fdb	$FFFF
		fdb	-10
	.endtst

;**********************************************************************

forth_core_source		; ( -- c-addr u )
		fdb	forth_core_s_m_slash_rem
		fdb	.xt - .name
.name		fcc	"SOURCE"
.xt		fdb	.body
.body		ldx	forth__source
		ldd	forth__source_len
		pshu	x,d
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_space		; ( n -- )
		fdb	forth_core_source
		fdb	.xt - .name
.name		fcc	"SPACE"
.xt		fdb	forth_core_colon.runtime
	;=======================================
	; : SPACE	BL EMIT ;
	;=======================================
		fdb	forth_core_b_l.xt
		fdb	forth_core_emit.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_spaces		; ( n -- )
		fdb	forth_core_space
		fdb	.xt - .name
.name		fcc	"SPACES"
.xt		fdb	forth_core_colon.runtime
	;====================================
	; SPACES	0 ?DO SPACE LOOP ;
	;====================================
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_ext_question_do.runtime_xt
		fdb	.L1
.L2		fdb	forth_core_space.xt
		fdb	forth_core_loop.runtime_xt
		fdb	.L2
.L1		fdb	forth_core_exit.xt

;**********************************************************************
;
;forth_core_state		; TOOLS-EXT STATE
;		fdb	forth_core_spaces
;		fdb	.xt - .name
;.name		fcc	"STATE"
;.xt		fdb	forth_core_spaces
;		fdb	forth_core_colon.runtime
;		fdb	forth_core_literal.runtime_xt
;		fdb	-13
;		fdb	forth_exception_throw.xt
;
;**********************************************************************

forth_core_swap			; ( x1 x2 -- x2 x1 )
		fdb	forth_core_spaces
		fdb	.xt - .name
.name		fcc	"SWAP"
.xt		fdb	.body
.body		pulu	x,d
		exg	d,x
		pshu	x,d
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_then			; C ( C: orig -- ) R ( -- )
		fdb	forth_core_swap
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"THEN"
.xt		fdb	.body
.body		pulu	x		; get orig
		ldd	forth__here
		std	,x
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_type			; ( c-addr u -- )
		fdb	forth_core_then
		fdb	.xt - .name
.name		fcc	"TYPE"
.xt		fdb	forth_core_colon.runtime
	;===================================================
	; : TYPE	0 ?DO DUP C@ EMIT CHAR+ LOOP DROP ;
	;==================================================
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_ext_question_do.runtime_xt
		fdb	.L1
.L2		fdb	forth_core_dupe.xt
		fdb	forth_core_c_fetch.xt
		fdb	forth_core_emit.xt
		fdb	forth_core_char_plus.xt
		fdb	forth_core_loop.runtime_xt
		fdb	.L2
.L1		fdb	forth_core_drop.xt
		fdb	forth_core_exit.xt

	;------------------------------------------

	.test	"TYPE"
	.opt	test	pokew	forth__vector_putchar , .sysnul
		ldu	#.datastack
		ldx	#forth_core_type.xt
		jmp	forth_core_execute.asm

.sysnul		rts

		fdb	0
.datastack	fdb	.len
		fdb	.text

.text		fcc	'Hello, world!'
.len		equ	* - .text
	.endtst

;**********************************************************************

forth_core_u_dot		; ( u -- )
		fdb	forth_core_type
		fdb	.xt - .name
.name		fcc	"U."
.xt		fdb	forth_core_colon.runtime
	;======================================
	; : U.	0 <# #S #> TYPE SPACE ;
	;======================================
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_less_number_sign.xt
		fdb	forth_core_number_sign_s.xt
		fdb	forth_core_number_sign_greater.xt
		fdb	forth_core_type.xt
		fdb	forth_core_space.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_u_less_than		; ( u -- )
		fdb	forth_core_u_dot
		fdb	.xt - .name
.name		fcc	"U<"
.xt		fdb	.body
.body		ldd	2,u
		cmpd	,u++
		blo	.lessthan
		clra
		clrb
		bra	.done
.lessthan	ldd	#-1
.done		std	,u
		ldx	,y++	; NEXT
		jmp	[,x]

;**********************************************************************

forth_core_u_m_star		; ( u1 u2 -- ud )
		fdb	forth_core_u_less_than
		fdb	.xt - .name
.name		fcc	"UM*"
.xt		fdb	.body
.body		tfr	u,x
		lbsr	forth__math_mul16
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_u_m_slash_mod	; ( ud u1 -- u2 u3 )
		fdb	forth_core_u_m_star
		fdb	.xt - .name
.name		fcc	"UM/MOD"
.xt		fdb	.body
.body		ldd	,u	; check for ud/0
		lbeq	forth_core_s_m_slash_rem.throw_div0
		tfr	u,x
		lbsr	forth__math_div32
		leau	2,u
		ldx	,y++
		jmp	[,x]

	;-----------------------------------------

	.test	"UM/MOD"
		ldu	#.datastack1
		ldx	#forth_core_u_m_slash_mod.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result1 , "U"
	.assert	@@/0,u = 1        , "q"
	.assert	@@/2,u = 3        , "r"
		rts

		fdb	0
		fdb	0
.datastack1	fdb	7
.result1	fdb	0
		fdb	10
	.endtst

	;--------------------------------------

	.test	"UM/MOD max"
		ldu	#.datastack2
		ldx	#forth_core_u_m_slash_mod.xt
		jsr	forth_core_execute.asm
	.assert	/u = .result2 , "U"
	.assert @@/0,u = $FFFF , "q"
	.assert	@@/2,u = 0     , "r"
		rts

		fdb	0
		fdb	0
.datastack2	fdb	$FFFF
.result2	fdb	$FFFE
		fdb	$0001
	.endtst

;**********************************************************************

forth_core_unloop		; ( -- ) ( R: loop-sys -- )
		fdb	forth_core_u_m_slash_mod
		fdb	_NOINTERP :: .xt - .name
.name		fcc	"UNLOOP"
.xt		fdb	.body
.body		leas	4,s
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_until		; ( C: dest -- ) ( x -- )
		fdb	forth_core_unloop
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"UNTIL"
.xt		fdb	.body
.body		ldx	forth__here
		ldd	#.runtime_xt
		std	,x++
		pulu	d
		std	,x++
		stx	forth__here
		ldx	,y++
		jmp	[,x]

.runtime_xt	fdb	.runtime
.runtime	ldd	,u++		; test condition
		bne	.done		; if false, don't jump
		ldy	,y		; jump
		ldx	,y++
		jmp	[,x]
.done		leay	2,y
		ldx	,y++		; NEXT
		jmp	[,x]

;**********************************************************************

forth_core_variable		; ( "<spaces>name" -- ) ( -- a-addr )
		fdb	forth_core_until
		fdb	.xt - .name
.name		fcc	"VARIABLE"
.xt		fdb	forth_core_colon.runtime
	;===============================
	; : VARIABLE	CREATE 0 , ;
	;===============================
		fdb	forth_core_create.xt
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_comma.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_while		; C ( C: dest -- orig dest ) R ( x -- )
		fdb	forth_core_variable
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"WHILE"
.xt		fdb	forth_core_colon.runtime
	;=======================================
	; : WHILE	POSTPONE IF 1 CS-ROLL ;
	;=======================================
		fdb	forth_core_if.xt
		fdb	forth_core_literal.runtime_xt
		fdb	1
		fdb	forth_tools_ext_c_s_roll.xt
		fdb	forth_core_exit.xt

;**********************************************************************
;	WORD
;
; This parses from the input buffer.  The input buffer cannot be
; changed, but the results of WORD can be, so the parse is copied into
; an internal buffer before returning.
;
;**********************************************************************

forth_core_word			; ( char "<chars>ccc<char>" -- c-addr )
		fdb	forth_core_while
		fdb	.xt - .name
.name		fcc	"WORD"
.xt		fdb	.body

.body		ldd	,u			; get delimiter
		pshs	y,b			; U,Y and delimiter
		ldx	forth__source
		ldd	forth__source_len
		leay	d,x
		pshs	y
		ldd	forth__in
		leax	d,x
.skip_delim	cmpx	,s
		bhs	.no_input
		lda	,x+
		cmpa	2,s
		beq	.skip_delim
		leax	-1,x			; adjust buffer pointer
		tfr	x,d			; adjust >IN
		subd	forth__source
		std	forth__in
		ldx	#forth_core_ext_parse.xt ; PARSE
		lbsr	forth_core_execute.asm
		ldd	,u++			; get count
.resume		ldy	forth__here
		leay	SLASH_PAD,y
		ldx	,u			; get buffer
		sty	,u			; return c-addr
		tsta				; too long for counted string?
		bne	.throw			; if so, throw
		stb	,y+			; save length
		beq	.done			; if 0, skip copy
.copy		lda	,x+
		sta	,y+
		decb
		bne	.copy
.done		puls	y,x,b			; clean up stack
		ldx	,y++			; NEXT
		jmp	[,x]
.no_input	clra
		clrb
		bra	.resume
.throw		ldd	#-18
		lbra	forth_exception_throw.asm

	;-------------------------------------------

	.test	"[CHAR] ) WORD"
	.opt	test	pokew forth__here       , .here
	.opt	test	pokew forth__source     , .buffer1
	.opt	test	pokew forth__source_len , .len1
	.opt	test	pokew forth__in         , 2
		ldu	#.datastack1
		ldx	#forth_core_word.xt
		jsr	forth_core_execute.asm
	.assert	/u          = .datastack1 , "U"
	.assert	@@/,u       = .word_len   , "c-addr"
	.assert	@.word_len  = 3           , "len"
	.assert .word_text  = 'one'       , "text"
	.assert @@forth__in = 6           , ">IN"
		rts

		fdb	0
.datastack1	fdb	')'

.buffer1	fcc	'( one) '
.len1		equ	* - .buffer1

.here		rmb	SLASH_PAD
.word_len	fcb	0
.word_text	fdb	0
		fdb	0
	.endtst

	;-------------------------------------------

	.test	"BL WORD (spaces)"
	.opt	test	pokew forth__here       , .here2
	.opt	test	pokew forth__source     , .buffer2
	.opt	test	pokew forth__source_len , .len2
	.opt	test	pokew forth__in         , 0
		ldu	#.datastack2
		ldx	#forth_core_word.xt
		jsr	forth_core_execute.asm
	.assert	/u          = .datastack2 , "U"
	.assert	@@/,u       = .word_len2  , "c-addr"
	.assert	@.word_len2 = 0           , "len"
	.assert @@forth__in = 0           , ">IN"
		rts

		fdb	0
.datastack2	fdb	' '

.buffer2	fcc	'      '
.len2		equ	* - .buffer2

.here2		rmb	SLASH_PAD
.word_len2	fcb	0
		fdb	0
		fdb	0
	.endtst

;**********************************************************************

forth_core_x_or			; ( x1 x2 -- x3 )
		fdb	forth_core_word
		fdb	.xt - .name
.name		fcc	"XOR"
.xt		fdb	.body
.body		ldd	,u++
		eora	,u
		eorb	1,u
		std	,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_left_bracket 	; ( -- )
		fdb	forth_core_x_or
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"["
.xt		fdb	forth_core_colon.runtime
	;=======================================
	; : [	FALSE STATE ! ; IMMEDIATE
	;=======================================
		fdb	forth_core_ext_false.xt
		fdb	forth_tools_ext_state.xt
		fdb	forth_core_store.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_bracket_tick		; ( "<spaces>name" -- )
		fdb	forth_core_left_bracket
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"[']"
.xt		fdb	forth_core_colon.runtime
	;==========================================
	; : [']	' LITERAL ; IMMEDIATE
	;==========================================
		fdb	forth_core_tick.xt
		fdb	forth_core_literal.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_bracket_char		; ( "<space>name"  -- )
		fdb	forth_core_bracket_tick
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"[CHAR]"
.xt		fdb	forth_core_colon.runtime
	;=========================================
	; [CHAR]	CHAR LITERAL ; IMMEDIATE
	;=========================================
		fdb	forth_core_char.xt
		fdb	forth_core_literal.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_right_bracket	; ( -- )
		fdb	forth_core_bracket_char
		fdb	.xt - .name
.name		fcc	"]"
.xt		fdb	forth_core_colon.runtime
	;========================================
	; : ]	TRUE STATE ! ;
	;========================================
		fdb	forth_core_ext_true.xt
		fdb	forth_tools_ext_state.xt
		fdb	forth_core_store.xt
		fdb	forth_core_exit.xt

;**********************************************************************
;		CORE-EXT
;**********************************************************************

forth_core_ext_dot_paren	; ( "ccc<paren>" -- )
		fdb	forth_core_right_bracket
		fdb	_IMMED :: .xt - .name
.name		fcc	".("
.xt		fdb	forth_core_colon.runtime
	;=======================================
	; : .(	[CHAR] ) PARSE TYPE ; IMMEDIATE
	;=======================================
		fdb	forth_core_literal.runtime_xt
		fdb	')'
		fdb	forth_core_ext_parse.xt
		fdb	forth_core_type.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_ext_dot_r		; ( n1 n2 -- )
		fdb	forth_core_ext_dot_paren
		fdb	.xt - .name
.name		fcc	".R"
.xt		fdb	forth_core_colon.runtime
	;==================================================
	; : .R	SWAP DUP >R ABS 0 <# #S R> SIGN #>
	;	ROT 2DUP < IF OVER - SPACES ELSE DROP THEN
	;	TYPE SPACE ;
	;==================================================
		fdb	forth_core_swap.xt
		fdb	forth_core_dupe.xt
		fdb	forth_core_to_r.xt
		fdb	forth_core_abs.xt
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_less_number_sign.xt
		fdb	forth_core_number_sign_s.xt
		fdb	forth_core_r_from.xt
		fdb	forth_core_sign.xt
		fdb	forth_core_number_sign_greater.xt
		fdb	forth_core_rote.xt
		fdb	forth_core_two_dupe.xt
		fdb	forth_core_less_than.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L1
		fdb	forth_core_over.xt
		fdb	forth_core_minus.xt
		fdb	forth_core_spaces.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L2
.L1		fdb	forth_core_drop.xt
.L2		fdb	forth_core_type.xt
		fdb	forth_core_space.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_ext_zero_not_equals	; ( x -- flag )
		fdb	forth_core_ext_dot_r
		fdb	.xt - .name
.name		fcc	"0<>"
.xt		fdb	forth_core_colon.runtime
	;==========================================
	; : 0<>	0 <> ;
	;==========================================
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_ext_not_equals.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_ext_zero_greater	; ( x -- flag )
		fdb	forth_core_ext_zero_not_equals
		fdb	.xt - .name
.name		fcc	"0>"
.xt		fdb	forth_core_colon.runtime
	;=====================================
	; : 0>	0 > ;
	;=====================================
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_greater_than.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_ext_two_to_r		; E ( x1 x2 -- ) ( R: -- x1 x2 )
		fdb	forth_core_ext_zero_greater
		fdb	_NOINTERP :: .xt - .name
.name		fcc	"2>R"
.xt		fdb	.body
.body		pulu	x,d
		pshs	x,d
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_ext_two_r_from	; E ( -- x1 x2 ) ( R: x1 x2 -- )
		fdb	forth_core_ext_two_to_r
		fdb	_NOINTERP :: .xt - .name
.name		fcc	"2R>"
.xt		fdb	.body
.body		puls	x,d
		pshu	x,d
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_ext_two_r_fetch	; E ( -- x1 x2 ) ( R: x1 x2 -- x1 x2 )
		fdb	forth_core_ext_two_r_from
		fdb	_NOINTERP :: .xt - .name
.name		fcc	"2R@"
.xt		fdb	.body
.body		ldd	,s
		ldx	2,s
		pshu	x,d
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_ext_colon_no_name	; ( C: -- colon-sys ) ( S: -- xt )
		fdb	forth_core_ext_two_r_fetch
		fdb	.xt - .name
.name		fcc	":NONAME"
.xt		fdb	.body
.body		ldd	#forth__leave_stack
		std	forth__leave_sp
		clra
		clrb
		std	forth__create_link	; no link
		std	forth__create_name	; no name
		ldd	#forth_core_colon.runtime ; set runtime action
		ldx	forth__here
		pshu	x			; save xt
		stx	forth__create_xt	; save create xt
		std	,x++			; compile runtime action
		stx	forth__here
		inc	forth__state + 1	; enter compilation state
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_ext_not_equals	; (x1 x2 -- flag )
		fdb	forth_core_ext_colon_no_name
		fdb	.xt - .name
.name		fcc	"<>"
.xt		fdb	.body
.body		ldd	,u++
		cmpd	,u
		bne	.not_equal
		clra
		clrb
		bra	.done
.not_equal	ldd	#-1
.done		std	,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_ext_question_do	; C ( C: -- do-sys ) R ( n1|u1 n2|u2 -- ) ( R: -- loop-sys )
		fdb	forth_core_ext_not_equals
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"?DO"
.xt		fdb	.body
.body		ldx	forth__leave_sp
		cmpx	#forth__leave_stack + 16
		lbeq	forth_core_do.throw_toodeep
		clra
		clrb
		std	,x++
		stx	forth__leave_sp
		ldx	forth__here	; lay down runtime
		ldd	#.runtime_xt
		std	,x++
		tfr	x,d		; c-orig
		leax	2,x		; u-dest
		pshu	x,d		; push c-orig u-dest
		stx	forth__here	; update compile location
		ldx	,y++		; NEXT
		jmp	[,x]
		
.runtime_xt	fdb	.runtime
.runtime	ldd	,u		; get starting point
		cmpd	2,u		; is it equal to ending point?
		beq	.skip		; if so, skip loop
		leay	2,y
		jmp	forth_core_ext_two_to_r.body
.skip		leau	4,u		; burn parameters
		ldy	,y		; GOTO
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_ext_action_of	; I ( "<spaces>name" -- xt ) C ( "<spaces>name" -- ) R ( -- xt )
		fdb	forth_core_ext_question_do
		fdb	_IMMED :: .xt - .name
.name		fcc	"ACTION-OF"
.xt		fdb	forth_core_colon.runtime
	;==============================================================
	; : ACTION-OF	' STATE @ IF
	;		LITERAL POSTPONE DEFER@
	;	ELSE DEFER@ THEN ;
	;==============================================================
		fdb	forth_core_tick.xt
		fdb	forth_tools_ext_state.xt
		fdb	forth_core_fetch.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L1
		fdb	forth_core_literal.xt
		fdb	forth_core_literal.runtime_xt
		fdb	forth_core_ext_defer_fetch.xt
		fdb	forth_core_ext_compile_comma.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L2
.L1		fdb	forth_core_ext_defer_fetch.xt
.L2		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_ext_again		; C ( C: dest -- ) R ( -- )
		fdb	forth_core_ext_action_of
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"AGAIN"
.xt		fdb	.body
.body		ldx	forth__here	; compile runtime
		ldd	#.runtime_xt
		std	,x++
		pulu	d		; pull dest
		std	,x++		; compile destination
		stx	forth__here
		ldx	,y++		; NEXT
		jmp	[,x]

.runtime_xt	fdb	.runtime
.runtime	ldy	,y	; GOTO
		ldx	,y++	; NEXT
		jmp	[,x]

;**********************************************************************

forth_core_ext_buffer_colon	; ( u "<spaces>name" -- ) ( -- a-addr )
		fdb	forth_core_ext_again
		fdb	.xt - .name
.name		fcc	"BUFFER:"
.xt		fdb	forth_core_colon.runtime
	;========================================
	; : BUFFER:	CREATE ALIGN ALLOT ;
	;========================================
		fdb	forth_core_create.xt
		fdb	forth_core_align.xt
		fdb	forth_core_allot.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_ext_c_quote		; ( "ccc<quote>" -- ) ( -- c-addr )
		fdb	forth_core_ext_buffer_colon
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	'C"'
.xt		fdb	.body
.body		ldd	#'"'
		pshu	d
		ldx	#forth_core_ext_parse.xt
		lbsr	forth_core_execute.asm
		pshs	u,y
		pulu	y,x		; get c-addr u
		ldu	forth__here
		ldd	#.runtime_xt
		std	,u++
		tfr	x,d
		stb	,u+
		beq	.empty
.copy		lda	,y+
		sta	,u+
		decb
		bne	.copy
.empty		stu	forth__here
		puls	u,y
		leau	4,u
		ldx	,y++
		jmp	[,x]

.runtime_xt	fdb	.runtime
.runtime	pshu	y		; push c-addr
		clra			; 
		ldb	,y+		; get length
		leay	d,y		; point past string
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_ext_case		; C ( C: -- case-sys ) R ( -- )
		fdb	forth_core_ext_c_quote
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"CASE"
.xt		fdb	forth_core_colon.runtime
	;============================================
	; : CASE	0 ; IMMEDIATE
	;============================================
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_ext_compile_comma	; ( xt -- )
		fdb	forth_core_ext_case
		fdb	_NOINTERP :: .xt - .name
.name		fcc	"COMPILE,"
.xt		fdb	.body
.body		pulu	d
		ldx	forth__here
		std	,x++
		stx	forth__here
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_ext_defer		; ( "<spaces>name" -- / i*x -- j*x )
		fdb	forth_core_ext_compile_comma
		fdb	.xt - .name
.name		fcc	"DEFER"
.xt		fdb	forth_core_colon.runtime
	;===================================================
	; : DEFER	CREATE ['] ABORT , DOES> @ EXECUTE ;
	;===================================================
		fdb	forth_core_create.xt
		fdb	forth_core_literal.runtime_xt
		fdb	forth_exception_ext_abort.xt
		fdb	forth_core_comma.xt
		fdb	forth_core_does.runtime_xt
		jsr	forth_core_create.does_hook
		fdb	forth_core_fetch.xt
		fdb	forth_core_execute.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_ext_defer_store	; ( xt2 xt1 -- )
		fdb	forth_core_ext_defer
		fdb	.xt - .name
.name		fcc	"DEFER!"
.xt		fdb	forth_core_colon.runtime
	;===================================
	; : DEFER!	>BODY ! ;
	;===================================
		fdb	forth_core_to_body.xt
		fdb	forth_core_store.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_ext_defer_fetch	; ( xt1 -- xt2 )
		fdb	forth_core_ext_defer_store
		fdb	.xt - .name
.name		fcc	"DEFER@"
.xt		fdb	forth_core_colon.runtime
	;=========================================
	; : DEFER@	>BODY @ ;
	;=========================================
		fdb	forth_core_to_body.xt
		fdb	forth_core_fetch.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_ext_end_case		; C ( C: case-sys -- ) R ( x -- )
		fdb	forth_core_ext_defer_fetch
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"ENDCASE"
.xt		fdb	.body
.body		ldx	forth__here
		ldd	#forth_core_drop.xt	; compile DROP
		std	,x++
		stx	forth__here
		tfr	x,d
		pulu	x			; get case-sys
		pshs	u			; save U
.fixup		cmpx	#0
		beq	.done_fixup
		ldu	,x			; get location
		std	,x			; fixup jump
		tfr	u,x			; get new location
		bra	.fixup			; continue
.done_fixup	puls	u			; restore U
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_ext_end_of		; C ( C: case-sys1 of-sys -- case-sys2 ) R ( -- )
		fdb	forth_core_ext_end_case
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"ENDOF"
.xt		fdb	.body
.body		ldd	#forth_core_ext_again.runtime_xt
		ldx	forth__here
		std	,x++
		ldd	2,u		; get case-sys1
		std	,x		; save here
		stx	2,u		; push case-sys2
		leax	2,x		; point to next compilation location
		stx	forth__here
		tfr	x,d
		pulu	x		; get of-sys
		std	,x		; patch jump of OF
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_ext_erase		; ( addr u -- )
		fdb	forth_core_ext_end_of
		fdb	.xt - .name
.name		fcc	"ERASE"
.xt		fdb	forth_core_colon.runtime
	;================================
	; : ERASE	0 FILL ;
	;================================
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_fill.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_ext_false		; ( -- false )
		fdb	forth_core_ext_erase
		fdb	.xt - .name
.name		fcc	"FALSE"
.xt		fdb	forth_core_constant.does
	;==========================================
	; 1 0= CONSTANT FALSE
	;==========================================
		fdb	0

;**********************************************************************

forth_core_ext_hex		; ( -- )
		fdb	forth_core_ext_false
		fdb	.xt - .name
.name		fcc	"HEX"
.xt		fdb	forth_core_colon.runtime
	;==========================================
	; : HEX	16 BASE ! ;
	;==========================================
		fdb	forth_core_literal.runtime_xt
		fdb	16
		fdb	forth_core_base.xt
		fdb	forth_core_store.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_ext_holds		; ( c-addr u -- )
		fdb	forth_core_ext_hex
		fdb	.xt - .name
.name		fcc	"HOLDS"
.xt		fdb	.body
.body		pshs	u,y		; save registers, we're using these
		pulu	y,x		; pull c-addr u
		tfr	x,d
		leay	d,y
		ldu	forth__hold	; get hold location
.copy		lda	,-y		; get character
		sta	,-u		; move to hold area
		leax	-1,x		; more?
		bne	.copy		; if so, copy more
		stu	forth__hold	; save new hold location
		puls	u,y		; restore registers
		leau	4,u		; adjust data stack
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_ext_is		; ( xt "<spaces>name" -- )
		fdb	forth_core_ext_holds
		fdb	_IMMED :: .xt - .name
.name		fcc	"IS"
.xt		fdb	forth_core_colon.runtime
	;=========================================
	; : IS	' STATE @ IF
	;		LITERAL POSTPONE DEFER!
	;	ELSE DEFER! THEN ;
	;=========================================
		fdb	forth_core_tick.xt
		fdb	forth_tools_ext_state.xt
		fdb	forth_core_fetch.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L1
		fdb	forth_core_literal.xt
		fdb	forth_core_literal.runtime_xt
		fdb	forth_core_ext_defer_store.xt
		fdb	forth_core_ext_compile_comma.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L2
.L1		fdb	forth_core_ext_defer_store.xt
.L2		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_ext_marker		; ( "<spacesname" -- )
		fdb	forth_core_ext_is
		fdb	.xt - .name
.name		fcc	"MARKER"
.xt		fdb	.body
.body		pshs	u,y
		ldd	forth__here
		std	,--s
		ldx	forth__current_wid
		ldd	,x
		pshs	x,d
		ldx	#forth_core_create.xt
		lbsr	forth_core_execute.asm
		ldx	forth__create_xt
		ldd	#.runtime
		std	,x++
		puls	d
		std	,x++
		puls	d
		std	,x++
		puls	d
		std	,x++
		ldd	forth__widnum
		std	,x++
		beq	.nowids
		ldu	#forth__widlist
		tfr	d,y
.savewids	ldd	,u++
		std	,x++
		leay	-1,y
		bne	.savewids
.nowids		stx	forth__here
		puls	u,y
		ldx	,y++
		jmp	[,x]

.runtime	pshs	u,y			; save some registers
		leax	2,x			; point to saved data
		ldd	,x++			; *forth__current_wid
		ldu	,x++			; forth__current_wid
		stu	forth__current_wid
		std	,u
		ldu	,x++			; forth__here
		stu	forth__here
		ldy	,x++			; forth__widnum
		beq	.rt_nowids
		sty	forth__widnum
		ldu	#forth__widlist
.restorewids	ldd	,x++
		std	,u++
		leay	-1,y
		bne	.restorewids
.rt_nowids	puls	u,y
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_ext_nip		; ( x1 x2 -- x2)
		fdb	forth_core_ext_marker
		fdb	.xt - .name
.name		fcc	"NIP"
.xt		fdb	forth_core_colon.runtime
	;========================================
	; : NIP	SWAP DROP ;
	;========================================
		fdb	forth_core_swap.xt
		fdb	forth_core_drop.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_ext_of		;  ( C: -- of-sys ) ( x1 x2 -- | x1 )
		fdb	forth_core_ext_nip
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"OF"
.xt		fdb	.body
.body		ldd	#.runtime_xt
		ldx	forth__here
		std	,x++	
		pshu	x		; push of-sys
		leax	2,x		; space space for jump
		stx	forth__here
		ldx	,y++
		jmp	[,x]

.runtime_xt	fdb	.runtime
.runtime	ldd	,u++
		cmpd	,u
		bne	.not_equal
		leau	2,u		; DROP
		leay	2,y		; skip GOTO
		ldx	,y++
		jmp	[,x]
.not_equal	ldy	,y		; GOTO
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_ext_pad		; ( -- c-addr )
		fdb	forth_core_ext_of
		fdb	.xt - .name
.name		fcc	"PAD"
.xt		fdb	forth_core_colon.runtime
	;==================================================
	; : PAD	HERE ; ( per 3.3.3.6 )
	;==================================================
		fdb	forth_core_here.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_ext_parse		; ( char "ccc<char>" -- c-addr u )
		fdb	forth_core_ext_pad
		fdb	.xt - .name
.name		fcc	"PARSE"
.xt		fdb	.body
.body		pshs	y			; save Y for other use
		ldd	forth__source_len	; get # bytes left in buffer
		subd	forth__in
		beq	.no_input
		bmi	.no_input
		tfr	d,y			; use Y as count
		ldx	forth__source		; get buffer
		ldd	forth__in		; get index into buffer
		leax	d,x			; point to current input
		ldd	,u			; get delimiter
		stx	,u			; return c-addr
		stb	,-s			; save delimiter
.input		lda	,x+			; get next input character
		cmpa	,s			; delimiter?
		beq	.done			; if so, done
		leay	-1,y			; else more input?
		bne	.input			; if not, keep going
		leax	1,x
.done		leas	1,s			; burn delimiter
		tfr	x,d			; get current pointer
		subd	forth__source		; get new >IN
		std	forth__in
		leax	-1,x			; adjust pointer to last character
		tfr	x,d			; get current pointer
		subd	,u			; get length
.return		puls	y			; restore Y
		pshu	d			; return u
		ldx	,y++			; NEXT
		jmp	[,x]
.no_input	clra				; no input
		clrb
		bra	.return

	;----------------------------------------------

	.test	"PARSE easy case"
	.opt	test	pokew forth__source     , .buffer1
	.opt	test	pokew forth__source_len , .len1
	.opt	test	pokew forth__in         , 5
		ldu	#.datastack1
		ldx	#forth_core_ext_parse.xt
		jsr	forth_core_execute.asm
	.assert	/u = .results1        , "U"
	.assert @@/,u = 5             , "len"
	.assert @@/2,u = .buffer1 + 5 , "c-addr"
		rts

.results1	fdb	0
.datastack1	fdb	' '

.buffer1	fcc	'     HELLO    '
.len1		equ	* - .buffer1
	.endtst

	;----------------------------------------------

	.test	"PARSE end of input"
	.opt	test	pokew forth__source     , .buffer2
	.opt	test	pokew forth__source_len , .len2
	.opt	test	pokew forth__in         , 0
		ldu	#.datastack2
		ldx	#forth_core_ext_parse.xt
		jsr	forth_core_execute.asm
	.assert	/u = .results2    , "U"
	.assert	@@/,u = 5         , "len"
	.assert	@@/2,u = .buffer2 , "c-addr"
		rts

.results2	fdb	0
.datastack2	fcb	' '

.buffer2	fcc	'HELLO'
.len2		equ	 * - .buffer2
	.endtst

	;----------------------------------------

	.test	"PARSE only delimiter"
	.opt	test	pokew forth__source     , .buffer3
	.opt	test	pokew forth__source_len , .len3
	.opt	test	pokew forth__in         , 0
		ldu	#.datastack3
		ldx	#forth_core_ext_parse.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .results3 , "U"
	.assert	@@/,u  = 0         , "len"
	.assert	@@/2,u = .buffer3  , "c-addr"
		rts

.results3	fdb	0
.datastack3	fdb	' '

.buffer3	fcc	'  '
.len3		equ	* - .buffer3
	.endtst

	;-----------------------------------------

	.test	"PARSE no data"
	.opt	test	pokew forth__source     , .buffer4
	.opt	test	pokew forth__source_len , .len4
	.opt	test	pokew forth__in         , 0
		ldu	#.datastack4
		ldx	#forth_core_ext_parse.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .results4 , "U"
	.assert	@@/,u  = 0         , "len"
	.assert @@/2,u = .buffer4  , "c-addr"
		rts

.results4	fdb	0
.datastack4	fdb	'"'

.buffer4	fcc	'"'
.len4		equ	* - .buffer4
	.endtst

;**********************************************************************

forth_core_ext_parse_name	; ( "<spaces>name<space>" -- c-addr u )
		fdb	forth_core_ext_parse
		fdb	.xt - .name
.name		fcc	"PARSE-NAME"
.xt		fdb	.body
.body		ldx	forth__source
		ldd	forth__in
		cmpd	forth__source_len
		bhs	.no_input
		leax	d,x
.skip_space	lda	,x+
		cmpa	#' '
		beq	.skip_space
		leax	-1,x
		tfr	x,d
		subd	forth__source
		std	forth__in
		ldd	#' '
		pshu	d
		ldx	#forth_core_ext_parse.xt
		lbsr	forth_core_execute.asm
		ldx	,y++
		jmp	[,x]
.no_input	clra
		clrb
		pshu	x,d
		ldx	,y++
		jmp	[,x]

	;---------------------------------------------

	.test	"PARSE-NAME easy"
	.opt	test	pokew forth__source     , .buffer1
	.opt	test	pokew forth__source_len , .len1
	.opt	test	pokew forth__in         , 5
		ldu	#.datastack1
		ldx	#forth_core_ext_parse_name.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .results1    , "U"
	.assert @@/,u  = 5            , "len"
	.assert @@/2,u = .buffer1 + 5 , "c-addr"
		rts

.results1	fdb	0
		fdb	0
.datastack1	fdb	0

.buffer1	fcc	'     HELLO    '
.len1		equ	* - .buffer1
	.endtst

	;------------------------------------------

	.test	"PARSE-NAME no leading delim"
	.opt	test	pokew forth__source     , .buffer2
	.opt	test	pokew forth__source_len , .len2
	.opt	test	pokew forth__in         , 0
		ldu	#.datastack2
		ldx	#forth_core_ext_parse_name.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .results2 , "U"
	.assert @@/,u  = 5         , "len"
	.assert	@@/2,u = .buffer2  , "c-addr"
		rts

.results2	fdb	0
		fdb	0
.datastack2	fdb	0

.buffer2	fcc	'HELLO'
.len2		equ	* - .buffer2
	.endtst

	;-----------------------------------------

	.test	"PARSE-NAME no input"
	.opt	test	pokew forth__source     , .buffer3
	.opt	test	pokew forth__source_len , .len3
	.opt	test	pokew forth__in         , 0
		ldu	#.datastack3
		ldx	#forth_core_ext_parse_name.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .results3 , "U"
	.assert	@@/,u  = 0         , "len"
	.assert	@@/2,u = .buffer3  , "c-addr"
		rts

.results3	fdb	0
		fdb	0
.datastack3	fdb	0
		nop

.buffer3	fcb	0
.len3		equ	0
	.endtst

;**********************************************************************

forth_core_ext_pick		; ( xu..x0 u -- x1..x0 xu )
		fdb	forth_core_ext_parse_name
		fdb	.xt - .name
.name		fcc	"PICK"
.xt		fdb	.body
.body		ldd	,u		; get index
		addd	,u++		; convert to byte index
		ldd	d,u		; get value at said index
		pshu	d		; save value
		ldx	,y++		; NEXT
		jmp	[,x]

;**********************************************************************

forth_core_ext_refill		; ( -- flag )
		fdb	forth_core_ext_pick
		fdb	.xt - .name
.name		fcc	"REFILL"
.xt		fdb	.body
.body		ldd	forth__source_id
		bmi	.string_input
		clra
		clrb
		std	forth__in
		ldd	forth__source
		pshu	d
		ldd	#INPUT_SIZE
		pshu	d
		ldx	#forth_core_accept.xt
		jsr	forth_core_execute.asm
		ldd	,u
		std	forth__source_len
		ldd	#-1
		std	,u
		ldx	,y++
		jmp	[,x]
.string_input	clr	,-u
		clr	,-u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_ext_restore_input	; ( xn..x1 n -- flag )
		fdb	forth_core_ext_refill
		fdb	.xt - .name
.name		fcc	"RESTORE-INPUT"
.xt		fdb	forth_core_colon.runtime
	;============================================
	; : RESTORE-INPUT
	; ( 1 )	DUP 4 = IF
	; ( 2 )		DROP set-source-id >IN ! source-restore
	; ( 4 )	ELSE
	; ( 5 )		0 ?DO DROP LOOP FALSE 
	; ( 6 )	THEN ;
	;============================================
		fdb	forth_core_dupe.xt		; ( 1 )
		fdb	forth_core_literal.runtime_xt
		fdb	4
		fdb	forth_core_equals.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L3
		fdb	forth_core_drop.xt		; ( 2 )
		fdb	forth__private_set_source_i_d
		fdb	forth_core_to_in.xt
		fdb	forth_core_store.xt
		fdb	forth__private_source_restore_xt
		fdb	forth_core_ext_true.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L6
.L3		fdb	forth_core_literal.runtime_xt	; ( 5 ) 
		fdb	0
		fdb	forth_core_ext_question_do.runtime_xt
		fdb	.L5
.L4		fdb	forth_core_drop.xt
		fdb	forth_core_loop.runtime_xt
		fdb	.L4
.L5		fdb	forth_core_ext_false.xt
.L6		fdb	forth_core_exit.xt

	;----------------------------------------------

	.test	"RESTORE-INPUT good"
	.opt	test	pokew	forth__source_id , 0
	.opt	test	pokew	forth__in        , 20
		ldu	#.datastack1
		ldx	#forth_core_ext_restore_input.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result1           , "U"
	.assert	@@/0,u = -1                 , "flag"
	.assert	@@forth__in         = 5     , ">IN"
	.assert	@@forth__source     = $6000 , "SOURCE c-addr"
	.assert	@@forth__source_len = 0     , "SOURCE u"
		rts

		fdb	0
		fdb	0
.datastack1	fdb	4
		fdb	0
		fdb	5
		fdb	0
.result1	fdb	$6000
	.endtst

	;---------------------------------------------

	.test	"RESTORE-INPUT bad number"
		ldu	#.datastack3
		ldx	#forth_core_ext_restore_input.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result3 , "U"
	.assert	@@/0,u = 0        , "flag"
		rts

		fdb	0
		fdb	0
.datastack3	fdb	6
		fdb	1
		fdb	2
		fdb	3
		fdb	4
		fdb	5
.result3	fdb	6
	.endtst

;**********************************************************************

forth_core_ext_roll		; ( xu..x0 u -- xu-1..x0 xu )
		fdb	forth_core_ext_restore_input
		fdb	.xt - .name
.name		fcc	"ROLL"
.xt		fdb	.body
.body		ldd	,u++
		beq	.done
		lslb
		rola
		leax	d,u		; point to proper location
		cmpx	forth__ds_top	; index out of range
		bhi	.throw_overflow
		ldd	,x		; get data stack entry
		pshs	u,d		; save pointer and data
.again		ldd	-2,x		; copy stack data
		std	,x
		leax	-2,x
		cmpx	2,s
		bne	.again
		puls	u,d		; get data (and clean stack of U)
		std	,u		; save buttom data to top
.done		ldx	,y++
		jmp	[,x]
.throw_overflow	ldd	#-3
		lbra	forth_exception_throw.asm

	;------------------------------------

	.test	"0 ROLL (nop)"
	.opt	test	pokew	forth__ds_top , .top1
	.opt	test	prot	n , .top1
		ldu	#.datastack1
		ldx	#forth_core_ext_roll.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result1
	.assert	@@/0,u = -1
	.assert	@@/2,u = -2
	.assert	@@/4,u = -3
	.assert	@@/6,u = -4
	.assert	@@/8,u = -5
		rts

		fdb	0
		fdb	0
.datastack1	fdb	0
.result1	fdb	-1
		fdb	-2
		fdb	-3
		fdb	-4
		fdb	-5
.top1		fdb	0
	.endtst

	;------------------------------------

	.test	"1 ROLL (swap)"
	.opt	test	pokew	forth__ds_top , .top2
	.opt	test	prot	n , .top2
		ldu	#.datastack2
		ldx	#forth_core_ext_roll.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result2
	.assert	@@/0,u = -2
	.assert	@@/2,u = -1
	.assert	@@/4,u = -3
	.assert	@@/6,u = -4
	.assert	@@/8,u = -5
		rts

		fdb	0
		fdb	0
.datastack2	fdb	1
.result2	fdb	-1
		fdb	-2
		fdb	-3
		fdb	-4
		fdb	-5
.top2		fdb	0
	.endtst

	;------------------------------------

	.test	"2 ROLL (rot)"
	.opt	test	pokew	forth__ds_top , .top3
	.opt	test	prot	n , .top3
		ldu	#.datastack3
		ldx	#forth_core_ext_roll.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result3
	.assert	@@/0,u = -3
	.assert	@@/2,u = -1
	.assert	@@/4,u = -2
	.assert	@@/6,u = -4
	.assert	@@/8,u = -5
		rts

		fdb	0
		fdb	0
.datastack3	fdb	2
.result3	fdb	-1
		fdb	-2
		fdb	-3
		fdb	-4
		fdb	-5
.top3		fdb	0
	.endtst

	;------------------------------------

	.test	"4 ROLL"
	.opt	test	pokew	forth__ds_top , .top4
	.opt	test	prot	n , .top4
		ldu	#.datastack4
		ldx	#forth_core_ext_roll.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result4
	.assert	@@/0,u = -5
	.assert	@@/2,u = -1
	.assert	@@/4,u = -2
	.assert	@@/6,u = -3
	.assert	@@/8,u = -4
		rts

		fdb	0
		fdb	0
.datastack4	fdb	4
.result4	fdb	-1
		fdb	-2
		fdb	-3
		fdb	-4
		fdb	-5
.top4		fdb	0
	.endtst

;**********************************************************************

forth_core_ext_s_backslash_quote	; ( "ccc<quote>" -- ) ( -- c-addr u )
		fdb	forth_core_ext_roll
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	'S\"'
.xt		fdb	.body
.body		pshs	u,y			; save registers for other use
		ldd	forth__source_len
		subd	forth__in
		beq	.no_input
		bmi	.no_input
		tfr	d,y
		ldx	forth__source
		ldd	forth__in
		leax	d,x			; calculate current byte in input buffer
		ldu	forth__here
		ldd	#forth_string_sliteral.runtime_xt
		std	,u++
		leau	2,u			; space for length
		pshs	u
.input		lda	,x+			; get character
		cmpa	#'"'			; done?
		beq	.done
		cmpa	#'\'			; escape?
		beq	.escape
		sta	,u+			; save character
		leay	-1,y			; more input?
		bne	.input
.throw_badinput	ldd	#-18			; no quote yet, so error
		lbra	forth_exception_throw.asm
.no_input	clr	-2,u
		clr	-1,u
		bra	.finish
.done		tfr	x,d			; calculate >IN
		subd	forth__source
		std	forth__in
		tfr	u,d			; get end of input
		ldx	forth__here
		leax	2,x			; point past xt
		subd	,s++			; calculate length of string
		std	,x			; save length of string
.finish		stu	forth__here		; update HERE
		puls	u,y			; restore registers
		ldx	,y++
		jmp	[,x]
.escape		leay	-1,y
		beq	.throw_badinput
		lda	,x+
		cmpa	#'a'
		beq	.escape_a
		cmpa	#'b'
		beq	.escape_b
		cmpa	#'e'
		beq	.escape_e
		cmpa	#'f'
		beq	.escape_f
		cmpa	#'l'
		beq	.escape_l
		cmpa	#'m'
		beq	.escape_m
		cmpa	#'n'
		beq	.escape_n
		cmpa	#'q'
		beq	.escape_q
		cmpa	#'r'
		beq	.escape_r
		cmpa	#'t'
		beq	.escape_t
		cmpa	#'v'
		beq	.escape_v
		cmpa	#'z'
		beq	.escape_z
		cmpa	#'"'
		beq	.escape_q
		cmpa	#'\'
		beq	.escape_bs
		cmpa	#'x'
		bne	.throw_badinput
		cmpy	#4
		blo	.throw_badinput
		bsr	.tohex
		lsla
		lsla
		lsla
		lsla
		sta	,u
		bsr	.tohex
		ora	,u
		sta	,u+			;\x##
		leay	-3,y
		cmpy	#1
		lbhs	.input
		lbra	.throw_badinput
.escape_a	lda	#7
		bra	.store
.escape_b	lda	#8
		bra	.store
.escape_e	lda	#27
		bra	.store
.escape_f	lda	#12
		bra	.store
.escape_l	lda	#10
		bra	.store
.escape_m	ldd	#13::10		; this escape is two characters long,
		std	,u++
		leay	-1,y
		lbeq	.throw_badinput
		lbra	.input
.escape_n	lda	#NL
		bra	.store
.escape_q	lda	#34
		bra	.store
.escape_r	lda	#13
		bra	.store
.escape_t	lda	#9
		bra	.store
.escape_v	lda	#11
		bra	.store
.escape_z	clra
		bra	.store
.escape_bs	lda	#92
.store		sta	,u+
		leay	-1,y
		lbeq	.throw_badinput
		lbra	.input
.tohex		lda	,x+
		suba	#'0'
		lbmi	.throw_badinput
		cmpa	#9
		bls	.tohex_done
		suba	#7
		cmpa	#36
		blo	.tohex_done
		suba	#32		; adjust for lower case
.tohex_done	cmpa	#15
		lbgt	.throw_badinput
		rts

	;-----------------------------------------

	.test	'S\\" one\\ttwo\\mthree\\x45 "'
	.opt	test	pokew forth__source     , .buffer
	.opt	test	pokew forth__source_len , .len
	.opt	test	pokew forth__in         , 0
	.opt	test	pokew forth__here       , .foo_body
		ldu	#.datastack
		ldx	#forth_core_ext_s_backslash_quote.xt
		jsr	forth_core_execute.asm
	.assert	/u          = .datastack                 , "U"
	.assert	@@.foo_body = forth_string_sliteral.runtime_xt , "xt"
	.assert	@@.foo_len  = 16                         , "len"
	.assert .foo_addr   = "one\ttwo\r\nthreeE "      , "text"
	.assert	@@.stop     = -1                         , "no-write"
		rts

		fdb	0
		fdb	0
.datastack	fdb	.len

.buffer		fcc	'one\ttwo\mthree\x45 "'
.len		equ	* - .buffer

		fdb	forth_core_colon.runtime
.foo_body	fdb	0
.foo_len	fdb	0
.foo_addr	rmb	16
.stop		fdb	-1
	.endtst

	;---------------------------------------

	.test	'S\\" Christmas String"'
	.opt	test	prot	n , .stop2
	.opt	test	pokew forth__source     , .buffer2
	.opt	test	pokew forth__source_len , .len2
	.opt	test	pokew forth__in         , 0
	.opt	test	pokew forth__here       , .foo2_body
		ldu	#.datastack2
		ldx	#forth_core_ext_s_backslash_quote.xt
		jsr	forth_core_execute.asm
	.assert	/u          = .datastack2            , "U"
	.assert	@@.foo2_body = forth_string_sliteral.runtime_xt , "xt"
	.assert	@@.foo2_len  = 18                    , "len"
	.assert	@@.stop2     = -1                    , "no-write"
		rts

		fdb	0
		fdb	0
.datastack2	fdb	0

.buffer2	fcc	'\a'
		fcc	'\b'
		fcc	'\e'
		fcc	'\f'
		fcc	'\l'
		fcc	'\m'
		fcc	'\n'
		fcc	'\q'
		fcc	'\r'
		fcc	'\t'
		fcc	'\v'
		fcc	'\z'
		fcc	'\"'
		fcc	'\x0F'
		fcc	'\xF1'
		fcc	'\xaB'
		fcc	'\\'
		fcc	'"'
.len2		equ	* - .buffer2
		fdb	.len2

		fdb	forth_core_colon.runtime
.foo2_body	fdb	0
.foo2_len	fdb	0
		rmb	18
.stop2		fdb	-1
	.endtst

;**********************************************************************

forth_core_ext_save_input	; ( -- xn..x1 n )
		fdb	forth_core_ext_s_backslash_quote
		fdb	.xt - .name
.name		fcc	"SAVE-INPUT"
.xt		fdb	forth_core_colon.runtime
	;=============================================
	; : SAVE-INPUT
	;	SOURCE >IN @ SOURCE-ID 4 ;
	;=============================================
		fdb	forth_core_source.xt
		fdb	forth_core_to_in.xt
		fdb	forth_core_fetch.xt
		fdb	forth_core_ext_source_i_d.xt
		fdb	forth_core_literal.runtime_xt
		fdb	4
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_ext_source_i_d	; ( -- 0 | -1 )
		fdb	forth_core_ext_save_input
		fdb	.xt - .name
.name		fcc	"SOURCE-ID"
.xt		fdb	.body
.body		ldd	forth__source_id
		pshu	d
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_ext_to		; I ( i*x "<spaces>name" -- ) C ( "<spaces>name -- )
		fdb	forth_core_ext_source_i_d
		fdb	_IMMED :: .xt - .name
.name		fcc	"TO"
.xt		fdb	.body
.body		ldx	#forth_core_tick.xt	; ' word 
		lbsr	forth_core_execute.asm
		tst	forth__state + 1	; compile mode?
		bne	.compile	; compile mode, handle it
		ldx	,u
		lbsr	forth__util_xt_to_name
		pulu	x		; get xt
		leax	2,x		; >BODY
		bita	#_DOUBLE
		bne	.imm_double
		pulu	d		; get data
		std	,x		; store in value
.done		ldx	,y++
		jmp	[,x]
.imm_double	pulu	d
		std	,x++
		pulu	d
		std	,x
		bra	.done
.compile	ldx	,u		; get xt
		lbsr	forth__util_xt_to_name
		ldx	forth__here	; preload X with HERE
		bita	#_LOCAL		; is this a LOCAL?
		bne	.local		; yes, handle it
		bita	#_DOUBLE	; is this a DOUBLE?
		bne	.double		; yes, handle it
		ldd	#.runtime_xt	; copmile runtime action
		std	,x++
		pulu	d		; get xt
		std	,x++		; save pointer
.storehere_done stx	forth__here
		bra	.done
.local		ldd	#forth__local_store ; compile local fetch
		std	,x++
		pshs	y
		pulu	y		; get xt
		ldd	2,y		; get frame pointer index in body
		std	,x++		; compile
		puls	y
		bra	.storehere_done
.double		ldd	#.runtime_double_xt
		std	,x++
		pulu	d
		std	,x++
		bra	.storehere_done

.runtime_xt	fdb	.runtime
.runtime	ldx	,y++
		leax	2,x		; point to body
		pulu	d
		std	,x
		ldx	,y++
		jmp	[,x]

.runtime_double_xt	fdb	.runtime_double
.runtime_double	ldx	,y++
		leax	2,x
		pulu	d
		std	,x++
		pulu	d
		std	,x
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_ext_true		; ( -- true )
		fdb	forth_core_ext_to
		fdb	.xt - .name
.name		fcc	"TRUE"
.xt		fdb	forth_core_constant.does
	;====================================
	; -1 CONSTANT TRUE
	;====================================
		fdb	-1

;**********************************************************************

forth_core_ext_tuck		; ( x1 x2 -- x2 x1 x2 )
		fdb	forth_core_ext_true
		fdb	.xt - .name
.name		fcc	"TUCK"
.xt		fdb	forth_core_colon.runtime
	;==================================
	; : TUCK	SWAP OVER ;
	;==================================
		fdb	forth_core_swap.xt
		fdb	forth_core_over.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_ext_u_dot_r		; ( u n -- )
		fdb	forth_core_ext_tuck
		fdb	.xt - .name
.name		fcc	"U.R"
.xt		fdb	forth_core_colon.runtime
	;===============================================
	; : U.R	SWAP <# 0 #S #> ROT 2DUP
	;	< IF OVER - SPACES ELSE DROP THEN
	;	TYPE SPACE ;
	;===============================================
		fdb	forth_core_swap.xt
		fdb	forth_core_less_number_sign.xt
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_number_sign_s.xt
		fdb	forth_core_number_sign_greater.xt
		fdb	forth_core_rote.xt
		fdb	forth_core_two_dupe.xt
		fdb	forth_core_less_than.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L1
		fdb	forth_core_over.xt
		fdb	forth_core_minus.xt
		fdb	forth_core_spaces.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L2
.L1		fdb	forth_core_drop.xt
.L2		fdb	forth_core_type.xt
		fdb	forth_core_space.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_ext_u_greater_than	; ( u1 u2 -- flag )
		fdb	forth_core_ext_u_dot_r
		fdb	.xt - .name
.name		fcc	"U>"
.xt		fdb	.body
.body		ldd	2,u
		cmpd	,u++
		bhi	.greaterthan
		clra
		clrb
		bra	.done
.greaterthan	ldd	#-1
.done		std	,u
		ldx	,y++	; NEXT
		jmp	[,x]

;**********************************************************************

forth_core_ext_unused		; ( -- n )
		fdb	forth_core_ext_u_greater_than
		fdb	.xt - .name
.name		fcc	"UNUSED"
.xt		fdb	.body
.body		ldd	forth__here_top	; get HERE top
		subd	forth__here	; subract out current HERE
		pshu	d		; return result
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_ext_value		; ( x "<spaces>name" -- ) E ( -- x )
		fdb	forth_core_ext_unused
		fdb	.xt - .name
.name		fcc	"VALUE"
.xt		fdb	.body
.body		ldx	#forth_core_create.xt	; CREATE
		lbsr	forth_core_execute.asm
		ldx	forth__create_xt
		ldd	#.runtime
		std	,x++			; set xt
		pulu	d
		std	,x++			; set body
		stx	forth__here		; update HERE
		ldx	,y++
		jmp	[,x]

.runtime	ldd	2,x
		pshu	d
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_core_ext_within		; ( test low high -- flag )
		fdb	forth_core_ext_value
		fdb	.xt - .name
.name		fcc	"WITHIN"
.xt		fdb	forth_core_colon.runtime
	;=========================================
	; : WITHIN	OVER - >R - R> U< ;
	;=========================================
		fdb	forth_core_over.xt
		fdb	forth_core_minus.xt
		fdb	forth_core_to_r.xt
		fdb	forth_core_minus.xt
		fdb	forth_core_r_from.xt
		fdb	forth_core_u_less_than.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_core_ext_bracket_compile	; obsolete
		fdb	forth_core_ext_within
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"[COMPILE]"
.xt		fdb	forth_core_colon.runtime
	;======================================
	; : [COMPILE]	-30 THROW ; IMMEDIATE
	;======================================
		fdb	forth_core_literal.runtime_xt
		fdb	-30
		fdb	forth_exception_throw.xt

;**********************************************************************

forth_core_ext_backslash	; ( "ccc<eol>" -- )
		fdb	forth_core_ext_bracket_compile
		fdb	_IMMED :: .xt - .name
.name		ascii	"\\"
.xt		fdb	forth_core_colon.runtime
	;===============================================
	; : \	SOURCE >IN ! DROP ;
	;===============================================
		fdb	forth_core_source.xt
		fdb	forth_core_to_in.xt
		fdb	forth_core_store.xt
		fdb	forth_core_drop.xt
		fdb	forth_core_exit.xt

;**********************************************************************
;		DOUBLE
;**********************************************************************

forth_double_two_constant	; ( x1 x2 "<spaces>name" -- ) E ( -- x1 x2 )
		fdb	forth_core_ext_backslash
		fdb	.xt - .name
.name		fcc	"2CONSTANT"
.xt		fdb	forth_core_colon.runtime
	;==========================================================
	; : 2CONSTANT	CREATE , ,  DOES> 2@ ;
	;==========================================================
		fdb	forth_core_create.xt
		fdb	forth_core_comma.xt
		fdb	forth_core_comma.xt
		fdb	forth_core_does.runtime_xt
.does		jsr	forth_core_create.does_hook
		fdb	forth_core_two_fetch.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_double_two_literal	; C ( x1 x2 -- ) R ( -- x1 x2 )
		fdb	forth_double_two_constant
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"2LITERAL"
.xt		fdb	.body
.body		ldx	forth__here
		ldd	#.runtime_xt
		std	,x++
		pulu	d
		std	,x++
		pulu	d
		std	,x++
		stx	forth__here
		ldx	,y++	; NEXT
		jmp	[,x]

.runtime_xt	fdb	.runtime
.runtime	leau	-4,u
		ldd	,y++
		std	,u
		ldd	,y++
		std	2,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_double_two_variable	; ( "<spaces>name" -- ) E ( -- a-addr )
		fdb	forth_double_two_literal
		fdb	.xt - .name
.name		fcc	"2VARIABLE"
.xt		fdb	forth_core_colon.runtime
	;==========================================
	; : 2VARIABLE	CREATE 0 , 0 , 
	;==========================================
		fdb	forth_core_create.xt
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_comma.xt
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_comma.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_double_d_plus		; ( d1|ud1 d2|ud2 -- d3|ud3 )
		fdb	forth_double_two_variable
		fdb	.xt - .name
.name		fcc	"D+"
.xt		fdb	.body
.body		ldd	2,u
		addd	6,u
		std	6,u
		ldd	,u
		adcb	5,u
		adca	4,u
		std	4,u
		leau	4,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_double_d_minus		; ( d1|ud1 d2|ud2 -- d3|ud3 )
		fdb	forth_double_d_plus
		fdb	.xt - .name
.name		fcc	"D-"
.xt		fdb	.body
.body		ldd	6,u
		subd	2,u
		std	6,u
		ldd	4,u
		sbcb	1,u
		sbca	0,u
		std	4,u
		leau	4,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_double_d_dot		; ( d -- )
		fdb	forth_double_d_minus
		fdb	.xt - .name
.name		fcc	"D."
.xt		fdb	forth_core_colon.runtime
	;==========================================
	; : D.	DUP >R DABS <# [CHAR] . HOLD #S R> SIGN #> TYPE SPACE ;
	;==========================================
		fdb	forth_core_dupe.xt
		fdb	forth_core_to_r.xt
		fdb	forth_double_d_abs.xt
		fdb	forth_core_less_number_sign.xt
		fdb	forth_core_literal.runtime_xt
		fdb	'.'
		fdb	forth_core_hold.xt
		fdb	forth_core_number_sign_s.xt
		fdb	forth_core_r_from.xt
		fdb	forth_core_sign.xt
		fdb	forth_core_number_sign_greater.xt
		fdb	forth_core_type.xt
		fdb	forth_core_space.xt
		fdb	forth_core_exit.xt

	;--------------------------------------------

	.test	"D."
	.opt	test	prot	rw,$6000,$6100
	.opt	test	pokew	forth__here           , $6000
	.opt	test	pokew	forth__vector_putchar , .sysnul
		ldu	#.datastack
		ldx	#forth_double_d_dot.xt
		jsr	forth_core_execute.asm
	.assert	/u = .result , "U"
		rts
.sysnul		rts

		fdb	0
		fdb	0
.datastack	fdb	$1234
		fdb	$5678
.result		fdb	0
	.endtst

;**********************************************************************

forth_double_d_dot_r		; ( d n -- )
		fdb	forth_double_d_dot
		fdb	.xt - .name
.name		fcc	"D.R"
.xt		fdb	forth_core_colon.runtime
	;======================================================
	; : D.R
	;	>R DUP >R DABS <# [CHAR] . HOLD #S R> SIGN #>
	;	R> 2DUP < IF OVER - SPACES ELSE DROP THEN
	;	TYPE SPACE ;
	;======================================================
		fdb	forth_core_to_r.xt
		fdb	forth_core_dupe.xt
		fdb	forth_core_to_r.xt
		fdb	forth_double_d_abs.xt
		fdb	forth_core_less_number_sign.xt
		fdb	forth_core_literal.runtime_xt
		fdb	'.'
		fdb	forth_core_hold.xt
		fdb	forth_core_number_sign_s.xt
		fdb	forth_core_r_from.xt
		fdb	forth_core_sign.xt
		fdb	forth_core_number_sign_greater.xt

		fdb	forth_core_r_from.xt
		fdb	forth_core_two_dupe.xt
		fdb	forth_core_less_than.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L1
		fdb	forth_core_over.xt
		fdb	forth_core_minus.xt
		fdb	forth_core_spaces.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L2
.L1		fdb	forth_core_drop.xt
.L2		fdb	forth_core_type.xt
		fdb	forth_core_space.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_double_d_zero_less	; ( d -- flag )
		fdb	forth_double_d_dot_r
		fdb	.xt - .name
.name		fcc	"D0<"
.xt		fdb	.body
.body		tst	,u
		bmi	.yes
		clra
		clrb
		bra	.done
.yes		ldd	#-1
.done		leau	2,u
		std	,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_double_d_zero_equal	; ( d -- flag )
		fdb	forth_double_d_zero_less
		fdb	.xt - .name
.name		fcc	"D0="
.xt		fdb	.body
.body		ldd	,u++
		bne	.false
		ldd	,u
		bne	.false
		ldd	#-1
		bra	.done
.false		clra
		clrb
.done		std	,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_double_d_two_star		; ( xd1 -- xd2 )
		fdb	forth_double_d_zero_equal
		fdb	.xt - .name
.name		fcc	"D2*"
.xt		fdb	.body
.body		lsl	3,u
		rol	2,u
		rol	1,u
		rol	0,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_double_d_two_slash	; ( xd1 -- xd2 )
		fdb	forth_double_d_two_star
		fdb	.xt - .name
.name		fcc	"D2/"
.xt		fdb	.body
.body		asr	0,u
		ror	1,u
		ror	2,u
		ror	3,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_double_d_less_than	; ( d1 d2 -- flag )
		fdb	forth_double_d_two_slash
		fdb	.xt - .name
.name		fcc	"D<"
.xt		fdb	.body
.body		ldb	4,u		; get MSB of d1
		sex			; sign extend
		pshs	a		; save even more MSB
		ldb	,u		; get MSB of d2
		sex			; sign extend
		pshs	a		; save even more MSB
.compare	ldd	6,u		; do 32-bit subtraction
		subd	2,u
		ldd	4,u
		sbcb	1,u
		sbca	0,u
		lda	1,s
		sbca	,s		; and extend to extension
		leau	6,u		; clean up parameter stack
		leas	2,s
		bmi	.true		; if negative, less than
		clra
		clrb
.done		std	,u
		ldx	,y++
		jmp	[,x]
.true		ldd	#-1
		bra	.done

;**********************************************************************

forth_double_d_equals		; ( xd1 xd2 -- flag )
		fdb	forth_double_d_less_than
		fdb	.xt - .name
.name		fcc	"D="
.xt		fdb	.body
.body		ldd	,u
		cmpd	4,u
		bne	.notequal
		ldd	2,u
		cmpd	6,u
		bne	.notequal
		ldd	#-1
.done		leau	6,u
		std	,u
		ldx	,y++
		jmp	[,x]
.notequal	clra
		clrb
		bra	.done

;**********************************************************************

forth_double_d_to_s		; ( d -- n )
		fdb	forth_double_d_equals
		fdb	.xt - .name
.name		fcc	"D>S"
.xt		fdb	forth_core_colon.runtime
	;=========================================
	; : D>S		DROP ;
	;=========================================
		fdb	forth_core_drop.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_double_d_abs		; ( d -- ud )
		fdb	forth_double_d_to_s
		fdb	.xt - .name
.name		fcc	"DABS"
.xt		fdb	forth_core_colon.runtime
	;===========================================
	; : DABS	2DUP D0< IF DNEGATE THEN ;
	;===========================================
		fdb	forth_core_two_dupe.xt
		fdb	forth_double_d_zero_less.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L1
		fdb	forth_double_d_negate.xt
.L1		fdb	forth_core_exit.xt

	;-----------------------------------------

	.test	"DABS -1"
		ldu	#.datastack1
		ldx	#forth_double_d_abs.xt
		jsr	forth_core_execute.asm
	.assert	/u = .datastack1
	.assert	@@/0,u = 0
	.assert	@@/2,u = 1
		rts

		fdb	0
		fdb	0
.datastack1	fdb	$FFFF
		fdb	$FFFF
	.endtst

;**********************************************************************

forth_double_d_max		; ( d1 d2 -- d3 )
		fdb	forth_double_d_abs
		fdb	.xt - .name
.name		fcc	"DMAX"
.xt		fdb	forth_core_colon.runtime
	;==============================================
	; : DMAX 2OVER 2OVER D< IF
	;		2>R 2DROP 2R>
	;	ELSE
	;		2DROP
	;	THEN ;
	;==============================================
		fdb	forth_core_two_over.xt
		fdb	forth_core_two_over.xt
		fdb	forth_double_d_less_than.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L1
		fdb	forth_core_ext_two_to_r.xt
		fdb	forth_core_two_drop.xt
		fdb	forth_core_ext_two_r_from.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L2
.L1		fdb	forth_core_two_drop.xt
.L2		fdb	forth_core_exit.xt

;**********************************************************************

forth_double_d_min		; ( d1 d2 -- d3 )
		fdb	forth_double_d_max
		fdb	.xt - .name
.name		fcc	"DMIN"
.xt		fdb	forth_core_colon.runtime
	;============================================
	; : DMIN 2OVER 2OVER D< IF
	;		2DROP
	;	ELSE
	;		2>R 2DROP 2R>
	;	THEN ;
	;============================================
		fdb	forth_core_two_over.xt
		fdb	forth_core_two_over.xt
		fdb	forth_double_d_less_than.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L1
		fdb	forth_core_two_drop.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L2
.L1		fdb	forth_core_ext_two_to_r.xt
		fdb	forth_core_two_drop.xt
		fdb	forth_core_ext_two_r_from.xt
.L2		fdb	forth_core_exit.xt

;**********************************************************************

forth_double_d_negate		; ( d1 -- d2 )
		fdb	forth_double_d_min
		fdb	.xt - .name
.name		fcc	"DNEGATE"
.xt		fdb	.body
.body		tfr	u,x
		lbsr	forth__math_neg32
		ldx	,y++
		jmp	[,x]

	;-----------------------------------------

	.test	"DNEGATE negative"
		ldu	#.datastack
		ldx	#forth_double_d_negate.xt
		jsr	forth_core_execute.asm
	.assert	/u = .datastack , "U"
	.assert	@@/0,u = 0      , "MSW"
	.assert @@/2,u = 1      , "LWS"
		rts

		fdb	0
.datastack	fdb	$FFFF
		fdb	$FFFF
	.endtst

	;-----------------------------------------

	.test	"DNEGATE positive"
		ldu	#.datastack1
		ldx	#forth_double_d_negate.xt
		jsr	forth_core_execute.asm
	.assert	/u = .datastack1 , "U"
	.assert	@@/0,u = $FFFF   , "MSW"
	.assert @@/2,u = $FFFF   , "LWS"
		rts

		fdb	0
.datastack1	fdb	0
		fdb	1
	.endtst

;**********************************************************************

Pd		set	7	; U
Pc		set	6
Pb		set	5
Pa		set	4
Pf		set	3
Pe		set	2
Pn2		set	0

Lsign		set	11	; S
Lbits		set	10
Ldf		set	8
Lcf		set	7
Lbf		set	6
Laf		set	5
Lde		set	7
Lce		set	6
Lbe		set	5
Lae		set	4
Lquo3		set	3
Lquo2		set	2
Lquo1		set	1
Lquo0		set	0

forth_double_m_star_slash	; ( d1 n1 +n2 -- d2 )
		fdb	forth_double_d_negate
		fdb	.xt - .name
.name		fcc	"M*/"
.xt		fdb	.body
.body		ldd	Pn2,u
		lbeq	forth_core_s_m_slash_rem.throw_div0
		ldd	#12
.clear		clr	,-s
		decb
		bne	.clear
		lda	Pe,u
		eora	Pa,u
		sta	Lsign,s		; save resulting sign
		ldd	Pe,u
		bpl	.n1_okay	; possibly negate inputs
		coma
		comb
		addd	#1
		std	Pe,u
.n1_okay	tst	Pa,u
		bpl	.d1_okay
		leax	Pa,u
		lbsr	forth__math_neg32

	;---------------------------------------------
	; do 16x32 bit multiply, giving 48 bit result
	;---------------------------------------------

.d1_okay	lda	Pf,u
		ldb	Pd,u
		mul
		std	Ldf,s
		lda	Pf,u
		ldb	Pc,u
		mul
		addd	Lcf,s
		std	Lcf,s
		ldb	Lbf,s		; propagate carry
		adcb	#0
		stb	Lbf,s
		lda	Pe,u
		ldb	Pd,u
		mul
		addd	Lde,s
		std	Lde,s
		ldb	Lbf,s		; everytime you see this
		adcb	#0		; we're propagating the carry
		stb	Lbf,s
		lda	Pf,u
		ldb	Pb,u
		mul
		addd	Lbf,s
		std	Lbf,s
		ldb	Laf,s
		adcb	#0
		stb	Laf,s
		lda	Pe,uk
		ldb	Pc,u
		mul
		addd	Lce,s
		std	Lce,s
		ldb	Lbe,s
		adcb	#0
		stb	Lbe,s
		lda	Pf,u
		ldb	Pa,u
		mul
		addd	Laf,s
		std	Laf,s
		ldb	Lae,s
		adcb	#0
		stb	Lae,s
		lda	Pe,u
		ldb	Pb,u
		mul
		addd	Lbe,s
		std	Lbe,s
		ldb	Lae,s
		adcb	#0
		stb	Lae,s
		lda	Pe,u
		ldb	Pa,u
		mul
		addd	Lae,s
		std	Lae,s

	;-------------------------------------
	; now do 48x16 divide
	;-------------------------------------

		lda	#48
		sta	Lbits,s
		clra
		clrb
.10		lsl	9,s
		rol	8,s
		rol	7,s
		rol	6,s
		rol	5,s
		rol	4,s
		rolb
		rola
		cmpd	Pn2,u
		blo	.20
		subd	Pn2,u
		orcc	{c}
		bra	.30
.20		andcc	{c}
.30		rol	Lquo3,s
		rol	Lquo2,s
		rol	Lquo1,s
		rol	Lquo0,s
		dec	Lbits,s
		bne	.10
		leau	8,u
		puls	x,d		; push result
		pshu	x,d
		tst	7,s		; negate result?
		bpl	.40
		tfr	u,x
		lbsr	forth__math_neg32
.40		leas	8,s
		ldx	,y++
		jmp	[,x]

	;----------------------------------------------

	.test	"M*/ positive positive"
		ldu	#.datastack1
		ldx	#forth_double_m_star_slash.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result1 , "U"
	.assert	@@/0,u = $17CE    , "MSW"
	.assert	@@/2,u = $49B0    , "LSW"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack1	fdb	13
		fdb	17
.result1	fdb	$1234
		fdb	$5678
	.endtst

;**********************************************************************

forth_double_m_plus		; ( d1|ud1 n -- d1|ud2 )
		fdb	forth_double_m_star_slash
		fdb	.xt - .name
.name		fcc	"M+"
.xt		fdb	.body
.body		tst	,u
		bpl	.positive
		ldd	#-1
		bra	.save
.positive	clra
		clrb
.save		pshu	d
		ldd	2,u
		addd	6,u
		std	6,u
		ldd	,u
		adcb	5,u
		adca	4,u
		std	4,u
		leau	4,u
		ldx	,y++
		jmp	[,x]

;**********************************************************************
;		DOUBLE-EXT
;**********************************************************************

forth_double_ext_two_rote	; ( x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )
		fdb	forth_double_m_plus
		fdb	.xt - .name
.name		fcc	"2ROT"
.xt		fdb	forth_core_colon.runtime
	;====================================
	; : 2ROLL	5 ROLL 5 ROLL ;
	;====================================
		fdb	forth_core_literal.runtime_xt
		fdb	5
		fdb	forth_core_ext_roll.xt
		fdb	forth_core_literal.runtime_xt
		fdb	5
		fdb	forth_core_ext_roll.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_double_ext_two_value	; ( x1 x2 "<spaces>naem" -- ) E ( -- x1 x2 )
		fdb	forth_double_ext_two_rote
		fdb	_IMMED :: .xt - .name
.name		fcc	"2VALUE"
.xt		fdb	.body
.body		ldx	#forth_core_create.xt
		lbsr	forth_core_execute.asm
		lda	[forth__create_name]	; mark as DOUBLE
		ora	#_DOUBLE		; for TO
		sta	[forth__create_name]
		ldx	forth__create_xt
		ldd	#.runtime
		std	,x++
		pulu	d
		std	,x++
		pulu	d
		std	,x++
		stx	forth__here
		ldx	,y++
		jmp	[,x]

.runtime	ldd	4,x
		pshu	d
		ldd	2,x
		pshu	d
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_double_ext_d_u_less	; ( ud1 ud2 -- flag )
		fdb	forth_double_ext_two_value
		fdb	.xt - .name
.name		fcc	"DU<"
.xt		fdb	.body
.body		clr	,-s	; Zero extend parameters
		clr	,-s
		lbra	forth_double_d_less_than.compare

;**********************************************************************
;		EXCEPTION
;based on 	https://forth-standard.org/standard/exception/CATCH
;		https://forth-standard.org/standard/exception/THROW
;**********************************************************************

forth_exception_catch		; ( i*x xt -- j*x 0 | i*x n )
		fdb	forth_double_ext_d_u_less
		fdb	.xt - .name
.name		fcc	"CATCH"
.xt		fdb	.body
.body		ldd	forth__source		; save input info
		pshs	d
		ldd	forth__source_len
		pshs	d
		ldd	forth__in
		pshs	d
		ldd	forth__source_id
		pshs	d
		ldd	forth__handler		; get previous handler
		pshs	u,y,d			; save IP, data stack and previous handler
		sts	forth__handler		; save return stack pointer
		pulu	x			; pull xt
		lbsr	forth_core_execute.asm	; EXECUTE
		puls	y,d			; remove exception frame (ignore previous data stack pointer)
		puls	x			; remove data stack pointer (saved version not needed)
		leas	8,s			; remove saved source info
		std	forth__handler		; save previous handler
		clr	,-u			; return 0
		clr	,-u
		ldx	,y++			; NEXT
		jmp	[,x]

;**********************************************************************
;	
forth_exception_throw		; ( k*x n -- k*x | i*x n )
		fdb	forth_exception_catch
		fdb	.xt - .name
.name		fcc	"THROW"
.xt		fdb	.body
.body		ldd	,u
		beq	.nothrow
		lds	forth__handler
		beq	.panic			; if no handler set, panic
		puls	d			; get previous handler
		std	forth__handler		; restore
		pulu	d			; save exception #
		puls	y,u			; pull IP and data stack
		std	,u			; replace xt with exceptin #
		puls	d			; restore input info
		std	forth__source_id
		puls	d
		std	forth__in
		puls	d
		std	forth__source_len
		puls	d
		std	forth__source
		ldx	,y++
		jmp	[,x]
.nothrow	leau	2,u			; remove 0
		ldx	,y++			; NEXT
		jmp	[,x]
.panic		swi

	;************************************************
	; 	forth_exception_throw.asm	Allow assembly code to throw excption
	;Entry:	D - exception #
	;Exit:	none
	;************************************************

.asm		pshu	d
		ldx	#forth_exception_throw.xt
		jmp	forth_core_execute.asm

	;----------------------------------------

	.test	"CATCH ... THROW"
	.opt	test	pokew	forth__vector_putchar , .putchar
		ldu	#.datastack
		ldx	#.baz_xt
		jsr	forth_core_execute.asm
	.assert	/u         = .results , "U"
	.assert	@@/,u      = -5000    , "0,U"
	.assert	.outputbuf = "error"  , "msg"
		rts

		fdb	-8
		fdb	-6
		fdb	-5
		fdb	-4
		fdb	-3
		fdb	-2
.results	fdb	-1
.datastack	fdb	0

.foo_xt		fdb	forth_core_colon.runtime
		fdb	forth_core_literal.runtime_xt
		fdb	4
		fdb	forth_core_literal.runtime_xt
		fdb	5
		fdb	forth_core_literal.runtime_xt
		fdb	-5000
		fdb	forth_exception_throw.xt
		fdb	forth_core_exit.xt

.bar_xt		fdb	forth_core_colon.runtime
		fdb	forth_core_literal.runtime_xt
		fdb	1
		fdb	forth_core_literal.runtime_xt
		fdb	2
		fdb	forth_core_literal.runtime_xt
		fdb	3
		fdb	.foo_xt
		fdb	forth_core_exit.xt

.baz_xt		fdb	forth_core_colon.runtime
		fdb	forth_core_literal.runtime_xt
		fdb	.bar_xt
		fdb	forth_exception_catch.xt
		fdb	forth_core_dupe.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L1
		fdb	forth_string_sliteral.runtime_xt
		fdb	.len1
.text1		fcc	/error/
.len1		equ	* - .text1
		fdb	forth_core_type.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L2
.L1		fdb	forth_string_sliteral.runtime_xt
		fdb	.len2
.text2		fcc	/okay/
.len2		equ	* - .text2
		fdb	forth_core_type.xt
.L2		fdb	forth_core_exit.xt

.putchar	pshs	x
		ldx	.output
		stb	,x+
		stx	.output
		puls	x,pc
.output		fdb	.outputbuf
.outputbuf	rmb	6

	.endtst

;**********************************************************************
;		EXCEPTION-EXT
;**********************************************************************

forth_exception_ext_abort	; ( i*x -- ) ( R: j*x -- )
		fdb	forth_exception_throw
		fdb	.xt - .name
.name		fcc	"ABORT"
.xt		fdb	forth_core_colon.runtime
	;===========================================
	; : ABORT -1 THROW ; 
	;===========================================
		fdb	forth_core_literal.runtime_xt
		fdb	-1
		fdb	forth_exception_throw.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_exception_ext_abort_quote	; C ( "ccc<quote>" -- ) R ( i*x x1 -- | i*x ) ( R: j*x -- | j*x )
		fdb	forth_exception_ext_abort
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	'ABORT"'
.xt		fdb	forth_core_colon.runtime
	;====================================================
	; : ABORT" 
	;	POSTPONE IF 
	;		POSTPONE S" POSTPONE save_abort_msg
	;		-2 POSTPONE LITERAL POSTPONE THROW
	;	POSTPONE THEN ; IMMEDIATE
	;====================================================
		fdb	forth_core_if.xt
		fdb	forth_core_s_quote.xt
		fdb	forth_core_literal.runtime_xt
		fdb	.save_abort_msg_xt
		fdb	forth_core_ext_compile_comma.xt
		fdb	forth_core_literal.runtime_xt
		fdb	-2
		fdb	forth_core_literal.xt
		fdb	forth_core_literal.runtime_xt
		fdb	forth_exception_throw.xt
		fdb	forth_core_ext_compile_comma.xt
		fdb	forth_core_then.xt
		fdb	forth_core_exit.xt

.save_abort_msg_xt
		fdb	.save_abort_msg_body
.save_abort_msg_body
		pulu	x,d
		stx	forth__abortq
		std	forth__abortql
		ldx	,y++
		jmp	[,x]

;**********************************************************************
;		LOCAL
;
; Follow 13.3.3.1
; Can create locals after : :NONAME DOES>
; Create temporary dictionary unil ; ;CODE DOES>
; release resources 
; release resources after ; ;CODE DOES> EXIT ABORT THROW
;
; Local FP on return stack:	( a-addr u locals... )
;	forth__local_fp		pointer to locals on return stack
;	forth__local_fps	size of locals on return stack
;**********************************************************************

forth__local_cleanup	
		clra
		clrb
		std	forth__local_wid
		ldx	forth__here
		ldd	#forth__local_leave
		std	,x++
		stx	forth__here
		rts

forth__local_semicolon
		fdb	0
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	";"
.xt		fdb	.body
.body		bsr	forth__local_cleanup
		lbra	forth_core_semicolon.body

forth__local_does
		fdb	forth__local_semicolon
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	'DOES>'
.xt		fdb	.body
.body		bsr	forth__local_cleanup
		lbra	forth_core_does.body

forth__local_exit
		fdb	forth__local_does
		fdb	_NOINTERP :: .xt - .name
.name		fcc	'EXIT'
.xt		fdb	.body
.body		bsr	forth__local_cleanup
		lbra	forth_core_exit.body

forth__local_abort	
		fdb	forth__local_exit
		fdb	.xt - .name
.name		fcc	'ABORT'
.xt		fdb	.body
.body		bsr	forth__local_cleanup
		ldx	#forth_exception_ext_abort.xt
		lbra	forth_core_execute.asm

forth__local_throw
		fdb	forth__local_abort
		fdb	.xt - .name
.name		fcc	'THROW'
.xt		fdb	.body
.body		bsr	forth__local_cleanup
		lbra	forth_exception_throw.body	

forth_local_paren_local_paren	; ( c-addr u -- ) E ( -- x )
		fdb	forth_exception_ext_abort_quote
		fdb	_NOINTERP :: .xt - .name
.name		fcc	"(LOCAL)"
.xt		fdb	.body
.body		ldd	forth__local_wid	; do we have a local wid?
		bne	.skip_init		; if so, skip this step
		clra
		clrb
		std	forth__local_e_cnt

	;--------------------------------------------------------------------
	; Here we check to see if we have enough dictionary space to do a
	; local wordlist.  Each word takes up 8 bytes + the word length, and
	; we only need to support up to NUMBER_LOCALS of entries, so we can
	; check that here.  The local dictionary is allocted in a transitory
	; section, at the top of the HERE section, minus space for 16 locals
	; with a maximum length name.  That should leave us enough space for
	; any FORTH word definition.  I hope.
	;--------------------------------------------------------------------

		ldx	forth__here_top
		leax	-(NUMBER_LOCALS * (2 + 2 + DEFINITION_MAX + 2 + 2)),x
		cmpx	forth__here
		bls	.throw_bad_dict
		ldd	forth__here
		std	forth__local_here
		stx	forth__here
		ldd	forth__current_wid
		std	forth__local_current
		ldd	#forth__local_throw	; intialize local wid
		std	forth__local_wid
		ldd	#forth__local_wid
		std	forth__current_wid
		ldd	forth__create_link	; save info from last CREATE
		std	forth__local_link	; and save it
		ldd	forth__create_name
		std	forth__local_name
		ldd	forth__create_xt
		std	forth__local_xt
.skip_init	ldd	,u			; check for 0 length
		bne	.continue
		leau	4,u
		bra	.finish			; if so, this marks the end of locals
.continue	ldx	#forth__private_create_quote_xt
		lbsr	forth_core_execute.asm
		lda	[forth__create_name]
		ora	#_LOCAL | _IMMED
		sta	[forth__create_name]
		ldx	forth__create_xt
		ldd	#.runtime	; initialize xt
		std	,x++
		clra
		ldb	forth__local_l_cnt 	; save body (index into fp)
		cmpd	forth__env_number_sign_locals.body
		bhs	.throw_setec		; too many secrets, uh, locals
		lslb
		std	,x++
		dec	forth__local_e_cnt
		inc	forth__local_l_cnt
		stx	forth__here		; save compilation location
		ldx	,y++			; NEXT
		jmp	[,x]
.finish		ldd	forth__local_current
		std	forth__current_wid
		ldx	forth__local_here
		stx	forth__here
		ldd	#forth__local_enter	
		std	,x++
		ldd	forth__local_e_cnt
		lsla
		lslb
		std	,x++
		stx	forth__here
		ldd	forth__local_xt		; restore CREATE data
		std	forth__create_xt
		ldd	forth__local_name
		std	forth__create_name
		ldd	forth__local_link
		std	forth__create_link
		ldx	,y++			; NEXT
		jmp	[,x]
.throw_bad_dict	ldd	#-8
		lbra	forth_exception_throw.asm
.throw_setec	ldd	#-256
		lbra	forth_exception_throw.asm

.runtime	ldd	2,x
		pshs	d
		ldx	forth__here
		ldd	#forth__local_fetch
		std	,x++
		puls	d
		std	,x++
		stx	forth__here
		ldx	,y++
		jmp	[,x]

forth__local_enter
		fdb	.body
.body		ldd	forth__local_fp 	; save sprevious FP
		pshs	d
		ldd	forth__local_fps	; save size of previous FP
		pshs	d
		ldd	,y++			; get # bytes
		std	forth__local_fps
		leas	a,s			; adjust stack downward
		tfr	s,x
		stx	forth__local_fp		; save new local frame pointer
		tstb
		beq	.done
.copy		lda	,u+			; copy data
		sta	,x+
		decb
		bne	.copy
.done		ldx	,y++			; NEXT
		jmp	[,x]

forth__local_fetch
		fdb	.body
.body		ldd	,y++
		ldx	forth__local_fp
		ldd	d,x
		pshu	d
		ldx	,y++
		jmp	[,x]

forth__local_store
		fdb	.body
.body		ldd	,y++
		ldx	forth__local_fp
		leax	d,x
		pulu	d
		std	,x
		ldx	,y++
		jmp	[,x]

forth__local_leave
		fdb	.body
.body		ldd	forth__local_fps	; get size of FP
		leas	b,s			; remove locals
		puls	d			; get old local frame pointer size
		std	forth__local_fps	; restore
		puls	d			; get old local frame pointer
		std	forth__local_fp 	; restore
		ldx	,y++			; NEXT
		jmp	[,x]

	;------------------------------------------

	.test	'(LOCAL)'
	.opt	test	prot	rw,$DB00 - 1024    , $DAFF
	.opt	test	prot	rw,$6000           , $6100
	.opt	test	pokew	forth__here_top    , $DB00
	.opt	test	pokew	forth__local_wid   , 0
	.opt	test	pokew	forth__here        , $6000
	.opt	test	pokew	forth__current_wid , forth__forth_wid
		ldu	#.datastack1
		ldx	#forth_local_paren_local_paren.xt	; 0
		jsr	forth_core_execute.asm
		ldx	#forth_local_paren_local_paren.xt	; 1
		jsr	forth_core_execute.asm
		ldx	#forth_local_paren_local_paren.xt	; 2
		jsr	forth_core_execute.asm
		ldx	#forth_local_paren_local_paren.xt	; 3
		jsr	forth_core_execute.asm
		ldx	#forth_local_paren_local_paren.xt	; 4
		jsr	forth_core_execute.asm
		ldx	#forth_local_paren_local_paren.xt	; 5
		jsr	forth_core_execute.asm
		ldx	#forth_local_paren_local_paren.xt	; 6
		jsr	forth_core_execute.asm
		ldx	#forth_local_paren_local_paren.xt	; 7
		jsr	forth_core_execute.asm
		ldx	#forth_local_paren_local_paren.xt	; 8
		jsr	forth_core_execute.asm
		ldx	#forth_local_paren_local_paren.xt	; 9
		jsr	forth_core_execute.asm
		ldx	#forth_local_paren_local_paren.xt	; 10
		jsr	forth_core_execute.asm
		ldx	#forth_local_paren_local_paren.xt	; 11
		jsr	forth_core_execute.asm
		ldx	#forth_local_paren_local_paren.xt	; 12
		jsr	forth_core_execute.asm
		ldx	#forth_local_paren_local_paren.xt	; 13
		jsr	forth_core_execute.asm
		ldx	#forth_local_paren_local_paren.xt	; 14
		jsr	forth_core_execute.asm
		ldx	#forth_local_paren_local_paren.xt	; 15
		jsr	forth_core_execute.asm
		ldx	#forth_local_paren_local_paren.xt	; no more
		jsr	forth_core_execute.asm
	.assert	/u      = .result1              , "U"
	.assert	@@$D890 = forth__local_throw    , "ab link"
	.assert	@@$D892 = _LOCAL | _IMMED :: 31 , "ab flags"
	.assert $D894   = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaa00" , "ab name"
	.assert	@@$D8B3 = forth_local_paren_local_paren.runtime , "ab xt"
	.assert @@$D8B5 = 0                     , "ab index"
	.assert	@@$DA16                         , "cd link"
	.assert	@@$DA18 = _LOCAL | _IMMED :: 31 , "cd flags"
	.assert $DA1A   = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaa0A" , "cd name"
	.assert	@@$DA39 = forth_local_paren_local_paren.runtime , "cd xt"
	.assert @@$DA3B = 20                      , "cd index"
	.assert	@@forth__local_wid   = $DAD9      , "wid"
	.assert	@@forth__here        = $6004      , "HERE"
	.assert	@@$6000              = forth__local_enter , "enter xt"
	.assert	@@$6002              = -32::32    , "enter frame"
	.assert	@@forth__current_wid = forth__forth_wid , "GET-CURRENT"
		rts

		fdb	0
.datastack1	fdb	31 , .text0
		fdb	31 , .text1
		fdb	31 , .text2
		fdb	31 , .text3
		fdb	31 , .text4
		fdb	31 , .text5
		fdb	31 , .text6
		fdb	31 , .text7
		fdb	31 , .text8
		fdb	31 , .text9
		fdb	31 , .textA
		fdb	31 , .textB
		fdb	31 , .textC
		fdb	31 , .textD
		fdb	31 , .textE
		fdb	31 , .textF
		fdb	 0 , 0
.result1	fdb	-1

.text0	fcc	'aaaaaaaaaaaaaaaaaaaaaaaaaaaaa00'
.text1	fcc	'aaaaaaaaaaaaaaaaaaaaaaaaaaaaa01'
.text2	fcc	'aaaaaaaaaaaaaaaaaaaaaaaaaaaaa02'
.text3	fcc	'aaaaaaaaaaaaaaaaaaaaaaaaaaaaa03'
.text4	fcc	'aaaaaaaaaaaaaaaaaaaaaaaaaaaaa04'
.text5	fcc	'aaaaaaaaaaaaaaaaaaaaaaaaaaaaa05'
.text6	fcc	'aaaaaaaaaaaaaaaaaaaaaaaaaaaaa06'
.text7	fcc	'aaaaaaaaaaaaaaaaaaaaaaaaaaaaa07'
.text8	fcc	'aaaaaaaaaaaaaaaaaaaaaaaaaaaaa08'
.text9	fcc	'aaaaaaaaaaaaaaaaaaaaaaaaaaaaa09'
.textA	fcc	'aaaaaaaaaaaaaaaaaaaaaaaaaaaaa0A'
.textB	fcc	'aaaaaaaaaaaaaaaaaaaaaaaaaaaaa0B'
.textC	fcc	'aaaaaaaaaaaaaaaaaaaaaaaaaaaaa0C'
.textD	fcc	'aaaaaaaaaaaaaaaaaaaaaaaaaaaaa0D'
.textE	fcc	'aaaaaaaaaaaaaaaaaaaaaaaaaaaaa0E'
.textF	fcc	'aaaaaaaaaaaaaaaaaaaaaaaaaaaaa0F'

	.endtst

	;-------------------------------------------

	.test	'(LOCAL) runtime'
		ldu	#.datastack2
		ldx	#.foo_xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result2
	.assert	@@/0,u = 42
	.assert	@@/2,u = 7
		rts

		fdb	0
		fdb	0
.datastack2	fdb	1
.result2	fdb	2
		fdb	3
		nop

.foo_xt		fdb	forth_core_colon.runtime
		fdb	forth_core_literal.runtime_xt
		fdb	40
		fdb	forth_core_literal.runtime_xt
		fdb	2
		fdb	forth__local_enter
		fdb	-10::10
		fdb	forth__local_fetch
		fdb	8
		fdb	forth__local_fetch
		fdb	6
		fdb	forth_core_star.xt
		fdb	forth__local_fetch
		fdb	4
		fdb	forth_core_plus.xt
		fdb	forth__local_fetch
		fdb	0
		fdb	forth__local_fetch
		fdb	2
		fdb	forth_core_plus.xt
		fdb	forth__local_leave
		fdb	forth_core_exit.xt
	.endtst

;**********************************************************************
;		LOCAL-EXT
;**********************************************************************

forth_local_ext_locals_bar	; obsolete
		fdb	forth_local_paren_local_paren
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"LOCALS|"
.xt		fdb	forth_core_colon.runtime
	;=================================================
	; : LOCALS|	-30 THROW ; IMMEDIATE
	;=================================================
		fdb	forth_core_literal.runtime_xt
		fdb	-30
		fdb	forth_exception_throw.xt

;**********************************************************************

forth_local_ext_brace_colon	; C ( i*x "<spaces>ccc :}" -- ) R ( x1 .. xn -- ) E ( -- X )
		fdb	forth_local_ext_locals_bar
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"{:"
.xt		fdb	forth_core_colon.runtime
	;=====================================================
	; : {:	{:-parse 0 0 (LOCAL) ;
	;=====================================================
		fdb	.parse_xt
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_local_paren_local_paren.xt
		fdb	forth_core_exit.xt

	;===============================================
	; : {:-ignore
	;	BEGIN PARSE-NAME S" :}" strcmp IF EXIT THEN AGAIN ;
	;===============================================
.ignore_xt	fdb	forth_core_colon.runtime
.L100		fdb	forth_core_ext_parse_name.xt
		fdb	forth_string_sliteral.runtime_xt
		fdb	2
		fcc	":}"
		fdb	forth__private_strcmp_xt
		fdb	forth_core_if.runtime_xt
		fdb	.L101
		fdb	forth_core_exit.xt
.L101		fdb	forth_core_ext_again.runtime_xt
		fdb	.L100
		fdb	forth_core_exit.xt

	;====================================================
	; : {:-vars
	; ( 1 )	PARSE-NAME
	; ( 2 )		2DUP S" --" strcmp IF 2DROP {:-ignore ELSE
	; ( 3 )		2DUP S" :}" strcmp IF 2DROP           ELSE
	; ( 4 )		0 POSTPONE LITERAL RECURSE (LOCAL)
	; ( 5 )	THEN THEN ;
	;====================================================
.vars_xt	fdb	forth_core_colon.runtime
		fdb	forth_core_ext_parse_name.xt		; 1
		fdb	forth_core_two_dupe.xt			; 2
		fdb	forth_string_sliteral.runtime_xt
		fdb	2
		fcc	"--"
		fdb	forth__private_strcmp_xt
		fdb	forth_core_if.runtime_xt
		fdb	.L203
		fdb	forth_core_two_drop.xt
		fdb	.ignore_xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L205
.L203		fdb	forth_core_two_dupe.xt			; 3
		fdb	forth_string_sliteral.runtime_xt
		fdb	2
		fcc	":}"
		fdb	forth__private_strcmp_xt
		fdb	forth_core_if.runtime_xt
		fdb	.L204
		fdb	forth_core_two_drop.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L205
.L204		fdb	forth_core_literal.runtime_xt		; 4
		fdb	0
		fdb	forth_core_literal.xt
		fdb	.vars_xt
		fdb	forth_local_paren_local_paren.xt
.L205		fdb	forth_core_exit.xt			; 5

	;===========================================================
	; {:-parse
	; ( 1 )	PARSE-NAME
	; ( 2 )		2DUP S" |"  strcmp IF 2DROP {:-vars   ELSE
	; ( 3 )		2DUP S" --" strcmp IF 2DROP {:-ignore ELSE
	; ( 4 )		2DUP S" :}" strcmp IF 2DROP           ELSE
	; ( 5 )		RECURSE (LOCAL)
	; ( 6 )	THEN THEN THEN ;
	;===========================================================
.parse_xt	fdb	forth_core_colon.runtime
		fdb	forth_core_ext_parse_name.xt		; 1
		fdb	forth_core_two_dupe.xt			; 2
		fdb	forth_string_sliteral.runtime_xt
		fdb	1
		fcc	"|"
		fdb	forth__private_strcmp_xt
		fdb	forth_core_if.runtime_xt
		fdb	.L303
		fdb	forth_core_two_drop.xt
		fdb	.vars_xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L306
.L303		fdb	forth_core_two_dupe.xt			; 3
		fdb	forth_string_sliteral.runtime_xt
		fdb	2
		fcc	"--"
		fdb	forth__private_strcmp_xt
		fdb	forth_core_if.runtime_xt
		fdb	.L304
		fdb	forth_core_two_drop.xt
		fdb	.ignore_xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L306
.L304		fdb	forth_core_two_dupe.xt			; 4
		fdb	forth_string_sliteral.runtime_xt
		fdb	2
		fcc	":}"
		fdb	forth__private_strcmp_xt
		fdb	forth_core_if.runtime_xt
		fdb	.L305
		fdb	forth_core_two_drop.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L306
.L305		fdb	.parse_xt				; 5
		fdb	forth_local_paren_local_paren.xt
.L306		fdb	forth_core_exit.xt			; 6

	;-----------------------------------------

	.test	"{:"	; let's see this blow up
	.opt	test	prot	rw,$DB00 - 1024 , $DAFF
	.opt	test	prot	rw,$6000 , $6100
	.opt	test	prot	n , ._nu1
	.opt	test	prot	n , ._nu2
	.opt	test	prot	n , ._nu3
	.opt	test	prot	n , ._nu4
	.opt	test	prot	n , ._nu5
	.opt	test	pokew	forth__here_top   , $DB00
	.opt	test	pokew	forth__in         , 0
	.opt	test	pokew	forth__source     , .text
	.opt	test	pokew	forth__source_len , .len
	.opt	test	pokew	forth__local_wid  , 0
	.opt	test	pokew	forth__here       , $6000
		ldu	#.datastack1
		ldx	#forth_local_ext_brace_colon.xt
		jsr	forth_core_execute.asm
		ldd	forth__in
		ldd	#.alpha
		pshu	d
		ldx	#forth_search_find.xt
		jsr	forth_core_execute.asm
	.assert	@@/,u  = 1     , "alpha found"
	.assert	@@/2,u = $D8CE , "alpha right"
		ldd	#.gamma
		pshu	d
		ldx	#forth_search_find.xt
		jsr	forth_core_execute.asm
	.assert	@@/,u  = 1     , "gamma found"
	.assert	@@/2,u = $D8B5 , "gamma right"
		ldd	#.semicolon
		pshu	d
		ldx	#forth_search_find.xt
		jsr	forth_core_execute.asm
	.assert	@@/,u  = 1                  , "; found"
	.assert	@@/2,u = forth__local_semicolon.xt , "; right"
		ldd	#.colon
		pshu	d
		ldx	#forth_search_find.xt
		jsr	forth_core_execute.asm
	.assert	@@/,u  = -1                  , ": found"
	.assert	@@/2,u = forth_core_colon.xt , ": right"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack1	fdb	0

.text		fcc	'alpha beta gamma | delta epsilon -- here there :}'
.len		equ	* - .text
._nu1		fcb	0
.alpha		ascii	'alpha'c
._nu2		fcb	0
.gamma		ascii	'gamma'c
._nu3		fcb	0
.semicolon	ascii	';'c
._nu4		fcb	0
.colon		ascii	':'c
._nu5		fcb	0
	.endtst

;**********************************************************************
;		TOOLS
;**********************************************************************

forth_tools_dot_s		; ( -- )
		fdb	forth_local_ext_brace_colon
		fdb	.xt - .name
.name		fcc	".S"
.xt		fdb	.body
.body		pshs	y,u
		ldy	forth__ds_top
.show		cmpy	2,s
		beq	.done
		ldd	,--y
		pshu	d
		ldx	#forth_core_dot.xt
		lbsr	forth_core_execute.asm
		bra	.show
.done		puls	y,u
		ldx	,y++
		jmp	[,x]
		
;**********************************************************************

forth_tools_question		; ( a-addr -- )
		fdb	forth_tools_dot_s
		fdb	.xt - .name
.name		fcc	"?"
.xt		fdb	forth_core_colon.runtime
	;=======================================
	; : ?	@ . ;
	;=======================================
		fdb	forth_core_fetch.xt
		fdb	forth_core_dot.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_tools_dump		; ( addr u -- )
		fdb	forth_tools_question
		fdb	.xt - .name
.name		fcc	"DUMP"
.xt		fdb	forth_core_colon.runtime
	;==============================================
	; : DUMP
	;	BASE @ >R HEX
	;	SWAP DUP 0 <# # # # # #> TYPE ." : "
	;	SWAP 0 ?DO
	;		DUP I CHARS + C@ 0 <# # # #> TYPE SPACE
	;	LOOP CR DROP R> BASE ! ;
	;==============================================
		fdb	forth_core_base.xt
		fdb	forth_core_fetch.xt
		fdb	forth_core_to_r.xt
		fdb	forth_core_ext_hex.xt
		fdb	forth_core_swap.xt
		fdb	forth_core_dupe.xt
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_less_number_sign.xt
		fdb	forth_core_number_sign.xt
		fdb	forth_core_number_sign.xt
		fdb	forth_core_number_sign.xt
		fdb	forth_core_number_sign.xt
		fdb	forth_core_number_sign_greater.xt
		fdb	forth_core_type.xt
		fdb	forth_string_sliteral.runtime_xt
		fdb	.len
.text		fcc	": "
.len		equ	* - .text
		fdb	forth_core_type.xt
		fdb	forth_core_swap.xt
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_ext_question_do.runtime_xt
		fdb	.L2
.L1		fdb	forth_core_dupe.xt
		fdb	forth_core_i.xt
		fdb	forth_core_chars.xt
		fdb	forth_core_plus.xt
		fdb	forth_core_c_fetch.xt
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_less_number_sign.xt
		fdb	forth_core_number_sign.xt
		fdb	forth_core_number_sign.xt
		fdb	forth_core_number_sign_greater.xt
		fdb	forth_core_type.xt
		fdb	forth_core_space.xt
		fdb	forth_core_loop.runtime_xt
		fdb	.L1
.L2		fdb	forth_core_c_r.xt
		fdb	forth_core_drop.xt
		fdb	forth_core_r_from.xt
		fdb	forth_core_base.xt
		fdb	forth_core_store.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth__see_ip	equ	forth__create_link	; reuse some variables
forth__see_exit	equ	forth__create_name	; (this is probably a bad idea)

forth_tools_see			; ( "<spaces>name" -- )
		fdb	forth_tools_dump
		fdb	.xt - .name
.name		fcc	"SEE"
.xt		fdb	.body
.body		ldd	#' '
		pshu	d
		ldx	#forth_core_word.xt
		lbsr	forth_core_execute.asm
		ldx	#forth_search_find.xt
		lbsr	forth_core_execute.asm
		ldd	,u++		; found?
		beq	.not_found	; nope

	;------------------------------------------------------------------
	; Don't attempt to decompile the Forth code for the implementation,
	; as there are too many exceptions and hidden xts to properly deal
	; with it.  If it's a Forth word, just claim it's code and be done
	; with it.
	;------------------------------------------------------------------

		pulu	x
		cmpx	#forth_string_ext_unescape.xt
		bhi	.decompile

.is_code	ldx	#.is_code_msg
		ldd	#.is_code_len
		bra	.display_exit

.is_unknown	ldx	#.is_unknown_msg
		ldd	#.is_unknown_len
		bra	.display_exit

.not_found	leau	2,u
		ldx	#.not_found_msg
		ldd	#.not_found_len
.display_exit	pshu	x,d
		lbsr	.type_text
		ldx	,y++
		jmp	[,x]

.not_found_msg	fcc	' not found'
.not_found_len	equ	* - .not_found_msg

.is_code_msg	fcc	' is code'
.is_code_len	equ	* - .is_code_msg

.is_unknown_msg	fcc	' is unknown'
.is_unknown_len	equ	* - .is_unknown_msg

	;**************************************************

.decompile	pshs	u,y
		stx	forth__create_xt	; save xt
		leay	2,x
		sty	forth__see_ip
		lbsr	forth__util_xt_to_name
		stx	forth__create_name	; save name link

		ldx	#.def_xt_tab
		ldd	[forth__create_xt]	; get xt
		cmpd	forth__create_link
		bne	.not_code
		puls	u,y
		bra	.is_code

.not_code	lbsr	.scan_table		; CREATE <name> DOES> ...
		beq	.not_does
		ldx	forth__create_xt
		ldx	,x
		lda	,x
		cmpa	#$BD
		bne	.not_known
		ldd	1,x
		cmpd	#forth_core_create.does_hook
		beq	.is_does
.not_known	puls	u,y
		bra	.is_unknown

.is_does	stx	forth__see_ip
		ldx	#.create_tab
		lbsr	.createf
		ldd	forth__see_ip
		std	forth__see_exit
		ldx	#.does_tab
		lbsr	.doesf
		bsr	.disassemble
		bra	.finished

.not_does	jsr	[4,x]			; jump to code
.finished	puls	u,y
		ldx	,y++
		jmp	[,x]

	;*************************************************
	; Runtime code table (format: xt, name, function
	;*************************************************

.def_xt_tab	fdb	.def_xt_items
.def_xt		fdb	forth_core_colon.runtime	; :
		fdb	forth_core_colon + 2
		fdb	.colonf
.create_tab	fdb	forth_core_create.runtime	; CREATE
		fdb	forth_core_create + 2
		fdb	.createf
		fdb	forth_core_ext_marker.runtime	; MARKER
		fdb	forth_core_ext_marker + 2
		fdb	.createf
		fdb	forth_core_ext_value.runtime	; VALUE
		fdb	forth_core_ext_value + 2
		fdb	.valuef
		fdb	forth_double_ext_two_value.runtime ; 2VALUE
		fdb	forth_double_ext_two_value + 2
		fdb	.twovaluef
		fdb	forth_core_constant.does	; CONSTANT
		fdb	forth_core_constant + 2
		fdb	.valuef
		fdb	forth_double_two_constant.does	; 2CONSTANT
		fdb	forth_double_two_constant + 2
		fdb	.twovaluef
.def_xt_items	equ	(* - .def_xt) / 6

	;*******************************************
	; Runtime and known DOES> functions
	;*******************************************

.colonf		ldy	2,x
		ldd	,y++		; get length of word
		clra			; clear flags
		pshu	y,d		; push c-addr u
		lbsr	.type_text
		ldx	forth__create_name ; display word name
		ldd	,x++
		clra
		pshu	x,d		; push c-addr u
		lbsr	.type_text

		ldd	forth__see_ip	; set up forth__see_exit to 
		std	forth__see_exit	; distinguish between EXIT and ;

.disassemble	ldx	forth__see_ip	; get next xt
		ldd	,x++
		stx	forth__see_ip
		ldx	#.run_xt_tab	; scan this table for special xt's
		lbsr	.scan_table
		beq	.dis_custom	; if so, handle
		cmpd	#forth_core_exit.xt ; EXIT?
		bne	.normal_xt	; if not, handle
		ldx	forth__see_ip
		cmpx	forth__see_exit
		bls	.normal_xt
		ldx	#forth_core_semicolon + 2 ; otherwise it's a ;
		ldd	,x++
		clra
		pshu	x,d
		lbsr	.type_text	; we're done
		ldx	forth__create_xt
		lbsr	forth__util_xt_to_name
		bita	#_IMMED
		beq	.dis_done
		ldx	#forth_core_immediate + 2
		ldd	,x++
		clra
		pshu	x,d
		lbsr	.type_text
.dis_done	rts

.normal_xt	tfr	d,x
		lbsr	forth__util_xt_to_name
		ldd	,x++
		clra
		pshu	x,d
		lbsr	.type_text
		bra	.disassemble

.dis_custom	jsr	[4,x]
		bra	.disassemble

	;******************************************

.createf	ldy	2,x
		ldd	,y++
		clra
		pshu	y,d
		lbsr	.type_text
		ldx	forth__create_name
		ldd	,x++
		clra
		pshu	x,d
		lbra	.type_text

	;*****************************************

.valuef		ldy	forth__create_xt
		ldd	2,y
		pshu	d
		pshs	x
		ldx	#forth_core_dot.xt
		lbsr	forth_core_execute.asm
		puls	x
		bra	.createf

	;****************************************

.twovaluef	pshs	x
		ldy	forth__create_xt
		ldd	4,y
		ldx	2,y
		pshu	x,d
		ldx	#forth_double_d_dot.xt
		lbsr	forth_core_execute.asm
		puls	x
		bra	.createf

	;*********************************************
	; List of non-standard xts
	;*********************************************

.run_xt_tab	fdb	.run_xt_items
.run_xt		fdb	forth_core_plus_loop.runtime_xt		; +LOOP
		fdb	forth_core_plus_loop + 2
		fdb	.locationf
		fdb	forth_core_do.runtime_xt		; DO
		fdb	forth_core_do + 2
		fdb	.printxtf
.does_tab	fdb	forth_core_does.runtime_xt		; DOES>
		fdb	forth_core_does + 2
		fdb	.doesf
		fdb	forth_core_if.runtime_xt		; IF
		fdb	forth_core_if + 2
		fdb	.locationf
		fdb	forth_core_leave.runtime_xt		; LEAVE
		fdb	forth_core_leave + 2
		fdb	.printxtf
		fdb	forth_core_literal.runtime_xt		; LITERAL
		fdb	forth_core_literal + 2
		fdb	.literalf
		fdb	forth_core_loop.runtime_xt		; LOOP
		fdb	forth_core_loop + 2
		fdb	.locationf
		fdb	forth_core_until.runtime_xt		; UNTIL
		fdb	forth_core_until + 2
		fdb	.locationf
		fdb	forth_core_ext_question_do.runtime_xt	; ?DO
		fdb	forth_core_ext_question_do + 2
		fdb	.locationf
		fdb	forth_core_ext_again.runtime_xt		; AGAIN
		fdb	forth_core_ext_again + 2
		fdb	.locationf
		fdb	forth_core_ext_c_quote.runtime_xt	; C"
		fdb	forth_core_ext_c_quote + 2
		fdb	.cquotef
		fdb	forth_core_ext_of.runtime_xt		; OF
		fdb	forth_core_ext_of + 2
		fdb	.locationf
		fdb	forth_core_ext_to.runtime_xt		; TO
		fdb	forth_core_ext_to + 2
		fdb	.printxtf
		fdb	forth_double_two_literal.runtime_xt	; 2LITERAL
		fdb	forth_double_two_literal + 2
		fdb	.twoliteralf
		fdb	forth_string_sliteral.runtime_xt	; SLITERAL (aka S")
		fdb	forth_core_s_quote + 2
		fdb	.sliteralf
		fdb	forth__local_enter			; (LOCAL)/{:
		fdb	.enter
		fdb	.localenterf
		fdb	forth__local_fetch			; x
		fdb	forth_local_paren_local_paren + 2
		fdb	.localfetchf
		fdb	forth__local_store			; TO x
		fdb	forth_core_ext_to + 2
		fdb	.localstoref
		fdb	forth__local_leave
		fdb	.cleanup
		fdb	.printxtf
.run_xt_items	equ	(* - .run_xt) / 6

.enter		fdb	.enter_len
		fcc	'( local-enter )'
.enter_len	equ	* - .enter

.cleanup	fdb	.cleanup_len
		fcc	'( local-cleanup )'
.cleanup_len	equ	* - .cleanup

	;**********************************************
	; Functions to handle nonstandard xt
	;**********************************************

.locationf	lbsr	.printxtf
		ldx	forth__see_ip
		ldd	,x++
		stx	forth__see_ip
		cmpd	forth__see_exit
		blo	.no_ip_exit
		std	forth__see_exit		; store for EXIT detection
.no_ip_exit	subd	forth__see_ip		; turn absolute jump into relative jump
		asra				; and conver to cells
		rorb
.print_num	pshu	d
		ldx	#forth_core_dot.xt	; print #cells to jump
		lbra	forth_core_execute.asm

	;********************************************

.doesf		lbsr	.printxtf
		ldx	forth__see_ip
		leax	3,x
		stx	forth__see_ip
		rts

	;********************************************

.literalf	lbsr	.prtinvxt
		ldx	forth__see_ip
		ldd	,x++
		stx	forth__see_ip
		bra	.print_num

	;**********************************************

.cquotef	bsr	.printxtf
		ldx	forth__see_ip
		clra
		ldb	,x+
		bra	.endtext

	;**********************************************

.twoliteralf	bsr	.prtinvxt
		ldx	forth__see_ip
		ldd	,x
		pshu	d
		ldd	2,x
		leax	4,x
		stx	forth__see_ip
		pshu	d
		ldx	#forth_double_d_dot.xt
		lbra	forth_core_execute.asm

	;**********************************************

.sliteralf	bsr	.printxtf
		ldx	forth__see_ip
		ldd	,x++
.endtext	pshu	x,d
		leax	d,x
		stx	forth__see_ip
		ldx	#forth_core_type.xt
		lbsr	forth_core_execute.asm
		ldb	#'"'
		jsr	[forth__vector_putchar]
		ldb	#' '
		jmp	[forth__vector_putchar]

	;**********************************************

.localenterf	bsr	.printxtf
		ldb	forth__base + 1
		pshs	b
		ldb	#16
		stb	forth__base + 1
		ldx	forth__see_ip
		ldd	,x++
		clra
		lsrb
		stx	forth__see_ip
		bsr	.print_num
		puls	b
		stb	forth__base + 1
		rts

	;**********************************************

.localfetchf	bsr	.prtinvxt
.print_local	ldx	forth__see_ip
		ldd	,x++
		stx	forth__see_ip
		lsrb
		jmp	.local_genname

.localstoref	bsr	.printxtf
		bra	.print_local

	;**********************************************

.local_genname	pshs	b
		ldb	#'L'
		jsr	[forth__vector_putchar]
		puls	b
		addb	#'A'
		jsr	[forth__vector_putchar]
		ldb	#' '
		jmp	[forth__vector_putchar]

	;**********************************************

.printxtf	ldx	2,x
.print_xt	ldd	,x++
		clra
		pshu	x,d
		bra	.type_text

	;**********************************************

.prtinvxt	pshs	x
		ldx	#forth_core_paren + 2
		bsr	.print_xt
		puls	x
		bsr	.printxtf
		ldb	#')'
		jsr	[forth__vector_putchar]
		ldb	#' '
		jmp	[forth__vector_putchar]

	;**********************************************		
	;Entry:	X - table
	;	D - xt
	;Exit:	Y - munged
	;	Zf - 1 if found
	;	   - 0 not found
	;**********************************************

.scan_table	ldy	,x++
.scan_check	cmpd	,x
		beq	.scan_found
		leax	6,x
		leay	-1,y
		bne	.scan_check
		andcc	{z}
.scan_found	rts	

.type_text	ldx	#forth_core_type.xt
		lbsr	forth_core_execute.asm
		ldx	#forth_core_space.xt
		lbra	forth_core_execute.asm

	;--------------------------------------------------

	.test	"SEE XLERB"
	.opt	test	prot	r , .buffer1 , .buffer1 + .len1
	.opt	test	prot	rw,$6000,$6100
	.opt	test	pokew forth__widnum         , 1
	.opt	test	pokew forth__here           , $6000
	.opt	test	pokew forth__source         , .buffer1
	.opt	test	pokew forth__source_len     , .len1
	.opt	test	pokew forth__in             , 0
	.opt	test	pokew forth__vector_putchar , .sysnul
		ldu	#.datastack1
		ldx	#forth_tools_see.xt
		jmp	forth_core_execute.asm

.sysnul		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack1	fcb	0
		
.buffer1	fcc	'XLERB'
.len1		equ	* - .buffer1
		fcb	0
	.endtst

	;--------------------------------------------

	.test	"SEE */MOD"
	.opt	test	prot	r , .buffer2 , .buffer2 + .len2 - 1
	.opt	test	prot	rw,$6000,$6100
	.opt	test	pokew forth__widnum         , 1
	.opt	test	pokew forth__here           , $6000
	.opt	test	pokew forth__source         , .buffer2
	.opt	test	pokew forth__source_len     , .len2
	.opt	test	pokew forth__in             , 0
	.opt	test	pokew forth__vector_putchar , .sysnul2
		ldu	#.datastack2
		ldx	#forth_tools_see.xt
		jmp	forth_core_execute.asm

.sysnul2	rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack2	fcb	0
		
.buffer2	fcc	'*/MOD'
.len2		equ	* - .buffer2
	.endtst

	;--------------------------------------------

	.test	"SEE variable"
	.opt	test	prot	r , .buffer3 , .buffer3 + .len3 - 1
	.opt	test	prot	rw,$6000,$6100
	.opt	test	pokew forth__widnum         , 2
	.opt	test	pokew forth__widlist + 2    , .wid3
	.opt	test	pokew forth__local_wid      , 0
	.opt	test	pokew forth__here           , $6000
	.opt	test	pokew forth__source         , .buffer3
	.opt	test	pokew forth__source_len     , .len3
	.opt	test	pokew forth__in             , 0
	.opt	test	pokew forth__vector_putchar , .sysnul3
		ldu	#.datastack3
		ldx	#forth_tools_see.xt
		jmp	forth_core_execute.asm

.sysnul3	rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack3	fcb	0

.wid3		fdb	.foo3

.foo3		fdb	0
		fdb	3
		fcc	'foo'
		fdb	forth_core_create.runtime
		fdb	10
		
.buffer3	fcc	'foo'
.len3		equ	* - .buffer3
	.endtst

	;----------------------------------------------

	.test	"SEE value"
	.opt	test	prot	r , .buffer4 , .buffer4 + .len4 - 1
	.opt	test	prot	rw,$6000,$6100
	.opt	test	pokew forth__widnum         , 2
	.opt	test	pokew forth__widlist + 2    , .wid4
	.opt	test	pokew forth__local_wid      , 0
	.opt	test	pokew forth__here           , $6000
	.opt	test	pokew forth__source         , .buffer4
	.opt	test	pokew forth__source_len     , .len4
	.opt	test	pokew forth__in             , 0
	.opt	test	pokew forth__vector_putchar , .sysnul4
		ldu	#.datastack4
		ldx	#forth_tools_see.xt
		jmp	forth_core_execute.asm

.sysnul4	rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack4	fcb	0

.wid4		fdb	.foo4

.foo4		fdb	0
		fdb	3
		fcc	'foo'
		fdb	forth_core_ext_value.runtime
		fdb	42
		
.buffer4	fcc	'foo'
.len4		equ	* - .buffer4
	.endtst

	;-------------------------------------------

	.test	"SEE 2value"
	.opt	test	prot	r , .buffer5 , .buffer5 + .len5 - 1
	.opt	test	prot	rw,$6000,$6100
	.opt	test	pokew forth__widnum         , 2
	.opt	test	pokew forth__widlist + 2    , .wid5
	.opt	test	pokew forth__local_wid      , 0
	.opt	test	pokew forth__here           , $6000
	.opt	test	pokew forth__source         , .buffer5
	.opt	test	pokew forth__source_len     , .len5
	.opt	test	pokew forth__in             , 0
	.opt	test	pokew forth__vector_putchar , .sysnul5
		ldu	#.datastack5
		ldx	#forth_tools_see.xt
		jmp	forth_core_execute.asm

.sysnul5	rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack5	fcb	0

.wid5		fdb	.foo5

.foo5		fdb	0
		fdb	3
		fcc	'foo'
		fdb	forth_double_ext_two_value.runtime
		fdb	$1234
		fdb	$5678
		
.buffer5	fcc	'foo'
.len5		equ	* - .buffer5
	.endtst

	;-------------------------------------------

	.test	"SEE code"
	.opt	test	prot	r , .buffer6 , .buffer6 + .len6 - 1
	.opt	test	prot	rw,$6000,$6100
	.opt	test	pokew forth__widnum         , 2
	.opt	test	pokew forth__widlist + 2    , .wid6
	.opt	test	pokew forth__local_wid      , 0
	.opt	test	pokew forth__here           , $6000
	.opt	test	pokew forth__source         , .buffer6
	.opt	test	pokew forth__source_len     , .len6
	.opt	test	pokew forth__in             , 0
	.opt	test	pokew forth__vector_putchar , .sysnul6
		ldu	#.datastack6
		ldx	#forth_tools_see.xt
		jmp	forth_core_execute.asm

.sysnul6	rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack6	fcb	0

.wid6		fdb	.foo6

.foo6		fdb	0
		fdb	3
		fcc	'foo'
		fdb	.foo6_body
.foo6_body	ldx	,y++
		jmp	[,x]
		
.buffer6	fcc	'foo'
.len6		equ	* - .buffer6
	.endtst

	;-------------------------------------------

	.test	"SEE foo"
	.opt	test	prot	r , .buffer7 , .buffer7 + .len7 - 1
	.opt	test	prot	rw,$6000,$6100
	.opt	test	pokew forth__widnum         , 2
	.opt	test	pokew forth__widlist + 2    , .wid7
	.opt	test	pokew forth__local_wid      , 0
	.opt	test	pokew forth__here           , $6000
	.opt	test	pokew forth__source         , .buffer7
	.opt	test	pokew forth__source_len     , .len7
	.opt	test	pokew forth__in             , 0
	.opt	test	pokew forth__vector_putchar , .sysnul7
		ldu	#.datastack7
		ldx	#forth_tools_see.xt
		jmp	forth_core_execute.asm

.sysnul7	rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack7	fcb	0

.wid7		fdb	.foo7

		fdb	0
		fdb	5
		fcc	'snafu'
.foo7_snafu	fdb	forth_core_colon.runtime
		fdb	forth_core_exit.xt
		fdb	0
		fdb	5
		fcc	'alpha'
.foo7_alpha	fdb	forth_core_colon.runtime
		fdb	forth_core_exit.xt
		fdb	0
		fdb	4
		fcc	'beta'
.foo7_beta	fdb	forth_core_colon.runtime
		fdb	forth_core_exit.xt
		fdb	0
		fdb	5
		fcc	'gamma'
.foo7_gamma	fdb	forth_core_colon.runtime
		fdb	forth_core_exit.xt
		fdb	0
		fdb	5
		fcc	'delta'
.foo7_delta	fdb	forth_core_colon.runtime
		fdb	forth_core_exit.xt
		fdb	0
		fdb	3
		fcc	'bar'
.foo7_bar	fdb	forth_core_colon.runtime
		fdb	forth_core_exit.xt
		fdb	0
		fdb	3
		fcc	'baz'
.foo7_baz	fdb	forth_core_colon.runtime
		fdb	forth_core_exit.xt
.foo7		fdb	0
		fdb	3
		fcc	'foo'
		fdb	forth_core_colon.runtime
	;==========================================================
	; : foo {: LD LC LB LA | LE LF :}
	;	S" Hello world!" TYPE C" Top o' the world!" TYPE 
	;	15 snafu CASE
	;		alpha OF EXIT                       ENDOF
	;		beta  OF bar IF EXIT THEN           ENDOF
	;		gamma OF bar IF EXIT ELSE baz  THEN ENDOF
	;		delta OF bar IF baz  ELSE EXIT THEN ENDOF
	;		EXIT
	;	ENDCASE bar set-source-id baz 
	;	LD TO LF 305419896. ;
	;==========================================================
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth__local_enter
		fdb	-12::12
		fdb	forth_string_sliteral.runtime_xt
		fdb	12
		fcc	'Hello world!'
		fdb	forth_core_ext_c_quote.runtime_xt
		ascii	"Top o' the world!"c
		fdb	forth_core_literal.runtime_xt
		fdb	15
		fdb	.foo7_snafu
		fdb	.foo7_alpha
		fdb	forth_core_ext_of.runtime_xt
		fdb	.Cbeta
		fdb	forth_core_exit.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.Endcase
.Cbeta		fdb	.foo7_beta
		fdb	forth_core_ext_of.runtime_xt
		fdb	.Cgamma
		fdb	.foo7_bar
		fdb	forth_core_if.runtime_xt
		fdb	.L11
		fdb	forth_core_exit.xt
.L11		fdb	forth_core_ext_again.runtime_xt
		fdb	.Endcase

.Cgamma		fdb	.foo7_gamma
		fdb	forth_core_ext_of.runtime_xt
		fdb	.Cdelta
		fdb	.foo7_bar
		fdb	forth_core_if.runtime_xt
		fdb	.L21
		fdb	forth_core_exit.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L31
.L21		fdb	.foo7_baz
.L31		fdb	forth_core_ext_again.runtime_xt
		fdb	.Endcase

.Cdelta		fdb	.foo7_delta
		fdb	forth_core_ext_of.runtime_xt
		fdb	.C_default
		fdb	.foo7_bar
		fdb	forth_core_if.runtime_xt
		fdb	.L4
		fdb	.foo7_baz
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L5
.L4		fdb	forth_core_exit.xt
.L5		fdb	forth_core_ext_again.runtime_xt
		fdb	.Endcase
.C_default	fdb	forth_double_two_literal.runtime_xt
		fdb	12345
		fdb	54321
		fdb	forth_core_exit.xt
.Endcase	fdb	forth_core_drop.xt
		fdb	.foo7_bar
		fdb	forth__private_set_source_i_d
		fdb	.foo7_baz
		fdb	forth__local_fetch
		fdb	6
		fdb	forth__local_store
		fdb	10
		fdb	forth_double_two_literal.runtime_xt
		fdb	$5678
		fdb	$1234
		fdb	forth__local_leave
		fdb	forth_core_exit.xt

.buffer7	fcc	'foo'
.len7		equ	* - .buffer7
	.endtst

	;-------------------------------------------

	.test	"SEE DOES>"
	.opt	test	prot	r , .buffer8 , .buffer8 + .len8 - 1
	.opt	test	prot	rw,$6000,$6100
	.opt	test	pokew forth__widnum         , 2
	.opt	test	pokew forth__widlist + 2    , .wid8
	.opt	test	pokew forth__local_wid      , 0
	.opt	test	pokew forth__here           , $6000
	.opt	test	pokew forth__source         , .buffer8
	.opt	test	pokew forth__source_len     , .len8
	.opt	test	pokew forth__in             , 0
	.opt	test	pokew forth__vector_putchar , .sysnul8
		ldu	#.datastack8
		ldx	#forth_tools_see.xt
		jsr	forth_core_execute.asm
		ldb	#10
		jsr	[forth__vector_putchar]
		ldx	#forth_tools_see.xt
		jmp	forth_core_execute.asm

.sysnul8	rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack8	fcb	0

.wid8		fdb	.foo8_man

		fdb	0
		fdb	4
		fcc	'.row'
.foo8_dotrow	fdb	forth_core_colon.runtime
		fdb	forth_core_exit.xt

.foo8		fdb	0
		fdb	3
		fcc	'foo'
		fdb	forth_core_colon.runtime
	;==================================================
	; : shape
	;	CREATE 8 0 DO C, LOOP
	;	DOES> DUP 7 + DO I C@ .row -1 +LOOP CR ;
	;==================================================
		fdb	forth_core_create.xt
		fdb	forth_core_literal.runtime_xt
		fdb	8
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_do.runtime_xt
.L1
		fdb	forth_core_c_comma.xt
		fdb	forth_core_loop.runtime_xt
		fdb	.L1
		fdb	forth_core_does.runtime_xt
.foo8_does	jsr	forth_core_create.does_hook
		fdb	forth_core_dupe.xt
		fdb	forth_core_literal.runtime_xt
		fdb	7
		fdb	forth_core_plus.xt
		fdb	forth_core_do.runtime_xt
.L2
		fdb	forth_core_i.xt
		fdb	forth_core_c_fetch.xt
		fdb	.foo8_dotrow
		fdb	forth_core_literal.runtime_xt
		fdb	-1
		fdb	forth_core_plus_loop.runtime_xt
		fdb	.L2
		fdb	forth_core_c_r.xt
		fdb	forth_core_exit.xt

.foo8_man	fdb	.foo8
		fdb	3
		fcc	'man'
		fdb	.foo8_does
		fdb	$2424
		fdb	$2499
		fdb	$5A3C
		fdb	$1818

.buffer8	fcc	'foo man'
.len8		equ	* - .buffer8
	.endtst

;**********************************************************************

forth_tools_words		; ( -- )
		fdb	forth_tools_see
		fdb	.xt - .name
.name		fcc	"WORDS"
.xt		fdb	forth_core_colon.runtime
	;====================================================
	; : type_word	( nt -- true ) NAME>STRING TYPE CR TRUE ;
	; : WORDS	GET-ORDER ?DUP IF 
	;			SWAP >R 1- 0 ?DO DROP LOOP
	;			['] type_word R> TRAVERSE-WORDLIST
	;		THEN ; 
	;====================================================
		fdb	forth_search_get_order.xt
		fdb	forth_core_question_dupe.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L4
		fdb	forth_core_swap.xt
		fdb	forth_core_to_r.xt
		fdb	forth_core_one_minus.xt
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_ext_question_do.runtime_xt
		fdb	.L2
.L1		fdb	forth_core_drop.xt
		fdb	forth_core_loop.runtime_xt
		fdb	.L1
.L2		fdb	forth_core_literal.runtime_xt
		fdb	.type_word_xt
		fdb	forth_core_r_from.xt
		fdb	forth_tools_ext_traverse_wordlist.xt
		fdb	forth_core_c_r.xt
.L4		fdb	forth_core_exit.xt

.type_word_xt	fdb	forth_core_colon.runtime
	;===================================================
	; : type_word	NAME>STRING TYPE SPACE TRUE ;
	;===================================================
		fdb	forth_tools_ext_name_to_string.xt
		fdb	forth_core_type.xt
		fdb	forth_core_space.xt
		fdb	forth_core_ext_true.xt
		fdb	forth_core_exit.xt

;**********************************************************************
;		TOOLS-EXT
;**********************************************************************
;
;forth_tools_ext_simicolon_code
;		fdb	forth_tools_words
;		fdb	_NOINTERP :: .xt - .name
;.name		fcc	";CODE"
;.xt		fdb	forth_core_colon.runtime
;		fdb	forth_core_literal.runtime_xt
;		fdb	-13
;		fdb	forth_exception_ext_abort.xt
;
;**********************************************************************

forth_tools_ext_ahead		; C ( C: -- orig ) R ( -- )
		fdb	forth_tools_words
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"AHEAD"
.xt		fdb	.body
.body		ldx	forth__here
		ldd	#forth_core_ext_again.runtime_xt
		std	,x++
		pshu	x		; push orig
		leax	2,x		; space for target address
		stx	forth__here
		ldx	,y++		; NEXT
		jmp	[,x]

;**********************************************************************
;
;forth_tools_ext_assembler
;		fdb	forth_tools_ext_ahead
;		fdb	.xt - .name
;.name		fcc	"ASSEMBLER"
;.xt		fdb	forth_core_colon.runtime
;		fdb	forth_core_literal.runtime_xt
;		fdb	-13
;		fdb	forth_exception_ext_abort.xt
;
;**********************************************************************

forth_tools_ext_bye		; ( -- )
		fdb	forth_tools_ext_ahead
		fdb	.xt - .name
.name		fcc	"BYE"
.xt		fdb	.body
.body		jmp	[forth__vector_bye]

;**********************************************************************
;
;forth_tools_ext_code
;		fdb	forth_tools_ext_bye
;		fdb	.xt - .name
;.name		fcc	"CODE"
;.xt		fdb	forth_core_colon.runtime
;		fdb	forth_core_literal.runtime_xt
;		fdb	-13
;		fdb	forth_exception_ext_abort.xt
;
;**********************************************************************

forth_tools_ext_c_s_pick	; E ( C: destu .. orig0 | dest0 -- destu .. orig0 | dest0 destu ) ( S: u -- )
		fdb	forth_tools_ext_bye
		fdb	_NOINTERP :: .xt - .name
.name		fcc	"CS-PICK"
.xt		fdb	forth_core_colon.runtime
	;===================================
	; : CS-PICK	PICK ;
	;===================================
		fdb	forth_core_ext_pick.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_tools_ext_c_s_roll	; ( C: ou|du o-1|d-1 .. o0|d0 -- o-1|d-1 .. og0|dt0 ou|du ) ( S: u -- )
		fdb	forth_tools_ext_c_s_pick
		fdb	_NOINTERP :: .xt - .name
.name		fcc	"CS-ROLL"
.xt		fdb	forth_core_colon.runtime
	;===================================
	; : CS-ROLL	ROLL ;
	;===================================
		fdb	forth_core_ext_roll.xt
		fdb	forth_core_exit.xt

;**********************************************************************
;
;forth_tools_ext_editor
;		fdb	forth_tools_ext_c_s_roll
;		fdb	.xt - .name
;.name		fcc	"EDITOR"
;.xt		fdb	forth_core_colon.runtime
;		fdb	forth_core_literal.runtime_xt
;		fdb	-13
;		fdb	forth_exception_ext_abort.xt
;
;**********************************************************************
;
;forth_tools_ext_forget		; obsolete
;		fdb	forth_tools_ext_c_s_roll
;		fdb	.xt - .name
;.name		fcc	"FORGET"
;.xt		fdb	forth_core_colon.runtime
;	;=========================================
;	; : FORGET	-30 THROW ;
;	;=========================================
;		fdb	forth_core_literal.runtime_xt
;		fdb	-30
;		fdb	forth_exception_ext_abort.xt
;
;**********************************************************************

forth_tools_ext_n_to_r		; E ( i*n +n -- ) ( R: j*x + n )
		fdb	forth_tools_ext_c_s_roll
		fdb	_NOINTERP :: .xt - .name
.name		fcc	"N>R"
.xt		fdb	.body
.body		ldx	,u++
		beq	.done
		stx	forth__nr_storage
.repeat		pulu	d
		pshs	d
		leax	-1,x
		bne	.repeat
		ldx	forth__nr_storage
.done		pshs	x
		ldx	,y++
		jmp	[,x]

;**********************************************************************
;	NAME>COMPILE
;
; Per A.15.6.2.1909.10 of the Forth-2012 standard, the x is the xt of the
; word found, and xt is either EXECUTE for immediate words, or COMPILE, for
; non-immediate words.
;
;**********************************************************************

forth_tools_ext_name_to_compile	; ( nt -- x xt )
		fdb	forth_tools_ext_n_to_r
		fdb	.xt - .name
.name		fcc	"NAME>COMPILE"
.xt		fdb	.body
.body		pulu	x	; get nt
		leax	2,x	; move past .next field
		ldd	,x++	; get length (and flags)
		abx		; move past text
		pshu	x	; save xt of word found
		tsta		; now push EXECUTE or COMPILE,
		bmi	.immed
		ldd	#forth_core_ext_compile_comma.xt
		bra	.exit
.immed		ldd	#forth_core_execute.xt
.exit		pshu	d
		ldx	,y++	; NEXT
		jmp	[,x]

;**********************************************************************

forth_tools_ext_name_to_interpret	; ( xt -- xt|0 )
		fdb	forth_tools_ext_name_to_compile
		fdb	.xt - .name
.name		fcc	"NAME>INTERPRET"
.xt		fdb	.body
.body		pulu	x	; get nt
		leax	2,x	; move past next field
		ldd	,x++	; get length (and flags)
		anda	#_NOINTERP
		bne	.no_interp
		abx		; move past text
		pshu	x	; save xt
.done		ldx	,y++
		jmp	[,x]
.no_interp	clra
		clrb
		pshu	d
		bra	.done

;**********************************************************************

forth_tools_ext_name_to_string	; ( xt -- c-addr u )
		fdb	forth_tools_ext_name_to_interpret
		fdb	.xt - .name
.name		fcc	"NAME>STRING"
.xt		fdb	.body
.body		pulu	x	; get nt
		leax	2,x	; move past next field
		ldd	,x++	; get length as 16-bit value
		clra		; mask of bits
		pshu	x,d	; push c-addr u onto stack
		ldx	,y++	; NEXT
		jmp	[,x]

;**********************************************************************

forth_tools_ext_n_r_from	; E ( -- i*x +n ) ( R: j*x +n -- )
		fdb	forth_tools_ext_name_to_string
		fdb	_NOINTERP :: .xt - .name
.name		fcc	"NR>"
.xt		fdb	.body
.body		ldx	,s++
		beq	.done
		stx	forth__nr_storage
.repeat		puls	d
		pshu	d
		leax	-1,x
		bne	.repeat
		ldx	forth__nr_storage
.done		pshu	x
		ldx	,y++
		jmp	[,x]
	
;**********************************************************************

forth_tools_ext_state		; ( -- a-addr )
		fdb	forth_tools_ext_n_r_from
		fdb	.xt - .name
.name		fcc	"STATE"
.xt		fdb	.body
.body		ldd	#forth__state
		pshu	d
		ldx	,y++
		jmp	[,x]

;**********************************************************************
;
;forth_tools_ext_synonym
;		fdb	forth_tools_ext_state
;		fdb	.xt - .name
;.name		fcc	"SYNONYM"
;.xt		fdb	forth_core_colon.runtime
;		fdb	forth_core_literal.runtime_xt
;		fdb	-13
;		fdb	forth_exception_ext_abort.xt
;
;**********************************************************************

Pwid		set	2
Pxt		set	0

forth_tools_ext_traverse_wordlist	; ( i*x xt wid -- j*x )
		fdb	forth_tools_ext_state
		fdb	.xt - .name
.name		fcc	"TRAVERSE-WORDLIST"
.xt		fdb	.body
.body		ldx	[,u++]		; get address out of wid
		pshs	x		; save
		pulu	d		; get xt
		pshs	d		; save
.again		ldx	Pwid,s		; get next address
		beq	.done		; if NULL, done
		ldd	,x		; get next word
		std	Pwid,s		; save
		lda	2,x		; get flags
		anda	#_HIDDEN	; HIDDEN?
		bne	.again		; skip if so
		pshu	x		; save as nt
		ldx	Pxt,s		; get xt to run
		lbsr	forth_core_execute.asm ; execute XT
		ldd	,u++		; get flag
		bne	.again		; repeat if true
.done		leas	4,s		; remove data from return stack
		ldx	,y++		; NEXT
		jmp	[,x]

	;-------------------------------------

	.test	"TRAVERSE-WORDLIST"
	.opt	test	pokew forth__vector_putchar , .sysnul
		ldu	#.datastack
		ldx	#forth_tools_ext_traverse_wordlist.xt
		jmp	forth_core_execute.asm

.sysnul		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack	fdb	forth__forth_wid ; forth__env_wid
		fdb	.noname_xt

	;===================================================
	; :NONAME ( nt -- flag) NAME>STRING TYPE CR FALSE ;
	;===================================================

.noname_xt	fdb	forth_core_colon.runtime
		fdb	forth_tools_ext_name_to_string.xt
		fdb	forth_core_type.xt
		fdb	forth_core_c_r.xt
		fdb	forth_core_ext_true.xt
		fdb	forth_core_exit.xt
	.endtst

;**********************************************************************

forth_tools_ext_bracket_defined	; ( "<spaces>name..." -- flag )
		fdb	forth_tools_ext_traverse_wordlist
		fdb	_IMMED :: .xt - .name
.name		fcc	"[DEFINED]"
.xt		fdb	forth_core_colon.runtime
	;==============================================
	; : [DEFINED] BL WORD FIND NIP 0<> ; IMMEDIATE
	;==============================================
		fdb	forth_core_b_l.xt
		fdb	forth_core_word.xt
		fdb	forth_search_find.xt
		fdb	forth_core_ext_nip.xt
		fdb	forth_core_ext_zero_not_equals.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_tools_ext_bracket_else	; ( "<spaces>name..." -- )
		fdb	forth_tools_ext_bracket_defined
		fdb	_IMMED :: .xt - .name
.name		fcc	"[ELSE]"
.xt		fdb	forth_core_colon.runtime
	;=================================================
	; : [ELSE]
	; ( 1 )	   1 BEGIN
	; ( 2 )	     BEGIN BL WORD COUNT DUP WHILE
	; ( 3 )	       2DUP S" [IF]" COMPARE 0= IF
	; ( 4 )		  2DROP 1+
	; ( 5 )	       ELSE
	; ( 6 )	         2DUP S" [ELSE]" COMPARE 0= IF
	; ( 7 )	           2DROP 1- DUP IF 1+ THEN
	; ( 8 )	         ELSE
	; ( 9 )	           S" [THEN]" COMPARE 0= IF
	; ( 10 )             1-
	; ( 11 )           THEN
	; ( 12 )         THEN
	; ( 13 )       THEN ?DUP 0= IF EXIT THEN
	; ( 14 )     REPEAT 2DROP
	; ( 15 )   REFILL 0= UNTIL
	; ( 16 )   DROP ; IMMEDIATE
	;=================================================
		fdb	forth_core_literal.runtime_xt
		fdb	1
.L3		fdb	forth_core_b_l.xt
		fdb	forth_core_word.xt
		fdb	forth_core_count.xt
		fdb	forth_core_dupe.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L58
		fdb	forth_core_two_dupe.xt
		fdb	forth_string_sliteral.runtime_xt
		fdb	4
		fcc	'[IF]'
		fdb	forth_string_compare.xt
		fdb	forth_core_zero_equals.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L23
		fdb	forth_core_two_drop.xt
		fdb	forth_core_one_plus.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L50
.L23		fdb	forth_core_two_dupe.xt
		fdb	forth_string_sliteral.runtime_xt
		fdb	6
		fcc	'[ELSE]'
		fdb	forth_string_compare.xt
		fdb	forth_core_zero_equals.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L39
		fdb	forth_core_two_drop.xt
		fdb	forth_core_one_minus.xt
		fdb	forth_core_dupe.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L37
		fdb	forth_core_one_plus.xt
.L37		fdb	forth_core_ext_again.runtime_xt
		fdb	.L50
.L39		fdb	forth_string_sliteral.runtime_xt
		fdb	6
		fcc	'[THEN]'
		fdb	forth_string_compare.xt
		fdb	forth_core_zero_equals.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L50
		fdb	forth_core_one_minus.xt
.L50		fdb	forth_core_question_dupe.xt
		fdb	forth_core_zero_equals.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L56
		fdb	forth_core_exit.xt
.L56		fdb	forth_core_ext_again.runtime_xt
		fdb	.L3
.L58		fdb	forth_core_two_drop.xt
		fdb	forth_core_ext_refill.xt
		fdb	forth_core_zero_equals.xt
		fdb	forth_core_until.runtime_xt
		fdb	.L3
		fdb	forth_core_drop.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_tools_ext_bracket_if	; ( flag -- )
		fdb	forth_tools_ext_bracket_else
		fdb	_IMMED :: .xt - .name
.name		fcc	"[IF]"
.xt		fdb	forth_core_colon.runtime
	;=======================================================
	; : [IF]	0= IF POSTPONE [ELSE] THEN ; IMMEDIATE
	;=======================================================
		fdb	forth_core_zero_equals.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L1
		fdb	forth_tools_ext_bracket_else.xt
.L1		fdb	forth_core_exit.xt

;**********************************************************************

forth_tools_ext_bracket_then	; ( -- )
		fdb	forth_tools_ext_bracket_if
		fdb	_IMMED :: .xt - .name
.name		fcc	"[THEN]"
.xt		fdb	forth_core_colon.runtime
	;================================
	; : [THEN]	; IMMEDIATE
	;================================
		fdb	forth_core_exit.xt

;**********************************************************************

forth_tools_ext_bracket_undefined	; ( "<spaces>name..." -- flag )
		fdb	forth_tools_ext_bracket_then
		fdb	_IMMED :: .xt - .name
.name		fcc	"[UNDEFINED]"
.xt		fdb	forth_core_colon.runtime
	;===============================================
	; : [UNDEFINED]	BL WORD FIND NIP 0= ; IMMEDIATE
	;===============================================
		fdb	forth_core_b_l.xt
		fdb	forth_core_word.xt
		fdb	forth_search_find.xt
		fdb	forth_core_ext_nip.xt
		fdb	forth_core_zero_equals.xt
		fdb	forth_core_exit.xt

;**********************************************************************
;		SEARCH
;**********************************************************************

forth_search_definitions	; ( -- )
		fdb	forth_tools_ext_bracket_undefined
		fdb	.xt - .name
.name		fcc	"DEFINITIONS"
.xt		fdb	forth_core_colon.runtime
	;============================================================
	; : DEFINITIONS	GET-ORDER OVER SET-CURRENT 0 DO DROP LOOP ;
	;============================================================
		fdb	forth_search_get_order.xt
		fdb	forth_core_over.xt
		fdb	forth_search_set_current.xt
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_do.runtime_xt
.L1		fdb	forth_core_drop.xt
		fdb	forth_core_loop.runtime_xt
		fdb	.L1
		fdb	forth_core_exit.xt

;**********************************************************************

Lcaddr		set	5
Lu		set	3
Lwid		set	1
Lcnt		set	0

forth_search_find		; ( c-addr -- c-addr 0 | xt 1 | xt -1 )
		fdb	forth_search_definitions
		fdb	.xt - .name
.name		fcc	"FIND"
.xt		fdb	.body

	;-----------------------------------------------------------------
	; Per 13.3.3.1, we need to find locals, but we don't have to
	; advertise the actual wid for them.  FIND is the only word that
	; scans a search order, so we "hide" searching the local wid
	; here---if it exists, it is scanned first.
	;-----------------------------------------------------------------

.body		pulu	x		; get caddr
		clra
		ldb	,x+		; extract length
		pshs	x,d		; save c-caddr u
		leas	-3,s		; adjust stack in case we match
		pshu	x,d		; also push onto stack
		ldd	forth__local_wid ; get local wordlist
		beq	.skip		; no locals, skip searching this wid
		ldd	#forth__local_wid
		pshu	d		; push wid
		ldx	#forth_search_search_wordlist.xt
		lbsr	forth_core_execute.asm
		ldd	,u		; found?
		bne	.done		; if so, we're done
		leau	-2,u		; if not, adjust parameter stack
.skip		leas	3,s		; adjust return stack, no match
		leau	4,u		; fix paramter stack
		ldx	#forth__widlist	; get list of wids
		ldb	forth__widnum+1	; get # wids
		beq	.none
		pshs	x,b		; save widlist and count
.again		ldd	Lcaddr,s	; push c-addr
		pshu	d
		ldd	Lu,s		; push u
		pshu	d
		ldx	Lwid,s		; get wid array
		ldd	,x++		; get wid
		stx	Lwid,s		; save pointer to next
		pshu	d		; push wid
		ldx	#forth_search_search_wordlist.xt
		lbsr	forth_core_execute.asm
		ldd	,u		; did we fine it?
		bne	.done		; yup, return data
		leau	2,u
		dec	Lcnt,s		; more?
		bne	.again		; loop for more
.none		ldx	Lcaddr,s	; get c-addr
		leax	-1,x		; readjust to caddr
		clra			; 0
		clrb
		pshu	x,d		; push caddr 0
.done		leas	7,s		; burn local vars
		ldx	,y++		; NEXT
		jmp	[,x]

	;--------------------------------------------

	.test	"FIND */MOD (word in one searchlist)"
	.opt	test	pokew	forth__local_wid , 0
	.opt	test	pokew	forth__widnum    , 1
	.opt	test	pokew	forth__widlist   , forth__forth_wid
	.opt	test	prot n , ._n11
	.opt	test	prot n , ._n12
		ldu	#.datastack1
		ldx	#forth_search_find.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .datastack1 - 2              , "U"
	.assert	@@/,u  = -1                           , "flag"
	.assert	@@/2,u = forth_core_star_slash_mod.xt , "xt"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack1	fdb	.word1
._n11		fcb	0

.word1		ascii	'*/MOD'c
._n12		fcb	0
	.endtst

	;--------------------------------------------
	; _Starting Forth_ 1st Edition, pg 16
	; _Starting Forth_ 2nd Edition, pg 15
	;--------------------------------------------

	.test	"FIND XLERB (in one searchlist)"
	.opt	test	pokew forth__local_wid , 0
	.opt	test	pokew forth__widnum    , 1
	.opt	test	pokew forth__widlist   , forth__forth_wid
	.opt	test	prot n , ._n21
	.opt	test	prot n , ._n22
		ldu	#.datastack2
		ldx	#forth_search_find.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .datastack2 -2 , "U"
	.assert	@@/,u  = 0              , "flag"
	.assert	@@/2,u = .word2         , "c-addr"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack2	fdb	.word2
._n21		fcb	0

.word2		ascii	'XLERB'c
._n22		fcb	0
	.endtst

	;----------------------------------------------

	.test	"FIND 2DROP override"
	.opt	test	pokew forth__local_wid   , 0
	.opt	test	pokew forth__widlist     , .wid3
	.opt	test	pokew forth__widlist + 2 , forth__forth_wid
	.opt	test	pokew forth__widlist + 2 , .wid3
	.opt	test	pokew forth__widnum      , 2
	.opt	test	prot  n , ._n31
	.opt	test	prot  n , ._n32

		ldu	#.datastack3
		ldx	#forth_search_find.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .datastack3 - 2 , "U"
	.assert	@@/,u  = -1              , "flag"
	.assert	@@/2,u = .two_drop3_xt   , "xt"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack3	fdb	.word3
._n31		fcb	0
.word3		ascii	'2DROP'c
._n32		fcb	0

.wid3		fdb	.foobar3

.two_fetch3	fdb	0
		fdb	.two_fetch3_xt - .two_fetch3_name
.two_fetch3_name fcc	'2@'
.two_fetch3_xt	fdb	forth_core_colon.runtime
		fdb	forth_core_exit.xt

.two_drop3	fdb	.two_fetch3
		fdb	.two_drop3_xt - .two_drop3_name
.two_drop3_name	fcc	'2DROP'
.two_drop3_xt	fdb	forth_core_colon.runtime
		fdb	forth_core_exit.xt

.foobar3	fdb	.two_drop3
		fdb	.foobar3_xt - .foobar3_name
.foobar3_name	fcc	'FOOBAR'
.foobar3_xt	fdb	forth_core_colon.runtime
		fdb	forth_core_exit.xt
	.endtst

	;----------------------------------------------

	.test	"FIND 2/ (two wordlists)"
	.opt	test	pokew forth__local_wid   , 0
	.opt	test	pokew forth__widlist     , .wid3
	.opt	test	pokew forth__widlist + 2 , forth__forth_wid
	.opt	test	pokew forth__widnum      , 2
	.opt	test	prot  n , ._n41
	.opt	test	prot  n , ._n42

		ldu	#.datastack4
		ldx	#forth_search_find.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .datastack4 - 2         , "U"
	.assert	@@/,u  = -1                      , "flag"
	.assert	@@/2,u = forth_core_two_slash.xt , "xt"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack4	fdb	.word4
._n41		fcb	0

.word4		ascii	'2/'c
._n42		fcb	0
	.endtst

	;--------------------------------------------

	.test	"FIND XLERB (in two wordlists)"
	.opt	test	pokew forth__local_wid   , 0
	.opt	test	pokew forth__widlist     , .wid3
	.opt	test	pokew forth__widlist + 2 , forth__forth_wid
	.opt	test	pokew forth__widnum      , 2
	.opt	test	prot n , ._n51
	.opt	test	prot n , ._n52
		ldu	#.datastack5
		ldx	#forth_search_find.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .datastack5 -2 , "U"
	.assert	@@/0,u = 0              , "flag"
	.assert	@@/2,u = .word5         , "c-addr"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack5	fdb	.word5
._n51		fcb	0

.word5		ascii	'XLERB'c
._n52		fcb	0
	.endtst

	;--------------------------------------------

	.test	"FIND : (with local wid)"
	.opt	test	pokew forth__local_wid , forth__local_throw
	.opt	test	prot r , .word6 , .word6 + 1
		ldu	#.datastack6
		ldx	#forth_search_find.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .results6           , "U"
	.assert	@@/,u  = -1                  , "flag"
	.assert	@@/2,u = forth_core_colon.xt , "xt"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.results6	fdb	0
.datastack6	fdb	.word6

.word6		ascii	':'c		
	.endtst

	;-------------------------------------------

	.test	"FIND ; ( with local wid) "
	.opt	test	pokew forth__local_wid , forth__local_throw
		ldu	#.datastack7
		ldx	#forth_search_find.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .results7                 , "U"
	.assert	@@/,u  = 1                         , "flag"
	.assert	@@/2,u = forth__local_semicolon.xt , "xt"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.results7	fdb	0
.datastack7	fdb	.word7

.word7		ascii	';'c
	.endtst

;**********************************************************************

forth_search_forth_wordlist	; ( -- wid )
		fdb	forth_search_find
		fdb	.xt - .name
.name		fcc	"FORTH-WORDLIST"
.xt		fdb	forth_core_constant.does
	;==========================================
	; forth__forth_wid CONSTANT FORTH-WORDLIST
	;==========================================
		fdb	forth__forth_wid

;**********************************************************************

forth_search_get_current	; ( -- wid )
		fdb	forth_search_forth_wordlist
		fdb	.xt - .name
.name		fcc	"GET-CURRENT"
.xt		fdb	.body
.body		ldd	forth__current_wid
		pshu	d
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_search_get_order		; ( -- widn..wid1 n )
		fdb	forth_search_get_current
		fdb	.xt - .name
.name		fcc	"GET-ORDER"
.xt		fdb	.body
.body		ldx	#forth__widlist	; point to order array
		ldb	forth__widnum+1	; get count
		beq	.none
		pshs	b		; save count for loop
		lslb
		abx
.loop		ldd	,--x		; get wordlist
		pshu	d		; push to datastack
		dec	,s		; loop for more
		bne	.loop
		leas	1,s		; clean stack
.none		ldd	forth__widnum	; push # wordlists
		pshu	d
		ldx	,y++		; NEXT
		jmp	[,x]

;**********************************************************************

forth_search_search_wordlist	; ( c-addr u wid -- 0 | xt 1 | xt -1 )
		fdb	forth_search_get_order
		fdb	.xt - .name
.name		fcc	"SEARCH-WORDLIST"
.xt		fdb	forth_core_colon.runtime
	;======================================================
	; : SEARCH-WORDLIST
	; ( 1 )	>R FALSE ['] callback R> TRAVERSE-WORDLIST
	; ( 2 )	NIP NIP DUP 0<> IF
	; ( 3 )		NAME>COMPILE CASE
	; ( 4 )			['] EXECUTE  OF  1 ENDOF
	; ( 5 )			['] COMPILE, OF -1 ENDOF
	; ( 6 )		DUP ENDCASE
	; ( 7 )	THEN ;
	;======================================================
		fdb	forth_core_to_r.xt	; ( 1 )
		fdb	forth_core_ext_false.xt
		fdb	forth_core_literal.runtime_xt
		fdb	forth__private_find_nt_cb_xt
		fdb	forth_core_r_from.xt
		fdb	forth_tools_ext_traverse_wordlist.xt
		fdb	forth_core_ext_nip.xt	; ( 2 )
		fdb	forth_core_ext_nip.xt
		fdb	forth_core_dupe.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L1
		fdb	forth_tools_ext_name_to_compile.xt ; ( 3 )
		fdb	forth_core_literal.runtime_xt	; ( 4 )
		fdb	forth_core_execute.xt
		fdb	forth_core_ext_of.runtime_xt
		fdb	.L2
		fdb	forth_core_literal.runtime_xt
		fdb	1
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L3
.L2		fdb	forth_core_literal.runtime_xt	; ( 5 )
		fdb	forth_core_ext_compile_comma.xt
		fdb	forth_core_ext_of.runtime_xt
		fdb	.L3
		fdb	forth_core_literal.runtime_xt
		fdb	-1
.L3		fdb	forth_core_dupe.xt		; ( 6 )
		fdb	forth_core_drop.xt
.L1		fdb	forth_core_exit.xt

	; ------------------------------------------------------

	.test	"SEARCH-WORDLIST UNESCAPE (first word in list)"
	.opt	test	prot	n , .nu1
		ldu	#.datastack1
		ldx	#forth_search_search_wordlist.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .results1 , "U address"
	.assert	@@/,u  = -1        , "flag"
	.assert	@@/2,u = forth_string_ext_unescape.xt , "xt"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack1	fdb	forth__forth_wid
.results1	fdb	.wordlen1
		fdb	.word1
.nu1		fdb	0

.word1		fcc	'UNESCAPE'
.wordlen1	equ	* - .word1
	.endtst

	;--------------------------------------------------

	.test	"SEARCH-WORDLIST ORDER (second word in list)"
		ldu	#.datastack2
		ldx	#forth_search_search_wordlist.xt
		jsr	forth_core_execute.asm
 	.assert	/u     = .results2 , "U address"
	.assert	@@/,u  = -1        , "flag"
	.assert	@@/2,u = forth_search_ext_order.xt, "xt"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack2	fdb	forth__forth_wid
.results2	fdb	.wordlen2
		fdb	.word2
		fdb	0

.word2		fcc	'ORDER'
.wordlen2	equ	* - .word2
	.endtst

	;----------------------------------

	.test	"SEARCH-WORDLIST */MOD"
		ldu	#.datastack3
		ldx	#forth_search_search_wordlist.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .results3 , "U address"
	.assert	@@/,u  = -1 , "flag"
	.assert	@@/2,u = forth_core_star_slash_mod.xt , "xt"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack3	fdb	forth__forth_wid
.results3	fdb	.wordlen3
		fdb	.word3
		fdb	0

.word3		fcc	'*/MOD'
.wordlen3	equ	* - .word3
	.endtst

	;--------------------------------

	.test	"SEARCH-WORDLIST XLERB"
		ldu	#.datastack5
		ldx	#forth_search_search_wordlist.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .results5 , "U address"
	.assert	@@/,u  = 0         , "flag"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack5	fdb	forth__forth_wid
		fdb	.wordlen5
.results5	fdb	.word5
		fdb	0

.word5		fcc	'XLERB'
.wordlen5	equ	* - .word5
	.endtst

;**********************************************************************

forth_search_set_current	; ( wid -- )
		fdb	forth_search_search_wordlist
		fdb	.xt - .name
.name		fcc	"SET-CURRENT"
.xt		fdb	.body
.body		pulu	d
		std	forth__current_wid
		ldx	,y++
		jmp	[,x]

;**********************************************************************

forth_search_set_order		; ( widn..wid1 n -- )
		fdb	forth_search_set_current
		fdb	.xt - .name
.name		fcc	"SET-ORDER"
.xt		fdb	.body
.body		pulu	d			; get n
		cmpd	#-1			; default?
		beq	.default
		cmpd	forth__env_wordlists.body
		bgt	.throw_too_many
		std	forth__widnum		; save # wordlists
		beq	.done
		pshs	b
		ldx	#forth__widlist		; and point to end
.loop		pulu	d			; get wordlist
		std	,x++			; save in list
		dec	,s			; keep looping
		bne	.loop
		leas	1,s
.done		ldx	,y++			; NEXT
		jmp	[,x]
.default	ldd	forth__forth_wid	; set default FORTH wordlist
		std	forth__widlist
		ldd	#1
		std	forth__widnum
		ldx	,y++			; NEXT
		jmp	[,x]
.throw_too_many	ldd	#-49
		lbra	forth_exception_throw.asm

;**********************************************************************

forth_search_wordlist		; ( -- wid )
		fdb	forth_search_set_order
		fdb	.xt - .name
.name		fcc	"WORDLIST"
.xt		fdb	.body
.body		ldx	forth__here	; allocate wid
		pshu	x		; return it
		clr	,x+		; set wid to empty
		clr	,x+
		stx	forth__here
		ldx	,y++	; NEXT
		jmp	[,x]

;**********************************************************************
;		SEARCH-EXT
;**********************************************************************

forth_search_ext_also		; ( -- )
		fdb	forth_search_wordlist
		fdb	.xt - .name
.name		fcc	"ALSO"
.xt		fdb	forth_core_colon.runtime
	;===================================================
	; : ALSO	GET-ORDER OVER SWAP 1+ SET-ORDER ;
	;===================================================
		fdb	forth_search_get_order.xt
		fdb	forth_core_over.xt
		fdb	forth_core_swap.xt
		fdb	forth_core_one_plus.xt
		fdb	forth_search_set_order.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_search_ext_forth		; ( -- )
		fdb	forth_search_ext_also
		fdb	.xt - .name
.name		fcc	"FORTH"
.xt		fdb	forth_core_colon.runtime
	;====================================================
	; : FORTH	GET-ORDER NIP FORTH-WORDLIST SWAP SET-ORDER ;
	;====================================================
		fdb	forth_search_get_order.xt
		fdb	forth_core_ext_nip.xt
		fdb	forth_search_forth_wordlist.xt
		fdb	forth_core_swap.xt
		fdb	forth_search_set_order.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_search_ext_only		; ( -- )
		fdb	forth_search_ext_forth
		fdb	.xt  - .name
.name		fcc	"ONLY"
.xt		fdb	forth_core_colon.runtime
	;=======================================================
	; : ONLY	-1 SET-ORDER ;
	;=======================================================
		fdb	forth_core_literal.runtime_xt
		fdb	-1
		fdb	forth_search_set_order.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_search_ext_order		; ( -- )
		fdb	forth_search_ext_only
		fdb	.xt - .name
.name		fcc	"ORDER"
.xt		fdb	forth_core_colon.runtime
	;================================================
	; : ORDER
	;	." Wordlist order" CR
	;	GET-ORDER 0 DO U. CR LOOP
	;	." Current wordlist" CR
	;	GET-CURRENT U. CR ;
	;================================================
		fdb	forth_string_sliteral.runtime_xt
		fdb	.len1
.text1		fcc	'Wordlist order'
.len1		equ	* - .text1
		fdb	forth_core_type.xt
		fdb	forth_core_c_r.xt
		fdb	forth_search_get_order.xt
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_do.runtime_xt
.L1		fdb	forth_core_u_dot.xt
		fdb	forth_core_c_r.xt
		fdb	forth_core_loop.runtime_xt
		fdb	.L1
		fdb	forth_string_sliteral.runtime_xt
		fdb	.len2
.text2		fcc	'Current wordlist'
.len2		equ	* - .text2
		fdb	forth_core_type.xt
		fdb	forth_core_c_r.xt
		fdb	forth_search_get_current.xt
		fdb	forth_core_u_dot.xt
		fdb	forth_core_c_r.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_search_ext_previous	; ( -- )
		fdb	forth_search_ext_order
		fdb	.xt - .name
.name		fcc	"PREVIOUS"
.xt		fdb	forth_core_colon.runtime
	;==================================================
	; : PREVIOUS	GET-ORDER NIP 1- SET-ORDER ;
	;==================================================
		fdb	forth_search_get_order.xt
		fdb	forth_core_ext_nip.xt
		fdb	forth_core_one_minus.xt
		fdb	forth_search_set_order.xt
		fdb	forth_core_exit.xt

;**********************************************************************
;		STRING
;**********************************************************************

forth_string_dash_trailing	; ( c-addr u1 -- c-addr u2 )
		fdb	forth_search_ext_previous
		fdb	.xt - .name
.name		fcc	"-TRAILING"
.xt		fdb	forth_core_colon.runtime
	;=======================================================
	; : -TRAILING
	;	DUP 0= IF EXIT THEN
	;	1- CHARS DUP >R OVER +
	;	0 R> DO
	;		DUP C@ BL = IF
	;			1 CHARS -
	;		ELSE
	;			DROP I 1+ UNLOOP EXIT
	;		THEN
	;	-1 +LOOP DROP 0;
	;=======================================================
		fdb	forth_core_dupe.xt
		fdb	forth_core_zero_equals.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L1
		fdb	forth_core_exit.xt
.L1		fdb	forth_core_one_minus.xt
		fdb	forth_core_chars.xt
		fdb	forth_core_dupe.xt
		fdb	forth_core_to_r.xt
		fdb	forth_core_over.xt
		fdb	forth_core_plus.xt
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_r_from.xt
		fdb	forth_core_do.runtime_xt
.L2		fdb	forth_core_dupe.xt
		fdb	forth_core_c_fetch.xt
		fdb	forth_core_b_l.xt
		fdb	forth_core_equals.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L3
		fdb	forth_core_literal.runtime_xt
		fdb	1
		fdb	forth_core_chars.xt
		fdb	forth_core_minus.xt
		fdb	forth_core_ext_again.runtime_xt
		fdb	.L4
.L3		fdb	forth_core_drop.xt
		fdb	forth_core_i.xt
		fdb	forth_core_one_plus.xt
		fdb	forth_core_unloop.xt
		fdb	forth_core_exit.xt
.L4		fdb	forth_core_literal.runtime_xt
		fdb	-1
		fdb	forth_core_plus_loop.runtime_xt
		fdb	.L2
		fdb	forth_core_drop.xt
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_exit.xt

	;--------------------------------------------

	.test	'S" " -TRAILING'
	.opt	test	prot n , .n1
		ldu	#.datastack1
		ldx	#forth_string_dash_trailing.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .datastack1 , "U"
	.assert	@@/,u  = 0           , ",U"
	.assert	@@/2,u = .text1      , "2,U"
		rts

		fdb	0
		fdb	0
.datastack1	fdb	.text1_len
		fdb	.text1
.text1		equ	*
.text1_len	equ	* - .text1
.n1		fcb	0
	.endtst

	;--------------------------------------------

	.test	'S" ONE   " -TRAILING'
	.opt	test	prot n , .n21
	.opt	test	prot n , .n22
		ldu	#.datastack2
		ldx	#forth_string_dash_trailing.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .datastack2 , "U"
	.assert	@@/,u  = 3           , ",U"
	.assert	@@/2,u = .text2      , "2,U"
		rts

		fdb	0
		fdb	0
		fdb	0
.datastack2	fdb	.text2_len
		fdb	.text2

.n21		fcb	0
.text2		fcc	'ONE   '
.text2_len	equ	* - .text2
.n22		fcb	0
	.endtst

	;------------------------------------------

	.test	'S" ONE" -TRAILING'
	.opt	test	prot n , .n31
	.opt	test	prot n , .n32
		ldu	#.datastack3
		ldx	#forth_string_dash_trailing.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .datastack3 , "U"
	.assert	@@/,u  = 3           , ",U"
	.assert	@@/2,u = .text3      , "2,U"
		rts

		fdb	0
		fdb	0
.datastack3	fdb	.text3_len
		fdb	.text3

.n31		fcb	0
.text3		fcc	'ONE'
.text3_len	equ	* - .text3
.n32		fcb	0
	.endtst

	;---------------------------------------

	.test	'S"    " -TRAILING'
	.opt	test	prot n , .n41
	.opt	test	prot n , .n42
		ldu	#.datastack4
		ldx	#forth_string_dash_trailing.xt
		jsr	forth_core_execute.asm
	.assert	/u	= .datastack4 , "U"
	.assert @@/,u   = 0           , ",U"
	.assert	@@/2,u  = .text4      , "2,U"
		rts

		fdb	0
		fdb	0
.datastack4	fdb	.text4_len
		fdb	.text4

.n41		fcb	0
.text4		fcc	'   '
.text4_len	equ	* - .text4
.n42		fcb	0
	.endtst

;**********************************************************************

forth_string_slash_string	; ( c-addr1 u1 n -- c-addr2 u2 )
		fdb	forth_string_dash_trailing
		fdb	.xt - .name
.name		fcc	"/STRING"
.xt		fdb	forth_core_colon.runtime
	;==========================================
	; : /STRING	DUP >R - SWAP R> + SWAP
	;==========================================
		fdb	forth_core_dupe.xt
		fdb	forth_core_to_r.xt
		fdb	forth_core_minus.xt
		fdb	forth_core_swap.xt
		fdb	forth_core_r_from.xt
		fdb	forth_core_plus.xt
		fdb	forth_core_swap.xt
		fdb	forth_core_exit.xt

	;----------------------------------------------

	.test	'S" ABC" 2 /STRING'
		ldu	#.datastack1
		ldx	#forth_string_slash_string.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .results1      , "U"
	.assert	@@/,u  = .text1_len - 2 , ",U"
	.assert	@@/2,u = .text1 + 2     , "2,U"
	.assert	@@/2,u = "C"            , "='C'"
		rts

		fdb	0
.datastack1	fdb	2
.results1	fdb	.text1_len
		fdb	.text1

.text1		fcc	'ABC'
.text1_len	equ	* - .text1
	
	.endtst

	.test	'S" ABC" 2 /STRING -1 /STRING'
		ldu	#.datastack2
		ldx	#forth_string_slash_string.xt
		jsr	forth_core_execute.asm

		ldd	#-1
		pshu	d
		ldx	#forth_string_slash_string.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .results2  , "U"
	.assert	@@/,u  = 2          , ",U"
	.assert	@@/2,u = .text2 + 1 , "2,U"
	.assert	@@/2,u = "BC"       , "='BC'"
		rts

		fdb	0
.datastack2	fdb	2
.results2	fdb	.text2_len
		fdb	.text2

.text2		fcc	'ABC'
.text2_len	equ	* - .text2
	.endtst

;**********************************************************************

forth_string_blank		; ( c-addr u -- )
		fdb	forth_string_slash_string
		fdb	.xt - .name
.name		fcc	"BLANK"
.xt		fdb	forth_core_colon.runtime
	;==========================================
	; : BLANK	BL FILL ;
	;==========================================
		fdb	forth_core_b_l.xt
		fdb	forth_core_fill.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth_string_c_move		; ( c-addr1 c-addr2 u -- )
		fdb	forth_string_blank
		fdb	.xt - .name
.name		fcc	"CMOVE"
.xt		fdb	.body
.body		pshs	y,u
		pulu	y,x,d
		tfr	d,u
.copy		cmpu	#0
		beq	.done
		lda	,y+
		sta	,x+
		leau	-1,u
		bra	.copy
.done		puls	y,u
		leau	6,u
		ldx	,y++
		jmp	[,x]

	;-------------------------------------------------
	
	.test	"CMOVE"
	.opt	test	prot n , .results , .results + 1
	.opt	test	prot n , ._n1
	.opt	test	prot n , ._n2
		ldu	#.datastack
		ldx	#forth_string_c_move.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .results , "U"
	.assert	.addr2 = "FOOBAR" , "FOOBAR"
		rts

		fdb	0
.datastack	fdb	.size
		fdb	.addr2
		fdb	.addr1
.results	fdb	0

.addr1		fcc	'FOOBAR'
.size		equ	* - .addr1
._n1		fcb	0
.addr2		rmb	.size
._n2		fcb	0
	.endtst

;**********************************************************************

forth_string_c_move_up		; ( c-addr1 c-addr2 u )
		fdb	forth_string_c_move
		fdb	.xt - .name
.name		fcc	"CMOVE>"
.xt		fdb	.body
.body		pshs	u,y
		pulu	y,x,d
		leax	d,x
		leay	d,y
		tfr	d,u
.copy		cmpu	#0
		beq	.done
		lda	,-y
		sta	,-x
		leau	-1,u
		bra	.copy
.done		puls	u,y
		leau	6,u
		ldx	,y++
		jmp	[,x]

	;-------------------------------------------------
	
	.test	"CMOVE>"
	.opt	test	prot n , .results , .results + 1
	.opt	test	prot n , ._n1
	.opt	test	prot n , ._n2
		ldu	#.datastack
		ldx	#forth_string_c_move_up.xt
		jsr	forth_core_execute.asm
	.assert	/u     = .results , "U"
	.assert	.addr2 = "FOOBAR" , "FOOBAR"
		rts

		fdb	0
.datastack	fdb	.size
		fdb	.addr2
		fdb	.addr1
.results	fdb	0

.addr1		fcc	'FOOBAR'
.size		equ	* - .addr1
._n1		fcb	0
.addr2		rmb	.size
._n2		fcb	0
	.endtst

;**********************************************************************

Lcaddr2		set	12
Lu2		set	10
Lcaddr1		set	8
Lu1		set	6
Lu		set	4
Ly		set	2
Lpreresult	set	0

forth_string_compare		; ( c-addr1 u1 c-addr2 u2 -- n )
		fdb	forth_string_c_move_up
		fdb	.xt - .name
.name		fcc	"COMPARE"
.xt		fdb	.body
.body		pulu	x,d
		pshs	x,d
		pulu	x,d
		pshs	x,d
		pshs	u,y,d
		ldy	Lu1,s
		cmpy	Lu2,s
		beq	.equal
		bhi	.greater
		ldd	#-1
		std	Lpreresult,s
		ldd	Lu2,s
		bra	.continue
.equal		clra
		clrb
		std	Lpreresult,s
		bra	.continue
.greater	ldd	#1
		std	Lpreresult,s
		ldy	Lu2,s
.continue	cmpy	#0
		beq	.return
		ldx	Lcaddr1,s
		ldu	Lcaddr2,s
.compare	lda	,x+
		cmpa	,u+
		blo	.cmplt
		bhi	.cmpgt
		leay	-1,y
		bne	.compare
.return		puls	u,y,d
		pshu	d
		leas	8,s
		ldx	,y++
		jmp	[,x]
.cmplt		ldd	#-1
		std	Lpreresult,s
		bra	.return
.cmpgt		ldd	#1
		std	Lpreresult,s
		bra	.return

	;-------------------------------------------

	.test	'S" ONE" S" ONE" COMPARE'
	.opt	test	prot	n , ._nu11
	.opt	test	prot	n , ._nu12
	.opt	test	prot	n , ._nu13
		ldu	#.datastack1
		ldx	#forth_string_compare.xt
		jsr	forth_core_execute.asm
	.assert	/u    = .result1 , "U"
	.assert @@/,u = 0        , ",U"
		rts

		fdb	0
		fdb	0
.datastack1	fdb	.text11_len
		fdb	.text11
		fdb	.text12_len
.result1	fdb	.text12

._nu11		fcb	0
.text11		fcc	'ONE'
.text11_len	equ	* - .text11
._nu12		fcb	0
.text12		fcc	'ONE'
.text12_len	equ	* - .text12
._nu13		fcb	0
	.endtst

	;-------------------------------------------

	.test	'S" ONE" S" TWO" COMPARE'
	.opt	test	prot	n , ._nu21
	.opt	test	prot	n , ._nu22
	.opt	test	prot	n , ._nu23
		ldu	#.datastack2
		ldx	#forth_string_compare.xt
		jsr	forth_core_execute.asm
	.assert	/u    = .result2 , "U"
	.assert @@/,u = -1       , ",U"
		rts

		fdb	0
		fdb	0
.datastack2	fdb	.text22_len
		fdb	.text22
		fdb	.text21_len
.result2	fdb	.text21

._nu21		fcb	0
.text21		fcc	'ONE'
.text21_len	equ	* - .text21
._nu22		fcb	0
.text22		fcc	'TWO'
.text22_len	equ	* - .text22
._nu23		fcb	0
	.endtst

	;-------------------------------------------

	.test	'S" TWO" S" ONE" COMPARE'
	.opt	test	prot	n , ._nu31
	.opt	test	prot	n , ._nu32
	.opt	test	prot	n , ._nu33
		ldu	#.datastack3
		ldx	#forth_string_compare.xt
		jsr	forth_core_execute.asm
	.assert	/u    = .result3 , "U"
	.assert @@/,u = 1        , ",U"
		rts

		fdb	0
		fdb	0
.datastack3	fdb	.text32_len
		fdb	.text32
		fdb	.text31_len
.result3	fdb	.text31

._nu31		fcb	0
.text31		fcc	'TWO'
.text31_len	equ	* - .text31
._nu32		fcb	0
.text32		fcc	'ONE'
.text32_len	equ	* - .text32
._nu33		fcb	0
	.endtst

	;-------------------------------------------

	.test	'S" " S" ONE" COMPARE'
	.opt	test	prot	n , ._nu41
	.opt	test	prot	n , ._nu42
	.opt	test	prot	n , ._nu43
		ldu	#.datastack4
		ldx	#forth_string_compare.xt
		jsr	forth_core_execute.asm
	.assert	/u    = .result4 , "U"
	.assert @@/,u = -1       , ",U"
		rts

		fdb	0
		fdb	0
.datastack4	fdb	.text42_len
		fdb	.text42
		fdb	.text41_len
.result4	fdb	.text41

._nu41		fcb	0
.text41		fcb	0
.text41_len	equ	0
._nu42		fcb	0
.text42		fcc	'ONE'
.text42_len	equ	* - .text42
._nu43		fcb	0
	.endtst

;**********************************************************************

forth_string_search		; ( c-addr1 u1 c-addr2 u2 -- c-addr3 u3 flag )
		fdb	forth_string_compare
		fdb	.xt - .name
.name		fcc	"SEARCH"
.xt		fdb	forth_core_colon.runtime
	;==========================================================
	; : SEARCH
	; ( 1 )	3 PICK 3 PICK 2>R 2>R BEGIN
	; ( 2 )		2DUP R@ MIN 2R@ DROP OVER COMPARE 0= DUP IF
	; ( 3 )			2R> 2DROP 2R> 2DROP EXIT
	; ( 4 )		THEN DROP 1 /STRING DUP 0= IF
	; ( 5 )			2DROP 2R> 2DROP 2R> FALSE EXIT
	; ( 6 )		THEN
	; ( 7 )	REPEAT ;
	;==========================================================
		fdb	forth_core_literal.runtime_xt	; 1
		fdb	3
		fdb	forth_core_ext_pick.xt
		fdb	forth_core_literal.runtime_xt
		fdb	3
		fdb	forth_core_ext_pick.xt
		fdb	forth_core_ext_two_to_r.xt
		fdb	forth_core_ext_two_to_r.xt
.L5		fdb	forth_core_two_dupe.xt		; 2
		fdb	forth_core_r_fetch.xt
		fdb	forth_core_min.xt
		fdb	forth_core_ext_two_r_fetch.xt
		fdb	forth_core_drop.xt
		fdb	forth_core_over.xt
		fdb	forth_string_compare.xt
		fdb	forth_core_zero_equals.xt
		fdb	forth_core_dupe.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L19
		fdb	forth_core_ext_two_r_from.xt	; 3
		fdb	forth_core_two_drop.xt
		fdb	forth_core_ext_two_r_from.xt
		fdb	forth_core_two_drop.xt
		fdb	forth_core_exit.xt
.L19		fdb	forth_core_drop.xt		; 4
		fdb	forth_core_literal.runtime_xt
		fdb	1
		fdb	forth_string_slash_string.xt
		fdb	forth_core_dupe.xt
		fdb	forth_core_zero_equals.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L31
		fdb	forth_core_two_drop.xt		; 5
		fdb	forth_core_ext_two_r_from.xt
		fdb	forth_core_two_drop.xt
		fdb	forth_core_ext_two_r_from.xt
		fdb	forth_core_ext_false.xt
		fdb	forth_core_exit.xt
.L31		fdb	forth_core_ext_again.runtime_xt
		fdb	.L5

	;-----------------------------------------------

	.test	'S" one two three" S" two" SEARCH'
	.opt	test	prot	n , ._nu11
	.opt	test	prot	n , ._nu12
	.opt	test	prot	n , ._nu13
		ldu	#.datastack1
		ldx	#forth_string_search.xt
		jsr	forth_core_execute.asm
	.assert	/u    = .results1 , "U"
	.assert	@@/,u = -1        , "flag"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack1	fdb	.text12_len
.results1	fdb	.text12
		fdb	.text11_len
		fdb	.text11

._nu11		fcb	0
.text11		fcc	'one two three'
.text11_len	equ	* - .text11
._nu12		fcb	0
.text12		fcc	'two'
.text12_len	equ	* - .text12
		fcb	0
._nu13		fcb	0
	.endtst

;**********************************************************************

forth_string_sliteral		; C ( c-addr1 u -- ) R ( -- c-addr2 u )
		fdb	forth_string_search
		fdb	_IMMED | _NOINTERP :: .xt - .name
.name		fcc	"SLITERAL"
.xt		fdb	.body
.body		pshs	u,y		; save some registers
		pulu	y,x		; get c-addr u
		ldu	forth__here
		ldd	#.runtime_xt	; compile xt
		std	,u++
		stx	,u++		; compile length
		beq	.done
.copy		lda	,y+		; compile text
		sta	,u+
		leax	-1,x
		bne	.copy
.done		stu	forth__here
		puls	u,y		; restore registers
		leau	4,u		; adjust stack
		ldx	,y++
		jmp	[,x]

.runtime_xt	fdb	.runtime
.runtime	ldd	,y++		; get length
		tfr	y,x		; get start of word
		leay	d,x		; point Forth IP past word
		pshu	x,d		; save c-addr u
		ldx	,y++
		jmp	[,x]

;**********************************************************************
;		STRING-EXT
;**********************************************************************

forth_string_ext_replaces	; ( c-addr1 u1 c-addr2 u2 -- )
		fdb	forth_string_sliteral
		fdb	.xt - .name
.name		fcc	"REPLACES"
.xt		fdb	forth_core_colon.runtime
	;===================================================
	; : REPLACES
	;	GET-CURRENT >R string-wid SET-CURRENT create" DUP ,
	;	0 ?D0
	;		DUP C@ C, CHAR+
	;	LOOP DROP R> SET-CURRENT
	;   DOES>
	;	DUP CELL+ SWAP @ ;
	;===================================================
		fdb	forth_search_get_current.xt
		fdb	forth_core_to_r.xt
		fdb	forth_core_literal.runtime_xt
		fdb	forth__string_wid
		fdb	forth_search_set_current.xt
		fdb	forth__private_create_quote_xt
		fdb	forth_core_dupe.xt
		fdb	forth_core_comma.xt
		fdb	forth_core_literal.runtime_xt
		fdb	0
		fdb	forth_core_ext_question_do.runtime_xt
		fdb	.L2
.L1		fdb	forth_core_dupe.xt
		fdb	forth_core_c_fetch.xt
		fdb	forth_core_c_comma.xt
		fdb	forth_core_char_plus.xt
		fdb	forth_core_loop.runtime_xt
		fdb	.L1
.L2		fdb	forth_core_drop.xt
		fdb	forth_core_r_from.xt
		fdb	forth_search_set_current.xt
		fdb	forth_core_does.runtime_xt

		.opt	* uses .does
.does		jsr	forth_core_create.does_hook
		fdb	forth_core_dupe.xt
		fdb	forth_core_cell_plus.xt
		fdb	forth_core_swap.xt
		fdb	forth_core_fetch.xt
		fdb	forth_core_exit.xt

	;------------------------------------------------

	.test	"REPLACES DOES>"
		ldu	#.datastack
		ldx	#.foo_xt
		jsr	forth_core_execute.asm
	.assert	/u     = .result    , "U"
	.assert	@@/0,u = @@.foo_len , "len"
	.assert	@@/2,u = .foo_text  , "text"
		rts

.result		fdb	0
		fdb	0
.datastack	fdb	0

		nop

.foo_xt		fdb	.does
.foo_len	fdb	24
.foo_text	fcc	'five till three past two'

	.endtst

;**********************************************************************

Lds		set	24
Lip		set	22
Lcaddr3		set	20
Llen		set	18
Lcend2		set	16
Lcaddr2		set	14
Lcend1		set	12
Lcaddr1		set	10
Lname_c		set	8
Lname_u		set	6
Lrepend		set	4
Lrepaddr	set	2
Ln		set	0

forth_string_ext_substitute	; ( c-addr1 u1 c-addr2 u2 -- c-addr2 u3 n )
		fdb	forth_string_ext_replaces
		fdb	.xt - .name
.name		fcc	"SUBSTITUTE"
.xt		fdb	.body
.body		pshs	u,y		; save U and Y
		pulu	x,d		; get c-addr2 u2
		pshs	x,d		; save as c-addr3 u3
		leay	d,x		; calculate c-end2
		pshs	y,x		; save c-end2 c-addr2
		pulu	x,d		; get c-addr1 u1
		leay	d,x		; calculate c-end1
		pshs	y,x		; save c-end1 c-addr1
		leas	-10,s		; and some more local vars
		clra
		clrb
		std	Ln,s		; n = 0
		tfr	d,y		; len (Y) = 0
		ldx	Lcaddr1,s	; c-addr1 == c-addr2?
		cmpx	Lcaddr2,s
		lbeq	.error
		ldx	Lcaddr1,s	; X = caddr1
		ldu	Lcaddr2,s	; U = caddr2
.next_char	cmpx	Lcend1,s	; any more input?
		lbeq	.done		; if not, we're done
		lda	,x+		; read character
		cmpa	#'%'		; if '%', try substitution
		beq	.start_sub	
.continue_char	cmpu	Lcend2,s	; end of output?
		lbeq	.error		; if so, throw due to lack of space
		sta	,u+		; save character
		leay	1,y		; increase length
		bra	.next_char
.start_sub	stx	Lname_c,s	; save start of name
		clra			; clear length
		clrb
		std	Lname_u,s
.get_name	cmpx	Lcend1,s	; check for end of input
		beq	.unbalanced	; if so, per 17.6.2.2255, emit single %
		lda	,x+		; get character
		cmpa	#'%'		; '%'?
		beq	.have_nameq	; if so, we have the full name
		ldd	Lname_u,s	; increment name length
		addd	#1
		std	Lname_u,s
		bra	.get_name
.have_nameq	ldd	Lname_u,s	; do we have a name?
		bne	.start_search	; if so, attempt substitution
		lda	#'%'		; else we had a '%%', so it subs
		bra	.continue_char	; as a single '%'
.unbalanced	ldx	Lname_c,s
		lda	#'%'
		bra	.continue_char
.start_search	sty	Llen,s		; save length
		stx	Lcaddr1,s	; save caddr1
		stu	Lcaddr2,s	; save caddr2
		ldu	Lds,s		; get data stack
		ldy	Lip,s		; restore Y IP
		ldd	Lname_c,s	; get name start
		pshu	d		; push into data stack
		ldd	Lname_u,s	; get length
		pshu	d		; push
		ldd	#forth__string_wid ; push string wordlist
		pshu	d
		ldx	#forth_search_search_wordlist.xt ; SEARCH-WORDLIST
		lbsr	forth_core_execute.asm
		ldd	,u++		; found?
		bne	.got_replace	; if so, do replacement
		ldx	Ln,s		; adjust n
		leax	-1,x
		stx	Ln,s
		ldx	Lname_c,s	; don't do substitution at all
		leax	-1,x		; point to leading '%'
		ldd	Lname_u,s	; get length
		addd	#2		; adjust for start and end '%'
		leay	d,x		; point to end of name
		sty	Lname_u,s	; save endpoint
		bra	.do_replace
.got_replace	pulu	x		; get xt
		lbsr	forth_core_execute.asm
		pulu	x,d		; get c-addr u
		stx	Lname_c,s	; move to variable
		leay	d,x		; point to end
		sty	Lname_u,s
.do_replace	ldu	Lcaddr2,s	; get caddr2
		ldy	Llen,s		; and current length
.copy_replace	cmpx	Lname_u,s	; more name to copy?
		beq	.done_replace
		lda	,x+
		cmpu	Lcend2,s	; more space to write?
		beq	.error		; throw if not
		sta	,u+
		leay	1,y		; increment c-addr3 length
		bra	.copy_replace
.done_replace	ldx	Ln,s		; increment sub counts
		leax	1,x
		stx	Ln,s
		ldx	Lcaddr1,s	; get caddr1
		lbra	.next_char
.done		ldu	Lds,s		; get data stack
		leau	8,u		; clean data stack
		ldx	Lcaddr3,s	; get caddr3
		tfr	y,d		; get length
		pshu	x,d		; push caddr2, length
		ldd	Ln,s		; get # conversions
		pshu	d		; return 
		leas	22,s		; clean return stack
		puls	y		; restore IP
		leas	2,s		; restore return stack
		ldx	,y++
		jmp	[,x]

.error		ldd	#-78
		std	Ln,s
		bra	.done

	;-------------------------------------------

	.test	"SUBSTITUTE"
	.opt	test	prot	n , ._nu1
	.opt	test	prot	n , ._nu2
	.opt	test	pokew	forth__string_wid , .str_time
		ldu	#.datastack
		ldx	#forth_string_ext_substitute.xt
		jsr	forth_core_execute.asm
	.assert /u      = .result  , "U"
	.assert @@/0,u  = 2        , "conversions"
	.assert @@/4,u  = .buffer  , "text"
	.assert .buffer = "Hello %name%, it is today at five till three past two with 30% humidity.\n","result"
		rts

		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
		fdb	0
.datastack	fdb	.buflen
.result		fdb	.buffer
		fdb	.len
		fdb	.text

._nu1		fcb	0

.str_date	fdb	0
		fdb	4
		fcc	'date'
		fdb	forth_string_ext_replaces.does
		fdb	5
		fcc	'today'

.str_time	fdb	.str_date
		fdb	4
		fcc	'time'
		fdb	forth_string_ext_replaces.does
		fdb	24
		fcc	'five till three past two'

.text		ascii	"Hello %name%, it is %date% at %time% with 30%% humidity.\n"
.len		equ	* - .text
._nu2		fcb	0
.buffer		rmb	80
.buflen		equ	* - .buffer
		fcb	0
	.endtst

;**********************************************************************

forth_string_ext_unescape	; ( c-addr1 u1 c-addr2 -- c-addr2 u2 )
		fdb	forth_string_ext_substitute
		fdb	.xt - .name
.name		fcc	"UNESCAPE"
.xt		fdb	forth_core_colon.runtime
	;======================================================
	; : UNESCAPE
	; ( 1 )	DUP 2SWAP OVER + SWAP ?DO
	; ( 2 )		I C@ [CHAR] % = IF
	; ( 3 )			[CHAR] % OVER C! 1+
	; ( 4 )		THEN I C@ OVER C! 1+
	; ( 5 )	LOOP OVER - ;
	;======================================================
		fdb	forth_core_dupe.xt		; 1
		fdb	forth_core_two_swap.xt
		fdb	forth_core_over.xt
		fdb	forth_core_plus.xt
		fdb	forth_core_swap.xt
		fdb	forth_core_ext_question_do.runtime_xt
		fdb	.L1
.L2		fdb	forth_core_i.xt
		fdb	forth_core_c_fetch.xt
		fdb	forth_core_literal.runtime_xt
		fdb	'%'
		fdb	forth_core_equals.xt
		fdb	forth_core_if.runtime_xt
		fdb	.L3
		fdb	forth_core_literal.runtime_xt
		fdb	'%'
		fdb	forth_core_over.xt
		fdb	forth_core_c_store.xt
		fdb	forth_core_one_plus.xt
.L3		fdb	forth_core_i.xt
		fdb	forth_core_c_fetch.xt
		fdb	forth_core_over.xt
		fdb	forth_core_c_store.xt
		fdb	forth_core_one_plus.xt
		fdb	forth_core_loop.runtime_xt
		fdb	.L2
.L1		fdb	forth_core_over.xt
		fdb	forth_core_minus.xt
		fdb	forth_core_exit.xt

;**********************************************************************

forth__free	equ	*
