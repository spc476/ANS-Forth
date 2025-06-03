
ANS Forth 2012 for the 6809.
Copyright 2025 by Sean Conner.
GPL3+

This is an implementation of ANS Forth (2012) for the 6809 CPU.  It
implements the following ANS Forth wordsets:

	CORE		CORE-EXT
	DOUBLE		DOUBLE-EXT
	EXCEPTION	EXCEPTION-EXT
	LOCAL		LOCAL-EXT
	TOOLS		some of TOOLS-EXT (see "Providing" section for list)
	SEARCH		SEARCH-EXT
	STRING		STRING-EXT

It does NOT implement the following ANS Forth wordsets for the following
reaons:

	BLOCK		BLOCK-EXT	system/OS dependent
	FACILITY	FACILITY-EXT	system/OS dependent
	FILE		FILE-EXT	system/OS dependent
	FLOATING	FLOATING-EXT	memory constraints
	MEMORY				memory constraints
	XCHAR		XCHAR-EXT	memory constraints

Only three routines need to be supplied to get a working system:

	getchar		input a character
	putchar		output a character
	bye		return to OS/system monitor

Further more, the amount of memory to be set aside for the dictionaries and
stacks is configurable at runtime (see the source code).  This code was
written to be assembled by a09 (https://github.com/spc476/a09) and will not
assemble without modifications on other 6809 assemblers.

The implementation is a standard indirect threaded code (ITC) system.
Register ussage is:

	D	free for use (top of stack always in memory)
	X	execution token of word
	Y	Forth IP register
	U	user stack pointer
	S	return stack pointer

The format for each word in the dictionary is:

	16 bit pointer to next word in dictionary
	16 bit length of word, with the following bits set aside for flags:
		bit 15 - word is immediate
		bit 14 - hidden word
		bit 13 - no interpretation semantics
		bit 12 - double variable (required for TO to operate)
		bit 11 - local variable (required for TO to operate)
	1..31 characters for name
	16 bit pointer to execution function
	0..n bytes for definition of word

This code passes the ANS Forth 2012 Test Suite [see note] for the wordsets
used, as well as all the internal tests in the code.  I'm reasonably sure
this system will work as intended, but the tests in and of themselves don't
prove the absence of bugs.

Note:	The ANS Forth Test Suite (github.com/gerryjackson/forth2012-test-suite) 
	isn't suitable as-is for testing this codebase, as some of the tests
	violate contraints that exists in this code, notably some tests
	assume case-insensitivity and support for lines longer than 80
	characters.

The code can be placed in ROM if desired.

And now for the Obligatory Documentation required by the ANS Forth Standard:

ANS Forth-2012 System Providing 
	the Core Extensions word set
	the Double-Number word set
	the Double-Number Extensions word set
	the Exception word set
	the Exception Extensions word set
	the Locals word set
	the Locals Extensions word set
	the Programming-Tools word set
	from the Programming-Tools Extensions word set
		AHEAD
		BYE
		CS-PICK
		CS-ROLL
		N>R
		NAME>COMPILE
		NAME>INTERPRET
		NAME>STRING
		NR>
		STATE
		TRAVERSE-WORDLIST
		[DEFINED]
		[ELSE]
		[IF]
		[THEN]
		[UNDEFINED]
	the Search-Order word set
	the Search-Order Extensions word set
	the String word set
	the String Extensions word set

Implementation-defined options

	* Aligned addresses: none
	* EMIT on nongraphic characters: what the provided output routine
	  does
	* ACCEPT: expects LF (ASCII 10) to end a line input
	* Character set: US-ASCII
	* Character aligned addresses: none
	* Conditions under which control characters match a space: none
	* Format of control-flow stack: data stack, each entry is one cell,
		an addresss
	* Conversion of digits larger than 35: -13 THROW
	* Display after input terminates: OK on its own line
	* Exception abort sequence: -2 THROW
	* Input line terminator: as defined in source (currently LF)
	* Maximum size of counted string: 255 address units
	* Maximum size of a pasrsed string: 80 address units
	* Maximum size of definition name: 31 address units
	* Maximum string length for ENVIRONMENT?: 31 address units
	* Method of selecting input device: none (terminal only)
	* Method of selecting output device: none (terminal only)
	* Methods of dictionary compilation:
		16 bit pointer to next word in dictionary
		16 bit length of word name
			bit 15 - immediate word
			bit 14 - hidden word
			bit 13 - no interpretation semantics
			bit 12 - double variable (required for TO)
			bit 11 - local variable (required for TO)
		1..31 characaters for name
		16 bit function pointer to execute word
		1..n bytes for word definition (code, Forth code, data)
	* Number of bits in one address unit: 8
	* Number representation and arithmatic: 2's compliment
	* Ranges for number types:
		n	     -32768 .. 32767
		+n	          0 .. 32767
		u	          0 .. 65535
		d	-2147483648 .. 2147483647
		+d	          0 .. 2147483647
		ud	          0 .. 4294967295
	* Read-only data-space: none
	* Size of WORD buffer: 33 address units
	* Size of one cell: 2 address units
	* Size of one character: 1 address unit
	* Size of terminal input buffer: 80 address units
	* Size of pictured numeric output buffer: 34 address units
	* Size of PAD: 84 address units
	* System case-sensitivity: none
	* System prompt: OK
	* Type of division rounding: symetrical
	* Value of STATE when true: non-zero
	* Values after arithmatic overflow: 2's compliment wrapping
	* Definition found after DOES>: no
	* Maximum number of locals in a definition: 16
	* source and format of display by SEE: Mostly Forth code output;
	  runtime actions of words will be displayed by the word commented
	  out (i.e. "( LITERAL ) 23").
	* maximum number of word lists in the search order: 8
	* minimum search order: FORTH-WORDLIST

Ambiguous conditions

	* a name is neither a valid definition name nor a valid number
	  during text interpretation: -13 THROW
	* a definition name exceeded the maximum length allowed: -19 THROW
	* addressing a region not listed in 3.3.3 Data space: allowed, but
	  results undefined.
	* argument type incompatible with specified input parameter, e.g.,
	  passing a flag to a word expecting an n: undefined
	* attempting to obtain the execution token, (e.g., with 6.1.0070 ',
	  6.1.1550 FIND, etc.  of a definition with undefined interpretation
	  semantics: -14 THROW
	* dividing by zero: -10 THROW
	* insufficient data-stack space or return-stack space (stack
	  overflow): -3 THROW (data-stack) -5 THROW (return-stack)
	* insufficient space for loop-control parameters: -7 THROW
	* insufficient space in the dictionary: undefined
	* interpreting a word with undefined interpretation semantics:
	  undefined
	* modifying the contents of the input buffer or a string literal:
	  undefined
	* overflow of a pictured numeric output string: undefined
	* parsed string overflow: undefined
	* producing a result out of range, e.g., multiplication (using *)
	  results in a value too big to be represented by a single-cell
	  integer: 2's compliment overflow
	* reading from an empty data stack or return stack: -4 THROW
	  (data-stack) -5 THROW (return-stack)
	* unexpected end of input buffer, resulting in an attempt to use a
	  zero-length string as a name: -16 THROW
	* >IN greater than size of input buffer: treated as end of input
	* RECURSE appears after DOES>: undefined
	* argument input source different than current input source for
	  RESTORE-INPUT: SOURCE-ID restored
	* data space containing definitions is de-allocated: no affect
	  (hopefully)
	* less than u+2 stack items: undefined results
	* loop-control parameters not available: undefined results
	* most recent definition does not have a name: undefined (corruption
	  of location 0)
	* TO not followed directly by a name defined by a word with
	  "TO name runtime" semantics: undefined
	* name not found ', POSTPONE, ['], [COMPILE]): -13 THROW
	* parameters are not of the same type (DO, ?DO, WITHIN): undefined
	* POSTPONE, [COMPILE], ' or ['] applied to TO: undefined
	* string longer than a counted string returned by WORD: undefined
	* u greater than or equal to the number of bits in a cell (LSHIFT,
	  RSHIFT): results in 0
	* word not defined via CREATE: undefined
	* words improperly used outside <# and #> (#, #S, HOLD, HOLDS,
	  SIGN): undefined
	* access to a deferred word, a word defined by DEFER, which has yet
	  to be assigned to an xt: undefined
	* access to a deferred word, a word defined by DEFER, which was not
	  defined by DEFER: undefined
	* POSTPONE, [COMPILE], ['] or ' applied to ACTION-OF or IS:
	  undefined
	* \x is not followed by two hexadecimal characters (S\"): undefined
	* a \ is placed before any character, other than those defined in
	  S\": -18 THROW
	* d outside range of n in D>S: result truncated
	* executing a named local while in interpretation state ((LOCAL)): 
	  undefined
	* a local name ends in ":", "[", "^": allowed
	* a local name is a single non-alphabetic character: undefined
	* the text between {: and :} extends over more than one line; {: ... 
	  :} is used more than once in a word: undefined
	* fewer than u+1 items on control-flow stack (CS-PICK, CS-ROLL):
	  undefined
	* POSTPONE applied to [IF]: undefined
	* reaching the end of the input source before matching [ELSE] or
	  [THEN] ([IF]): undefined
	* NR> is used with data not stored by N>R: undefined
	* adding to or deleting from the wordlist during the execution of
	  TRAVERSE-WORDLIST: undefined
	* changing the compilation word list: undefined
	* search order empty: broken system
	* too many word lists in search order: -49 THROW
	* The substitution cannot be created (REPLACES): undefined
	* The name of a substitution contains the `%' delimiter character
	  (REPLACES): potential issue if string goes through another
	  substitution.
	* Result of a substitution is too long to fit into the given buffer
	  (SUBSTITUTE and UNESCAPE): -78 THROW
	* Source and destination buffers for SUBSTITUTE are the same: -78
	  THROW
