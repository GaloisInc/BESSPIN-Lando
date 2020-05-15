lexer grammar SSLLexer;

@members {
    public Boolean debug = false;

    //We override nextToken() to give us some optional information about
    //lexing, which is critical given our complicated usage of lexer modes. Just
    //use the argument '-d' or '--debug' to see what is being lexed as well
    //as mode transitions and the top of the mode stack.
    public Token nextToken() {
        int incomingMode = this._mode ;
        int startIndex = _input.index();

        Token token = super.nextToken();

        if (debug) {
            String text = "'" + token.getText().replace("\n","\\n") + "'";
            int tokenType = token.getType();
            int outgoingMode = this._mode;
            int endIndex = _input.index();

            System.out.printf("%-12s | %-2d | mode %d -> %d | peek %d | idx %d -> %d \n",
                              text, tokenType, incomingMode, outgoingMode,
                              this.peekMode(), startIndex, endIndex);
        }

        return token;
    }

    //We override popMode() to default to mode 0 if the stack is empty
    public int popMode() {
    	if ( _modeStack.isEmpty() ) _modeStack.push(0);
    	return super.popMode();
    }

    public int peekMode() {
        if ( _modeStack.isEmpty() ) return 0;
    	return _modeStack.peek();
    }

    //See [Note 1]
    public Boolean isWordInTopMode() {
        switch (peekMode()) {
            //F_NAME_WORD
            case MODE_NAMEPHRASE:
            case MODE_NAMEPHRASEREL:
            case MODE_IDENT_LINE:
                return getText().matches("[^\\(\\)]*[^\\(\\).!?]");
            //F_SENT_WORD
            case MODE_PARAGRAPH:
            case MODE_SINGLE_SENTENCE:
                return getText().matches(".*[^.!?]");
            //F_INDEX_WORD
            case MODE_INDEXING:
                return getText().matches("[^:]+");
            default:
                return false;
        }
    }
}

tokens {
    EMPTYLINE,
    WORD, SPACE,
    COMMANDTERM, CONSTRAINTTERM, QUERYTERM
}

//Common Fragments
fragment F_WHITESPACE   : [ \t] ;
fragment F_WHITESPACES  : F_WHITESPACE+ ;
fragment F_LINESEP      : ('\r'? '\n' | '\r') F_WHITESPACES? //We always ignore leading WS
                        | EOF ;
fragment F_NONLINESEP   : ~ [\r\n];
fragment F_EMPTYLINE    : F_LINESEP F_LINESEP ; //NOTE: This consumes the second line

//Words - this must match isWordInTopMode() for the relevant modes
fragment F_NAME_WORD  : (~ [\r\n \t()])* (~ [\r\n \t.!?()]) ;
fragment F_SENT_WORD  : (~ [\r\n \t]  )* (~ [\r\n \t.!?]  ) ;
fragment F_INDEX_WORD : (~ [\r\n \t:]    )+ ;
fragment F_GEN_WORD   : (~ [\r\n \t]     )+ ;

//Keywords
fragment F_SYSTEM       : 'system' ;
fragment F_SUBSYSTEM    : 'subsystem' ;
fragment F_COMPONENT    : 'component' ;
fragment F_EVENTS       : 'events' ;
fragment F_SCENARIOS    : 'scenarios' ;
fragment F_REQUIREMENTS : 'requirements' ;
fragment F_RELATION     : 'relation' ;
fragment F_INDEXING     : 'indexing' ;
fragment F_RELKEYWORD   : 'inherit' | 'client' | 'contains' ;

fragment F_DICTSEP          : ':' ;
fragment F_ABBREVSTART      : '(' ;
fragment F_ABBREVEND        : ')' ;
fragment F_COMMANDTERM      : '!' ;
fragment F_CONSTRAINTTERM   : '.' ;
fragment F_QUERYTERM        : '?' ;
fragment F_LINECOMMENTSTART : '//' ;


//Top-level (default) lexer - mode 0

COMMENTSTART : F_LINECOMMENTSTART -> pushMode(MODE_COMMENT) ;

WS           : F_WHITESPACES -> skip ;

LINESEP      : F_LINESEP ;

SYSTEM       : F_SYSTEM       -> pushMode(MODE_PARAGRAPH), pushMode(MODE_NAMEPHRASEREL) ;
SUBSYSTEM    : F_SUBSYSTEM    -> pushMode(MODE_PARAGRAPH), pushMode(MODE_NAMEPHRASEREL) ;
COMPONENT    : F_COMPONENT    -> pushMode(MODE_PARAGRAPH), pushMode(MODE_NAMEPHRASEREL) ;

EVENTS       : F_EVENTS       -> pushMode(MODE_IDENT_LINE), pushMode(0), pushMode(MODE_NAMEPHRASE) ; //see [Note 3]
SCENARIOS    : F_SCENARIOS    -> pushMode(MODE_IDENT_LINE), pushMode(0), pushMode(MODE_NAMEPHRASE) ;
REQUIREMENTS : F_REQUIREMENTS -> pushMode(MODE_IDENT_LINE), pushMode(0), pushMode(MODE_NAMEPHRASE) ;

RELATION     : F_RELATION     -> pushMode(MODE_NAMEPHRASEREL) ;

INDEXING     : F_INDEXING     -> pushMode(MODE_INDEXING) ;

NON_KEYWORD  : F_GEN_WORD { isWordInTopMode() }? -> type(WORD), popMode ;
//[Note 1] If hit, switches to the mode stored at the top of the stack, if there is one.
//         This is used for continuing MODE_PARAGRAPH after an empty line ([Note 2]),
//         continuing MODE_INDEXING after an empty line, and starting MODE_IDENT_LINE
//         for an optional event/scenario/requirement entry ([Note 3]). Note that since
//         a WORD in each of these three modes is slightly different, we must include
//         the semantic predicate isWordInTopMode() to avoid edge cases.



//Name-phrase lexing
mode MODE_NAMEPHRASE;

NP_COMMENTSTART     : F_LINECOMMENTSTART -> type(COMMENTSTART), popMode, pushMode(MODE_COMMENT) ;

NP_LINESEP          : F_LINESEP          -> type(LINESEP),      popMode ;

NP_NAMEPHRASE_WORD  : F_NAME_WORD   -> type(WORD) ;
NP_NAMEPHRASE_SPACE : F_WHITESPACES -> type(SPACE) ;

//Name-phrase lexing with relation keywords and abbreviations
mode MODE_NAMEPHRASEREL;

NPR_COMMENTSTART     : F_LINECOMMENTSTART -> type(COMMENTSTART), popMode, pushMode(MODE_COMMENT) ;

NPR_LINESEP          : F_LINESEP          -> type(LINESEP),      popMode ;

RELKEYWORD           : F_RELKEYWORD ;

ABBREVSTART          : F_ABBREVSTART ;
ABBREVEND            : F_ABBREVEND ;

NPR_NAMEPHRASE_WORD  : F_NAME_WORD   -> type(WORD) ;
NPR_NAMEPHRASE_SPACE : F_WHITESPACES -> type(SPACE) ;


//Paragraphs, terminated by empty lines
mode MODE_PARAGRAPH;

PAR_COMMENTSTART : F_LINECOMMENTSTART -> type(COMMENTSTART), pushMode(MODE_COMMENT) ;

PAR_LINESEP    : F_LINESEP   -> type(LINESEP) ;

PAR_EMPTYLINE  : F_EMPTYLINE -> type(EMPTYLINE), pushMode(0) ;
//[Note 2] We `pushMode(0)` instead of `popMode` here to store MODE_PARAGRAPH at
//         the top of the stack. By [Note 1], this ensures MODE_PARAGRAPH can
//         continue if the next line does not start with a keyword. This is used
//         for lexing component parts after an explanation.

PAR_COMMANDTERM    : F_COMMANDTERM    -> type(COMMANDTERM) ;
PAR_CONSTRAINTTERM : F_CONSTRAINTTERM -> type(CONSTRAINTTERM) ;
PAR_QUERYTERM      : F_QUERYTERM      -> type(QUERYTERM) ;

PAR_WORD       : F_SENT_WORD   -> type(WORD) ;
PAR_SPACE      : F_WHITESPACES -> type(SPACE) ;


//The start line of a specific event, requirement or scenario
//(similar to MODE_NAMEPHRASE)
mode MODE_IDENT_LINE ;

IL_COMMENTSTART     : F_LINECOMMENTSTART -> type(COMMENTSTART), popMode, pushMode(MODE_SINGLE_SENTENCE), pushMode(MODE_COMMENT) ;

IL_LINESEP          : F_LINESEP          -> type(LINESEP),      popMode, pushMode(MODE_SINGLE_SENTENCE) ;

IL_NAMEPHRASE_WORD  : F_NAME_WORD   -> type(WORD) ;
IL_NAMEPHRASE_SPACE : F_WHITESPACES -> type(SPACE) ;

//The content of a specific event, requirement or scenario
//Single sentences, terminated by empty lines or punctuation
//(similar to MODE_PARAGRAPH)
//[Note 3] When exiting this mode, instead of just `popMode` we store
//         MODE_IDENT_LINE at the top of the stack then `pushMode(0)`.
//         By [Note 1], this ensures MODE_IDENT_LINE can lex the next
//         entry, if there is one that does not start with a keyword.
mode MODE_SINGLE_SENTENCE ;

SS_COMMENTSTART   : F_LINECOMMENTSTART -> type(COMMENTSTART), pushMode(MODE_COMMENT) ;

SS_LINESEP        : F_LINESEP        -> type(LINESEP) ;

SS_EMPTYLINE      : F_EMPTYLINE      -> type(EMPTYLINE),      popMode, pushMode(MODE_IDENT_LINE), pushMode(0) ;

SS_COMMANDTERM    : F_COMMANDTERM    -> type(COMMANDTERM),    popMode, pushMode(MODE_IDENT_LINE), pushMode(0) ;
SS_CONSTRAINTTERM : F_CONSTRAINTTERM -> type(CONSTRAINTTERM), popMode, pushMode(MODE_IDENT_LINE), pushMode(0) ;
SS_QUERYTERM      : F_QUERYTERM      -> type(QUERYTERM),      popMode, pushMode(MODE_IDENT_LINE), pushMode(0) ;

SS_WORD           : F_SENT_WORD   -> type(WORD) ;
SS_SPACE          : F_WHITESPACES -> type(SPACE) ;


//Indexing (similar to MODE_PARAGRAPH)
mode MODE_INDEXING ;

IND_COMMENTSTART : F_LINECOMMENTSTART -> type(COMMENTSTART), pushMode(MODE_COMMENT) ;

IND_LINESEP      : F_LINESEP   -> type(LINESEP) ;

IND_EMPTYLINE    : F_EMPTYLINE -> type(EMPTYLINE), pushMode(0) ; //see [Note 2]

INDEXSEP         : F_DICTSEP ;

IND_WORD         : F_INDEX_WORD  -> type(WORD) ;
IND_SPACE        : F_WHITESPACES -> type(SPACE) ;


mode MODE_COMMENT ;

CMT_LINESEP : F_LINESEP -> type(LINESEP), popMode ;

COMMENT     : F_NONLINESEP+ ;