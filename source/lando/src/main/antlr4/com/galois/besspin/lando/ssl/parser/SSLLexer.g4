lexer grammar SSLLexer;

@members {
    public Boolean debug = false;

    //We override nextToken() to handle a special rewind token which can
    //be used to reset the input stream to the start of the current token; this
    //is effectively a backtracking mechanism.
    //The function has also been enhanced to give us some information about
    //lexing, which is critical given our complicated usage of lexer modes. Just
    //use the argument '-d' or '--debug' to see what is being lexed as well
    //as mode transitions and rewinding
    public Token nextToken() {
        int incomingMode = this._mode ;
        String mode_str = String.format("%s", incomingMode);
        String text = "<null>";
        String rewind = "No Rewind";

        while (true) {
            int mark = _input.mark();
            try {
                int startIndex = _input.index();
                Token token = super.nextToken();
                int tokenType = token.getType();

                text = token.getText().replace("\n","\\n");
                mode_str = String.format("%s -> %d", mode_str, this._mode);

                if (token != null && tokenType == SPECIAL_REWIND) {
                    rewind = "Rewinded on \"" + text + "\"" ;

                    // since we're backtracking, we've no longer hit the EOF
                    super._hitEOF = false;

                    _input.seek(startIndex);
                    continue;
                }

                if (debug) {
                    int endIndex = _input.index();
                    System.out.printf("%-9s | %-2d | mode %-11s | idx %d -> %d | %s \n",
                                      text, tokenType, mode_str, startIndex, endIndex, rewind);
                }

                return token;
            }
            finally {
                _input.release(mark);
            }
        }
    }
}

tokens {
    SPECIAL_REWIND,
    LINESEP,
    NAMECHAR,
    SENTENCE,
    RELKEYWORD
}

//Common Fragments
fragment F_LINESEP      : ('\r'? '\n' | '\r') | EOF ;
fragment F_WHITESPACE   : [ \t] ;
fragment F_WHITESPACES  : F_WHITESPACE+ ;
fragment F_EMPTYLINE    : F_LINESEP F_WHITESPACE* F_LINESEP ; //NOTE: This consumes the second line

fragment F_SYSTEM       : 'system' ;
fragment F_SUBSYSTEM    : 'subsystem' ;
fragment F_COMPONENT    : 'component' ;
fragment F_EVENTS       : 'events' ;
fragment F_SCENARIOS    : 'scenarios' ;
fragment F_REQUIREMENTS : 'requirements' ;
fragment F_RELATION     : 'relation' ;
fragment F_BLOCK_KEYWORDS : F_SYSTEM | F_SUBSYSTEM | F_COMPONENT | F_EVENTS | F_SCENARIOS | F_REQUIREMENTS | F_RELATION ;

fragment F_KEYWORD_SEP  : F_WHITESPACE | F_LINESEP ;

fragment F_RELKEYWORD   : 'inherit' | 'client' | 'contains' ;

fragment F_ABBREVSTART  : '(' ;
fragment F_ABBREVEND    : ')' ;
fragment F_ABBREV       : [A-Za-z0-9_-]+ ;

fragment F_INDEXING     : 'indexing' ;
fragment F_DICTSEP      : ':' ;

fragment F_SENTENCESTART : [a-zA-Z0-9] ; // Sentences currently start with an alpha numeric character. It used to be (~ [ \t\r\n/]) ;
fragment F_SENTENCECHAR  : ~ [.!?] ;
fragment F_SENTENCEEND   : [.!?] ;
fragment F_SENTENCE      : F_SENTENCESTART F_SENTENCECHAR*? F_SENTENCEEND  ;

fragment F_SENTENCEENDLOOKAHEAD : F_SENTENCEEND | EOF ;

fragment F_COMMAND      : F_SENTENCESTART F_SENTENCECHAR* [!] ;
fragment F_CONSTRAINT   : F_SENTENCESTART F_SENTENCECHAR* [.] ;
fragment F_QUERY        : F_SENTENCESTART F_SENTENCECHAR* [?] ;

fragment F_LINECOMMENTSTART : '//' ;

//Top-level (default) lexer

WS           : F_WHITESPACES -> skip ;

LINESEP      : F_LINESEP ;

SYSTEM       : F_SYSTEM -> pushMode(MODE_PARAGRAPH), pushMode(MODE_NAMEPHRASEREL) ;

SUBSYSTEM    : F_SUBSYSTEM -> pushMode(MODE_PARAGRAPH), pushMode(MODE_NAMEPHRASERELABBREV) ;

COMPONENT    : F_COMPONENT -> pushMode(MODE_COMPONENT_PARTS), pushMode(MODE_PARAGRAPH), pushMode(MODE_NAMEPHRASERELABBREV) ;

EVENTS       : F_EVENTS -> pushMode(MODE_MAYBE_IDENT_LINE), pushMode(MODE_NAMEPHRASE) ;

SCENARIOS    : F_SCENARIOS -> pushMode(MODE_MAYBE_IDENT_LINE), pushMode(MODE_NAMEPHRASE) ;

REQUIREMENTS : F_REQUIREMENTS -> pushMode(MODE_MAYBE_IDENT_LINE), pushMode(MODE_NAMEPHRASE) ;

RELATION     : F_RELATION -> pushMode(MODE_NAMEPHRASEREL) ;

INDEXING     : F_INDEXING -> pushMode(MODE_INDEXING) ;

COMMENT      : F_LINECOMMENTSTART -> pushMode(MODE_COMMENTS) ;


//Name-phrase-rel lexing
//Essentially: Word characters _including_ spaces. Terminated by either a relation key word or end of line
mode MODE_NAMEPHRASEREL;

NMR_LINESEP      : F_LINESEP -> type(LINESEP), popMode ;

NMR_RELKEYWORD   : F_WHITESPACES F_RELKEYWORD F_WHITESPACES -> type(RELKEYWORD), popMode, pushMode(MODE_NAMEPHRASE) ;

NMR_NAMECHAR     : . -> type(NAMECHAR) ;

NMR_COMMENT      : F_LINECOMMENTSTART -> type(COMMENT), pushMode(MODE_COMMENTS) ;


//Name-phrase lexing
//Essentially: Word characters _including_ spaces. Terminated by an end of line
mode MODE_NAMEPHRASE;

NM_LINESEP   : F_LINESEP -> type(LINESEP), popMode ;

NM_NAMECHAR  : . -> type(NAMECHAR) ;

NM_COMMENT   : F_LINECOMMENTSTART -> type(COMMENT), pushMode(MODE_COMMENTS) ;


//Name-phrase-rel-abbrev lexing
//Essentially: Word characters _including_ spaces and abbreviations. Terminated by one of - relation key word or end of line
mode MODE_NAMEPHRASERELABBREV;

NMRA_LINESEP      : F_LINESEP -> type(LINESEP), popMode ;

NMRA_RELKEYWORD   : F_WHITESPACES F_RELKEYWORD F_WHITESPACES -> type(RELKEYWORD), popMode, pushMode(MODE_NAMEPHRASE) ;

//Finding an abbrev is essentially a termination of the _name_ part of this mode. After this you can still get a RELKEYWORD
//however. So switch to a different mode and let it do the necessary circus. Note: we could potentially inline the abbrev mode
//entirely here, but then we had have to strip out the parenthesis in code.
NMRA_ABBREVSTART  : F_ABBREVSTART -> skip, popMode, pushMode(MODE_ABBREV) ;

NMRA_NAMECHAR     : . -> type(NAMECHAR) ;

NMRA_COMMENT      : F_LINECOMMENTSTART -> type(COMMENT), pushMode(MODE_COMMENTS) ;


mode MODE_ABBREV ;

ABB_WHITESPACE    : F_WHITESPACE -> skip ;

ABBREV            : F_ABBREV ;

//After abbreviations, we can have an optional RELKEYWORD
ABB_ABBREVEND     : F_ABBREVEND -> skip, popMode, pushMode(MODE_MAYBE_RELKEYWORD) ;


mode MODE_MAYBE_RELKEYWORD ;

REL_LINESEP       : F_LINESEP -> type(LINESEP), popMode ;

REL_WHITESPACE    : F_WHITESPACE -> skip ;

REL_RELKEYWORD    : F_RELKEYWORD -> type(RELKEYWORD), popMode, pushMode(MODE_NAMEPHRASE) ;

REL_COMMENT       : F_LINECOMMENTSTART -> type(COMMENT), pushMode(MODE_COMMENTS) ;

//Paragraph
mode MODE_PARAGRAPH;

PAR_WS         : F_WHITESPACES -> skip ;

PAR_EMPTYLINE  : F_EMPTYLINE -> type(LINESEP), popMode ;

//PARAGRAPH      : (F_SENTENCE F_WHITESPACES?)+? F_WHITESPACES? F_LINESEP F_WHITESPACES? F_LINESEP F_WHITESPACES? -> popMode ;
PARAGRAPH      : F_SENTENCE (F_WHITESPACES? F_SENTENCE)* ;

//Component Parts -- Queries, Commands, Constraints
mode MODE_COMPONENT_PARTS ;

CP_WHITESPACES  : F_WHITESPACES -> skip ;

CP_LINESEP      : F_LINESEP -> type(LINESEP) ;

CP_ALL_KEYWORDS : F_BLOCK_KEYWORDS F_KEYWORD_SEP (F_SENTENCECHAR+ F_SENTENCEEND)? -> popMode, type(SPECIAL_REWIND) ;

CONSTRAINT      : F_CONSTRAINT ;

COMMAND         : F_COMMAND ;

QUERY           : F_QUERY ;

CP_COMMENT      : F_LINECOMMENTSTART -> type(COMMENT), pushMode(MODE_COMMENTS) ;

//The start line of a specific event, requirement or scenario
mode MODE_IDENT_LINE ;

IL_LINESEP     : F_LINESEP -> type(LINESEP), popMode, pushMode(MODE_SINGLE_SENTENCE) ;

IL_NAMECHAR    : . -> type(NAMECHAR) ;

IL_COMMENT     : F_LINECOMMENTSTART -> type(COMMENT), pushMode(MODE_COMMENTS) ;

//The content of a specific event, requirement or scenario
mode MODE_SINGLE_SENTENCE ;

SS_WHITESPACE   : F_WHITESPACE -> skip ;

SS_LINESEP      : F_LINESEP -> type(LINESEP) ;

SS_SENTENCE     : F_SENTENCE -> type(SENTENCE), popMode, pushMode(MODE_MAYBE_IDENT_LINE) ;


mode MODE_MAYBE_IDENT_LINE ;

MRIL_WHITESPACE   : F_WHITESPACE -> skip ;

MRIL_LINESEP      : F_LINESEP -> type(LINESEP) ;

MRIL_ALL_KEYWORDS : F_BLOCK_KEYWORDS -> popMode, type(SPECIAL_REWIND) ;

MRIL_ANYCHAR      : . -> type(SPECIAL_REWIND), popMode, pushMode(MODE_IDENT_LINE) ;

MRIL_COMMENT      : F_LINECOMMENTSTART -> type(COMMENT), pushMode(MODE_COMMENTS) ;


mode MODE_INDEXING ;

INDEXSEP         : F_DICTSEP ;

IND_LINESEP      : F_LINESEP -> type(LINESEP) ;

IND_EMPTYLINE    : F_EMPTYLINE -> type(LINESEP) ;

IND_ALL_KEYWORDS : F_BLOCK_KEYWORDS F_KEYWORD_SEP (F_SENTENCECHAR+ F_SENTENCEEND)? -> popMode, type(SPECIAL_REWIND) ;

INDEXCHAR        : . ;

IND_COMMENT      : F_LINECOMMENTSTART -> type(COMMENT), pushMode(MODE_COMMENTS) ;


mode MODE_COMMENTS ;

CMT_LINESEP      : F_LINESEP -> popMode, type(SPECIAL_REWIND) ;

COMMENTCHAR      : . ;