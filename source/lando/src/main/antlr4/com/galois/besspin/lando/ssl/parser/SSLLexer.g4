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

    //   //See [Note 1]
    //    public Boolean isWordInTopMode() {
    //        switch (peekMode()) {
    //            //F_NAME_WORD
    //            case MODE_NAME:
    //            case MODE_INAME:
    //                return getText().matches("[^\\(\\):]*[^\\(\\).!?,:]");
    //            //F_SENT_WORD
    //            case MODE_PARAGRAPH:
    //            case MODE_ENTRY:
    //                return getText().matches(".*[^.!?]");
    //            //F_INDEX_WORD
    //	    case MODE_INDEXING:
    //                return getText().matches(".+");
    //            default:
    //                //better to throw a parse error than a lexical error
    //                return true;
    //        }
    //    }

    public void less() {
      _input.seek(this._tokenStartCharIndex);
      _type = MORE;
    }

}

tokens {
    EMPTYLINE,
    NMWORD, SWORD, IWORD, SPACE, INDEXSEP, RELSEP,
    QNAMESEP, ABBREVSTART, ABBREVEND,
    CONTAINS, END,
    INHERIT, CLIENT,
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
fragment F_NAME_WORD  : (~ [\r\n \t():])* (~ [\r\n \t.!?,():]) ;
fragment F_SENT_WORD  : (~ [\r\n \t]  )* (~ [\r\n \t.!?]  ) ;
fragment F_INDEX_WORD : (~ [\r\n \t] )* (~ [\r\n \t:]) ;
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
fragment F_INHERIT	    : 'inherit' ;
fragment F_CLIENT       : 'client' ; 
fragment F_CONTAINS  	: 'contains' ;
fragment F_END		    : 'end' ;
fragment F_IMPORT_COMPONENT      : 'import' ([ \t])+ 'component' ;
fragment F_IMPORT_SUBSYSTEM      : 'import' ([ \t])+ 'subsystem' ;

fragment F_INDEXSEP         : ':' ;
fragment F_RELSEP	        : ',' ;
fragment F_QNAMESEP         : ':' ;
fragment F_ABBREVSTART      : '(' ;
fragment F_ABBREVEND        : ')' ;
fragment F_COMMANDTERM      : '!' ;
fragment F_CONSTRAINTTERM   : '.' ;
fragment F_QUERYTERM        : '?' ;
fragment F_LINECOMMENTSTART : '//' ;

fragment F_LINECOMMENT      : F_LINECOMMENTSTART F_NONLINESEP* ;


//Top-level (default) lexer - mode 0

COMMENT      : F_LINECOMMENT ;

WS           : F_WHITESPACES -> skip ;

LINESEP      : F_LINESEP ;

EMPTYLINE    : F_EMPTYLINE ;

SYSTEM       : F_SYSTEM       -> pushMode(MODE_PARAGRAPH), pushMode(MODE_NAME) ;
SUBSYSTEM    : F_SUBSYSTEM    -> pushMode(MODE_PARAGRAPH), pushMode(MODE_NAME) ;
COMPONENT    : F_COMPONENT    -> pushMode(MODE_PARAGRAPH), pushMode(MODE_NAME) ;
IMPORT_SUBSYSTEM : F_IMPORT_SUBSYSTEM       -> pushMode(MODE_NAME);
IMPORT_COMPONENT : F_IMPORT_COMPONENT       -> pushMode(MODE_NAME);

CONTAINS   : F_CONTAINS  ;
END        : F_END ;

EVENTS       : F_EVENTS       -> pushMode(MODE_ENTRY), pushMode(MODE_NAME), pushMode(0), pushMode(MODE_NAME) ; //see [Note 3]
SCENARIOS    : F_SCENARIOS    -> pushMode(MODE_ENTRY), pushMode(MODE_NAME), pushMode(0), pushMode(MODE_NAME) ;
REQUIREMENTS : F_REQUIREMENTS -> pushMode(MODE_ENTRY), pushMode(MODE_NAME), pushMode(0), pushMode(MODE_NAME) ;

RELATION     : F_RELATION     -> pushMode(MODE_NAME) ;

INDEXING     : F_INDEXING     -> pushMode(MODE_INDEXING) ;

NON_KEYWORD  : F_GEN_WORD      { less(); popMode(); };
//[Note 1] If hit, switches to the mode stored at the top of the stack, if there is one,
//         and relexes in that mode.
//         This is used for continuing MODE_PARAGRAPH after an empty line ([Note 2]),
//         continuing MODE_INDEXING after an empty line, and starting MODE_NAME/MODE_ENTRY
//         for an optional event/scenario/requirement entry ([Note 3]).

//Name lexing, terminated by end of line
mode MODE_NAME;

NM_COMMENTSTART     : F_LINECOMMENT -> type(COMMENT) ;

NM_LINESEP          : F_LINESEP -> type(LINESEP), popMode ;

NM_EMPTYLINE	    : F_EMPTYLINE -> type(EMPTYLINE), popMode ;

NM_INHERIT          : F_INHERIT -> type(INHERIT);
NM_CLIENT 		    : F_CLIENT -> type(CLIENT);

NM_ABBREVSTART      : F_ABBREVSTART -> type(ABBREVSTART);
NM_ABBREVEND        : F_ABBREVEND  -> type(ABBREVEND);

NM_QNAMESEP	        : F_QNAMESEP -> type(QNAMESEP);
NM_RELSEP	        : F_RELSEP -> type(RELSEP);

NM_WORD  	        : F_NAME_WORD   -> type(NMWORD) ;
NM_SPACE 	        : F_WHITESPACES -> type(SPACE) ;

//Paragraphs, terminated by empty lines
mode MODE_PARAGRAPH;

PAR_COMMENTSTART : F_LINECOMMENT -> type(COMMENT) ;

PAR_LINESEP    : F_LINESEP   -> type(LINESEP) ;

PAR_EMPTYLINE  : F_EMPTYLINE -> type(EMPTYLINE), pushMode(0) ;
//[Note 2] We `pushMode(0)` instead of `popMode` here to keep MODE_PARAGRAPH at
//         the top of the stack. By [Note 1], this ensures MODE_PARAGRAPH can
//         continue if the next line does not start with a keyword. This is used
//         for lexing component parts after an explanation.

PAR_COMMANDTERM    : F_COMMANDTERM    -> type(COMMANDTERM) ;
PAR_CONSTRAINTTERM : F_CONSTRAINTTERM -> type(CONSTRAINTTERM) ;
PAR_QUERYTERM      : F_QUERYTERM      -> type(QUERYTERM) ;

PAR_WORD       : F_SENT_WORD   -> type(SWORD) ;
PAR_SPACE      : F_WHITESPACES -> type(SPACE) ;

//The content of a specific event, requirement or scenario 
//(similar to MODE_PARAGRAPH)
//[Note 3] When exiting this mode, instead of just `popMode` we store
//         MODE_ENTRY, MODE_NAME at the top of the stack then `pushMode(0)`.
//         By [Note 1], this ensures MODE_NAME and then MODE_ENTRY can lex the next
//         entry, if there is one that does not start with a keyword.
mode MODE_ENTRY ;

EN_COMMENTSTART   : F_LINECOMMENT -> type(COMMENT) ;

EN_LINESEP        : F_LINESEP        -> type(LINESEP) ;

EN_EMPTYLINE      : F_EMPTYLINE      -> type(EMPTYLINE), pushMode(MODE_ENTRY), pushMode(MODE_NAME), pushMode(0) ;

EN_COMMANDTERM    : F_COMMANDTERM    -> type(COMMANDTERM) ;
EN_CONSTRAINTTERM : F_CONSTRAINTTERM -> type(CONSTRAINTTERM) ;
EN_QUERYTERM      : F_QUERYTERM      -> type(QUERYTERM) ;

EN_WORD           : F_SENT_WORD   -> type(SWORD) ;
EN_SPACE          : F_WHITESPACES -> type(SPACE) ;

//Index name lexing, terminated by index separator
// (Similar to MODE_NAME)
mode MODE_INAME;

INM_COMMENTSTART     : F_LINECOMMENT -> type(COMMENT) ;

INM_LINESEP          : F_LINESEP -> type(LINESEP);

INM_EMPTYLINE	     : F_EMPTYLINE -> type(EMPTYLINE);

INM_INDEXSEP	     : F_INDEXSEP -> type(INDEXSEP), popMode;

INM_INHERIT          : F_INHERIT -> type(INHERIT);
INM_CLIENT           : F_CLIENT -> type(CLIENT);

INM_ABBREVSTART         : F_ABBREVSTART -> type(ABBREVSTART);
INM_ABBREVEND           : F_ABBREVEND -> type(ABBREVEND);

INM_RELSEP	    : F_RELSEP -> type(RELSEP);

INM_WORD  	    : F_NAME_WORD   -> type(NMWORD) ;
INM_SPACE 	    : F_WHITESPACES -> type(SPACE) ;

//Indexing (similar to MODE_PARAGRAPH)
mode MODE_INDEXING ;

IND_COMMENTSTART : F_LINECOMMENT -> type(COMMENT) ;

IND_LINESEP      : F_LINESEP   -> type(LINESEP) ;

IND_EMPTYLINE    : F_EMPTYLINE -> type(EMPTYLINE), pushMode(0) ; //see [Note 2]

IND_INDSEP       : F_INDEXSEP -> type(INDEXSEP) ;

IND_WORD         : F_INDEX_WORD  -> type(IWORD) ;
IND_SPACE        : F_WHITESPACES -> type(SPACE) ;


