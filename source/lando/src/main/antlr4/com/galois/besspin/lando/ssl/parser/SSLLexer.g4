lexer grammar SSLLexer;

//@lexer::members {
//    java.util.List tokens = new java.util.LinkedList();
//
//    public void emit(Token token) {
//            tokens.add(token);
//            super.emit(token);
//    }
//
//    public Token nextToken() {
//            super.nextToken();
//            return (Token)tokens.remove(0);
//    }
//
//    public Token getToken() {
//        return (Token)tokens.remove(0);
//    }
//}

@members {
    public Token nextToken() {
        while (true) {
            int mark = _input.mark();
            try {
                int startIndex = _input.index();
                Token token = super.nextToken();
                if (token != null && token.getType() == SPECIAL_REWIND) {
                    _input.seek(startIndex);
                    continue;
                }

                return token;
            }
            finally {
                _input.release(mark);
            }
        }
    }
}

channels {
    COMMENTS
}

tokens {
    SPECIAL_REWIND,
    LINESEP,
    NAME, NAMECHAR,
    SENTENCE
}

//Common Fragments
fragment F_LINESEP      : ('\r'? '\n' | '\r') ;
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
fragment F_ALL_KEYWORDS : F_SYSTEM | F_SUBSYSTEM | F_COMPONENT | F_EVENTS | F_SCENARIOS | F_REQUIREMENTS | F_RELATION ;

fragment F_KEYWORD_SEP  : F_WHITESPACE | F_LINESEP ;

fragment F_RELKEYWORD   : 'inherit' | 'client' | 'contains' ;

fragment F_INDEXING     : 'indexing' ;
fragment F_DICTSEP      : ':' ;

fragment F_SENTENCESTART : [a-zA-Z0-9] ; // Sentences currently start with an alpha numeric character. It used to be (~ [ \t\r\n/]) ;
fragment F_SENTENCECHAR  : ~ [.!?] ;
fragment F_SENTENCEEND   : [.!?] ;
fragment F_SENTENCE      : F_SENTENCESTART F_SENTENCECHAR*? F_SENTENCEEND  ;

fragment F_COMMAND      : F_SENTENCESTART F_SENTENCECHAR* [!] ;
fragment F_CONSTRAINT   : F_SENTENCESTART F_SENTENCECHAR* [.] ;
fragment F_QUERY        : F_SENTENCESTART F_SENTENCECHAR* [?] ;

fragment F_LINECOMMENTSTART : '//' ;

//Top-level (default) lexer

WS           : F_WHITESPACES -> skip ;

LINESEP      : F_LINESEP ;

SYSTEM       : F_SYSTEM -> pushMode(MODE_PARAGRAPH), pushMode(MODE_NAMEPHRASEREL) ;

SUBSYSTEM    : F_SUBSYSTEM -> pushMode(MODE_PARAGRAPH), pushMode(MODE_NAMEPHRASEREL) ;

COMPONENT    : F_COMPONENT -> pushMode(MODE_COMPONENT_PARTS), pushMode(MODE_NAMEPHRASEREL) ;

EVENTS       : F_EVENTS -> pushMode(MODE_IDENT_LINE), pushMode(MODE_EMPTY_LINE), pushMode(MODE_NAMEPHRASE) ;

SCENARIOS    : F_SCENARIOS -> pushMode(MODE_IDENT_LINE), pushMode(MODE_EMPTY_LINE), pushMode(MODE_NAMEPHRASE) ;

REQUIREMENTS : F_REQUIREMENTS -> pushMode(MODE_IDENT_LINE), pushMode(MODE_EMPTY_LINE), pushMode(MODE_NAMEPHRASE) ;

RELATION     : F_RELATION -> pushMode(MODE_NAMEPHRASEREL) ;

INDEXING     : F_INDEXING -> pushMode(MODE_INDEXING) ;

COMMENT      : F_LINECOMMENTSTART -> pushMode(MODE_COMMENTS) ;


//Name-phrase-rel lexing
//Essentially: Word characters _including_ spaces. Terminated by either a relation key word or end of line
mode MODE_NAMEPHRASEREL;

NMR_LINESEP  : F_LINESEP -> type(LINESEP), popMode ;

RELKEYWORD   : F_WHITESPACES F_RELKEYWORD F_WHITESPACES -> popMode, pushMode(MODE_NAMEPHRASE) ;

NMR_NAMECHAR : . -> type(NAMECHAR) ;

NMR_COMMENT  : F_LINECOMMENTSTART -> type(COMMENT), pushMode(MODE_COMMENTS) ;


//Name-phrase lexing
//Essentially: Word characters _including_ spaces. Terminated by an end of line
mode MODE_NAMEPHRASE;

NM_LINESEP   : F_LINESEP -> type(LINESEP), popMode ;

NM_NAMECHAR  : . -> type(NAMECHAR) ;

NM_COMMENT   : F_LINECOMMENTSTART -> type(COMMENT), pushMode(MODE_COMMENTS) ;


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

CP_ALL_KEYWORDS : F_ALL_KEYWORDS F_KEYWORD_SEP (F_SENTENCECHAR+ F_SENTENCEEND)? -> popMode, type(SPECIAL_REWIND) ;

CONSTRAINT      : F_CONSTRAINT ;

COMMAND         : F_COMMAND ;

QUERY           : F_QUERY ;

CP_COMMENT      : F_LINECOMMENTSTART -> type(COMMENT), pushMode(MODE_COMMENTS) ;

//The start line of a specific event, requirement or scenario
mode MODE_IDENT_LINE ;

IL_WHITESPACES : F_WHITESPACES -> skip ;

IL_LINESEP     : F_LINESEP -> type(LINESEP), popMode, pushMode(MODE_SINGLE_SENTENCE) ;

IL_NAMECHAR    : . -> type(NAMECHAR) ;

IL_COMMENT     : F_LINECOMMENTSTART -> type(COMMENT), pushMode(MODE_COMMENTS) ;

//The content of a specific event, requirement or scenario
mode MODE_SINGLE_SENTENCE ;

SS_WHITESPACES  : F_WHITESPACES -> skip ;

SS_LINESEP      : F_LINESEP -> type(LINESEP), popMode, pushMode(MODE_IDENT_LINE), pushMode(MODE_EMPTY_LINE) ;

SS_ALL_KEYWORDS : F_ALL_KEYWORDS F_KEYWORD_SEP (F_SENTENCECHAR+ F_SENTENCEEND)? -> popMode, type(SPECIAL_REWIND) ;

SS_SENTENCE     : F_SENTENCE -> type(SENTENCE) ;

SS_COMMENT      : F_LINECOMMENTSTART -> type(COMMENT), pushMode(MODE_COMMENTS) ;


mode MODE_EMPTY_LINE ;

ELS_EMPTY_LINE  : F_WHITESPACE* F_LINESEP -> type(LINESEP) ;

ELS_ANYCHAR     : . -> type(SPECIAL_REWIND), popMode ;

ELS_COMMENT      : F_LINECOMMENTSTART -> type(COMMENT), pushMode(MODE_COMMENTS) ;


mode MODE_INDEXING ;

INDEXSEP         : F_DICTSEP ;

IND_LINESEP      : F_LINESEP -> type(LINESEP) ;

IND_EMPTYLINE    : F_EMPTYLINE -> type(LINESEP) ;

IND_ALL_KEYWORDS : F_ALL_KEYWORDS F_KEYWORD_SEP (F_SENTENCECHAR+ F_SENTENCEEND)? -> popMode, type(SPECIAL_REWIND) ;

INDEXCHAR        : . ;

IND_COMMENT      : F_LINECOMMENTSTART -> type(COMMENT), pushMode(MODE_COMMENTS) ;


mode MODE_COMMENTS ;

CMT_LINESEP      : F_LINESEP -> popMode, type(SPECIAL_REWIND) ;

COMMENTCHAR      : . ;