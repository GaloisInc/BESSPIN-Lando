lexer grammar SSLLexer;

//Common Fragments
fragment LINESEPS     : ('\r'? '\n' | '\r')+ ;
fragment WHITESPACE   : (' ' | '\t') ;

fragment SYSTEM       : 'system' ;
fragment SUBSYSTEM    : 'subsystem' ;
fragment COMPONENT    : 'component' ;
fragment EVENT        : 'event' ;
fragment SCENARIO     : 'scenario' ;
fragment REQUIREMENT  : 'requirement' ;


//Top-level (default) lexer



