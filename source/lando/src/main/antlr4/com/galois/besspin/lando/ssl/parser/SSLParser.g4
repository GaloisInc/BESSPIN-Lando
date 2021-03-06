parser grammar SSLParser;

//@header {
//    package com.galois.symmetries.compiler.frontend;
//}

@members {
    public static final String warningPrefix = "Warning: ";

    public void emitWarning(String msg) {
        notifyErrorListeners(warningPrefix + msg);
    }
}

options { tokenVocab = SSLLexer; }

landoSource    : lineseps? specElement* lineseps? lineComments? lineseps? EOF;

specElement    : system           #systemElement
               | subsystem        #subsystemElement
               | component        #componentElement
               | events           #eventsElement
               | scenarios        #scenariosElement
               | requirements     #requirementsElement
               | relation         #relationElement ;

system     : lineComments?
             SYSTEM
             sysname=name abbrev? (RELKEYWORD relname=name)? comment? lineseps
             paragraph
             (indexing blockend)?
	     ; 

subsystem  : lineComments?
             SUBSYSTEM
             subsysname=name abbrev? (RELKEYWORD relname=name)? comment? lineseps
             paragraph
	     (indexing blockend)?
             ;	

component  : lineComments?
             COMPONENT
             compname=name abbrev? (RELKEYWORD relname=name)? comment? lineseps
             paragraph
             (componentParts blockend)?
             ;

componentParts : componentPart (lineseps componentPart)* ;

componentPart  : command          #commandPart
               | constraint       #constraintPart
               | query            #queryPart ;

command         : lineComments? sentBody COMMANDTERM    wordSep? comment? ;

query           : lineComments? sentBody QUERYTERM      wordSep? comment? ;

constraint      : lineComments? sentBody CONSTRAINTTERM wordSep? comment? ;


events          : lineComments?
                  EVENTS
                  name comment?
                  (lineseps eventEntries)?
                  blockend ;

eventEntries    : eventEntry (lineseps eventEntry)* ;

eventEntry      : lineComments? name nameComment=comment? lineseps sentence sentenceComment=comment? ;


scenarios       : lineComments?
                  SCENARIOS
                  name comment?
                  (lineseps scenarioEntries)?
                  blockend ;

scenarioEntries : scenarioEntry (lineseps scenarioEntry)* ;

scenarioEntry   : lineComments? name nameComment=comment? lineseps sentence sentenceComment=comment? ;


requirements       : lineComments?
                     REQUIREMENTS
                     name comment?
                     (lineseps requirementEntries)?
                     blockend ;

requirementEntries : requirementEntry (lineseps requirementEntry)* ;

requirementEntry   : lineComments? name nameComment=comment? lineseps sentence sentenceComment=comment? ;


relation          : lineComments? RELATION left=name RELKEYWORD right=name comment? blockend ;


indexing          : INDEXING spaces? (lineseps indexEntries)? ;

indexEntries      : indexEntry (lineseps indexEntry)* ;

indexEntry        : name INDEXSEP indexValueList ;

indexValueList    : indexValuePart (lineseps indexValuePart)* ;

indexValuePart    : name lineseps? comment? ;


comment      : COMMENT ;

comments     : comment (lineseps comment)* ;

lineComments : comments lineseps ;


//Helpers
spaces     : SPACE+ ;

nameTrim   : WORD (spaces WORD)*;

name       : spaces? nameTrim spaces? ;

abbrev     : spaces? ABBREVSTART spaces? WORD spaces? ABBREVEND spaces? ;

wordSep    : spaces                   #wordSepSpaces
           | spaces? LINESEP 	      #wordSepLinesep ; 

sentBody   : WORD (wordSep WORD)* wordSep? ;

sentTerm   : COMMANDTERM     #commandTerm
           | CONSTRAINTTERM  #constraintTerm
           | QUERYTERM       #queryTerm ;

sentence   : sentBody sentTerm wordSep?
           | sentBody          wordSep?
             { emitWarning("forgotten '.', '!', or '?'"); } ;

paragraph  : sentence+ parend;

parend 	   : EMPTYLINE lineseps? | EOF;

lineseps   : (LINESEP | EMPTYLINE)+ ;

blockend   : lineseps | EOF ;