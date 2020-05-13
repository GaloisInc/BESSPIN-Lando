parser grammar SSLParser;

//@header {
//    package com.galois.symmetries.compiler.frontend;
//}

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
             (lineseps indexing)?
             blockend ;

subsystem  : lineComments?
             SUBSYSTEM
             subsysname=name abbrev? (RELKEYWORD relname=name)? comment? lineseps
             paragraph
             (lineseps indexing)?
             blockend ;

component  : lineComments?
             COMPONENT
             compname=name abbrev? (RELKEYWORD relname=name)? comment? lineseps
             paragraph
             (lineseps componentParts)?
             blockend ;

componentParts : componentPart (lineseps componentPart)* ;

componentPart  : command          #commandPart
               | constraint       #constraintPart
               | query            #queryPart ;

command         : lineComments? sentenceBody COMMANDTERM comment? ;

query           : lineComments? sentenceBody QUERYTERM comment? ;

constraint      : lineComments? sentenceBody CONSTRAINTTERM comment? ;


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


relation          : lineComments? RELATION left=name (RELKEYWORD right=name)? comment? blockend ;


indexing          : INDEXING (lineseps indexEntries)? ;

indexEntries      : indexEntry (lineseps indexEntry)* ;

indexEntry        : name INDEXSEP indexValueList ;

indexValueList    : indexValuePart (lineseps indexValuePart)* ;

indexValuePart    : name comment? ;


comment      : COMMENTSTART COMMENT ;

comments     : comment (lineseps comment)* ;

lineComments : comments lineseps ;


//Helpers
spaces     : SPACE+ ;

wordSep    : spaces                   #wordSepSpaces
           | spaces? LINESEP spaces?  #wordSepLinesep ;

words      : WORD (wordSep WORD)* ;

name       : WORD (spaces WORD)* ;

abbrev     : ABBREVSTART name ABBREVEND ;

lineseps   : (LINESEP | EMPTYLINE)+ ;

sentTerm   : COMMANDTERM     #commandTerm
           | CONSTRAINTTERM  #constraintTerm
           | QUERYTERM       #queryTerm ;

sentenceBody : spaces? words wordSep? ;

sentence   : sentenceBody sentTerm ;

paragraph  : sentence (LINESEP? sentence)* ;

blockend   : lineseps | EOF ;