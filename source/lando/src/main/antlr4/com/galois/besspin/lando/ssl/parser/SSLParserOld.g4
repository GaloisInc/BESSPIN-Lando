parser grammar SSLParserOld;

//@header {
//    package com.galois.symmetries.compiler.frontend;
//}

options { tokenVocab = SSLLexerOld; }

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
             sysname=name (RELKEYWORD relname=name)? comment? lineseps
             paragraph
             (lineseps indexing)?
             blockend ;

subsystem  : lineComments?
             SUBSYSTEM
             subsysname=name ABBREV? (RELKEYWORD relname=name)? comment? lineseps
             paragraph
             (lineseps indexing)?
             blockend ;

component  : lineComments?
             COMPONENT
             compname=name ABBREV? (RELKEYWORD relname=name)? comment? lineseps
             paragraph
             (lineseps componentParts)?
             blockend ;

componentParts : componentPart (lineseps componentPart)* ;

componentPart  : command          #commandPart
               | constraint       #constraintPart
               | query            #queryPart ;

command         : lineComments? COMMAND comment? ;

query           : lineComments? QUERY comment? ;

constraint      : lineComments? CONSTRAINT comment? ;


events          : lineComments?
                  EVENTS
                  name comment?
                  (lineseps eventEntries)?
                  blockend ;

eventEntries    : eventEntry (lineseps eventEntry)* ;

eventEntry      : lineComments? name nameComment=comment? lineseps SENTENCE sentenceComment=comment? ;


scenarios       : lineComments?
                  SCENARIOS
                  name comment?
                  (lineseps scenarioEntries)?
                  blockend ;

scenarioEntries : scenarioEntry (lineseps scenarioEntry)* ;

scenarioEntry   : lineComments? name nameComment=comment? lineseps SENTENCE sentenceComment=comment? ;


requirements       : lineComments?
                     REQUIREMENTS
                     name comment?
                     (lineseps requirementEntries)?
                     blockend ;

requirementEntries : requirementEntry (lineseps requirementEntry)* ;

requirementEntry   : lineComments? name nameComment=comment? lineseps SENTENCE sentenceComment=comment? ;


relation          : lineComments? RELATION left=name (RELKEYWORD right=name)? comment? blockend ;


indexing          : INDEXING (lineseps indexEntries)? ;

indexEntries      : indexEntry (lineseps indexEntry)* ;

indexEntry        : indexKey INDEXSEP indexValueList ;

indexValueList    : indexValuePart (lineseps indexValuePart)* ;

indexString       :  INDEXCHAR+ ;

indexKey          : indexString ;

indexValuePart    : indexString comment? ;


comment      : COMMENT COMMENTCHAR* ;

comments     : comment (lineseps comment)* ;

lineComments : comments lineseps ;


//Helpers
name       : NAMECHAR+ ;

lineseps   : LINESEP+ ;

paragraph  : PARAGRAPH ;

blockend   : lineseps | EOF ;