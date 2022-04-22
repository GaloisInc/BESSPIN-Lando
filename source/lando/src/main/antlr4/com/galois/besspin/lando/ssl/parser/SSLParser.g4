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

landoSource    : lineseps? body lineseps? lineComments? lineseps? EOF;

specElement    : system           #systemElement
               | subsystem        #subsystemElement
	           | subsystemImport  #subsystemImportElement
               | component        #componentElement
   	           | componentImport  #componentImportElement
               | events           #eventsElement
               | scenarios        #scenariosElement
               | requirements     #requirementsElement
               | relation         #relationElement ;

system     : lineComments?
             SYSTEM
             sysname=name abbrev? comment? lineseps
             paragraph lineComments?
             (indexing blockend)?
	         (CONTAINS lineseps? body END comment? blockend)? ;

body    :  specElement* ;

subsystem  : lineComments?
             SUBSYSTEM
             subsysname=name abbrev?
	     clientClause* comment? lineseps
	     paragraph lineComments?
	     (indexing blockend)?
	     (CONTAINS lineseps? body END comment? blockend)? ;

subsystemImport  : lineComments? IMPORT_SUBSYSTEM
   	           subsysname=qname abbrev?
                   clientClause* comment? blockend ;

component  : lineComments?
             COMPONENT
             compname=name abbrev?
             (inheritClause | clientClause)* comment? lineseps
	         paragraph
     	     (componentParts blockend)? ;

componentParts : componentPart (lineseps componentPart)* ;

componentPart  : command          #commandPart
               | constraint       #constraintPart
               | query            #queryPart ;

command         : lineComments? sentBody COMMANDTERM    wordSep? comment? ;

query           : lineComments? sentBody QUERYTERM      wordSep? comment? ;

constraint      : lineComments? sentBody CONSTRAINTTERM wordSep? comment? ;

componentImport	: lineComments? IMPORT_COMPONENT
		  componentname=qname abbrev?
		  clientClause* comment? blockend ;

clientClause : CLIENT qname lineseps? (RELSEP lineseps? qname lineseps?)* ;

inheritClause : INHERIT qname lineseps? (RELSEP lineseps? qname lineseps?)* ;

events          : lineComments?
                  EVENTS
                  name comment?
                  lineseps eventEntry* ;

eventEntry      : lineComments? name nameComment=comment? lineseps paragraph ;

scenarios       : lineComments?
                  SCENARIOS
                  name comment?
                  lineseps scenarioEntry* ;

scenarioEntry   : lineComments? name nameComment=comment? lineseps paragraph ;


requirements       : lineComments?
                     REQUIREMENTS
                     name comment?
                     lineseps requirementEntry* ;

requirementEntry   : lineComments? name nameComment=comment? lineseps paragraph ;

relation          : lineComments? RELATION left=qname (inheritClause | clientClause)+ comment? blockend ;

indexing          : INDEXING spaces? (lineseps indexEntries)? ;

indexEntries      : indexEntry (lineseps indexEntry)* ;

indexEntry        : indexValue INDEXSEP indexValueList ;

indexValueList    : indexValuePart (lineseps indexValuePart)* ;

indexValuePart    : indexValue lineseps? comment? ;


comment      : COMMENT ;

comments     : comment (lineseps comment)* ;

lineComments : comments lineseps ;


//Helpers
spaces     : SPACE+ ;

nameTrim   : NMWORD (spaces NMWORD)*;

name       : spaces? nameTrim spaces? ;

qname	   : name (QNAMESEP name)* ; // morally should be left recursive

abbrev     : spaces? ABBREVSTART spaces? NMWORD spaces? ABBREVEND spaces? ;

ivTrim     : IWORD (spaces IWORD)* ;

indexValue : spaces? ivTrim spaces? ;

wordSep    : spaces                   #wordSepSpaces
           | spaces? comment? LINESEP 	      #wordSepLinesep ;

sentBody   : SWORD (wordSep SWORD)* wordSep? ;

sentTerm   : COMMANDTERM     #commandTerm
           | CONSTRAINTTERM  #constraintTerm
           | QUERYTERM       #queryTerm ;

sentence   : sentBody sentTerm wordSep?
           | sentBody          wordSep?
             { emitWarning("forgotten '.', '!', or '?'"); } ;

paragraph  : sentence+ parend;

parend 	   : comment? (EMPTYLINE lineseps? | EOF);

lineseps   : (LINESEP | EMPTYLINE)+ ;

blockend   : lineseps | EOF ;