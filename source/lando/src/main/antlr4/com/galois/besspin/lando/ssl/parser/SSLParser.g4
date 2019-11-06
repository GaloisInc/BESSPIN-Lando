parser grammar SSLParser;

//@header {
//    package com.galois.symmetries.compiler.frontend;
//}

options { tokenVocab = SSLLexer; }

ssl        : lineseps? element* lineseps? EOF;

element    : system           #systemElement
           | subsystem        #subsystemElement
           | component        #componentElement
           | events           #eventsElement
           | scenarios        #scenariosElement
           | requirements     #requirementsElement ;

system     : SYSTEM sysname=name (RELKEYWORD relname=name)? lineseps paragraph blockend ;

subsystem  : SUBSYSTEM subsysname=name (RELKEYWORD relname=name)? lineseps paragraph blockend ;



component       : COMPONENT compname=name (RELKEYWORD relname=name)? (lineseps componentParts)? blockend ;

componentParts : componentPart (lineseps componentPart)* ;

componentPart  : command          #commandPart
               | constraint       #constraintPart
               | query            #queryPart ;

command         : COMMAND ;

query           : QUERY ;

constraint      : CONSTRAINT ;


events          : EVENTS name (lineseps eventEntries)? blockend ;

eventEntries   : eventEntry (lineseps eventEntry)* ;

eventEntry     : name lineseps SENTENCE ;


scenarios          : SCENARIOS name (lineseps scenarioEntries)? blockend ;

scenarioEntries   : scenarioEntry (lineseps scenarioEntry)* ;

scenarioEntry     : name lineseps SENTENCE ;


requirements          : REQUIREMENTS name (lineseps requirementEntries)? blockend ;

requirementEntries   : requirementEntry (lineseps requirementEntry)* ;

requirementEntry     : name lineseps SENTENCE ;


//Helpers
name       : NAMECHAR+ ;

lineseps   : LINESEP+ ;

paragraph  : PARAGRAPH ;

blockend   : lineseps | EOF ;