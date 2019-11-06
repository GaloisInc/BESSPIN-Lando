parser grammar SSLParser;

//@header {
//    package com.galois.symmetries.compiler.frontend;
//}

options { tokenVocab = SSLLexer; }

ssl        : lineseps? element* lineseps? EOF;

element    : system | subsystem | component | events ;

system     : SYSTEM name (RELKEYWORD name)? lineseps paragraph blockend ;

subsystem  : SUBSYSTEM name (RELKEYWORD name)? lineseps paragraph blockend ;



component       : COMPONENT name (RELKEYWORD name)? (lineseps component_parts)? blockend ;

component_parts : component_part (lineseps component_part)* ;

component_part  : command | constraint | query ;

command         : COMMAND ;

query           : QUERY ;

constraint      : CONSTRAINT ;


events          : EVENTS name (lineseps event_entries)? blockend ;

event_entries   : event_entry (lineseps event_entry)* ;

event_entry     : name lineseps SENTENCE ;


scenarios          : SCENARIOS name (lineseps scenario_entries)? blockend ;

scenario_entries   : scenario_entry (lineseps scenario_entry)* ;

scenario_entry     : name lineseps SENTENCE ;


requirements          : REQUIREMENTS name (lineseps requirement_entries)? blockend ;

requirement_entries   : requirement_entry (lineseps requirement_entry)* ;

requirement_entry     : name lineseps SENTENCE ;


//Helpers
name       : NAMECHAR+ ;

lineseps   : LINESEP+ ;

paragraph  : PARAGRAPH ;

blockend   : lineseps | EOF ;