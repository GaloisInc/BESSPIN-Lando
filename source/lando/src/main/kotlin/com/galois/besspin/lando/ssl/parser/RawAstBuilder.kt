package com.galois.besspin.lando.ssl.parser

import com.galois.besspin.lando.ssl.ast.*
import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.ParserRuleContext


class RawAstBuilder(private val landoSourceCxt: SSLParser.LandoSourceContext) {

    private var nextUid = 1

    // private var lastSystem: RawSystem? = null
    // private var lastSubsystem: RawSubsystem? = null

    fun build(): RawSSL = toAst(landoSourceCxt)

    //    landoSource    : lineseps? specElement* lineseps? lineComments? lineseps? EOF;
    //    specElement    : system           #systemElement
    //                   | subsystem        #subsystemElement
    //                   | subsystemImport  #subsystemImportElement
    //                   | component        #componentElement
    //                   | componentImport  #componentImportElement
    //                   | events           #eventsElement
    //                   | scenarios        #scenariosElement
    //                   | requirements     #requirementsElement
    //                   | relation         #relationElement ;
    private fun toAst(cxt: SSLParser.LandoSourceContext): RawSSL {
        val body = toAst(cxt.body())
        val comments = toAst(cxt.lineComments())
        return RawSSL(body, comments)
    }

    //    system     : lineComments?
    //                 SYSTEM
    //                 sysname=name abbrev? comment? lineseps
    //                 paragraph lineComments?
    //                 (indexing blockend)?
    //                 (CONTAINS lineseps? body END comment? blockend)? ;
    private fun toAst(cxt: SSLParser.SystemContext): RawSystem {
        val name = toAst(cxt.sysname)
        val abbrevName = cxt.abbrev()?.let { toAst(it) }
        val description = toAst(cxt.paragraph())
        val index = toAst(cxt.indexing())
        val comments = collectComments(cxt.lineComments(), cxt.comment())
        val body = cxt.body()?.let { toAst(it) }

        // lastSystem = system

        return RawSystem(nextUid++,toPos(cxt),name, abbrevName, description, index, body, comments)
    }

    // body: specElement* ;
    private fun toAst(cxt: SSLParser.BodyContext): Body {
        val elements = cxt.specElement().map {
            when (it) {
                is SSLParser.SystemElementContext -> toAst(it.system())
                is SSLParser.SubsystemElementContext -> toAst(it.subsystem())
                is SSLParser.SubsystemImportElementContext -> toAst(it.subsystemImport())
                is SSLParser.ComponentElementContext -> toAst(it.component())
                is SSLParser.ComponentImportElementContext -> toAst(it.componentImport())
                is SSLParser.EventsElementContext -> toAst(it.events())
                is SSLParser.ScenariosElementContext -> toAst(it.scenarios())
                is SSLParser.RequirementsElementContext -> toAst(it.requirements())
                is SSLParser.RelationElementContext -> toAst(it.relation())
                else -> throw UnsupportedOperationException(it.toString())
            }
        }.toMutableList()

        // process element list to convert implicit bodies into explicit ones
        var openSystem : MutableList<RawElement>? = null
        var openSubsystem : MutableList<RawElement>? = null

        val iter = elements.iterator()
        while (iter.hasNext()) {
            when (val elem = iter.next()) {
               is RawSystem -> {
                   openSystem = null
                   openSubsystem = null
                   if (elem.body == null) {  // no explicit body; make an empty one
                       openSystem = mutableListOf()
                       elem.body = openSystem
                   }
               }
               is RawSubsystem -> {
                   openSubsystem = null
                   if (elem.body == null) {  // no explicit body; make an empty one
                       openSubsystem = mutableListOf()
                       elem.body = openSubsystem
                   }
                   if (openSystem != null) { // we're building a system body
                       openSystem.add(elem)
                       iter.remove()
                   }
               }
               else -> {
                   if (openSubsystem != null) { // we're building a subsystem body
                       openSubsystem.add(elem)
                       iter.remove()
                   } else if (openSystem != null) { // we're building a system body
                       openSystem.add(elem)
                       iter.remove()
                   }
               }
            }
        }
        return elements
    }

    //  subsystem  : lineComments?
    //               SUBSYSTEM
    //               subsysname=name abbrev?
    //               (inheritClause | clientClause)* comment? lineseps
    //               paragraph comment?
    //               (indexing blockend)?
    //               (CONTAINS lineseps? body END comment? blockend)? ;
    private fun toAst(cxt: SSLParser.SubsystemContext): RawSubsystem {
        val name = toAst(cxt.subsysname)
        val abbrevName = cxt.abbrev()?.let { toAst(it) }
        val description = toAst(cxt.paragraph())
        val index = toAst(cxt.indexing())
        val inherits = cxt.inheritClause().map { toAst(it) }.flatten()
        val clients = cxt.clientClause().map { toAst(it) }.flatten()
        val comments = collectComments(cxt.lineComments(), cxt.comment())
        val body = cxt.body()?.let { toAst(it) }

        // lastSubsystem = subsystem

        return RawSubsystem(nextUid++,toPos(cxt),name, abbrevName, inherits, clients, description, index, body, comments)
    }

    // subsystemImport  : lineComments? IMPORT spaces SUBSYSTEM
    //   	              subsysname=qname abbrev?
    //                    clientClause* comment? blockend ;
    private fun toAst(cxt: SSLParser.SubsystemImportContext): RawSubsystemImport {
        val name = toAst(cxt.subsysname)
        val abbrevName = cxt.abbrev()?.let { toAst(it) }
        val clients = cxt.clientClause().map { toAst(it) }.flatten()
        val comments = collectComments(cxt.lineComments(), cxt.comment())
        return RawSubsystemImport(nextUid++,toPos(cxt),name, abbrevName, clients, comments)
    }

    // component  : lineComments?
    //              COMPONENT
    //              compname=name abbrev?
    //              (inheritClause | clientClause)* comment? lineseps
    //	            paragraph
    //     	        (componentParts blockend)? ;
    private fun toAst(cxt: SSLParser.ComponentContext): RawComponent {
        val name = toAst(cxt.compname)
        val abbrevName = cxt.abbrev()?.let { toAst(it) }
        val inherits = cxt.inheritClause().map { toAst(it) }.flatten()
        val clients = cxt.clientClause().map { toAst(it) }.flatten()
        val description = toAst(cxt.paragraph())
        val parts = if (cxt.componentParts() != null) toAst(cxt.componentParts()) else arrayListOf()
        val comments = collectComments(cxt.lineComments(), cxt.comment())

        return RawComponent(nextUid++,toPos(cxt),name, abbrevName, inherits, clients, description, parts, comments)
    }

    // componentImport	: lineComments? IMPORT spaces COMPONENT
    //		              componentname=qname abbrev?
    //		              clientClause* comment? blockend ;
    private fun toAst(cxt: SSLParser.ComponentImportContext): RawComponentImport {
        val name = toAst(cxt.componentname)
        val abbrevName = cxt.abbrev()?.let { toAst(it) }
        val clients = cxt.clientClause().map { toAst(it) }.flatten()
        val comments = collectComments(cxt.lineComments(), cxt.comment())
        return RawComponentImport(nextUid++,toPos(cxt),name, abbrevName, clients, comments)
    }

    //  componentParts : componentPart (lineseps componentPart)* ;
    //  componentPart  : command          #commandPart
    //                 | constraint       #constraintPart
    //                 | query            #queryPart ;
    private fun toAst(cxt: SSLParser.ComponentPartsContext): List<RawComponentPart> {

        return cxt.componentPart().map {
            when (it) {
                is SSLParser.QueryPartContext -> toAst(it.query())
                is SSLParser.CommandPartContext -> toAst(it.command())
                is SSLParser.ConstraintPartContext -> toAst(it.constraint())
                else -> throw UnsupportedOperationException(it.toString())
            }
        }.toMutableList()
    }

    //  query           : lineComments? sentBody QUERYTERM      wordSep? comment? ;
    private fun toAst(cxt: SSLParser.QueryContext): RawQuery {
        val text = toAst(cxt.sentBody())
        val term = toAst(cxt.QUERYTERM())
        val comments = collectComments(cxt.lineComments(), cxt.comment())

        return RawQuery(toPos(cxt),text + term, comments)
    }

    //  command         : lineComments? sentBody COMMANDTERM    wordSep? comment?
    private fun toAst(cxt: SSLParser.CommandContext): RawCommand {
        val text = toAst(cxt.sentBody())
        val term = toAst(cxt.COMMANDTERM())
        val comments = collectComments(cxt.lineComments(), cxt.comment())

        return RawCommand(toPos(cxt),text + term, comments)
    }

    //  constraint      : lineComments? sentBody CONSTRAINTTERM wordSep? comment? ;
    private fun toAst(cxt: SSLParser.ConstraintContext): RawConstraint {
        val text = toAst(cxt.sentBody())
        val term = toAst(cxt.CONSTRAINTTERM())
        val comments = collectComments(cxt.lineComments(), cxt.comment())

        return RawConstraint(toPos(cxt),text + term, comments)
    }


    // events          : lineComments?
    //                  EVENTS
    //                  name comment?
    //                  lineseps eventEntry* ;
    private fun toAst(cxt: SSLParser.EventsContext): RawEvents {
        val name = toAst(cxt.name())
        val events = cxt.eventEntry().map { toAst(it) }
        val comments = collectComments(cxt.lineComments(), cxt.comment())

        return RawEvents(nextUid++,toPos(cxt),name, events, comments)
    }

    //  eventEntry      : lineComments? name nameComment=comment? lineseps paragraph ;
    private fun toAst(cxt: SSLParser.EventEntryContext): RawItem =
        RawItem(
            pos = toPos(cxt),
            id = toAst(cxt.name()),
            text = toAst(cxt.paragraph()),
            comments = collectComments(cxt.lineComments(), cxt.nameComment)
        )


    //  scenarios       : lineComments?
    //                    SCENARIOS
    //                    name comment?
    //                    lineseps scenarioEntry* ;
    private fun toAst(cxt: SSLParser.ScenariosContext): RawScenarios {
        val name = toAst(cxt.name())
        val scenarios = cxt.scenarioEntry().map { toAst(it) }
        val comments = collectComments(cxt.lineComments(), cxt.comment())

        return RawScenarios(nextUid++,toPos(cxt),name, scenarios, comments)
    }

    //  scenarioEntry   : lineComments? name nameComment=comment? lineseps paragraph ;
    private fun toAst(cxt: SSLParser.ScenarioEntryContext): RawItem =
        RawItem(
            pos = toPos(cxt),
            id = toAst(cxt.name()),
            text = toAst(cxt.paragraph()),
            comments = collectComments(cxt.lineComments(), cxt.nameComment)
        )


    //  requirements       : lineComments?
    //                       REQUIREMENTS
    //                       name comment?
    //                       lineseps requirementEntry* ;
    private fun toAst(cxt: SSLParser.RequirementsContext): RawRequirements {
        val name = toAst(cxt.name())
        val requirements = cxt.requirementEntry().map { toAst(it) }
        val comments = collectComments(cxt.lineComments(), cxt.comment())

        return RawRequirements(nextUid++,toPos(cxt),name, requirements, comments)
    }

    //  requirementEntry   : lineComments? name nameComment=comment? lineseps paragraph ;
    private fun toAst(cxt: SSLParser.RequirementEntryContext): RawItem =
        RawItem(
            pos = toPos(cxt),
            id = toAst(cxt.name()),
            text = toAst(cxt.paragraph()),
            comments = collectComments(cxt.lineComments(), cxt.nameComment)
        )


    //  relation          : lineComments? RELATION left=qname (inheritClause | clientClause)+ comment? blockend ;
    private fun toAst(cxt: SSLParser.RelationContext): RawRelation {
        val name = toAst(cxt.left)
        val inherits = cxt.inheritClause().map { toAst(it) }.flatten()
        val clients = cxt.clientClause().map { toAst(it) }.flatten()
        val comments = collectComments(cxt.lineComments(), cxt.comment())

        return RawRelation(nextUid++,toPos(cxt),name, inherits, clients, comments)
    }

    // clientClause : CLIENT qname lineseps? (RELSEP lineseps? qname lineseps?)* ;
    private fun toAst(cxt: SSLParser.ClientClauseContext): List<QName> =
        cxt.qname().map { toAst(it) }


    // inheritClause : INHERIT qname lineseps? (RELSEP lineseps? qname lineseps?)* ;
    private fun toAst(cxt: SSLParser.InheritClauseContext): List<QName> =
        cxt.qname().map { toAst(it) }


    //  indexing          : INDEXING spaces? (lineseps indexEntries)? ;
    private fun toAst(cxt: SSLParser.IndexingContext?): List<RawIndexEntry> =
        if (cxt != null) toAst(cxt.indexEntries()) else listOf()

    //  indexEntries      : indexEntry (lineseps indexEntry)* ;
    private fun toAst(cxt: SSLParser.IndexEntriesContext): List<RawIndexEntry> =
        cxt.indexEntry().map { toAst(it) }

    //  indexEntry        : indexValue INDEXSEP indexValueList ;
    private fun toAst(cxt: SSLParser.IndexEntryContext): RawIndexEntry {
        val key = toAst(cxt.indexValue())
        val (values, comments) = toAst(cxt.indexValueList())
        return RawIndexEntry(toPos(cxt),key, values, comments)
    }

    //  indexValueList    : indexValuePart (lineseps indexValuePart)* ;
    private fun toAst(cxt: SSLParser.IndexValueListContext): Pair<List<String>, List<RawComment>> {
        val (values, maybeComments) = cxt.indexValuePart().map { toAst(it) }.unzip()
        return Pair(values, maybeComments.filterNotNull())
    }

    //  indexValuePart    : indexValue comment? ;
    private fun toAst(cxt: SSLParser.IndexValuePartContext): Pair<String, RawComment?> {
        return Pair(toAst(cxt.indexValue()), toAst(cxt.comment()))
    }


    //  comment      : COMMENT ;
    private fun toAst(cxt: SSLParser.CommentContext?): RawComment? =
        cxt?.COMMENT()?.let{ RawComment(toPos(cxt),toAst(it).removePrefix("//").trimStart())}
        //if (cxt != null)
        //    RawComment(toAst(cxt.COMMENT()).removePrefix("//").trimStart())
        //else
        //    null

    //  comments     : comment (lineseps comment)* ;
    //  lineComments : comments lineseps ;
    private fun toAst(cxt: SSLParser.LineCommentsContext?): List<RawComment> =
        cxt?.comments()?.comment()?.mapNotNull { toAst(it) } ?: listOf()


    //  spaces     : SPACE+ ;
    private fun toAst(cxt: SSLParser.SpacesContext): String =
        cxt.text

    //  nameTrim   : NMWORD (spaces NMWORD)*;
    //  name       : spaces? nameTrim spaces? ;
    private fun toAst(cxt: SSLParser.NameContext): String {
        val words = cxt.nameTrim().NMWORD().map { toAst(it) }
        val spaces = cxt.nameTrim().spaces().map { toAst(it) } + listOf("")
        return (words zip spaces).joinToString("") { it.first + it.second }
    }

    // qname	   : name (QNAMESEP name)* ;
    private fun toAst(cxt: SSLParser.QnameContext): QName =
        cxt.name().map { toAst(it) }

    // ivTrim      : IWORD (spaces IWORD)* ;
    // indexValue  : spaces? ivTrim spaces?
    private fun toAst(cxt: SSLParser.IndexValueContext) : String {
        val words = cxt.ivTrim().IWORD().map { toAst(it) }
        val spaces = cxt.ivTrim().spaces().map {toAst(it) } + listOf("")
        return (words zip spaces).joinToString("") { it.first + it.second }
    }

    //  abbrev     : spaces? ABBREVSTART spaces? NMWORD spaces? ABBREVEND spaces? ;
    private fun toAst(cxt: SSLParser.AbbrevContext): String =
        toAst(cxt.NMWORD())

    //  wordSep    : spaces                            #wordSepSpaces
    //             | spaces? comment? LINESEP          #wordSepLinesep ;
    // NOTE: comments here are ignored
    private fun toAst(cxt: SSLParser.WordSepContext): String =
        when(cxt) {
            is SSLParser.WordSepSpacesContext -> toAst(cxt.spaces())
            is SSLParser.WordSepLinesepContext -> " "
            else -> throw UnsupportedOperationException(cxt.toString())
        }

    //  sentBody   : SWORD (wordSep SWORD)* wordSep? ;
    private fun toAst(cxt: SSLParser.SentBodyContext): String {
        val words = cxt.SWORD().map { toAst(it) }
        val seps = cxt.wordSep().map { toAst(it) } + listOf("")
        return (words zip seps).joinToString("") { it.first + it.second }
    }

    //  sentTerm   : COMMANDTERM     #commandTerm
    //             | CONSTRAINTTERM  #constraintTerm
    //             | QUERYTERM       #queryTerm ;
    private fun toAst(cxt: SSLParser.SentTermContext): String =
        when(cxt) {
            is SSLParser.CommandTermContext    -> toAst(cxt.COMMANDTERM())
            is SSLParser.ConstraintTermContext -> toAst(cxt.CONSTRAINTTERM())
            is SSLParser.QueryTermContext      -> toAst(cxt.QUERYTERM())
            else -> throw UnsupportedOperationException(cxt.toString())
        }

    //  sentence   : sentBody sentTerm wordSep?
    //             | sentBody          wordSep?
    private fun toAst(cxt: SSLParser.SentenceContext): String =
        // "(" + cxt.getStart().line.toString() + "@" + cxt.getStart().charPositionInLine.toString() + ")" +
        toAst(cxt.sentBody()) + (cxt.sentTerm()?.let { toAst(it) } ?: "")

    //  paragraph  : sentence+ parend;
    // parend 	   : comment? (EMPTYLINE lineseps? | EOF);
    // NOTE: comments here are ignored
    private fun toAst(cxt: SSLParser.ParagraphContext): String =
        cxt.sentence().joinToString(" ") { toAst(it) }.trimEnd()

    //  <all terminals>
    private fun toAst(term: TerminalNode): String =
        term.symbol.text.trim()


    private fun collectComments(
        lineCommentsCxt: SSLParser.LineCommentsContext?,
        commentCxt: SSLParser.CommentContext?
    ): List<RawComment> {
        val comments = toAst(lineCommentsCxt) + toAst(commentCxt)
        return comments.filterNotNull()
    }

    private fun collectComments(
        lineCommentsCxt: SSLParser.LineCommentsContext?,
        commentCxt1: SSLParser.CommentContext?,
        commentCxt2: SSLParser.CommentContext?
    ): List<RawComment> {
        val comments = toAst(lineCommentsCxt) + toAst(commentCxt1) + toAst(commentCxt2)
        return comments.filterNotNull()
    }

    private fun collectComments(
        lineCommentsCxt: SSLParser.LineCommentsContext?,
        commentCxts: List<SSLParser.CommentContext>
    ): List<RawComment> {
        val comments = toAst(lineCommentsCxt) + commentCxts.map { toAst(it) }
        return comments.filterNotNull()
    }
    private fun collectComments(
        lineCommentsCxts: List<SSLParser.LineCommentsContext>,
        commentCxts: List<SSLParser.CommentContext>
    ): List<RawComment> {
        val comments = lineCommentsCxts.map { toAst(it) }.flatten() + commentCxts.map { toAst(it) }
        return comments.filterNotNull()
    }

    private fun toPos(cxt: ParserRuleContext) : RawPos =
        RawPos(cxt.getStart().line,cxt.getStart().charPositionInLine)

    // private fun nextUid(): Int = lastUid++
}