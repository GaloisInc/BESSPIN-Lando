package com.galois.besspin.lando.ssl.parser

import com.galois.besspin.lando.ssl.ast.*
import org.antlr.v4.runtime.tree.TerminalNode

class RawAstBuilder(var landoSourceContext: SSLParser.LandoSourceContext) {

    private var inheritRelations: MutableList<RawInheritRelation> = arrayListOf()
    private var containsRelations: MutableList<RawContainsRelation> = arrayListOf()
    private var clientRelations: MutableList<RawClientRelation> = arrayListOf()

    private var lastUid = 1

    private var lastSystem: RawSystem? = null
    private var lastSubsystem: RawSubsystem? = null

    fun build(): RawSSL = toAst(landoSourceContext)

//  landoSource    : lineseps? specElement* lineseps? lineComments? lineseps? EOF
//  specElement    : system           #systemElement
//                 | subsystem        #subsystemElement
//                 | component        #componentElement
//                 | events           #eventsElement
//                 | scenarios        #scenariosElement
//                 | requirements     #requirementsElement
//                 | relation         #relationElement ;
    private fun toAst(landoSourceContext: SSLParser.LandoSourceContext): RawSSL {
        val (elements, relationsListOfLists) = landoSourceContext.specElement().map {
            when(it) {
                is SSLParser.SystemElementContext -> toAst(it.system())
                is SSLParser.SubsystemElementContext -> toAst(it.subsystem())
                is SSLParser.ComponentElementContext -> toAst(it.component())
                is SSLParser.EventsElementContext -> toAst(it.events())
                is SSLParser.ScenariosElementContext -> toAst(it.scenarios())
                is SSLParser.RequirementsElementContext -> toAst(it.requirements())
                is SSLParser.RelationElementContext -> Pair(null, toAst(it.relation()))
                else -> throw UnsupportedOperationException(it.toString())
            }
        }.unzip()

        val relationship = RawRelationships.fromRelationList(relationsListOfLists.flatten())
        return RawSSL(nextUid(), elements.filterNotNull(), relationship, listOf());
    }

//  system     : lineComments?
//               SYSTEM
//               sysname=name abbrev? (RELKEYWORD relname=name)? comment? lineseps
//               paragraph
//               (lineseps indexing)?
//               blockend ;
    private fun toAst(sysCxt: SSLParser.SystemContext): Pair<RawSystem, List<RawRelation>> {
        val name = toAst(sysCxt.sysname)
        val abbrevName = sysCxt.abbrev()?.let { toAst(it) }
        val description = toAst(sysCxt.paragraph())
        val index = toAst(sysCxt.indexing())
        val comments = collectComments(sysCxt.lineComments(), sysCxt.comment())
        val system = RawSystem(nextUid(), name, abbrevName, description, index, comments)

        val reltype = sysCxt.RELKEYWORD()?.let { toAst(it) }
        val relname = sysCxt.relname?.let { toAst(it) }
        val relation = createRelation(reltype, name, relname)

        lastSystem = system

        return Pair(system, if (relation != null) listOf(relation) else listOf())
    }

//  subsystem  : lineComments?
//               SUBSYSTEM
//               subsysname=name abbrev? (RELKEYWORD relname=name)? comment? lineseps
//               paragraph
//               (lineseps indexing)?
//               blockend ;
    private fun toAst(subsysCxt: SSLParser.SubsystemContext): Pair<RawSubsystem, List<RawRelation>> {
        val name = toAst(subsysCxt.subsysname)
        val abbrevName = subsysCxt.abbrev()?.let { toAst(it) }
        val description = toAst(subsysCxt.paragraph())
        val index = toAst(subsysCxt.indexing())
        val comments = collectComments(subsysCxt.lineComments(), subsysCxt.comment())
        val subsystem = RawSubsystem(nextUid(), name, abbrevName, description, index, comments)

        val reltype = subsysCxt.RELKEYWORD()?.let { toAst(it) }
        val relname = subsysCxt.relname?.let { toAst(it) }
        val relation = createRelation(reltype, name, relname)

        lastSubsystem = subsystem

        val possibleRelations: MutableList<RawRelation?> = arrayListOf(relation)
        if (lastSystem != null) {
            possibleRelations.add(createImplicitContainsRelation(subsystem.uid, lastSystem!!.uid))
        }

        return Pair(subsystem, possibleRelations.filterNotNull())
    }

//  component  : lineComments?
//               COMPONENT
//               compname=name abbrev? (RELKEYWORD relname=name)? comment? lineseps
//               paragraph
//               (lineseps componentParts)?
//               blockend ;
    private fun toAst(cmpCxt: SSLParser.ComponentContext): Pair<RawComponent, List<RawRelation>> {
        val name = toAst(cmpCxt.compname)
        val abbrevName = cmpCxt.abbrev()?.let { toAst(it) }
        val description = toAst(cmpCxt.paragraph())
        val parts = if (cmpCxt.componentParts() != null) toAst(cmpCxt.componentParts()) else arrayListOf()
        val comments = collectComments(cmpCxt.lineComments(), cmpCxt.comment())
        val component = RawComponent(nextUid(), name, abbrevName, description, parts, comments)

        val reltype = cmpCxt.RELKEYWORD()?.let { toAst(it) }
        val relname = cmpCxt.relname?.let { toAst(it) }
        val relation = createRelation(reltype, name, relname)

        val possibleRelations: MutableList<RawRelation?> = arrayListOf(relation)
        if (lastSubsystem != null) {
            possibleRelations.add(createImplicitContainsRelation(component.uid, lastSubsystem!!.uid))
        }

        return Pair(component, possibleRelations.filterNotNull())
    }

//  componentParts : componentPart (lineseps componentPart)* ;
//  componentPart  : command          #commandPart
//                 | constraint       #constraintPart
//                 | query            #queryPart ;
    private fun toAst(cmpPartCxt: SSLParser.ComponentPartsContext): List<RawComponentPart> {
        val componentParts = cmpPartCxt.componentPart().map {
            when (it) {
                is SSLParser.QueryPartContext -> toAst(it.query())
                is SSLParser.CommandPartContext -> toAst(it.command())
                is SSLParser.ConstraintPartContext -> toAst(it.constraint())
                else -> throw UnsupportedOperationException(it.toString())
            }
        }.toMutableList()

        return componentParts
    }

//  query           : lineComments? words QUERYTERM      wordSep? comment? ;
    private fun toAst(queryCxt: SSLParser.QueryContext): RawQuery {
        val text = toAst(queryCxt.words())
        val term = toAst(queryCxt.QUERYTERM())
        val comments = collectComments(queryCxt.lineComments(), queryCxt.comment())

        return RawQuery(text + term, comments)
    }

//  command         : lineComments? words COMMANDTERM    wordSep? comment?
    private fun toAst(cmdCxt: SSLParser.CommandContext): RawCommand {
        val text = toAst(cmdCxt.words())
        val term = toAst(cmdCxt.COMMANDTERM())
        val comments = collectComments(cmdCxt.lineComments(), cmdCxt.comment())

        return RawCommand(text + term, comments)
    }

//  constraint      : lineComments? words CONSTRAINTTERM wordSep? comment? ;
    private fun toAst(constraintCxt: SSLParser.ConstraintContext): RawConstraint {
        val text = toAst(constraintCxt.words())
        val term = toAst(constraintCxt.CONSTRAINTTERM())
        val comments = collectComments(constraintCxt.lineComments(), constraintCxt.comment())

        return RawConstraint(text + term, comments)
}


//  events          : lineComments?
//                    EVENTS
//                    name comment?
//                    (lineseps eventEntries)?
//                    blockend ;
    private fun toAst(eventsCxt: SSLParser.EventsContext): Pair<RawEvents, List<RawRelation>> {
        val name = toAst(eventsCxt.name())
        val eventList = if (eventsCxt.eventEntries() != null) toAst(eventsCxt.eventEntries()) else arrayListOf()
        val comments = collectComments(eventsCxt.lineComments(), eventsCxt.comment())
        val events = RawEvents(nextUid(), name, eventList, comments)

        val possibleRelations = mutableListOf<RawRelation?>()
        if (lastSubsystem != null) {
            possibleRelations.add(createImplicitContainsRelation(events.uid, lastSubsystem!!.uid))
        }

        return Pair(events, possibleRelations.filterNotNull())
    }

//  eventEntries    : eventEntry (lineseps eventEntry)* ;
    private fun toAst(eventEntriesCxt: SSLParser.EventEntriesContext): List<RawEvent> =
        eventEntriesCxt.eventEntry().map { toAst(it) }

//  eventEntry      : lineComments? name nameComment=comment? lineseps sentence sentenceComment=comment? ;
    private fun toAst(eventCxt: SSLParser.EventEntryContext): RawEvent =
        RawEvent(
            id = toAst(eventCxt.name()),
            text = toAst(eventCxt.sentence()),
            comments = collectComments(eventCxt.lineComments(), eventCxt.nameComment, eventCxt.sentenceComment)
        )


//  scenarios       : lineComments?
//                    SCENARIOS
//                    name comment?
//                    (lineseps scenarioEntries)?
//                    blockend ;
    private fun toAst(scenariosCxt: SSLParser.ScenariosContext): Pair<RawScenarios, List<RawRelation>> {
        val name = toAst(scenariosCxt.name())
        val scenarioList = if (scenariosCxt.scenarioEntries() != null) toAst(scenariosCxt.scenarioEntries()) else arrayListOf()
        val comments = arrayListOf<RawComment>()
        val scenarios = RawScenarios(nextUid(), name, scenarioList, comments)

        val possibleRelations = mutableListOf<RawRelation?>()
        if (lastSubsystem != null) {
            possibleRelations.add(createImplicitContainsRelation(scenarios.uid, lastSubsystem!!.uid))
        }

        return Pair(scenarios, possibleRelations.filterNotNull())
    }

//  scenarioEntries : scenarioEntry (lineseps scenarioEntry)* ;
    private fun toAst(scenarioEntriesCxt: SSLParser.ScenarioEntriesContext): List<RawScenario> =
        scenarioEntriesCxt.scenarioEntry().map { toAst(it) }

//  scenarioEntry   : lineComments? name nameComment=comment? lineseps sentence sentenceComment=comment? ;
    private fun toAst(scenarioCxt: SSLParser.ScenarioEntryContext): RawScenario =
        RawScenario(
            id = toAst(scenarioCxt.name()),
            text = toAst(scenarioCxt.sentence()),
            comments = collectComments(scenarioCxt.lineComments(), scenarioCxt.nameComment, scenarioCxt.sentenceComment)
        )


//  requirements       : lineComments?
//                       REQUIREMENTS
//                       name comment?
//                       (lineseps requirementEntries)?
//                       blockend ;
    private fun toAst(requirementsCxt: SSLParser.RequirementsContext): Pair<RawRequirements, List<RawRelation>> {
        val name = toAst(requirementsCxt.name())
        val requirementList = if (requirementsCxt.requirementEntries() != null) toAst(requirementsCxt.requirementEntries()) else arrayListOf()
        val comments = arrayListOf<RawComment>()
        val requirements = RawRequirements(nextUid(), name, requirementList, comments)

        val possibleRelations = mutableListOf<RawRelation?>()
        if (lastSubsystem != null) {
            possibleRelations.add(createImplicitContainsRelation(requirements.uid, lastSubsystem!!.uid))
        }

        return Pair(requirements, possibleRelations.filterNotNull())
    }

//  requirementEntries : requirementEntry (lineseps requirementEntry)* ;
    private fun toAst(requirementEntriesCxt: SSLParser.RequirementEntriesContext): List<RawRequirement> =
        requirementEntriesCxt.requirementEntry().map { toAst(it) }

//  requirementEntry   : lineComments? name nameComment=comment? lineseps sentence sentenceComment=comment? ;
    private fun toAst(requirementCxt: SSLParser.RequirementEntryContext): RawRequirement =
        RawRequirement(
            id = toAst(requirementCxt.name()),
            text = toAst(requirementCxt.sentence()),
            comments = collectComments(requirementCxt.lineComments(), requirementCxt.nameComment, requirementCxt.sentenceComment)
        )


//  relation          : lineComments? RELATION left=name (RELKEYWORD right=name)? comment? blockend ;
    private fun toAst(relationCxt: SSLParser.RelationContext): List<RawRelation> =
        arrayListOf(
            createRelation(toAst(relationCxt.RELKEYWORD()), toAst(relationCxt.left), toAst(relationCxt.right))
        ).filterNotNull()


//  indexing          : INDEXING (lineseps indexEntries)? ;
    private fun toAst(indexCxt: SSLParser.IndexingContext?): List<RawIndexEntry> =
        if (indexCxt != null) toAst(indexCxt.indexEntries()) else listOf()

//  indexEntries      : indexEntry (lineseps indexEntry)* ;
    private fun toAst(indexEntriesCxt: SSLParser.IndexEntriesContext): List<RawIndexEntry> =
        indexEntriesCxt.indexEntry().map { toAst(it) }

//  indexEntry        : name INDEXSEP indexValueList ;
    private fun toAst(indexEntryCxt: SSLParser.IndexEntryContext): RawIndexEntry {
        val key = toAst(indexEntryCxt.name())
        val (values, comments) = toAst(indexEntryCxt.indexValueList())
        return RawIndexEntry(key, values, comments)
    }

//  indexValueList    : indexValuePart (lineseps indexValuePart)* ;
    private fun toAst(indexValueCxt: SSLParser.IndexValueListContext): Pair<List<String>, List<RawComment>> {
        val (values, maybeComments) = indexValueCxt.indexValuePart().map { toAst(it) }.unzip()
        return Pair(values, maybeComments.filterNotNull())
    }

//  indexValuePart    : name comment? ;
    private fun toAst(indexValuePartCxt: SSLParser.IndexValuePartContext): Pair<String, RawComment?> {
        return Pair(toAst(indexValuePartCxt.name()), toAst(indexValuePartCxt.comment()))
    }


//  comment      : COMMENTSTART COMMENT ;
    private fun toAst(commentCxt: SSLParser.CommentContext?): RawComment? =
        if (commentCxt != null) RawComment(toAst(commentCxt.COMMENT())) else null

//  comments     : comment (lineseps comment)* ;
//  lineComments : comments lineseps ;
    private fun toAst(lineCommentCxt: SSLParser.LineCommentsContext?): List<RawComment> {
        if (lineCommentCxt != null) {
            return lineCommentCxt.comments().comment().map { toAst(it) }.filterNotNull()
        } else {
            return listOf()
        }
    }


//  spaces     : SPACE+ ;
    private fun toAst(spacesCxt: SSLParser.SpacesContext): String =
            (spacesCxt.SPACE().map { it.symbol.text }).joinToString("")

//  name       : WORD (spaces WORD)* ;
    private fun toAst(nameCxt: SSLParser.NameContext): String {
        val words = nameCxt.WORD().map { toAst(it) }
        val seps = nameCxt.spaces().map { toAst(it) } + listOf("")
        return ((words zip seps).map { it.first + it.second }).joinToString("")
    }

//  abbrev     : ABBREVSTART name ABBREVEND ;
    private fun toAst(abbrevCxt: SSLParser.AbbrevContext): String =
            toAst(abbrevCxt.name())

//  wordSep    : spaces                   #wordSepSpaces
//             | spaces? LINESEP spaces?  #wordSepLinesep ;
    private fun toAst(wordSepCxt: SSLParser.WordSepContext): String =
        when(wordSepCxt) {
            is SSLParser.WordSepSpacesContext -> toAst(wordSepCxt.spaces())
            is SSLParser.WordSepLinesepContext -> " "
            else -> throw UnsupportedOperationException(wordSepCxt.toString())
        }

//  words      : WORD (wordSep WORD)* wordSep? ;
    private fun toAst(wordsCxt: SSLParser.WordsContext): String {
        val words = wordsCxt.WORD().map { toAst(it) }
        val seps = wordsCxt.wordSep().map { toAst(it) } + listOf("")
        return ((words zip seps).map { it.first + it.second }).joinToString("")
    }

//  sentTerm   : COMMANDTERM     #commandTerm
//             | CONSTRAINTTERM  #constraintTerm
//             | QUERYTERM       #queryTerm ;
    private fun toAst(sentTermCxt: SSLParser.SentTermContext): String =
        when(sentTermCxt) {
            is SSLParser.CommandTermContext    -> toAst(sentTermCxt.COMMANDTERM())
            is SSLParser.ConstraintTermContext -> toAst(sentTermCxt.CONSTRAINTTERM())
            is SSLParser.QueryTermContext      -> toAst(sentTermCxt.QUERYTERM())
            else -> throw UnsupportedOperationException(sentTermCxt.toString())
        }

//  sentence   : words sentTerm wordSep? ;
    private fun toAst(sentenceCxt: SSLParser.SentenceContext): String =
        toAst(sentenceCxt.words()) + toAst(sentenceCxt.sentTerm())

//  paragraph  : sentence+ ;
    private fun toAst(paragraphCxt: SSLParser.ParagraphContext): String =
        (paragraphCxt.sentence().map { toAst(it) }).joinToString(" ")

//  <all terminals>
    private fun toAst(term: TerminalNode): String =
        term.symbol.text.trim()


    private fun createRelation(reltype: String?, left: String, right: String?): RawRelation? {
        return when(reltype) {
            "inherit" -> RawInheritRelation(left, right!!)
            "contains" -> RawContainsRelation(right!!, left)
            "client" -> RawClientRelation(left, right!!)
            else -> null
        }
    }

    private fun createImplicitContainsRelation(left: Int, right: Int): RawRelation? =
        RawImplicitContainsRelation(left, right)

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

    private fun nextUid(): Int = lastUid++
}