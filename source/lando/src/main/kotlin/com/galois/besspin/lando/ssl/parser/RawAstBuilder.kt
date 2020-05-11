package com.galois.besspin.lando.ssl.parser

import com.galois.besspin.lando.ssl.ast.*

class RawAstBuilder(var landoSourceContext: SSLParserOld.LandoSourceContext) {

    private var inheritRelations: MutableList<RawInheritRelation> = arrayListOf()
    private var containsRelations: MutableList<RawContainsRelation> = arrayListOf()
    private var clientRelations: MutableList<RawClientRelation> = arrayListOf()

    private var lastUid = 1

    private var lastSystem: RawSystem? = null
    private var lastSubsystem: RawSubsystem? = null

    fun build(): RawSSL = toAst(landoSourceContext)

    private fun toAst(landoSourceContext: SSLParserOld.LandoSourceContext): RawSSL {
        val (elements, relationsListOfLists) = landoSourceContext.specElement().map {
            when(it) {
                is SSLParserOld.SystemElementContext -> toAst(it.system())
                is SSLParserOld.SubsystemElementContext -> toAst(it.subsystem())
                is SSLParserOld.ComponentElementContext -> toAst(it.component())
                is SSLParserOld.EventsElementContext -> toAst(it.events())
                is SSLParserOld.ScenariosElementContext -> toAst(it.scenarios())
                is SSLParserOld.RequirementsElementContext -> toAst(it.requirements())
                is SSLParserOld.RelationElementContext -> Pair(null, toAst(it.relation()))
                else -> throw UnsupportedOperationException(it.toString())
            }
        }.unzip()

        val relationship = RawRelationships.fromRelationList(relationsListOfLists.flatten())
        return RawSSL(nextUid(), elements.filterNotNull(), relationship, listOf());
    }

    private fun toAst(sysCxt: SSLParserOld.SystemContext): Pair<RawSystem, List<RawRelation>> {
        val name = toAst(sysCxt.sysname)
        val description = toAst(sysCxt.paragraph())
        val index = toAstOptional(sysCxt.indexing())
        val comments = collectComments(sysCxt.lineComments(), sysCxt.comment())
        val system = RawSystem(nextUid(), name, description, index, comments)

        val reltype = sysCxt.RELKEYWORD()?.symbol?.text?.trim()
        val relname = toAstOptional(sysCxt.relname)
        val relation = createRelation(reltype, name, relname)

        lastSystem = system

        return Pair(system, if (relation != null) listOf(relation) else listOf())
    }

    private fun toAst(subsysCxt: SSLParserOld.SubsystemContext): Pair<RawSubsystem, List<RawRelation>> {
        val name = toAst(subsysCxt.subsysname)
        val abbrevName = subsysCxt.ABBREV()?.symbol?.text?.trim()
        val description = toAst(subsysCxt.paragraph())
        val index = toAstOptional(subsysCxt.indexing())
        val comments = collectComments(subsysCxt.lineComments(), subsysCxt.comment())
        val subsystem = RawSubsystem(nextUid(), name, abbrevName, description, index, comments)

        val reltype = subsysCxt.RELKEYWORD()?.symbol?.text?.trim() ;
        val relname = toAstOptional(subsysCxt.relname)
        val relation = createRelation(reltype, name, relname)

        lastSubsystem = subsystem

        val possibleRelations: MutableList<RawRelation?> = arrayListOf(relation)
        if (lastSystem != null) {
            possibleRelations.add(createImplicitContainsRelation(subsystem.uid, lastSystem!!.uid))
        }

        return Pair(subsystem, possibleRelations.filterNotNull())
    }

    private fun toAst(cmpCxt: SSLParserOld.ComponentContext): Pair<RawComponent, List<RawRelation>> {
        val name = toAst(cmpCxt.compname)
        val abbrevName = cmpCxt.ABBREV()?.symbol?.text?.trim() ;
        val description = toAst(cmpCxt.paragraph())
        val parts = if (cmpCxt.componentParts() != null) toAst(cmpCxt.componentParts()) else arrayListOf()
        val comments = collectComments(cmpCxt.lineComments(), cmpCxt.comment())
        val component = RawComponent(nextUid(), name, abbrevName, description, parts, comments)

        val reltype = cmpCxt.RELKEYWORD()?.symbol?.text?.trim() ;
        val relname = toAstOptional(cmpCxt.relname)
        val relation = createRelation(reltype, name, relname)

        val possibleRelations: MutableList<RawRelation?> = arrayListOf(relation)
        if (lastSubsystem != null) {
            possibleRelations.add(createImplicitContainsRelation(component.uid, lastSubsystem!!.uid))
        }

        return Pair(component, possibleRelations.filterNotNull())
    }

    private fun toAst(cmpPartCxt: SSLParserOld.ComponentPartsContext): List<RawComponentPart> {
        val componentParts = cmpPartCxt.componentPart().map {
            when (it) {
                is SSLParserOld.QueryPartContext -> toAst(it.query())
                is SSLParserOld.CommandPartContext -> toAst(it.command())
                is SSLParserOld.ConstraintPartContext -> toAst(it.constraint())
                else -> throw UnsupportedOperationException(it.toString())
            }
        }.toMutableList()

        return componentParts
    }

    private fun toAst(queryCxt: SSLParserOld.QueryContext): RawQuery =
        RawQuery(
            cleanSentence(queryCxt.QUERY().text),
            collectComments(queryCxt.lineComments(), queryCxt.comment())
        )

    private fun toAst(cmdCxt: SSLParserOld.CommandContext): RawCommand =
        RawCommand(
            cleanSentence(cmdCxt.COMMAND().text),
            collectComments(cmdCxt.lineComments(), cmdCxt.comment())
        )

    private fun toAst(constraintCxt: SSLParserOld.ConstraintContext): RawConstraint =
        RawConstraint(
            cleanSentence(constraintCxt.CONSTRAINT().text),
            collectComments(constraintCxt.lineComments(), constraintCxt.comment())
        )


    private fun toAst(eventsCxt: SSLParserOld.EventsContext): Pair<RawEvents, List<RawRelation>> {
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

    private fun toAst(eventEntriesCxt: SSLParserOld.EventEntriesContext): List<RawEvent> =
        eventEntriesCxt.eventEntry().map { toAst(it) }

    private fun toAst(eventCxt: SSLParserOld.EventEntryContext): RawEvent =
        RawEvent(
            id = toAst(eventCxt.name()),
            text = cleanSentence(eventCxt.SENTENCE().text),
            comments = collectComments(eventCxt.lineComments(), eventCxt.nameComment, eventCxt.sentenceComment)
        )


    private fun toAst(scenariosCxt: SSLParserOld.ScenariosContext): Pair<RawScenarios, List<RawRelation>> {
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

    private fun toAst(scenarioEntriesCxt: SSLParserOld.ScenarioEntriesContext): List<RawScenario> =
        scenarioEntriesCxt.scenarioEntry().map { toAst(it) }

    private fun toAst(scenarioCxt: SSLParserOld.ScenarioEntryContext): RawScenario =
        RawScenario(
            id = toAst(scenarioCxt.name()),
            text = cleanSentence(scenarioCxt.SENTENCE().text),
            comments = collectComments(scenarioCxt.lineComments(), scenarioCxt.nameComment, scenarioCxt.sentenceComment)
        )


    private fun toAst(requirementsCxt: SSLParserOld.RequirementsContext): Pair<RawRequirements, List<RawRelation>> {
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

    private fun toAst(requirementEntriesCxt: SSLParserOld.RequirementEntriesContext): List<RawRequirement> =
        requirementEntriesCxt.requirementEntry().map { toAst(it) }

    private fun toAst(requirementCxt: SSLParserOld.RequirementEntryContext): RawRequirement =
        RawRequirement(
            id = toAst(requirementCxt.name()),
            text = cleanSentence(requirementCxt.SENTENCE().text),
            comments = collectComments(requirementCxt.lineComments(), requirementCxt.nameComment, requirementCxt.sentenceComment)
        )

    private fun toAst(relationCxt: SSLParserOld.RelationContext): List<RawRelation> =
        arrayListOf(
            createRelation(relationCxt.RELKEYWORD().symbol?.text?.trim(), toAst(relationCxt.left), toAst(relationCxt.right))
        ).filterNotNull()

    private fun toAstOptional(indexCxt: SSLParserOld.IndexingContext?): List<RawIndexEntry> =
        if (indexCxt != null) toAst(indexCxt.indexEntries()) else listOf()

    private fun toAst(indexEntriesCxt: SSLParserOld.IndexEntriesContext): List<RawIndexEntry> =
        indexEntriesCxt.indexEntry().map { toAst(it) }

    private fun toAst(indexEntryCxt: SSLParserOld.IndexEntryContext): RawIndexEntry {
        val key = toAst(indexEntryCxt.indexKey())
        val (values, comments) = toAst(indexEntryCxt.indexValueList())
        return RawIndexEntry(key, values, comments)
    }

    private fun toAst(indexKeyCxt: SSLParserOld.IndexKeyContext): String =
        toAst(indexKeyCxt.indexString())

    private fun toAst(indexValueCxt: SSLParserOld.IndexValueListContext): Pair<List<String>, List<RawComment>> {
        val (values, maybeComments) = indexValueCxt.indexValuePart().map { toAst(it) }.unzip()
        return Pair(values, maybeComments.filterNotNull())
    }

    private fun toAst(indexValuePartCxt: SSLParserOld.IndexValuePartContext): Pair<String, RawComment?> {
        return Pair(toAst(indexValuePartCxt.indexString()), toAst(indexValuePartCxt.comment()))
    }

    private fun toAst(indexStringCxt: SSLParserOld.IndexStringContext): String =
        indexStringCxt.INDEXCHAR().map { it.symbol.text }.joinToString(separator = "").trim()


    private fun toAst(nameCxt: SSLParserOld.NameContext): String =
        nameCxt.NAMECHAR().map { it.symbol.text }.joinToString(separator = "").trim()

    private fun toAstOptional(nameCxt: SSLParserOld.NameContext?): String? =
        if (nameCxt != null) toAst(nameCxt) else null

    private fun toAst(paragraphCxt: SSLParserOld.ParagraphContext): String =
        cleanParagraph(paragraphCxt.PARAGRAPH().text)

    private fun toAst(commentCxt: SSLParserOld.CommentContext?): RawComment? {
        if (commentCxt != null) {
            val text = commentCxt.COMMENTCHAR().map { it.symbol.text }.joinToString(separator = "").trim()
            return RawComment(text)
        } else {
            return null
        }
    }

    private fun toAst(lineCommentCxt: SSLParserOld.LineCommentsContext?): List<RawComment> {
        if (lineCommentCxt != null) {
            return lineCommentCxt.comments().comment().map { toAst(it) }.filterNotNull()
        } else {
            return listOf()
        }
    }

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

    private fun cleanSentence(text: String): String {
        //TODO: Give this more thought
        return Regex("\\s*[\\r\\n]\\s*", RegexOption.MULTILINE).replace(text, " ").trim();
    }

    private fun cleanParagraph(text: String): String {
        //TODO: Give this more thought
        return Regex("\\s*[\\r\\n]\\s*", RegexOption.MULTILINE).replace(text, " ").trim();
    }

    private fun cleanIndexKeyValue(text: String): String {
        return text.trim();
    }

    private fun collectComments(
        lineCommentsCxt: SSLParserOld.LineCommentsContext?,
        commentCxt: SSLParserOld.CommentContext?
    ): List<RawComment> {
        val comments = toAst(lineCommentsCxt) + toAst(commentCxt)
        return comments.filterNotNull()
    }

    private fun collectComments(
        lineCommentsCxt: SSLParserOld.LineCommentsContext?,
        commentCxt1: SSLParserOld.CommentContext?,
        commentCxt2: SSLParserOld.CommentContext?
    ): List<RawComment> {
        val comments = toAst(lineCommentsCxt) + toAst(commentCxt1) + toAst(commentCxt2)
        return comments.filterNotNull()
    }

    private fun nextUid(): Int = lastUid++
}