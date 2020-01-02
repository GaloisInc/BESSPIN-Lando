package com.galois.besspin.lando.ssl.parser

import com.galois.besspin.lando.ssl.ast.*

class RawAstBuilder(var sslCxt: SSLParser.SslContext) {

    private var inheritRelations: MutableList<InheritRelation> = arrayListOf()
    private var containsRelations: MutableList<ContainsRelation> = arrayListOf()
    private var clientRelations: MutableList<ClientRelation> = arrayListOf()

    private var lastUid = 1

    private var lastSystem: String? = null
    private var lastSubsystem: String? = null

    fun build(): SSL = toAst(sslCxt)

    private fun toAst(sslCxt: SSLParser.SslContext): SSL {
        val (elements, relationsListOfLists) = sslCxt.element().map {
            when(it) {
                is SSLParser.SystemElementContext -> toAst(it.system())
                is SSLParser.SubsystemElementContext -> toAst(it.subsystem())
                is SSLParser.ComponentElementContext -> toAst(it.component())
                is SSLParser.EventsElementContext -> toAst(it.events())
                is SSLParser.ScenariosElementContext -> toAst(it.scenarios())
                is SSLParser.RequirementsElementContext -> toAst(it.requirements())
                else -> throw UnsupportedOperationException(it.toString())
            }
        }.unzip()

        val relationship = Relationships.fromRelationList(relationsListOfLists.flatten())
        return SSL(nextUid(), elements, relationship, listOf());
    }

    private fun toAst(sysCxt: SSLParser.SystemContext): Pair<System, List<Relation>> {
        val name = toAst(sysCxt.sysname)
        val description = toAst(sysCxt.paragraph())
        val index = if (sysCxt.index() != null) toAst(sysCxt.index()) else listOf()
        val comments = arrayListOf<Comment>()
        val system = System(nextUid(), name, description, index, comments)

        val reltype = sysCxt.RELKEYWORD()?.symbol?.text?.trim() ;
        val relname = if (sysCxt.relname != null) toAst(sysCxt.relname) else null
        val relation = createRelation(reltype, name, relname)

        lastSystem = name

        return Pair(system, if (relation != null) listOf(relation) else listOf())
    }

    private fun toAst(subsysCxt: SSLParser.SubsystemContext): Pair<Subsystem, List<Relation>> {
        val name = toAst(subsysCxt.subsysname)
        val description = toAst(subsysCxt.paragraph())
        val index = if (subsysCxt.index() != null) toAst(subsysCxt.index()) else listOf()
        val comments = arrayListOf<Comment>()
        val subsystem = Subsystem(nextUid(), name, description, index, comments)

        val reltype = subsysCxt.RELKEYWORD()?.symbol?.text?.trim() ;
        val relname = if (subsysCxt.relname != null) toAst(subsysCxt.relname) else null
        val relation = createRelation(reltype, name, relname)

        lastSubsystem = name

        val possibleRelations: MutableList<Relation?> = arrayListOf(relation)
        if (lastSystem != null) {
            possibleRelations.add(createRelation("contains", name, lastSystem))
        }

        return Pair(subsystem, possibleRelations.filterNotNull())
    }

    private fun toAst(cmpCxt: SSLParser.ComponentContext): Pair<Component, List<Relation>> {
        val name = toAst(cmpCxt.compname)
        val parts = if (cmpCxt.componentParts() != null) toAst(cmpCxt.componentParts()) else arrayListOf()
        val comments = arrayListOf<Comment>()
        val component = Component(nextUid(), name, parts, comments)

        val reltype = cmpCxt.RELKEYWORD()?.symbol?.text?.trim() ;
        val relname = if (cmpCxt.relname != null) toAst(cmpCxt.relname) else null
        val relation = createRelation(reltype, name, relname)

        val possibleRelations: MutableList<Relation?> = arrayListOf(relation)
        if (lastSubsystem != null) {
            possibleRelations.add(createRelation("contains", name, lastSubsystem))
        }

        return Pair(component, possibleRelations.filterNotNull())
    }

    private fun toAst(cmpPartCxt: SSLParser.ComponentPartsContext): List<ComponentPart> {
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

    private fun toAst(queryCxt: SSLParser.QueryContext): Query =
        Query(cleanSentence(queryCxt.QUERY().text), listOf())

    private fun toAst(cmdCxt: SSLParser.CommandContext): Command =
        Command(cleanSentence(cmdCxt.COMMAND().text), listOf())

    private fun toAst(constraintCxt: SSLParser.ConstraintContext): Constraint =
        Constraint(cleanSentence(constraintCxt.CONSTRAINT().text), listOf())


    private fun toAst(eventsCxt: SSLParser.EventsContext): Pair<Events, List<Relation>> {
        val name = toAst(eventsCxt.name())
        val events = toAst(eventsCxt.eventEntries())

        val comments = arrayListOf<Comment>()

        val possibleRelations = mutableListOf<Relation?>()
        if (lastSubsystem != null) {
            possibleRelations.add(createRelation("contains", name, lastSubsystem))
        }

        return Pair(Events(nextUid(), name, events, comments), possibleRelations.filterNotNull())
    }

    private fun toAst(eventEntriesCxt: SSLParser.EventEntriesContext): List<Event> =
        eventEntriesCxt.eventEntry().map { toAst(it) }

    private fun toAst(eventCxt: SSLParser.EventEntryContext): Event =
        Event(
            id = toAst(eventCxt.name()),
            text = cleanSentence(eventCxt.SENTENCE().text),
            comments = listOf()
        )


    private fun toAst(scenariosCxt: SSLParser.ScenariosContext): Pair<Scenarios, List<Relation>> {
        val name = toAst(scenariosCxt.name())
        val scenarios = toAst(scenariosCxt.scenarioEntries())

        val comments = arrayListOf<Comment>()

        val possibleRelations = mutableListOf<Relation?>()
        if (lastSubsystem != null) {
            possibleRelations.add(createRelation("contains", name, lastSubsystem))
        }

        return Pair(Scenarios(nextUid(), name, scenarios, comments), possibleRelations.filterNotNull())
    }

    private fun toAst(scenarioEntriesCxt: SSLParser.ScenarioEntriesContext): List<Scenario> =
        scenarioEntriesCxt.scenarioEntry().map { toAst(it) }

    private fun toAst(scenarioEntryCxt: SSLParser.ScenarioEntryContext): Scenario =
        Scenario(
            id = toAst(scenarioEntryCxt.name()),
            text = cleanSentence(scenarioEntryCxt.SENTENCE().text),
            comments = listOf()
        )


    private fun toAst(requirementsCxt: SSLParser.RequirementsContext): Pair<Requirements, List<Relation>> {
        val name = toAst(requirementsCxt.name())
        val requirements = toAst(requirementsCxt.requirementEntries())

        val comments = arrayListOf<Comment>()

        val possibleRelations = mutableListOf<Relation?>()
        if (lastSubsystem != null) {
            possibleRelations.add(createRelation("contains", name, lastSubsystem))
        }

        return Pair(Requirements(nextUid(), name, requirements, comments), possibleRelations.filterNotNull())
    }

    private fun toAst(requirementEntriesCxt: SSLParser.RequirementEntriesContext): List<Requirement> =
        requirementEntriesCxt.requirementEntry().map { toAst(it) }

    private fun toAst(requirementEntryCxt: SSLParser.RequirementEntryContext): Requirement =
        Requirement(
            id = toAst(requirementEntryCxt.name()),
            text = cleanSentence(requirementEntryCxt.SENTENCE().text),
            comments = listOf()
        )


    private fun toAst(indexCxt: SSLParser.IndexContext): List<IndexEntry> =
        toAst(indexCxt.indexEntries())

    private fun toAst(indexEntriesCxt: SSLParser.IndexEntriesContext): List<IndexEntry> =
        indexEntriesCxt.indexEntry().map { toAst(it) }

    private fun toAst(indexEntryCxt: SSLParser.IndexEntryContext): IndexEntry {
        var key = toAst(indexEntryCxt.indexKey())
        var (values, comments) = toAst(indexEntryCxt.indexValue())
        return IndexEntry(key, values, comments)
    }

    private fun toAst(indexKeyCxt: SSLParser.IndexKeyContext): String =
        toAst(indexKeyCxt.indexString())

    private fun toAst(indexValueCxt: SSLParser.IndexValueContext): Pair<List<String>, List<Comment>> {
        return Pair(indexValueCxt.indexString().map { toAst(it) }, listOf())
    }

    private fun toAst(indexStringCxt: SSLParser.IndexStringContext): String =
        indexStringCxt.INDEXCHAR().map { it.symbol.text }.joinToString(separator = "").trim()


    private fun toAst(nameCxt: SSLParser.NameContext): String =
        nameCxt.NAMECHAR().map { it.symbol.text }.joinToString(separator = "").trim()

    private fun toAst(paragraphCxt: SSLParser.ParagraphContext): String =
        cleanParagraph(paragraphCxt.PARAGRAPH().text)

    private fun createRelation(reltype: String?, left: String, right: String?): Relation? {
        return when(reltype) {
            "inherit" -> InheritRelation(left, right!!)
            "contains" -> ContainsRelation(left, right!!)
            "client" -> ClientRelation(left, right!!)
            else -> null
        }
    }

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

    private fun nextUid(): Int = lastUid++
}