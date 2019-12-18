package com.galois.besspin.lando.ssl.parser

import com.galois.besspin.lando.ssl.ast.*
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import java.io.File
import java.lang.IllegalStateException
import kotlin.UnsupportedOperationException


fun parseFile(file: File): SSL {
    val stream = CharStreams.fromPath(file.toPath())

    val lexer = SSLLexer(stream)
    val tokenStream = CommonTokenStream(lexer)

    val parsed = SSLParser(tokenStream)
    val ssl = parsed.ssl()

    if(parsed.numberOfSyntaxErrors != 0) {
        throw IllegalStateException("Parser Failed")
    }

    return ssl.toAst()
}

fun parseText(text: String): SSL {
    val stream = CharStreams.fromString(text)

    val lexer = SSLLexer(stream)
    val tokenStream = CommonTokenStream(lexer)

    val parsed = SSLParser(tokenStream)
    val ssl = parsed.ssl()

    if(parsed.numberOfSyntaxErrors != 0) {
        throw IllegalStateException("Parser Failed")
    }

    return ssl.toAst()
}


fun SSLParser.SslContext.toAst(): SSL {
    val elements = this.element().map {

         when(it) {
            is SSLParser.SystemElementContext -> it.system().toAst()
            is SSLParser.SubsystemElementContext -> it.subsystem().toAst()
            is SSLParser.ComponentElementContext -> it.component().toAst()
            is SSLParser.EventsElementContext -> it.events().toAst()
            is SSLParser.ScenariosElementContext -> it.scenarios().toAst()
            is SSLParser.RequirementsElementContext -> it.requirements().toAst()
            else -> throw UnsupportedOperationException(it.toString())
        }
    }
    return SSL(elements);
}


fun SSLParser.SystemContext.toAst(): System {
    val name = this.sysname.toAst();
    val reltype = this.RELKEYWORD()?.symbol?.text ;
    val relname = this.relname?.toAst();

    val description = this.paragraph().toAst()

    val index = this.index()?.toAst() ?: mapOf()

    //TODO: Handle other relationships. Generally we have to push
    //the resolution to a later stage. So we have to store this information
    //somewhere for later use. This may mean some state threading :(
    val inherits = when(reltype) {
        "inherit" -> arrayListOf<String>(relname!!)
        else -> arrayListOf()
    }

    return System(
        name = name,
        description = description,
        inherits = inherits,
        indexing = index
    )
}

fun SSLParser.SubsystemContext.toAst(): Subsystem {
    val name = this.subsysname.toAst();
    val reltype = this.RELKEYWORD()?.symbol?.text ;
    val relname = this.relname?.toAst();

    val description = this.paragraph().toAst()

    val index = this.index()?.toAst() ?: mapOf()

    //TODO: Handle other relationships. Generally we have to push
    //the resolution to a later stage. So we have to store this information
    //somewhere for later use. This may mean some state threading :(
    val inherits = when(reltype) {
        "inherit" -> arrayListOf<String>(relname!!)
        else -> arrayListOf()
    }

    return Subsystem(
        name = name,
        description = description,
        inherits = inherits,
        indexing = index
    )
}

fun SSLParser.ComponentContext.toAst(): Component {
    val name = this.compname.toAst();
    val reltype = this.RELKEYWORD()?.symbol?.text ;
    val relname = this.relname?.toAst();

    //TODO: Handle other relationships. Generally we have to push
    //the resolution to a later stage. So we have to store this information
    //somewhere for later use. This may mean some state threading :(
    val inherits = when(reltype) {
        "inherit" -> arrayListOf<String>(relname!!)
        else -> arrayListOf()
    }

    val parts = this.componentParts()?.toAst() ?: arrayListOf()

    return Component(
        name = name,
        inherits = inherits,
        parts = parts
    )
}

fun SSLParser.ComponentPartsContext.toAst(): List<ComponentPart> {
    val componentParts = this.componentPart().map {
        when (it) {
            is SSLParser.QueryPartContext -> it.query().toAst()
            is SSLParser.CommandPartContext -> it.command().toAst()
            is SSLParser.ConstraintPartContext -> it.constraint().toAst()
            else -> throw UnsupportedOperationException(it.toString())
        }
    }.toMutableList()

    return componentParts
}

fun SSLParser.QueryContext.toAst(): Query =
    Query(text = cleanSentence(this.QUERY().text))

fun SSLParser.CommandContext.toAst(): Command =
    Command(text = cleanSentence(this.COMMAND().text))

fun SSLParser.ConstraintContext.toAst(): Constraint =
    Constraint(text = cleanSentence(this.CONSTRAINT().text))


fun SSLParser.EventsContext.toAst(): Events =
    Events(
        name = this.name().toAst(),
        events = this.eventEntries().toAst()
    )

fun SSLParser.EventEntriesContext.toAst(): List<Event> =
    this.eventEntry().map { it.toAst() }

fun SSLParser.EventEntryContext.toAst(): Event =
    Event(
        id = this.name().toAst(),
        text = cleanSentence(this.SENTENCE().text)
    )


fun SSLParser.ScenariosContext.toAst(): Scenarios =
    Scenarios(
        name = this.name().toAst(),
        scenarios = this.scenarioEntries().toAst()
    )

fun SSLParser.ScenarioEntriesContext.toAst(): List<Scenario> =
    this.scenarioEntry().map { it.toAst() }

fun SSLParser.ScenarioEntryContext.toAst(): Scenario =
    Scenario(
        id = this.name().toAst(),
        text = cleanSentence(this.SENTENCE().text)
    )


fun SSLParser.RequirementsContext.toAst(): Requirements =
    Requirements(
        name = this.name().toAst(),
        requirements = this.requirementEntries().toAst()
    )

fun SSLParser.RequirementEntriesContext.toAst(): List<Requirement> =
    this.requirementEntry().map { it.toAst() }

fun SSLParser.RequirementEntryContext.toAst(): Requirement =
    Requirement(
        id = this.name().toAst(),
        text = cleanSentence(this.SENTENCE().text)
    )


fun SSLParser.IndexContext.toAst(): Map<String, List<String>> =
    this.indexEntries().toAst()

fun SSLParser.IndexEntriesContext.toAst(): Map<String, List<String>> =
    this.indexEntry().map { it.toAst() }.toMap()

fun SSLParser.IndexEntryContext.toAst(): Pair<String, List<String>> =
    Pair(this.indexKey().toAst(), this.indexValue().toAst())

fun SSLParser.IndexKeyContext.toAst(): String =
    this.indexString().toAst()

fun SSLParser.IndexValueContext.toAst(): List<String> =
    this.indexString().map { it.toAst() }

fun SSLParser.IndexStringContext.toAst(): String =
    this.INDEXCHAR().map { it.symbol.text }.joinToString(separator = "").trim()


fun SSLParser.NameContext.toAst(): String =
    this.NAMECHAR().map { it.symbol.text }.joinToString(separator = "").trim()

fun SSLParser.ParagraphContext.toAst(): String =
    cleanParagraph(this.PARAGRAPH().text)


fun cleanSentence(text: String): String {
    //TODO: Give this more thought
    return Regex("\\s*[\\r\\n]\\s*", RegexOption.MULTILINE).replace(text, " ").trim();
}

fun cleanParagraph(text: String): String {
    //TODO: Give this more thought
    return Regex("\\s*[\\r\\n]\\s*", RegexOption.MULTILINE).replace(text, " ").trim();
}

fun cleanIndexKeyValue(text: String): String {
    return text.trim();
}
