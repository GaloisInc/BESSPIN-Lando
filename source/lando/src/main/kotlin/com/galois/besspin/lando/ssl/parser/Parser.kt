package com.galois.besspin.lando.ssl.parser

import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import kotlin.UnsupportedOperationException


fun parseFile(fileName: String): SSL {
    val stream = CharStreams.fromFileName(fileName)

    val lexer = SSLLexer(stream)
    val tokenStream = CommonTokenStream(lexer)

    val parsed = SSLParser(tokenStream)
    val ssl = parsed.ssl()

    return ssl.toAst()
}

fun parseText(text: String): SSL {
    val stream = CharStreams.fromString(text)

    val lexer = SSLLexer(stream)
    val tokenStream = CommonTokenStream(lexer)

    val parsed = SSLParser(tokenStream)
    val ssl = parsed.ssl()
    
    return ssl.toAst()
}


fun SSLParser.SslContext.toAst(): SSL {
    val elements = this.element().map {
        when(it) {
            is SSLParser.SystemContext -> it.toAst()
            is SSLParser.SubsystemContext -> it.toAst()
            is SSLParser.ComponentContext -> it.toAst()
            is SSLParser.EventsContext -> it.toAst()
            is SSLParser.ScenariosContext -> it.toAst()
            is SSLParser.RequirementsContext -> it.toAst()
            else -> throw UnsupportedOperationException(it.toString())
        }
    }
    return SSL(elements);
}

fun SSLParser.SystemContext.toAst(): System {
    val name = this.sysname.toAst();
    val reltype = this.RELKEYWORD().symbol.text ;
    val relname = this.relname.toAst();

    val description = this.paragraph().toAst()

    //TODO: Handle other relationships. Generally we have to push
    //the resolution to a later stage. So we have to store this information
    //somewhere for later use. This may mean some state threading :(
    val inherits = when(reltype) {
        "inherit" -> arrayListOf<String>(relname)
        else -> arrayListOf()
    }

    return System(name = name, description = description, inherits = inherits)
}

fun SSLParser.SubsystemContext.toAst(): Subsystem {
    val name = this.subsysname.toAst();
    val reltype = this.RELKEYWORD().symbol.text ;
    val relname = this.relname.toAst();

    val description = this.paragraph().toAst()

    //TODO: Handle other relationships. Generally we have to push
    //the resolution to a later stage. So we have to store this information
    //somewhere for later use. This may mean some state threading :(
    val inherits = when(reltype) {
        "inherit" -> arrayListOf<String>(relname)
        else -> arrayListOf()
    }

    return Subsystem(name = name, description = description, inherits = inherits)
}

fun SSLParser.ComponentContext.toAst(): Component {
    val name = this.compname.toAst();
    val reltype = this.RELKEYWORD().symbol.text ;
    val relname = this.relname.toAst();

    //TODO: Handle other relationships. Generally we have to push
    //the resolution to a later stage. So we have to store this information
    //somewhere for later use. This may mean some state threading :(
    val inherits = when(reltype) {
        "inherit" -> arrayListOf<String>(relname)
        else -> arrayListOf()
    }

    val parts = this.componentParts().toAst()

    return Component(name = name, inherits = inherits, parts = parts)
}

fun SSLParser.ComponentPartsContext.toAst(): List<ComponentPart> {
    val componentParts = this.componentPart().map {
        when (it) {
            is SSLParser.QueryContext -> it.toAst()
            is SSLParser.CommandContext -> it.toAst()
            is SSLParser.ConstraintContext -> it.toAst()
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
    Events(name = this.name().toAst(), events = this.eventEntries().toAst())

fun SSLParser.EventEntriesContext.toAst(): List<Event> =
    this.eventEntry().map { it.toAst() }

fun SSLParser.EventEntryContext.toAst(): Event =
    Event(id = this.name().toAst(), text = cleanSentence(this.SENTENCE().text))


fun SSLParser.ScenariosContext.toAst(): Scenarios =
    Scenarios(name = this.name().toAst(), scenarios = this.scenarioEntries().toAst())

fun SSLParser.ScenarioEntriesContext.toAst(): List<Scenario> =
    this.scenarioEntry().map { it.toAst() }

fun SSLParser.ScenarioEntryContext.toAst(): Scenario =
    Scenario(id = this.name().toAst(), text = cleanSentence(this.SENTENCE().text))


fun SSLParser.RequirementsContext.toAst(): Requirements =
    Requirements(name = this.name().toAst(), requirements = this.requirementEntries().toAst())

fun SSLParser.RequirementEntriesContext.toAst(): List<Requirement> =
    this.requirementEntry().map { it.toAst() }

fun SSLParser.RequirementEntryContext.toAst(): Requirement =
    Requirement(id = this.name().toAst(), text = cleanSentence(this.SENTENCE().text))


fun SSLParser.NameContext.toAst(): String =
    this.NAMECHAR().map { it.symbol.text }.joinToString(separator = "").trim()

fun SSLParser.ParagraphContext.toAst(): String =
    cleanParagraph(this.PARAGRAPH().text)


fun cleanSentence(text : String): String {
    //TODO: More cleanup required
    return text.trim();
}

fun cleanParagraph(text : String): String {
    //TODO: More cleanup required
    return text.trim();
}
