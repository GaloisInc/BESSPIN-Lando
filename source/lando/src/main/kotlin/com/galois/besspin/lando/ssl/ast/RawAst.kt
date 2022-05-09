package com.galois.besspin.lando.ssl.ast

import kotlinx.serialization.*
import kotlinx.serialization.json.*
import kotlinx.serialization.modules.*

typealias Uid = Int
typealias Name = String
typealias QName = List<Name>
typealias Body = List<RawElement>

@Serializable
data class RawPos(
    val line: Int,
    val col: Int
)

@Serializable
data class RawComment(
    val pos : RawPos,
    val text: String
) {
    fun toMarkdown(): String {
        return "<!-- ${this.text} -->\n"
    }
}

interface RawNamed {
    val pos: RawPos
}

interface RawElement : RawNamed {
    val uid: Uid
    // val name: String
    override val pos : RawPos

    fun toMarkdown(): String {
        return "RawElement\n"
    }
}


interface RawComponentPart {
    val pos : RawPos
    val text: String
    fun toMarkdown(): String
}

@Serializable
data class RawQuery(
    override val pos: RawPos,
    override val text: String,
    val comments: List<RawComment>
) : RawComponentPart {
    override fun toMarkdown(): String {
        var result = "* $text"
        for (elem in comments) {
            result += elem.toMarkdown()
        }
        return result
    }
}

@Serializable
data class RawConstraint(
    override val pos: RawPos,
    override val text: String,
    val comments: List<RawComment>
) : RawComponentPart {
    override fun toMarkdown(): String {
        var result = "* $text"
        for (elem in comments) {
            result += elem.toMarkdown()
        }
        return result
    }
}

@Serializable
data class RawCommand(
    override val pos: RawPos,
    override val text: String,
    val comments: List<RawComment>
) : RawComponentPart {
    override fun toMarkdown(): String {
        var result = "* $text"
        for (elem in comments) {
            result += elem.toMarkdown()
        }
        return result
    }
}

@Serializable
data class RawComponent(
    override val uid: Int,
    override val pos: RawPos,
    val name: Name,
    val abbrevName: Name?,
    val inherits: List<QName>,
    val clientOf: List<QName>,
    val explanation: String,
    val parts: List<RawComponentPart> = arrayListOf(),
    val comments: List<RawComment>
) : RawElement {
    override fun toMarkdown(): String {
        var result =  "### $name"
        if (abbrevName != null) {
            result += " ($abbrevName)"
        }
        result += "\n$explanation\n"
        for (elem in inherits) {
            result += "  * inherits $elem"
        }
        for (elem in clientOf) {
            result += "  * client of $elem"
        }
        for (elem in parts) {
            result += "  * part {$elem.toMarkdown()}"
        }
        return result
    }
}

@Serializable
data class RawItem(
    override val pos : RawPos,
    val id : Name,
    val text : String,
    val comments : List<RawComment>
) : RawNamed {
    fun toMarkdown(): String {
        var result = text
        for (comment in comments) {
            result += comment.toMarkdown()
        }
        return result
    }
}


@Serializable
data class RawEvents(
    override val uid: Int,
    override val pos: RawPos,
    val name: Name,
    val events: List<RawItem>,
    val comments: List<RawComment>
) : RawElement {
    override fun toMarkdown(): String {
        var result = "## Event $name"
        for (elem in events) {
            result += elem.toMarkdown()
        }
        for (comment in comments) {
            result += comment.toMarkdown()
        }
        return result
    }
}

@Serializable
data class RawScenarios(
    override val uid: Int,
    override val pos: RawPos,
    val name: Name,
    val scenarios: List<RawItem>,
    val comments: List<RawComment>
) : RawElement {
    override fun toMarkdown(): String {
        var result = "## Scenario $name"
        for (elem in scenarios) {
            result += elem.toMarkdown()
        }
        for (comment in comments) {
            result += comment.toMarkdown()
        }
        return result
    }
}


@Serializable
data class RawRequirements(
    override val uid: Int,
    override val pos: RawPos,
    val name: Name,
    val requirements: List<RawItem>,
    val comments: List<RawComment>
) : RawElement

@Serializable
data class RawIndexEntry(
    val pos: RawPos,
    val key: Name,
    val values: List<String>,
    val comments: List<RawComment>
) {
    fun toMarkdown(): String {
        var result = "Indexing $key: "
        for (value in values) {
            result += "* $value\n"
        }
        for (comment in comments) {
            result += "* ${comment.toMarkdown()}"
        }
        return result
    }
}

@Serializable
data class RawComponentImport(
    override val uid: Int,
    override val pos: RawPos,
    val name: QName,
    val abbrevName: Name?,
    val clientOf: List<QName>,
    val comments: List<RawComment>
) : RawElement {
    override fun toMarkdown(): String {
        var result = "#### import component ${name.last()}"
        if (abbrevName != null) {
            result += " ($abbrevName)"
        }
        result += "\n"
        for (value in clientOf) {
            result += "* client of ${value.last()}\n"
        }
        for (comment in comments) {
            result += comment.toMarkdown()
        }
        return result
    }
}

@Serializable
data class RawSubsystem(
    override val uid: Int,
    override val pos: RawPos,
    val name: Name,
    val abbrevName: Name?,
    val clientOf: List<QName>,
    val explanation: String,
    val indexing: List<RawIndexEntry>,
    var body: Body?,
    val comments: List<RawComment>
) : RawElement {
    override fun toMarkdown(): String {
        var result =  "## $name"
        if (abbrevName != null) {
            result += " ($abbrevName)"
        }
        result += "\n$explanation\n"
        for (value in clientOf) {
            result += "  * client of ${value.last()}\n"
        }
        for (elem in indexing) {
            result += elem.toMarkdown();
        }
        for (elem in body!!) {
            result += elem.toMarkdown()
        }
        return result
    }
}

@Serializable
data class RawSubsystemImport(
    override val uid: Int,
    override val pos: RawPos,
    val name: QName,
    val abbrevName: Name?,
    val clientOf: List<QName>,
    val comments: List<RawComment>
) : RawElement {
    override fun toMarkdown(): String {
        var result = "#### import subsystem ${name.last()}\n"
        if (abbrevName != null) {
            result += " ($abbrevName)"
        }
        for (value in clientOf) {
            result += "* client of $value\n"
        }
        for (comment in comments) {
            result += comment.toMarkdown()
        }
        return result
    }
}

@Serializable
data class RawSystem(
    override val uid: Int,
    override val pos: RawPos,
    val name: String,
    val abbrevName: String?,
    val explanation: String,
    val indexing: List<RawIndexEntry>,
    var body: Body?,
    val comments: List<RawComment>
) : RawElement {
    override fun toMarkdown(): String {
        var result =  "# $name"
        if (abbrevName != null) {
            result += " ($abbrevName)"
        }
        for (elem in body!!) {
            result += elem.toMarkdown()
        }
        for (elem in comments) {
            result += elem.toMarkdown()
        }
        result += "\n$explanation\n"
        for (elem in indexing) {
            result += elem.toMarkdown();
        }
        return result
    }
}

@Serializable
data class RawRelation(
    override val uid: Int,
    override val pos: RawPos,
    val name: QName,
    val inherits: List<QName>,
    val clientOf: List<QName>,
    val contains: List<QName>,
    val comments: List<RawComment>
): RawElement {
    override fun toMarkdown(): String {
        var result = "#### relation ${name.last()}"
        for (value in inherits) {
            result += "* inherits ${value.last()}\n"
        }
        for (value in clientOf) {
            result += "* client of ${value.last()}\n"
        }
        for (value in contains) {
            result += "* contains ${value.last()}\n"
        }
        return  result
    }
}

@Serializable
data class RawSSL(
    // val uid: Int,
    val body : Body,
    val comments: List<RawComment>
) {
    fun toMarkdown(): String {
        var result = ""
        for (elem in body) {
            result += elem.toMarkdown()
        }
        return result
    }
}


private val sslModule = SerializersModule {
    polymorphic(RawElement::class) {
        subclass(RawSystem::class)
        subclass(RawSubsystem::class)
        subclass(RawSubsystemImport::class)
        subclass(RawComponent::class)
        subclass(RawComponentImport::class)
        subclass(RawEvents::class)
        subclass(RawScenarios::class)
        subclass(RawRequirements::class)
        subclass(RawRelation::class)
    }

    polymorphic(RawComponentPart::class) {
        subclass(RawQuery::class)
        subclass(RawConstraint::class)
        subclass(RawCommand::class)
    }
}

val jsonRawSSL = Json {serializersModule = sslModule; prettyPrint = true }

fun RawSSL.toJSON(): String {
    return jsonRawSSL.encodeToString(RawSSL.serializer(), this)
}

fun rawSSLFromJSON(text: String): RawSSL {
    return jsonRawSSL.decodeFromString(text)
}
