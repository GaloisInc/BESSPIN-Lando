package com.galois.besspin.lando.ssl.ast

import kotlinx.serialization.*
import kotlinx.serialization.json.*
import kotlinx.serialization.modules.*
import java.util.*

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

    fun toMarkdownReference(ref: String): String {
        val link_name = ref.replace(" ","-").lowercase(Locale.getDefault())
        return "[$ref](#$link_name)"
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
        var result = "<!--QUERY-->\n* $text"
        for (elem in comments) {
            result += elem.toMarkdown()
        }
        result += "<!--QUERY/-->\n"
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
        var result = "<!--CONSTRAINT-->\n* $text"
        for (elem in comments) {
            result += elem.toMarkdown()
        }
        result += "<!--CONSTRAINT/-->\n"
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
        var result = "<!--COMMAND-->\n* $text"
        for (elem in comments) {
            result += elem.toMarkdown()
        }
        result += "<!--COMMAND/-->\n"
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
    // ### <a id="{self.link_name}"></a> {self.name}
    override fun toMarkdown(): String {
        val link_name = name.replace(" ","-").lowercase(Locale.getDefault())
        var result =  "<!--COMPONENT-->\n### <a id =\"$link_name\"></a>$name"
        if (abbrevName != null) {
            result += " ($abbrevName)"
        }
        result += "\n$explanation\n"
        for (elem in inherits) {
            result += "  * inherits ${toMarkdownReference(elem.last())}"
        }
        for (elem in clientOf) {
            result += "  * client of ${(elem.last())}"
        }
        for (elem in parts) {
            result += "  * part ${elem.toMarkdown()}"
        }
        result += "<!--COMPONENT/-->\n"
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
        var result = "<!--ITEM-->\n### $id\n$text\n"
        for (comment in comments) {
            result += comment.toMarkdown()
        }
        result += "<!--ITEM/-->\n"
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
        var result = "<!--EVENTS-->\n## $name\n"
        for (elem in events) {
            result += elem.toMarkdown()
        }
        for (comment in comments) {
            result += comment.toMarkdown()
        }
        result += "<!--EVENTS/-->\n"
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
        var result = "<!--SCENARIOS-->\n## $name\n"
        for (elem in scenarios) {
            result += elem.toMarkdown()
        }
        for (comment in comments) {
            result += comment.toMarkdown()
        }
        result += "<!--SCENARIOS/-->\n"
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
) : RawElement {
    override fun toMarkdown(): String {
        var result = "<!--REQUIREMENTS-->\n## $name\n"
        for (elem in requirements) {
            result += elem.toMarkdown()
        }
        for (comment in comments) {
            result += comment.toMarkdown()
        }
        result += "<!--REQUIREMENTS/-->\n"
        return result
    }
}

@Serializable
data class RawIndexEntry(
    val pos: RawPos,
    val key: Name,
    val values: List<String>,
    val comments: List<RawComment>
) {
    fun toMarkdown(): String {
        var result = "<!--INDEX_ENTRY-->\nIndexing $key: "
        for (value in values) {
            result += "* $value\n"
        }
        for (comment in comments) {
            result += "* ${comment.toMarkdown()}"
        }
        result += "<!--INDEX_ENTRY/-->\n"
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
        var result = "<!--COMPONENT_IMPORT-->\n#### import component ${name.last()}"
        if (abbrevName != null) {
            result += " ($abbrevName)"
        }
        result += "\n"
        for (elem in clientOf) {
            result += "* client of ${toMarkdownReference(elem.last())}\n"
        }
        for (comment in comments) {
            result += comment.toMarkdown()
        }
        result += "<!--COMPONENT_IMPORT/-->\n"
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
        val link_name = name.replace(" ","-").lowercase(Locale.getDefault())
        var result =  "<!--SUBSYSTEM $name-->\n## <a id =\"$link_name\"></a>$name"
        if (abbrevName != null) {
            result += " ($abbrevName)"
        }
        result += "\n$explanation\n"
        result += "\n"
        for (elem in clientOf) {
            result += "  * client of ${toMarkdownReference(elem.last())}\n"
        }
        for (elem in indexing) {
            result += elem.toMarkdown();
        }
        for (elem in body!!) {
            result += elem.toMarkdown()
        }
        result += "<!--SUBSYSTEM $name/-->"
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
        var result = "<!--SUBSYSTEM_IMPORT-->\n#### import subsystem ${name.last()}\n"
        if (abbrevName != null) {
            result += " ($abbrevName)"
        }
        for (elem in clientOf) {
            result += "* client of ${toMarkdownReference(elem.last())}\n"
        }
        for (comment in comments) {
            result += comment.toMarkdown()
        }
        result += "<!--SUBSYSTEM_IMPORT/-->\n"
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
        val link_name = name.replace(" ","-").lowercase(Locale.getDefault())
        var result =  "<!--SYSTEM $name-->\n# <a id =\"$link_name\"></a>$name"
        if (abbrevName != null) {
            result += " ($abbrevName)"
        }
        result += "\n$explanation\n"
        result += "\n"
        for (elem in body!!) {
            result += elem.toMarkdown()
        }
        for (elem in comments) {
            result += elem.toMarkdown()
        }
        for (elem in indexing) {
            result += elem.toMarkdown();
        }
        result += "<!--SYSTEM $name/-->"
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
        var result = "<!--RELATION-->\n#### relation ${name.last()}\n"
        for (elem in inherits) {
            result += "* inherits ${toMarkdownReference(elem.last())}\n"
        }
        for (elem in clientOf) {
            result += "* client of ${toMarkdownReference(elem.last())}\n"
        }
        for (elem in contains) {
            result += "* contains ${toMarkdownReference(elem.last())}\n"
        }
        result += "<!--RELATION/-->\n"
        return result
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
            result += "\n"
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

@OptIn(ExperimentalSerializationApi::class)
fun rawSSLFromJSON(text: String): RawSSL {
    return jsonRawSSL.decodeFromString(text)
}
