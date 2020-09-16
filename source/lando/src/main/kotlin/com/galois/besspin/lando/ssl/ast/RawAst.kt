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
)

interface RawNamed {
    val pos: RawPos
}

interface RawElement : RawNamed {
    val uid: Uid
    // val name: String
    override val pos : RawPos
}

interface RawComponentPart {
    val pos : RawPos
    val text: String
}

@Serializable
data class RawQuery(
    override val pos: RawPos,
    override val text: String,
    val comments: List<RawComment>
) : RawComponentPart

@Serializable
data class RawConstraint(
    override val pos: RawPos,
    override val text: String,
    val comments: List<RawComment>
) : RawComponentPart

@Serializable
data class RawCommand(
    override val pos: RawPos,
    override val text: String,
    val comments: List<RawComment>
) : RawComponentPart

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
) : RawElement

@Serializable
data class RawItem(
    override val pos : RawPos,
    val id : Name,
    val text : String,
    val comments : List<RawComment>
) : RawNamed


@Serializable
data class RawEvents(
    override val uid: Int,
    override val pos: RawPos,
    val name: Name,
    val events: List<RawItem>,
    val comments: List<RawComment>
) : RawElement

@Serializable
data class RawScenarios(
    override val uid: Int,
    override val pos: RawPos,
    val name: Name,
    val scenarios: List<RawItem>,
    val comments: List<RawComment>
) : RawElement


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
)

@Serializable
data class RawComponentImport(
    override val uid: Int,
    override val pos: RawPos,
    val name: QName,
    val abbrevName: Name?,
    val clientOf: List<QName>,
    val comments: List<RawComment>
) : RawElement

@Serializable
data class RawSubsystem(
    override val uid: Int,
    override val pos: RawPos,
    val name: Name,
    val abbrevName: Name?,
    val inherits: List<QName>,
    val clientOf: List<QName>,
    val explanation: String,
    val indexing: List<RawIndexEntry>,
    var body: Body?,
    val comments: List<RawComment>
) : RawElement

@Serializable
data class RawSubsystemImport(
    override val uid: Int,
    override val pos: RawPos,
    val name: QName,
    val abbrevName: Name?,
    val clientOf: List<QName>,
    val comments: List<RawComment>
) : RawElement

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
) : RawElement

@Serializable
data class RawRelation(
    override val uid: Int,
    override val pos: RawPos,
    val name: QName,
    val inherits: List<QName>,
    val clientOf: List<QName>,
    val comments: List<RawComment>
): RawElement

@Serializable
data class RawSSL(
    // val uid: Int,
    val body : Body,
    val comments: List<RawComment>
)

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