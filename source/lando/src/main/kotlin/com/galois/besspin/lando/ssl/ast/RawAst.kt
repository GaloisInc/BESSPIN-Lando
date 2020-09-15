package com.galois.besspin.lando.ssl.ast

import kotlinx.serialization.*
import kotlinx.serialization.json.*
import kotlinx.serialization.modules.*

typealias Name = String
typealias QName = List<Name>
typealias Body = List<RawElement>

@Serializable
data class RawPos(
    var line: Int,
    var col: Int
)

@Serializable
data class RawComment(
    var pos : RawPos,
    var text: String
)

interface RawElement {
    // val uid: Int
    // var name: String
    var pos : RawPos
}


interface RawComponentPart {
    var pos : RawPos
    var text: String
}

@Serializable
data class RawQuery(
    override var pos: RawPos,
    override var text: String,
    var comments: List<RawComment>
) : RawComponentPart

@Serializable
data class RawConstraint(
    override var pos: RawPos,
    override var text: String,
    var comments: List<RawComment>
) : RawComponentPart

@Serializable
data class RawCommand(
    override var pos: RawPos,
    override var text: String,
    var comments: List<RawComment>
) : RawComponentPart

@Serializable
data class RawComponent(
    // override val uid: Int,
    override var pos: RawPos,
    var name: Name,
    var abbrevName: Name?,
    var inherits: List<QName>,
    var clientOf: List<QName>,
    var explanation: String,
    var parts: List<RawComponentPart> = arrayListOf(),
    var comments: List<RawComment>
) : RawElement

@Serializable
data class RawEvents(
    // override val uid: Int,
    override var pos: RawPos,
    var name: Name,
    var events: List<RawEvent> = arrayListOf(),
    var comments: List<RawComment>
) : RawElement

@Serializable
data class RawEvent(
    var pos: RawPos,
    var id: Name,
    var text: String,
    var comments: List<RawComment>
)

@Serializable
data class RawScenarios(
    // override val uid: Int,
    override var pos: RawPos,
    var name: Name,
    var scenarios: List<RawScenario> = arrayListOf(),
    var comments: List<RawComment>
) : RawElement

@Serializable
data class RawScenario(
    var pos: RawPos,
    var id: Name,
    var text: String,
    var comments: List<RawComment>
)

@Serializable
data class RawRequirements(
    // override val uid: Int,
    override var pos: RawPos,
    var name: Name,
    var requirements: List<RawRequirement>,
    var comments: List<RawComment>
) : RawElement

@Serializable
data class RawRequirement(
    var pos: RawPos,
    var id: Name,
    var text: String,
    var comments: List<RawComment>
)

@Serializable
data class RawIndexEntry(
    var pos: RawPos,
    var key: Name,
    var values: List<String>,
    var comments: List<RawComment>
)

@Serializable
data class RawComponentImport(
    override var pos: RawPos,
    var name: QName,
    var abbrevName: Name?,
    var clientOf: List<QName>,
    var comments: List<RawComment>
) : RawElement

@Serializable
data class RawSubsystem(
    // override val uid: Int,
    override var pos: RawPos,
    var name: Name,
    var abbrevName: Name?,
    var inherits: List<QName>,
    var clientOf: List<QName>,
    var explanation: String,
    var indexing: List<RawIndexEntry>,
    var body: Body?,
    var comments: List<RawComment>
) : RawElement

@Serializable
data class RawSubsystemImport(
    override var pos: RawPos,
    var name: QName,
    var abbrevName: Name?,
    var clientOf: List<QName>,
    var comments: List<RawComment>
) : RawElement

@Serializable
data class RawSystem(
    // override val uid: Int,
    override var pos: RawPos,
    var name: String,
    var abbrevName: String?,
    var explanation: String,
    var indexing: List<RawIndexEntry>,
    var body: Body?,
    var comments: List<RawComment>
) : RawElement

@Serializable
data class RawRelation(
    override var pos: RawPos,
    var name: QName,
    var inherits: List<QName>,
    var clientOf: List<QName>,
    var comments: List<RawComment>
): RawElement

@Serializable
data class RawSSL(
    // var uid: Int,
    var body : Body,
    var comments: List<RawComment>
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