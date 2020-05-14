package com.galois.besspin.lando.ssl.ast

import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration
import kotlinx.serialization.modules.SerializersModule

@Serializable
data class RawComment(
    var text: String
)

interface RawElement {
    val uid: Int
    var name: String
}

interface RawComponentPart {
    var text: String
}

@Serializable
data class RawQuery(
    override var text: String,
    var comments: List<RawComment>
) : RawComponentPart

@Serializable
data class RawConstraint(
    override var text: String,
    var comments: List<RawComment>
) : RawComponentPart

@Serializable
data class RawCommand(
    override var text: String,
    var comments: List<RawComment>
) : RawComponentPart

@Serializable
data class RawComponent(
    override val uid: Int,
    override var name: String,
    var abbrevName: String?,
    var explanation: String,
    var parts: List<RawComponentPart> = arrayListOf(),
    var comments: List<RawComment>
) : RawElement

@Serializable
data class RawEvents(
    override val uid: Int,
    override var name: String,
    var events: List<RawEvent> = arrayListOf(),
    var comments: List<RawComment>
) : RawElement

@Serializable
data class RawEvent(
    var id: String,
    var text: String,
    var comments: List<RawComment>
)

@Serializable
data class RawScenarios(
    override val uid: Int,
    override var name: String,
    var scenarios: List<RawScenario> = arrayListOf(),
    var comments: List<RawComment>
) : RawElement

@Serializable
data class RawScenario(
    var id: String,
    var text: String,
    var comments: List<RawComment>
)

@Serializable
data class RawRequirements(
    override val uid: Int,
    override var name: String,
    var requirements: List<RawRequirement>,
    var comments: List<RawComment>
) : RawElement

@Serializable
data class RawRequirement(
    var id: String,
    var text: String,
    var comments: List<RawComment>
)

@Serializable
data class RawIndexEntry(
    var key: String,
    var values: List<String>,
    var comments: List<RawComment>
)

@Serializable
data class RawSubsystem(
    override val uid: Int,
    override var name: String,
    var abbrevName: String?,
    var explanation: String,
    var indexing: List<RawIndexEntry>,
    var comments: List<RawComment>
) : RawElement

@Serializable
data class RawSystem(
    override val uid: Int,
    override var name: String,
    var explanation: String,
    var indexing: List<RawIndexEntry>,
    var comments: List<RawComment>
) : RawElement

@Serializable
data class RawSSL(
    var uid: Int,
    var elements: List<RawElement>,
    var relationShips: RawRelationships,
    var comments: List<RawComment>
)

@Serializable
sealed class RawRelation

@Serializable
data class RawInheritRelation(
    var name: String,
    var base: String
): RawRelation()

@Serializable
data class RawContainsRelation(
    var name: String,
    var parent: String
): RawRelation()

@Serializable
data class RawImplicitContainsRelation(
    var uid: Int,
    var parentUid: Int
): RawRelation()

@Serializable
data class RawClientRelation(
    var client: String,
    var provider: String
): RawRelation()


@Serializable
data class RawRelationships(
    private var _inheritRelations: MutableList<RawInheritRelation> = mutableListOf(),
    private var _containsRelations: MutableList<RawContainsRelation> = mutableListOf(),
    private var _implicitContainsRelations: MutableList<RawImplicitContainsRelation> = mutableListOf(),
    private var _clientRelations: MutableList<RawClientRelation> = mutableListOf()
) {
    val inheritRelations: List<RawInheritRelation>
        get() = _inheritRelations

    val containsRelations: List<RawContainsRelation>
        get() = _containsRelations

    val clientRelations: List<RawClientRelation>
        get() = _clientRelations

    val implicitContainsRelation: List<RawImplicitContainsRelation>
        get() = _implicitContainsRelations

    companion object {
        fun fromRelationList(relations: List<RawRelation>): RawRelationships {
            val result = RawRelationships()
            for (relation in relations) {
                when(relation) {
                    is RawInheritRelation -> result._inheritRelations.add(relation)
                    is RawContainsRelation -> result._containsRelations.add(relation)
                    is RawImplicitContainsRelation -> result._implicitContainsRelations.add(relation)
                    is RawClientRelation -> result._clientRelations.add(relation)
                }
            }
            return result
        }
    }
}


fun RawSSL.toJSON(): String {
    val sslModule = SerializersModule {
        polymorphic(RawElement::class) {
            RawSystem::class with RawSystem.serializer()
            RawSubsystem::class with RawSubsystem.serializer()
            RawComponent::class with RawComponent.serializer()
            RawEvents::class with RawEvents.serializer()
            RawScenarios::class with RawScenarios.serializer()
            RawRequirements::class with RawRequirements.serializer()
        }

        polymorphic(RawComponentPart::class) {
            RawQuery::class with RawQuery.serializer()
            RawConstraint::class with RawConstraint.serializer()
            RawCommand::class with RawCommand.serializer()
        }
    }

    val config = JsonConfiguration(prettyPrint = true)
    val json = Json(context = sslModule, configuration = config)

    return json.stringify(RawSSL.serializer(), this)
}