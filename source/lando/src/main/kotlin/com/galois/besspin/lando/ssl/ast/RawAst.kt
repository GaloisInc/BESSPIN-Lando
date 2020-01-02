package com.galois.besspin.lando.ssl.ast

import kotlinx.serialization.*
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration
import kotlinx.serialization.modules.SerializersModule

enum class RelationType {
    INHERIT, CLIENT, CONTAINS
}

@Serializable
data class Comment(
    var text: String
)

interface Element {
    val uid: Int
    var name: String
}

interface ComponentPart {
    var text: String
}

@Serializable
data class Query(
    override var text: String,
    var comments: List<Comment>
) : ComponentPart

@Serializable
data class Constraint(
    override var text: String,
    var comments: List<Comment>
) : ComponentPart;

@Serializable
data class Command(
    override var text: String,
    var comments: List<Comment>
) : ComponentPart;

@Serializable
data class Component(
    override val uid: Int,
    override var name: String,
    var parts: List<ComponentPart> = arrayListOf(),
    var comments: List<Comment>
) : Element;

@Serializable
data class Events(
    override val uid: Int,
    override var name: String,
    var events: List<Event> = arrayListOf(),
    var comments: List<Comment>
) : Element

@Serializable
data class Event(
    var id: String,
    var text: String,
    var comments: List<Comment>
)

@Serializable
data class Scenarios(
    override val uid: Int,
    override var name: String,
    var scenarios: List<Scenario> = arrayListOf(),
    var comments: List<Comment>
) : Element

@Serializable
data class Scenario(
    var id: String,
    var text: String,
    var comments: List<Comment>
)

@Serializable
data class Requirements(
    override val uid: Int,
    override var name: String,
    var requirements: List<Requirement>,
    var comments: List<Comment>
) : Element

@Serializable
data class Requirement(
    var id: String,
    var text: String,
    var comments: List<Comment>
)

@Serializable
data class IndexEntry(
    var key: String,
    var values: List<String>,
    var comments: List<Comment>
)

@Serializable
data class Subsystem(
    override val uid: Int,
    override var name: String,
    var description: String,
    var indexing: List<IndexEntry>,
    var comments: List<Comment>
) : Element

@Serializable
data class System(
    override val uid: Int,
    override var name: String,
    var description: String,
    var indexing: List<IndexEntry>,
    var comments: List<Comment>
) : Element

@Serializable
data class SSL(
    var uid: Int,
    var elements: List<Element>,
    var relationShips: Relationships,
    var comments: List<Comment>
)

@Serializable
sealed class Relation

@Serializable
data class InheritRelation(
    var name: String = "",
    var base: String = ""
): Relation()

@Serializable
data class ContainsRelation(
    var name: String = "",
    var parent: String = ""
): Relation()

@Serializable
data class ClientRelation(
    var name: String,
    var client: String
): Relation()


@Serializable
data class Relationships(
    private var _inheritRelations: MutableList<InheritRelation> = mutableListOf(),
    private var _containsRelations: MutableList<ContainsRelation> = mutableListOf(),
    private var _clientRelations: MutableList<ClientRelation> = mutableListOf()
) {
    val inheritRelations: List<InheritRelation>
        get() = _inheritRelations

    val containsRelations: List<ContainsRelation>
        get() = _containsRelations

    val clientRelations: List<ClientRelation>
        get() = _clientRelations

    companion object {
        fun fromRelationList(relations: List<Relation>): Relationships {
            val result = Relationships()
            for (relation in relations) {
                when(relation) {
                    is InheritRelation -> result._inheritRelations.add(relation)
                    is ContainsRelation -> result._containsRelations.add(relation)
                    is ClientRelation -> result._clientRelations.add(relation)
                }
            }
            return result
        }
    }
}


fun SSL.toJSON(): String {
    val sslModule = SerializersModule {
        polymorphic(Element::class) {
            System::class with System.serializer()
            Subsystem::class with Subsystem.serializer()
            Component::class with Component.serializer()
            Events::class with Events.serializer()
            Scenarios::class with Scenarios.serializer()
            Requirements::class with Requirements.serializer()
        }

        polymorphic(ComponentPart::class) {
            Query::class with Query.serializer()
            Constraint::class with Constraint.serializer()
            Command::class with Command.serializer()
        }
    }

    val config = JsonConfiguration(prettyPrint = true)
    val json = Json(context = sslModule, configuration = config)

    return json.stringify(SSL.serializer(), this)
}