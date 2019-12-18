package com.galois.besspin.lando.ssl.ast

import kotlinx.serialization.*
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration
import kotlinx.serialization.modules.SerializersModule

enum class RelationType {
    INHERIT, CLIENT, CONTAINS
}

interface Element {
    var name: String
}

interface ComponentPart {
    var text: String
}

@Serializable
data class Query(
    override var text: String = ""
) : ComponentPart;

@Serializable
data class Constraint(
    override var text: String = ""
) : ComponentPart;

@Serializable
data class Command(
    override var text: String = ""
) : ComponentPart;

@Serializable
data class Component(
    override var name: String = "",
    var inherits: List<String> = arrayListOf(),
    var parts: List<ComponentPart> = arrayListOf()
) : Element;

@Serializable
data class Events(
    override var name: String = "",
    var events: List<Event> = arrayListOf()
) : Element

@Serializable
data class Event(
    var id: String = "",
    var text: String = ""
)

@Serializable
data class Scenarios(
    override var name: String = "",
    var scenarios: List<Scenario> = arrayListOf()
) : Element

@Serializable
data class Scenario(
    var id: String = "",
    var text: String = ""
)

@Serializable
data class Requirements(
    override var name: String = "",
    var requirements: List<Requirement> = arrayListOf()
) : Element

@Serializable
data class Requirement(
    var id: String = "",
    var text: String = ""
)

@Serializable
data class Subsystem(
    override var name: String = "",
    var description: String = "",
    var inherits: List<String> = arrayListOf(),
    var indexing: Map<String, List<String>> = mapOf(),
    var components: List<Component> = arrayListOf()
) : Element

@Serializable
data class System(
    override var name: String = "",
    var description: String = "",
    var inherits: List<String> = arrayListOf(),
    var indexing: Map<String, List<String>> = mapOf(),
    var subsystems: List<Subsystem> = arrayListOf()
) : Element

@Serializable
data class SSL(
    var elements: List<Element> = arrayListOf()
)


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