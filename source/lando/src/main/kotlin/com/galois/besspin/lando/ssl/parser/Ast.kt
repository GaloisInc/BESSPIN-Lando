package com.galois.besspin.lando.ssl.parser

enum class RelationType {
    INHERIT, CLIENT, CONTAINS
}

open class Element(open var name: String = "") {
    open operator fun component1(): String {
        return this.name
    }
}

interface ComponentPart {
    var text: String ;
} ;

data class Query(
    override var text: String = ""
) : ComponentPart ;

data class Constraint(
    override var text: String = ""
) : ComponentPart ;

data class Command(
    override var text: String = ""
) : ComponentPart ;

data class Component(
    override var name: String = "",
    var inherits: List<String> = arrayListOf(),
    var parts: List<ComponentPart> = arrayListOf()
) : Element(name);

data class Events(
    override var name: String = "",
    var events: List<Event> = arrayListOf()
) : Element(name)

data class Event(
    var id: String = "",
    var text: String = ""
)

data class Scenarios(
    override var name: String = "",
    var scenarios: List<Scenario> = arrayListOf()
) : Element(name)

data class Scenario(
    var id: String = "",
    var text: String = ""
)

data class Requirements(
    override var name: String = "",
    var requirements: List<Requirement> = arrayListOf()
) : Element(name)

data class Requirement(
    var id: String = "",
    var text: String = ""
)

data class Subsystem(
    override var name: String = "",
    var description: String = "",
    var inherits: List<String> = arrayListOf(),
    var components: List<Component> = arrayListOf()
) : Element(name);

data class System(
    override var name: String = "",
    var description: String = "",
    var inherits: List<String> = arrayListOf(),
    var subsystems: List<Subsystem> = arrayListOf()
) : Element(name);

data class SSL(
    var elements: List<Element> = arrayListOf()
)
