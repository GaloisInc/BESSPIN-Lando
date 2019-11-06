package com.galois.besspin.lando.ssl.parser

open class Element(open var name: String = "") {
    open operator fun component1(): String {
        return this.name
    }
}

data class Component(
    override var name: String = "",
    var inherit: List<String> = arrayListOf()
) : Element(name);

data class Subsystem(
    override var name: String = "",
    var inherit: List<String> = arrayListOf(),
    var components: List<Component> = arrayListOf()
) : Element(name);

data class System(
    override var name: String = "",
    var inherit: List<String> = arrayListOf(),
    var subsystems: List<Subsystem> = arrayListOf()
) : Element(name);

