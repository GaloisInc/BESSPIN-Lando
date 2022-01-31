package com.galois.besspin.lando.ssl.checker

import com.galois.besspin.lando.ssl.ast.*

/**
 * Context for Type Raw Checking
 *
 * From the well-formedness document, we see that a list es implies a context, but
 * a context is also a mapping from name -> elem being Name and RawElement in this case.
 * A context can be represented as a list or map, we can convert from and to.
 *
 * When interpreting for type check, we can introduce elements to the context, supported
 * by addElements and addElement interface. An element contains its own name, so the necessary
 * relation is implied by the object.
 *
 */
class Context(es: List<Pair<Name, RawElement>> = listOf()) {
    var ctx = es.toMutableList()

    fun addSystem(e : RawSystem) {
        if (e.abbrevName != null) {
            ctx.add(Pair(e.abbrevName, e))
        }
        ctx.add(Pair(e.name, e))
    }

    fun addSubsystem(e : RawSubsystem) {
        if (e.abbrevName != null) {
            ctx.add(Pair(e.abbrevName, e))
        }
        ctx.add(Pair(e.name, e))
    }

    fun addSubsystemImport(e : RawSubsystemImport) {
        TODO()
    }

    fun addComponent(e : RawComponent) {
        TODO()
    }

    fun toMap() : Map<Name, RawElement> {
        return ctx.associateBy({ it.first }, { it.second })
    }

    /**
     * qualified name resolution
     *
     * equivalent to Gamma(n)
     */
    fun qLook(qname : QName) : RawElement? {
        val res = ctx.filter { it.first in qname }.map { it.second }
        if (res.size != 1) {
            return null
        } else {
            return res[0]
        }
    }
}