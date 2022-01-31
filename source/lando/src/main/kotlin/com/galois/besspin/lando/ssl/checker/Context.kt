package com.galois.besspin.lando.ssl.checker

import com.galois.besspin.lando.ssl.ast.RawElement
import com.galois.besspin.lando.ssl.ast.Name

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
class Context(es: List<RawElement> = listOf()) {
    var ctx = es.toMutableList()

    fun addElement(e : RawElement) {
        ctx.add(e)
    }

    /**
    fun toMap() : Map<Name, RawElement> {
        return ctx.associateBy({ it.first }, { it.second })
    }
    */
}