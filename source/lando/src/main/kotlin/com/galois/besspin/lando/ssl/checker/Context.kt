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

class TextType(
    override val uid: Int,
    override val pos: RawPos,
    val name: Name,
): RawElement


class Context(es: List<Pair<Name, RawElement>> = listOf()) {
    var ctx = es.toMutableList()

    fun addSystem(e: RawSystem) {
        if (e.abbrevName != null) {
            ctx.add(Pair(e.abbrevName, e))
        }
        ctx.add(Pair(e.name, e))
        /** TODO: introduce text as a type -- how to do that with the Raw types?*/
        ctx.add(Pair(e.explanation, e))
    }

    fun addSubsystem(e: RawSubsystem) {
        if (e.abbrevName != null) {
            ctx.add(Pair(e.abbrevName, e))
        }
        ctx.add(Pair(e.name, e))
        /** TODO: introduce text as a type -- how to do that with the Raw types?*/
        ctx.add(Pair(e.explanation, e))
    }

    /**
     * Text Type Introduction
     *
     * Text gets introduced to the context as its own type
     */
    fun addTextType(e: String, elem: RawElement) {
        // TODO get the right position
        ctx.add(Pair("<TextType${elem.uid}-${e}>", TextType(
            uid = elem.uid,
            pos = elem.pos,
            name = e
        )))
    }

    /**
     * Text Type Introduction for Component Part
     *
     * TODO: what to do with UID?
     */
    fun addTextTypeComponentPart(e: String, elem: RawComponentPart) {
        // TODO get the right position
        ctx.add(Pair("<TextType${elem.pos}-${e}>", TextType(
            uid = 0,
            pos = elem.pos,
            name = e
        )))
    }

    fun addSubsystemImport(e: RawSubsystemImport) {
        TODO()
    }

    fun addComponent(e: RawComponent) {
        TODO()
    }

    fun toMap(): Map<Name, RawElement> {
        return ctx.associateBy({ it.first }, { it.second })
    }

    /**
     * qualified name resolution
     *
     * equivalent to Gamma(n)
     * This is wrong
     */
    fun qLook(qname: QName, phi: ElementMap): RawElement? {
        /** if size is one qualified name exists in current context */
        if (qname.size == 1) {
            val res = ctx.filter { it.first in qname }.map { it.second }
            if (res.size != 1) {
                return null
            } else {
                return res[0]
            }
        } else {
            /** if size > 1, then the qualified name exists in another context Phi(qname[0]) */
            return phi[qname[0]]!!.qLook(qname.slice(1 until qname.size), phi)
        }
    }
}