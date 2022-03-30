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
) : RawElement


/**
 * Qualified Name Resolver Return
 *
 * Resolver is not guaranteed to return a resolved element. this data type captures
 * those errors without requiring qlook to throw an exception.
 */
sealed class QNameReturn {
    /** the element passed checking */
    class ResolvedElement(
        val element: RawElement
    ) : QNameReturn()

    /** could not resolve to anything */
    class NullElement(
        val qname: String
    ) : QNameReturn()

    /** could resolve to multiple things */
    class MultipleElement(
        val qname: String,
        val elements: List<RawElement>
    ) : QNameReturn()
}


class Context(es: List<Pair<Name, RawElement>> = listOf()) {
    var ctx = es.toMutableList()

    fun addSystem(e: RawSystem) {
        if (e.abbrevName != null) {
            ctx.add(Pair(e.abbrevName, e))
        }
        ctx.add(Pair(e.name, e))
        addTextType(e.explanation, e)
    }

    fun addSubsystem(e: RawSubsystem) {
        if (e.abbrevName != null) {
            ctx.add(Pair(e.abbrevName, e))
        }
        ctx.add(Pair(e.name, e))
        addTextType(e.explanation, e)
    }

    fun addConstraint(e: RawConstraint) {
        TODO()
    }

    fun addQuery(e: RawQuery) {
        TODO()
    }

    fun addCommand(e: RawCommand) {
        TODO()
    }

    fun addComponentImport(e: RawComponentImport) {
        if (e.abbrevName == null) {
            ctx.add(Pair(e.name.last(), e))
        } else {
            ctx.add(Pair(e.abbrevName, e))
        }
    }

    fun addRelation(e: RawRelation) {
        /** do nothing... */
    }

    /**
     * Text Type Introduction
     *
     * Text gets introduced to the context as its own type
     */
    fun addTextType(e: String, elem: RawElement) {
        // TODO get the right position
        ctx.add(
            Pair(
                "<TextType${elem.uid}-${e}>", TextType(
                    uid = elem.uid,
                    pos = elem.pos,
                    name = e
                )
            )
        )
    }

    /**
     * Text Type Introduction for Component Part
     *
     * TODO: what to do with UID?
     */
    fun addTextTypeComponentPart(e: String, elem: RawComponentPart) {
        // TODO get the right position
        ctx.add(
            Pair(
                "<TextType${elem.pos}-${e}>", TextType(
                    uid = 0,
                    pos = elem.pos,
                    name = e
                )
            )
        )
    }

    fun addSubsystemImport(e: RawSubsystemImport) {
        TODO()
    }

    fun addEvents(e: RawEvents) {
        TODO()
    }

    fun addScenarios(e: RawScenarios) {
        TODO()
    }

    fun addRequirements(e: RawRequirements) {
        TODO()
    }

    fun addComponent(e: RawComponent) {
        ctx.add(Pair(e.name, e))
        if (e.abbrevName != null) {
            ctx.add(Pair(e.abbrevName, e))
        }
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
    fun qLook(qname: QName, phi: ElementMap): QNameReturn {
        /** if size is one qualified name exists in current context */
        if (qname.size == 1) {
            val res = ctx.filter { it.first in qname }.map { it.second }
            when (res.size) {
                1 -> return QNameReturn.ResolvedElement(res[0])
                0 -> return QNameReturn.NullElement(qname[0])
                else -> return QNameReturn.MultipleElement(qname[0], res)
            }
        } else {
            /** if size > 1, then the qualified name exists in another context Phi(qname[0]) */
            val context = phi[qname[0]]
            if (context == null) {
                throw IllegalStateException("${qname[0]} could not resolve to a context")
            }
            return context.qLook(qname.slice(1 until qname.size), phi)
        }
    }
}