@file:Suppress("unused")

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
    object NullElement : QNameReturn()

    /** could resolve to multiple things */
    class MultipleElement(
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

    fun addConstraint(@Suppress("UNUSED_PARAMETER") e: RawConstraint) {
        TODO()
    }

    fun addQuery(@Suppress("UNUSED_PARAMETER") e: RawQuery) {
        TODO()
    }

    fun addCommand(@Suppress("UNUSED_PARAMETER") e: RawCommand) {
        TODO()
    }

    fun addComponentImport(e: RawComponentImport) {
        if (e.abbrevName == null) {
            ctx.add(Pair(e.name.last(), e))
        } else {
            ctx.add(Pair(e.abbrevName, e))
        }
    }

    fun addRelation(@Suppress("UNUSED_PARAMETER") e: RawRelation) {
        /** do nothing... (keep this way for interface consistency) */
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

    fun addSubsystemImport(@Suppress("UNUSED_PARAMETER") e: RawSubsystemImport) {
    }

    fun addEvents(@Suppress("UNUSED_PARAMETER") e: RawEvents) {
        TODO()
    }

    fun addScenarios(@Suppress("UNUSED_PARAMETER") e: RawScenarios) {
        TODO()
    }

    fun addRequirements(@Suppress("UNUSED_PARAMETER") e: RawRequirements) {
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
        return if (qname.size == 1) {
            val res = ctx.filter { it.first in qname }.map { it.second }
            when (res.size) {
                1 -> QNameReturn.ResolvedElement(res[0])
                0 -> QNameReturn.NullElement
                else -> QNameReturn.MultipleElement(res)
            }
        } else {
            /** if size > 1, then the qualified name exists in another context Phi(qname[0]) */
            val ret = qLook(listOf(qname[0]), phi)
            if (ret is QNameReturn.ResolvedElement) {
                val context = phi[ret.element] ?: throw IllegalStateException("${qname[0]} could not resolve to a context")
                context.qLook(qname.slice(1 until qname.size), phi)
            } else {
                throw IllegalStateException("${qname[0]} could not resolve to a context")
            }
        }
    }
}