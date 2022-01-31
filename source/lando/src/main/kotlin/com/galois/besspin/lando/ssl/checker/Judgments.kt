package com.galois.besspin.lando.ssl.checker

import com.galois.besspin.lando.ssl.ast.*

/**
 * RawAst Well-Formedness Judgments
 */
class Judgments {
    fun isValidTopLevel(element: RawElement) : Boolean {
        return (element is RawSystem) || (element is RawSubsystem) || (element is RawComponent) ||
                (element is RawEvents) || (element is RawScenarios) || (element is RawRequirements) ||
                (element is RawRelation)
    }

    fun isValidContains(parent : RawElement, child : RawElement) : Boolean {
        return when {
            (parent is RawSystem) ->
                ((child is RawSubsystem) || (child is RawSubsystemImport) || (child is RawRelation))
            (parent is RawSubsystem) ->
                ((child is RawSubsystem) || (child is RawSubsystemImport) ||
                        (child is RawComponent) || (child is RawComponentImport)||
                        (child is RawRelation) || (child is RawEvents) || (child is RawRequirements ||
                        (child is RawScenarios)))
            else -> false
        }
    }

    fun isValidClient(parent : RawElement, child : RawElement) : Boolean {
        return when {
            ((parent is RawSubsystem) || (parent is RawSubsystemImport)) ->
                ((child is RawSubsystem) || (child is RawSubsystemImport) ||
                        (child is RawComponent) || (child is RawComponentImport))
            ((parent is RawComponent) || (parent is RawComponentImport)) ->
                ((child is RawSubsystem) || (child is RawSubsystemImport) ||
                        (child is RawComponent) || (child is RawComponentImport))
            else -> false
        }
    }

    fun checkSource(source : List<RawElement>) : Boolean {
        /** precond: the source implies a context */
        /** TODO: check the the context is well-formed */
        val gamma0 = Context()
        for (elem in source) {
            when {
                (elem is RawSystem) -> {checkIntroduceSystem(elem); gamma0.addSystem(elem)}
                (elem is RawSubsystem) -> {checkIntroduceSubsystem(gamma0, elem); gamma0.addSubsystem(elem)}
            }
        }

        /** precond: all elements referenced in the source are top level elements */
        for (elem in source) {
            assert(isValidTopLevel(elem))
        }

        /** precond: noCycles in the inheritance relations */
        /** TODO: implement this */

        /** precond: is any two elements are systems, they must be the same system -- (what is the equality here) */
        /** TODO: is this the right way of doing equality here */
        for (e1 in source) {
            for (e2 in source) {
                if (e1 is RawSystem && e2 is RawSystem) {
                    assert(e1 == e2)
                }
            }
        }

        return true
    }

    /**
     * Gamma |- e => Gamma'  Gamma |- es => Gamma''
     * --------------------------------------------
     * Gamma |- e :: es => Gamma' DisjUnion Gamma''
     */
    fun checkIntroduceElement() : Boolean {
        TODO()
    }

    /**
     * -----------------------
     * Gamma |- []_elem => {}
     */
    fun check_empty_context() : Boolean {
        TODO()
    }

    /**
     * n != na Gamma |- t \forall e \in esb, valid-contains(es, e) \Phi_o(es) = \Gamma' \Gamma' <| Gamma |- esb => Gamma
     * -----------------------------------------------------------------------------------------------------------------
     * Gamma |- e_s @ System{name = n, abbrev = |na|, explanation=t, body=esb, ...} => {n |-> es, na |-> es}
     */
    fun checkIntroduceSystem(element: RawSystem) : Boolean {
        TODO()
    }

    fun checkIntroduceSubsystem(currentContext : Context, element: RawSubsystem) : Boolean {
        /** precond: if abbrev name is defined, it must not equal the system's name */
        if (element.abbrevName != null) {
            assert(element.abbrevName == element.name)
        }

        /** precond: all elements in the subsystem body must be a valid contains type */
        if (element.body != null) {
            for (elem in element.body!!.toList()) {
                isValidContains(element, elem)
            }
        }

        /** precond: all clients referenced are of the valid type */
        for (q in element.clientOf) {
            isValidClient(element, currentContext.qLook(q)!!)
        }

        /** precond: all parents referenced are of the valid type */
        /** TODO: this field doesn't exist!? */

        return true
    }
}