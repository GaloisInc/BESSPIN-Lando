package com.galois.besspin.lando.ssl.checker
import com.galois.besspin.lando.ssl.ast.*

typealias ElementMap = MutableMap<RawElement, Context>

/**
 * RawAst Well-Formedness Judgments
 */
class Judgments {
    fun getCheckStatus(identifier : String, elements: List<RawElement>, message : String, preconds : List<CheckStatus>) : CheckStatus {
        if (preconds.all { it is CheckStatus.Ok }) {
            return CheckStatus.Ok(identifier = identifier, preconds = preconds)
        } else {
            return CheckStatus.Error(identifier = identifier, elements = elements, message = message, preconds = preconds)
        }
    }

    fun checkNameAbbrev(name : String, abbrev : String, element: RawElement) : CheckStatus {
        if(name != abbrev) {
            return CheckStatus.Ok("validNameAbbrev")
        } else {
            return CheckStatus.Error("validNameAbbrev", listOf(element), "TODO", listOf())
        }
    }

    fun checkValidTopLevel(element: RawElement) : CheckStatus {
        val isTopLevel = (element is RawSystem) || (element is RawSubsystem) || (element is RawComponent) ||
                (element is RawEvents) || (element is RawScenarios) || (element is RawRequirements) ||
                (element is RawRelation)
        if (isTopLevel) {
            return CheckStatus.Ok("validTopLevel")
        } else {
            return CheckStatus.Error("validTopLevel", listOf(element), "TODO")
        }
    }

    fun checkValidContains(parent : RawElement, child : RawElement) : CheckStatus {
        val isValidContains = when {
            (parent is RawSystem) ->
                ((child is RawSubsystem) || (child is RawSubsystemImport) || (child is RawRelation))
            (parent is RawSubsystem) ->
                ((child is RawSubsystem) || (child is RawSubsystemImport) ||
                        (child is RawComponent) || (child is RawComponentImport)||
                        (child is RawRelation) || (child is RawEvents) || (child is RawRequirements ||
                        (child is RawScenarios)))
            else -> false
        }
        if (isValidContains) {
            return CheckStatus.Ok("validSystemContains")
        } else {
            return CheckStatus.Error("validSystemContains", listOf(parent, child), "TODO")
        }
    }

    fun checkValidClient(parent : RawElement, child : RawElement) : CheckStatus {
        val isValidClient = when {
            ((parent is RawSubsystem) || (parent is RawSubsystemImport)) ->
                ((child is RawSubsystem) || (child is RawSubsystemImport) ||
                        (child is RawComponent) || (child is RawComponentImport))
            ((parent is RawComponent) || (parent is RawComponentImport)) ->
                ((child is RawSubsystem) || (child is RawSubsystemImport) ||
                        (child is RawComponent) || (child is RawComponentImport))
            else -> false
        }
        if (isValidClient) {
            return CheckStatus.Ok("validClient")
        } else {
            return CheckStatus.Error("validClient", listOf(parent, child), "TODO")
        }
    }

    fun checkIntroduceElements(gamma : Context, phi : ElementMap, es : List<RawElement>) : CheckStatus {
        var res = mutableListOf<CheckStatus>()

        for (elem in es) {
            when {
                (elem is RawSystem) -> {
                    /** this introduction involves a new context */
                    val gammap = Context();
                    res.add(checkIntroduceSystem(gammap, phi, elem));
                    gamma.addSystem(elem);
                    phi.put(elem, gammap);
                }
                (elem is RawSubsystem) -> {
                    /** this introduction involves a new context */
                    val gammap = Context();
                    res.add(checkIntroduceSubsystem(gammap, phi, elem));
                    gamma.addSubsystem(elem);
                    phi.put(elem, gammap)
                }
                else -> {
                    /* TODO: this should be a valid rule, but Element has no name so it doesn't imply a
                    * valid element by the document standards
                    gamma.addElement(elem);
                    phi.put(elem, gamma)
                    */
                }
            }
        }
        return getCheckStatus("validElementsList", listOf(), "TODO", res)
    }

    fun checkSource(source : List<RawElement>) : CheckStatus {
        var res = mutableListOf<CheckStatus>()

        /** precond: the source implies a valid context */
        val gamma0 = Context()
        var phi0 = mutableMapOf<RawElement, Context>()
        res.add(checkIntroduceElements(gamma0, phi0, source))

        /** precond: all elements referenced in the source are top level elements */
        for (elem in source) {
            res.add(checkValidTopLevel(elem))
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
        return getCheckStatus("validSource", listOf(), "TODO", res)
    }

    /**
     * n != na Gamma |- t \forall e \in esb, valid-contains(es, e) \Phi_o(es) = \Gamma' \Gamma' <| Gamma |- esb => Gamma
     * -----------------------------------------------------------------------------------------------------------------
     * Gamma |- e_s @ System{name = n, abbrev = |na|, explanation=t, body=esb, ...} => {n |-> es, na |-> es}
     */
    fun checkIntroduceSystem(currentContext: Context, phi : ElementMap, element: RawSystem) : CheckStatus {
        var res = mutableListOf<CheckStatus>()

        /** precond: if abbrev name is defined, it must not equal the elements name */
        if (element.abbrevName != null) {
            res.add(checkNameAbbrev(element.name, element.abbrevName, element))
        }

        /** precond: all elements in the subsystem body must imply a valid context and be a valid contains type */
        if (element.body != null) {
            checkIntroduceElements(currentContext, phi, element.body!!)
            for (elem in element.body!!.toList()) {
                res.add(checkValidContains(element, elem))
            }
        }
        return getCheckStatus("validSystem", listOf(), "TODO", res)
    }

    fun checkIntroduceSubsystem(currentContext : Context, phi : ElementMap, element: RawSubsystem) : CheckStatus {
        var res = mutableListOf<CheckStatus>()

        /** precond: if abbrev name is defined, it must not equal the elements name */
        if (element.abbrevName != null) {
            res.add(checkNameAbbrev(element.name, element.abbrevName, element))
        }

        /** precond: all elements in the subsystem body must imply a valid context and be a valid contains type */
        if (element.body != null) {
            checkIntroduceElements(currentContext, phi, element.body!!)
            for (elem in element.body!!.toList()) {
                res.add(checkValidContains(element, elem))
            }
        }

        /** precond: all clients referenced are of the valid type */
        for (q in element.clientOf) {
            res.add(checkValidClient(element, currentContext.qLook(q)!!))
        }

        /** precond: all parents referenced are of the valid type */
        /** TODO: this field doesn't exist!? */
        return getCheckStatus("validSubsystem", listOf(), "TODO", res)
    }
}