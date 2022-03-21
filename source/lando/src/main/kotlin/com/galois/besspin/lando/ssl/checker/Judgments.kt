package com.galois.besspin.lando.ssl.checker

import com.galois.besspin.lando.ssl.ast.*

typealias ElementMap = MutableMap<RawElement, Context>

/**
 * RawAst Well-Formedness Judgments
 */
class Judgments {
    /**
     * For judgments with preconditions, determine if it should be Ok if all preconds are also OK
     * No judgment with a precondition Error can be Ok
     */
    fun getCheckStatus(
        identifier: String,
        elements: List<RawElement>,
        message: String,
        preconds: List<CheckStatus>
    ): CheckStatus {
        if (preconds.all { it is CheckStatus.Ok }) {
            return CheckStatus.Ok(identifier = identifier, preconds = preconds)
        } else {
            return CheckStatus.Error(
                identifier = identifier,
                elements = elements,
                message = message,
                preconds = preconds
            )
        }
    }

    /**
     * Judgment: abbreviations cannot equal the name identifier
     */
    fun checkNameAbbrev(name: String, abbrev: String, element: RawElement): CheckStatus {
        if (name != abbrev) {
            return CheckStatus.Ok("validNameAbbrev")
        } else {
            return CheckStatus.Error("validNameAbbrev", listOf(element), "TODO", listOf())
        }
    }

    /**
     * Judgment: top level elements is a subset of types
     */
    fun checkValidTopLevel(element: RawElement): CheckStatus {
        val isTopLevel = (element is RawSystem) || (element is RawSubsystem) || (element is RawComponent) ||
                (element is RawEvents) || (element is RawScenarios) || (element is RawRequirements) ||
                (element is RawRelation)
        if (isTopLevel) {
            return CheckStatus.Ok("validTopLevel")
        } else {
            return CheckStatus.Error("validTopLevel", listOf(element), "TODO")
        }
    }

    /**
     * Judgment: for a system or subsystem, the contains field body can only have a subset of types
     */
    fun checkValidContains(parent: RawElement, child: RawElement): CheckStatus {
        val isValidContains = when {
            (parent is RawSystem) ->
                ((child is RawSubsystem) || (child is RawSubsystemImport) || (child is RawRelation))
            (parent is RawSubsystem) ->
                ((child is RawSubsystem) || (child is RawSubsystemImport) ||
                        (child is RawComponent) || (child is RawComponentImport) ||
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

    /**
     * Judgment: for a subsystem/component (import), the client field body can only have a subset of types
     */
    fun checkValidClient(parent: RawElement, child: RawElement): CheckStatus {
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
            return CheckStatus.Error("validClient", listOf(parent, child), "child fails necessary element type ${child.javaClass.name} for parent ${child.javaClass.name}")
        }
    }

    /**
     * Judgment: an inherit must be two components
     */
    fun checkValidInherit(parent: RawElement, child: RawElement): CheckStatus {
        return when (parent is RawComponent && child is RawComponent) {
            true -> CheckStatus.Ok("validInherit")
            else -> CheckStatus.Error("validClient", listOf(parent, child), "parent ${parent.javaClass.name} and child ${child.javaClass.name} are not both components")
        }
    }

    /**
     * Judgement: when introducing a list of elements (body), they imply a valid context
     */
    fun checkIntroduceElements(gamma: Context, phi: ElementMap, relation: Relation, es: List<RawElement>): CheckStatus {
        var res = mutableListOf<CheckStatus>()

        for (elem in es) {
            when {
                (elem is RawSystem) -> {
                    /** this introduction involves a new context */
                    res.add(checkIntroduceSystem(gamma, phi, relation, elem));
                    //gamma.addSystem(elem);
                }
                (elem is RawSubsystem) -> {
                    /** this introduction involves a new context */
                    res.add(checkIntroduceSubsystem(gamma, phi, relation, elem));
                    //gamma.addSubsystem(elem);
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

    /**
     * Judgement: a source and its body are valid
     */
    fun checkSource(source: List<RawElement>): CheckStatus {
        var res = mutableListOf<CheckStatus>()

        /** precond: the source implies a valid context */
        val gamma0 = Context()
        var phi0 = mutableMapOf<RawElement, Context>()
        var relationI = Relation()
        res.add(checkIntroduceElements(gamma0, phi0, relationI, source))

        /** precond: all elements referenced in the source are top level elements */
        for (elem in source) {
            res.add(checkValidTopLevel(elem))
        }

        /** precond: noCycles in the inheritance relations */
        if (relationI.hasNoCycles()) {
            res.add(CheckStatus.Ok("validInheritance"))
        } else {
            res.add(CheckStatus.Error("validInheritance", listOf(), "TODO"))
        }

        /** precond: is any two elements are systems, they must be the same system -- (what is the equality here) */
        /** TODO: is this the right way of doing equality here */
        for (e1 in source) {
            for (e2 in source) {
                if (e1 is RawSystem && e2 is RawSystem) {
                    if (e1 == e2) {
                        res.add(CheckStatus.Ok("validSystemEquiv", listOf()))
                    } else {
                        res.add(CheckStatus.Error("validSystemEqiv", listOf(e1, e2), "TODO", listOf()))
                    }
                }
            }
        }
        return getCheckStatus("validSource", listOf(), "TODO", res)
    }

    /**
     * Judgment: a system is properly introduced
     */
    fun checkIntroduceSystem(
        currentContext: Context,
        phi: ElementMap,
        relation: Relation,
        element: RawSystem
    ): CheckStatus {
        var res = mutableListOf<CheckStatus>()

        /** precond: if abbrev name is defined, it must not equal the elements name */
        if (element.abbrevName != null) {
            res.add(checkNameAbbrev(element.name, element.abbrevName, element))
        }

        /** introduce explanation as a type */
        currentContext.addTextType(element.explanation, element)

        /** precond: all elements in the system body must imply a valid context and be a valid contains type */
        if (element.body != null) {
            checkIntroduceElements(currentContext, phi, relation, element.body!!)
            for (elem in element.body!!.toList()) {
                res.add(checkValidContains(element, elem))
            }
        }

        /** relate element to a local context */
        val gammap = Context();
        phi.put(element, gammap)

        /** introduce body to the local context */
        if (element.body != null) res.add(checkIntroduceElements(gammap, phi, relation, element.body!!))

        /** now add it to the context */
        currentContext.addSystem(element)

        return getCheckStatus("validSystem", listOf(), "${element.name} is not a valid system", res)
    }

    /**
     * Judgment: a subsystem is properly introduced
     */
    fun checkIntroduceSubsystem(
        currentContext: Context,
        phi: ElementMap,
        relation: Relation,
        element: RawSubsystem
    ): CheckStatus {
        var res = mutableListOf<CheckStatus>()

        /** precond: if abbrev name is defined, it must not equal the elements name */
        if (element.abbrevName != null) {
            res.add(checkNameAbbrev(element.name, element.abbrevName, element))
        }

        /** introduce explanation as a type */
        currentContext.addTextType(element.explanation, element)

        /** precond: all elements in the subsystem body must imply a valid context and be a valid contains type */
        if (element.body != null) {
            checkIntroduceElements(currentContext, phi, relation, element.body!!)
            for (elem in element.body!!.toList()) {
                res.add(checkValidContains(element, elem))
            }
        }

        /** precond: all inherits must be valid inherits */
        // TODO: there is no inherit

        /** precond: all clients referenced are of the valid type */
        for (q in element.clientOf) {
            // TODO: handle qlook null pointer exception
            res.add(checkValidClient(element, currentContext.qLook(q, phi)!!))
            relation.addRelation(
                Pair(currentContext.qLook(q, phi)!!, element)
            )
        }

        /** relate element to a local context */
        val gammap = Context();
        phi.put(element, gammap)

        /** introduce body to the local context */
        if (element.body != null) res.add(checkIntroduceElements(gammap, phi, relation, element.body!!))

        /** now add it to the context */
        currentContext.addSubsystem(element)

        /** precond: all parents referenced are of the valid type */
        /** TODO: this field doesn't exist!? */
        return getCheckStatus("validSubsystem", listOf(), "${element.name} is not a valid subsystem", res)
    }

    /**
     * Judgment: a subsystem import is properly introduced
     */
    fun checkIntroduceSubsystemImport(
        currentContext: Context,
        phi: ElementMap,
        relation: Relation,
        element: RawSubsystemImport
    ): CheckStatus {
        var res = mutableListOf<CheckStatus>()

        /** precond: all clients referenced are of the valid type */
        for (q in element.clientOf) {
            // TODO: handle qlook null pointer exception
            res.add(checkValidClient(element, currentContext.qLook(q, phi)!!))
            relation.addRelation(
                Pair(currentContext.qLook(q, phi)!!, element)
            )
        }

        /** precond: import resolves to a qualified named element */
        res.add(
            when (currentContext.qLook(element.name, phi) is RawSubsystem) {
                true -> CheckStatus.Ok("validImportedSubsystem", listOf())
                else -> CheckStatus.Error(
                    "validImportedSubsystem",
                    listOf(element),
                    "could not resolve subsystem '${element.name}' import to a subsystem",
                    listOf()
                )
            }
        )

        /** now add it to the context */
        currentContext.addSubsystemImport(element)

        return getCheckStatus("validSubsystemImport", listOf(), "${element.name} is not a valid subsystem import", res)
    }


    /**
     * Judgment: a component is properly introduced
     */
    fun checkIntroduceComponent(
        currentContext: Context,
        phi: ElementMap,
        relation: Relation,
        element: RawComponent
    ): CheckStatus {
        var res = mutableListOf<CheckStatus>()

        /** precond: if abbrev name is defined, it must not equal the elements name */
        if (element.abbrevName != null) {
            res.add(checkNameAbbrev(element.name, element.abbrevName, element))
        }

        /** introduce explanation as a type */
        currentContext.addTextType(element.explanation, element)

        /** precond: all clients referenced are of the valid type */
        for (q in element.clientOf) {
            // TODO: handle qlook null pointer exception
            res.add(checkValidClient(element, currentContext.qLook(q, phi)!!))
            relation.addRelation(
                Pair(currentContext.qLook(q, phi)!!, element)
            )
        }

        /** precond: all inherits must be valid inherits */
        for (q in element.inherits) {
            // TODO: handle qlook null pointer exception
            res.add(checkValidClient(element, currentContext.qLook(q, phi)!!))
            relation.addRelation(
                Pair(currentContext.qLook(q, phi)!!, element)
            )
        }

        /** relate element to a local context */
        val gammap = Context();
        phi.put(element, gammap)

        /** introduce body to the local context */
        //TODO: what is a RawComponentPart?
        //if (element.parts != null) res.add(checkIntroduceElements(gammap, phi, relation, element.parts))

        /** now add it to the context */
        currentContext.addComponent(element)

        /** precond: all parents referenced are of the valid type */
        /** TODO: this field doesn't exist!? */
        return getCheckStatus("validComponent", listOf(), "${element.name} is not a valid component", res)
    }

    /**
     * Judgement: Constraint is introduced
     */
    fun checkIntroduceConstraint(currentContext: Context, phi: ElementMap, element: RawConstraint): CheckStatus {
        currentContext.addTextTypeComponentPart(element.text, element)
        return CheckStatus.Ok("validConstraint", listOf())
    }

    /**
     * Judgement: Query is introduced
     */
    fun checkIntroduceQuery(currentContext: Context, phi: ElementMap, element: RawQuery): CheckStatus {
        currentContext.addTextTypeComponentPart(element.text, element)
        return CheckStatus.Ok("validQuery", listOf())
    }

    /**
     * Judgement: Command is introduced
     */
    fun checkIntroduceCommand(currentContext: Context, phi: ElementMap, element: RawCommand): CheckStatus {
        currentContext.addTextTypeComponentPart(element.text, element)
        return CheckStatus.Ok("validCommand", listOf())
    }

    /**
     * Judgment: a component import is properly introduced
     */
    fun checkIntroduceComponentImport(
        currentContext: Context,
        phi: ElementMap,
        relation: Relation,
        element: RawComponentImport
    ): CheckStatus {
        var res = mutableListOf<CheckStatus>()

        /** precond: import resolves to a qualified named element */
        res.add(
            when (currentContext.qLook(element.name, phi) is RawComponent) {
                true -> CheckStatus.Ok("validImportedComponent", listOf())
                else -> CheckStatus.Error(
                    "validImportedComponent",
                    listOf(element),
                    "could not resolve component '${element.name}' import to a component",
                    listOf()
                )
            }
        )

        /** precond: all clients referenced are of the valid type */
        for (q in element.clientOf) {
            // TODO: handle qlook null pointer exception
            res.add(checkValidClient(element, currentContext.qLook(q, phi)!!))
            relation.addRelation(
                Pair(currentContext.qLook(q, phi)!!, element)
            )
        }

        return getCheckStatus("validComponentImport", listOf(), "${element.name} is not a valid component import", res)
    }


    /**
     * Judgment: an events is properly introduced
     */
    fun checkIntroduceEvents(
        currentContext: Context,
        phi: ElementMap,
        relation: Relation,
        element: RawEvents
    ): CheckStatus {
        TODO()
    }


    /**
     * Judgment: a scenario is properly introduced
     */
    fun checkIntroduceScenarios(
        currentContext: Context,
        phi: ElementMap,
        relation: Relation,
        element: RawScenarios
    ): CheckStatus {
        TODO()
    }

    /**
     * Judgment: a requirements is properly introduced
     */
    fun checkIntroduceRequirements(
        currentContext: Context,
        phi: ElementMap,
        relation: Relation,
        element: RawRequirements
    ): CheckStatus {
        TODO()
    }


    /**
     * Judgment: a relation is properly introduced
     */
    fun checkIntroduceRelation(
        currentContext: Context,
        phi: ElementMap,
        relation: Relation,
        element: RawRelation
    ): CheckStatus {
        TODO()
    }
}