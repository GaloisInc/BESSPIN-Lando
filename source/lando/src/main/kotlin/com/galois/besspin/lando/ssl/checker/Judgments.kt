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
        preconditions: List<CheckStatus>
    ): CheckStatus {
        return if (preconditions.all { it is CheckStatus.Ok }) {
            CheckStatus.Ok(identifier = identifier, preconds = preconditions)
        } else {
            CheckStatus.Error(
                identifier = identifier,
                elements = elements,
                message = message,
                preconditions = preconditions
            )
        }
    }

    /**
     * Judgement: a source and its body are valid
     */
    fun checkSource(source: List<RawElement>): CheckStatus {
        val res = mutableListOf<CheckStatus>()

        /** precond: the source implies a valid context */
        val gamma0 = Context()
        val phi0 = mutableMapOf<RawElement, Context>()
        val relationI = Relation()

        /** First pass: don't resolve dependencies */
        res.add(checkIntroduceElementsFirstPass(gamma0, gamma0, phi0, relationI, source))
        when (val status = getCheckStatus("validSourceFirstPass", listOf(), "Source is invalid!", res)) {
            is CheckStatus.Error -> return status
            else -> {}
        }

        /** Second pass: resolve dependencies and relations */
        res.add(checkIntroduceElementsSecondPass(gamma0, gamma0, phi0, relationI, source))
        when (val status = getCheckStatus("validSourceSecondPass", listOf(), "Source is invalid!", res)) {
            is CheckStatus.Error -> return status
            else -> {}
        }

        /** precond: all elements referenced in the source are top level elements */
        for (elem in source) {
            res.add(checkValidTopLevel(elem))
        }

        /** precond: noCycles in the inheritance relations */
        if (relationI.hasNoCycles()) {
            res.add(CheckStatus.Ok("validInheritance"))
        } else {
            // TODO: to report offending elements we need different graph calls
            res.add(CheckStatus.Error("validInheritance", listOf(), "Inheritance map has cycles!"))
        }

        /** precond: only one system at a time */
        if (gamma0.systems.size > 1) {
            // The equality check is hidden into Context
            var names = ""
            for (system in gamma0.systems) {
                names += "${system.name}, "
            }
            res.add(
                CheckStatus.Error(
                    "validSystemEquiv",
                    gamma0.systems,
                    "only one system allowed at top level, found systems: $names",
                    listOf()
                )
            )
        } else {
            res.add(CheckStatus.Ok("validSystemEquiv", listOf()))
        }

        /** here we should resolve `inherit` and `contain` */
        res.add(checkInheritElements(gamma0, phi0, relationI, source))

        return getCheckStatus("validSourceFinalPass", listOf(), "Source is invalid!", res)
    }

    /**
     * Judgment: abbreviations cannot equal the name identifier
     */
    fun checkNameAbbrev(name: String, abbrev: String, element: RawElement): CheckStatus {
        return if (name != abbrev) {
            CheckStatus.Ok("validNameAbbrev")
        } else {
            CheckStatus.Error(
                "validNameAbbrev",
                listOf(element),
                "name $name conflicts with its abbreviation $abbrev",
                listOf()
            )
        }
    }

    /**
     * Judgment: top level elements is a subset of types
     */
    private fun checkValidTopLevel(element: RawElement): CheckStatus {
        val isTopLevel = (element is RawSystem) || (element is RawSubsystem) || (element is RawComponent) ||
                (element is RawEvents) || (element is RawScenarios) || (element is RawRequirements) ||
                (element is RawRelation)
        return if (isTopLevel) {
            CheckStatus.Ok("validTopLevel")
        } else {
            CheckStatus.Error("validTopLevel", listOf(element), "element of type ${element.javaClass.name} cannot be at the top level")
        }
    }

    /**
     * Judgment: for a system or subsystem, the contains field body can only have a subset of types
     */
    private fun checkValidContains(parent: RawElement, child: RawElement): CheckStatus {
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
        return if (isValidContains) {
            CheckStatus.Ok("validSystemContains")
        } else {
            CheckStatus.Error(
                "validSystemContains",
                listOf(parent, child),
                "parent of type ${parent.javaClass.name} cannot contain a child of type ${child.javaClass.name}"
            )
        }
    }

    /**
     * Judgment: for a subsystem/component (import), the client field body can only have a subset of types
     */
    private fun checkValidClient(parent: RawElement, child: RawElement): CheckStatus {
        val isValidClient = when {
            ((parent is RawSubsystem) || (parent is RawSubsystemImport)) ->
                ((child is RawSubsystem) || (child is RawSubsystemImport) ||
                        (child is RawComponent) || (child is RawComponentImport))
            ((parent is RawComponent) || (parent is RawComponentImport)) ->
                ((child is RawSubsystem) || (child is RawSubsystemImport) ||
                        (child is RawComponent) || (child is RawComponentImport))
            else -> false
        }
        return if (isValidClient) {
            CheckStatus.Ok("validClient")
        } else {
            CheckStatus.Error(
                "validClient",
                listOf(parent, child),
                "child fails necessary element type ${child.javaClass.name} for parent ${child.javaClass.name}"
            )
        }
    }

    /**
     * Judgment: an inherit must be two components
     */
    private fun checkValidInherit(parent: RawElement, child: RawElement): CheckStatus {
        return when (parent is RawComponent && child is RawComponent) {
            true -> CheckStatus.Ok("validInherit")
            else -> CheckStatus.Error(
                "validClient",
                listOf(parent, child),
                "parent ${parent.javaClass.name} and child ${child.javaClass.name} are not both components"
            )
        }
    }

    /**
     * Judgement: when introducing a list of elements (body), they imply a valid context
     * First pass: Scoping, not resolving dependencies
     */
    fun checkIntroduceElementsFirstPass(gamma0: Context, gamma: Context, phi: ElementMap, relation: Relation, es: List<RawElement>): CheckStatus {

        val res = mutableListOf<CheckStatus>()

        for (elem in es) {
            when (elem) {
                is RawSystem -> res.add(checkIntroduceSystem(gamma0, gamma, phi, relation, elem, true))
                is RawSubsystem -> res.add(checkIntroduceSubsystem(gamma0, gamma, phi, relation, elem, true))
                is RawComponent -> res.add(checkIntroduceComponent(gamma, phi, relation, elem, true))
                is RawScenarios -> res.add(checkIntroduceScenarios(gamma, phi, relation, elem))
                is RawRequirements -> res.add(checkIntroduceRequirements(gamma, phi, relation, elem))
                is RawEvents -> res.add(checkIntroduceEvents(gamma, phi, relation, elem))
            }
        }
        return getCheckStatus("validElementsListFirstPass", listOf(), "Elements Introduction is invalid.", res)
    }

    /**
     * Judgement: when introducing a list of elements (body), they imply a valid context
     * Second pass: Resolve dependencies and relations
     */
    private fun checkIntroduceElementsSecondPass(gamma0: Context, gamma: Context, phi: ElementMap, relation: Relation, es: List<RawElement>): CheckStatus {
        val res = mutableListOf<CheckStatus>()
        for (elem in es) {
            when (elem) {
                is RawSystem -> res.add(checkIntroduceSystem(gamma0, gamma, phi, relation, elem, false))
                is RawSubsystem -> res.add(checkIntroduceSubsystem(gamma0, gamma, phi, relation, elem, false))
                is RawSubsystemImport -> res.add(checkIntroduceSubsystemImport(gamma0, gamma, phi, relation, elem))
                is RawComponentImport -> res.add(checkIntroduceComponentImport(gamma0, gamma, phi, relation, elem))
                is RawComponent -> res.add(checkIntroduceComponent(gamma, phi, relation, elem, false))
                is RawRelation -> res.add(checkIntroduceRelation(gamma0, phi, relation, elem))
            }
        }
        return getCheckStatus("validElementsListSecondPass", listOf(), "Elements Introduction is invalid.", res)
    }

    private fun checkInheritElements(gamma: Context, phi: ElementMap, relation: Relation, es: List<RawElement>): CheckStatus {

        val res = mutableListOf<CheckStatus>()

        for (elem in es) {
            when (elem) {
                is RawComponent -> {
                    /** precond: all inherits must be valid inherits */
                    for (q in elem.inherits) {
                        attemptResolveCheck(res, elem, gamma, q, phi, relation, ::checkValidInherit)
                    }
                }
            }
        }
        return getCheckStatus("validElementsList", listOf(), "Elements Introduction is invalid.", res)
    }

    /**
     * Judgment: a system is properly introduced
     */
    private fun checkIntroduceSystem(
        topLevelContext: Context,
        currentContext: Context,
        phi: ElementMap,
        relation: Relation,
        element: RawSystem,
        firstPass: Boolean
    ): CheckStatus {
        val res = mutableListOf<CheckStatus>()
        /** relate element to a local context */
        val gammap = Context()
        phi[element] = gammap

        if (firstPass) {
            /** precond: check if the element is unique */
            // TODO: doesn't seem to check against a subsystem with the same name
            val resResult = currentContext.qLook(listOf(element.name), phi)
            if (resResult is QNameReturn.ResolvedElement) {
                res.add(
                    CheckStatus.Error(
                        "duplicateElement", listOf(element),
                        "system ${element.name} already exists at ${resResult.element.pos}", listOf()
                    )
                )
            }

            /** precond: if abbrev name is defined, it must not equal the elements name */
            if (element.abbrevName != null) {
                res.add(checkNameAbbrev(element.name, element.abbrevName, element))
            }

            /** introduce explanation as a type */
            currentContext.addTextType(element.explanation, element)

            /** precond: all elements in the system body must imply a valid context and be a valid contains type */
            if (element.body != null) {
                //checkIntroduceElements(currentContext, phi, relation, element.body!!)
                for (elem in element.body!!.toList()) {
                    res.add(checkValidContains(element, elem))
                }
            }

            /** introduce body to the local context */
            if (element.body != null) res.add(checkIntroduceElementsFirstPass(topLevelContext, gammap, phi, relation, element.body!!))

            /** now add it to the context */
            currentContext.addSystem(element)
        } else {
            if (element.body != null) res.add(checkIntroduceElementsSecondPass(topLevelContext, gammap, phi, relation, element.body!!))
        }

        return getCheckStatus("validSystem", listOf(), "${element.name} is not a valid system", res)
    }

    /**
     * Judgment: a subsystem is properly introduced
     */
    fun checkIntroduceSubsystem(
        toplevelContext: Context,
        currentContext: Context,
        phi: ElementMap,
        relation: Relation,
        element: RawSubsystem,
        firstPass: Boolean
    ): CheckStatus {
        val res = mutableListOf<CheckStatus>()

        /** relate element to a local context */
        val gammap = Context()
        phi[element] = gammap

        if (firstPass) {
            /** precond: check if the element is unique */
            val resResult = currentContext.qLook(listOf(element.name), phi)
            if (resResult is QNameReturn.ResolvedElement) {
                res.add(
                    CheckStatus.Error(
                        "duplicateElement", listOf(element),
                        "subsystem ${element.name} already exists at ${resResult.element.pos}", listOf()
                    )
                )
            }

            /** precond: if abbrev name is defined, it must not equal the elements name */
            if (element.abbrevName != null) {
                res.add(checkNameAbbrev(element.name, element.abbrevName, element))
            }

            /** introduce explanation as a type */
            currentContext.addTextType(element.explanation, element)

            /** precond: all elements in the subsystem body must imply a valid context and be a valid contains type */
            if (element.body != null) {
                // checkIntroduceElements(gammap, phi, relation, element.body!!)
                for (elem in element.body!!.toList()) {
                    res.add(checkValidContains(element, elem))
                }
            }

            /** introduce body to the local context */
            if (element.body != null) res.add(checkIntroduceElementsFirstPass(toplevelContext, gammap, phi, relation, element.body!!))

            // TODO: we clearly need to do a second pass, to resolve relations, but this is wrong and leads to resolve errors.
            // if (element.body != null) res.add(checkIntroduceElementsSecondPass(toplevelContext, gammap, phi, relation, element.body!!))

            /** now add it to the context */
            currentContext.addSubsystem(element)
        } else {
            /** precond: all clients referenced are of the valid type */
            for (q in element.clientOf) {
                attemptResolveCheck(res, element, currentContext, q, phi, relation, ::checkValidClient)
            }
        }

        return getCheckStatus("validSubsystem", listOf(), "${element.name} is not a valid subsystem", res)
    }

    /**
     * Judgment: a subsystem import is properly introduced
     */
    private fun checkIntroduceSubsystemImport(
        toplevelContext: Context,
        currentContext: Context,
        phi: ElementMap,
        relation: Relation,
        element: RawSubsystemImport
    ): CheckStatus {
        val res = mutableListOf<CheckStatus>()

        /** precond: all clients referenced are of the valid type */
        for (q in element.clientOf) {
            attemptResolveCheck(res, element, currentContext, q, phi, relation, ::checkValidClient)
        }

        /** precond: import resolves to a qualified named element */
        val resResult = toplevelContext.qLook(element.name, phi)
        res.add(
            when (resResult is QNameReturn.ResolvedElement) {
                true -> when (resResult.element is RawSubsystem) {
                    true -> CheckStatus.Ok("validImportedSubsystem", listOf())
                    else -> CheckStatus.Error(
                        "validImportedSubsystem",
                        listOf(element),
                        "could resolve ${element.name}, but it's not a subsystem",
                        listOf()
                    )
                }
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
    private fun checkIntroduceComponent(
        currentContext: Context,
        phi: ElementMap,
        relation: Relation,
        element: RawComponent,
        firstPass: Boolean
    ): CheckStatus {
        val res = mutableListOf<CheckStatus>()

        if (firstPass) {
            /** precond: check if the element is unique */
            val resResult = currentContext.qLook(listOf(element.name), phi)
            if (resResult is QNameReturn.ResolvedElement) {
                res.add(
                    CheckStatus.Error(
                        "duplicateElement", listOf(element),
                        "component ${element.name} already exists at ${resResult.element.pos}", listOf()
                    )
                )
            }

            /** precond: if abbrev name is defined, it must not equal the elements name */
            if (element.abbrevName != null) {
                res.add(checkNameAbbrev(element.name, element.abbrevName, element))
            }

            /** introduce explanation as a type */
            currentContext.addTextType(element.explanation, element)

            /** relate element to a local context */
            val gammap = Context()
            phi[element] = gammap

            /** introduce body to the local context */
            //TODO: what is a RawComponentPart?
            //if (element.parts != null) res.add(checkIntroduceElements(gammap, phi, relation, element.parts))

            /** now add it to the context */
            currentContext.addComponent(element)
        } else {
            /** precond: all clients referenced are of the valid type */
            for (q in element.clientOf) {
                attemptResolveCheck(res, element, currentContext, q, phi, relation, ::checkValidClient)
            }

            /** precond: all inherits must be valid inherits */
            for (q in element.inherits) {
                attemptResolveCheck(res, element, currentContext, q, phi, relation, ::checkValidInherit)
            }
        }

        /** precond: all parents referenced are of the valid type */
        return getCheckStatus("validComponent", listOf(), "${element.name} is not a valid component", res)
    }

    /**
     * this is an (ugly) private method to handle logic of attempting to resolve a list of identifiers and
     * check that the elements that they map to are valid and handl any errors along the way
     */
    private fun attemptResolveCheck(
        res: MutableList<CheckStatus>,
        element: RawElement,
        currentContext: Context,
        q: QName,
        phi: ElementMap,
        relation: Relation,
        checker: (RawElement, RawElement) -> CheckStatus
    ) {
        val resResult = currentContext.qLook(q, phi)
        if (resResult is QNameReturn.ResolvedElement) {
            val resElement = resResult.element
            res.add(checker(element, resElement))
            relation.addRelation(
                Pair(resElement, element)
            )
        } else {
            when {
                (resResult is QNameReturn.MultipleElement) -> res.add(
                    CheckStatus.Error(
                        "validClientResolve", listOf(element),
                        "$q resolved to multiple elements ${resResult.elements}", listOf()
                    )
                )
                (resResult is QNameReturn.NullElement) -> res.add(
                    CheckStatus.Error(
                        "validClientResolve", listOf(element),
                        "$q couldn't be resolved to an element", listOf()
                    )
                )
            }
        }
    }

    /**
     * Judgement: Constraint is introduced
     */
    @Suppress("unused")
    fun checkIntroduceConstraint(currentContext: Context, element: RawConstraint): CheckStatus {
        currentContext.addTextTypeComponentPart(element.text, element)
        return CheckStatus.Ok("validConstraint", listOf())
    }

    /**
     * Judgement: Query is introduced
     */
    @Suppress("unused")
    fun checkIntroduceQuery(currentContext: Context, element: RawQuery): CheckStatus {
        currentContext.addTextTypeComponentPart(element.text, element)
        return CheckStatus.Ok("validQuery", listOf())
    }

    /**
     * Judgement: Command is introduced
     */
    @Suppress("unused")
    fun checkIntroduceCommand(currentContext: Context, element: RawCommand): CheckStatus {
        currentContext.addTextTypeComponentPart(element.text, element)
        return CheckStatus.Ok("validCommand", listOf())
    }

    /**
     * Judgment: a component import is properly introduced
     */
    private fun checkIntroduceComponentImport(
        toPlevelContext: Context,
        currentContext: Context,
        phi: ElementMap,
        relation: Relation,
        element: RawComponentImport
    ): CheckStatus {
        val res = mutableListOf<CheckStatus>()

        /** precond: import resolves to a qualified named element */
        val resResult = toPlevelContext.qLook(element.name, phi)
        res.add(
            when (resResult is QNameReturn.ResolvedElement) {
                true -> when (resResult.element is RawComponent) {
                    true -> CheckStatus.Ok("validImportedComponent", listOf())
                    else -> CheckStatus.Error(
                        "validImportedComponent",
                        listOf(element),
                        "could resolve ${element.name}, but it's not a component",
                        listOf()
                    )
                }
                else -> CheckStatus.Error(
                    "validImportedComponent",
                    listOf(element),
                    "could not resolve '${element.name}' import to a component",
                    listOf()
                )
            }
        )

        /** precond: all clients referenced are of the valid type */
        for (q in element.clientOf) {
            attemptResolveCheck(res, element, currentContext, q, phi, relation, ::checkValidClient)
        }

        return getCheckStatus("validComponentImport", listOf(), "${element.name} is not a valid component import", res)
    }


    /**
     * Judgment: an events is properly introduced
     * TODO: needs work
     */
    @Suppress("UNUSED_PARAMETER")
    fun checkIntroduceEvents(
        currentContext: Context,
        phi: ElementMap,
        relation: Relation,
        element: RawEvents
    ): CheckStatus {
        /** relate element to a local context */
        val gammap = Context()
        phi[element] = gammap

        gammap.addEvents(element)

        return CheckStatus.Ok("validEvents", listOf())
    }


    /**
     * Judgment: a scenario is properly introduced
     * TODO: needs work
     */
    @Suppress("UNUSED_PARAMETER")
    private fun checkIntroduceScenarios(
        currentContext: Context,
        phi: ElementMap,
        relation: Relation,
        element: RawScenarios
    ): CheckStatus {
        /** relate element to a local context */
        val gammap = Context()
        phi[element] = gammap

        gammap.addScenarios(element)

        return CheckStatus.Ok("validScenarios", listOf())
    }

    /**
     * Judgment: a requirements is properly introduced
     * TODO: needs work
     */
    @Suppress("UNUSED_PARAMETER")
    fun checkIntroduceRequirements(
        currentContext: Context,
        phi: ElementMap,
        relation: Relation,
        element: RawRequirements
    ): CheckStatus {
        /** relate element to a local context */
        val gammap = Context()
        phi[element] = gammap

        gammap.addRequirements(element)

        return CheckStatus.Ok("validRequirements", listOf())
    }


    /**
     * Judgment: a relation is properly introduced
     */
    private fun checkIntroduceRelation(
        currentContext: Context,
        phi: ElementMap,
        relation: Relation,
        element: RawRelation
    ): CheckStatus {
        val res = mutableListOf<CheckStatus>()

        val resResult = currentContext.qLook(element.name, phi)

        res.add(
            when (resResult is QNameReturn.ResolvedElement) {
                // TODO: we should check that all parts of the relation has been introduced
                // I.e. the inherits,clientOf and contains lists
                true -> CheckStatus.Ok("validIntroducedRelation", listOf())
                else -> CheckStatus.Error(
                    "validIntroducedRelation",
                    listOf(element),
                    "could not resolve '${element.name}'",
                    listOf()
                )
            }
        )

        if (resResult is QNameReturn.ResolvedElement) {
            val relem = resResult.element

            /** precond: all clients referenced are of the valid type */
            for (q in element.clientOf) {
                attemptResolveCheck(res, relem, currentContext, q, phi, relation, ::checkValidClient)
            }

            /** precond: all inherits must be valid inherits */
            for (q in element.inherits) {
                attemptResolveCheck(res, relem, currentContext, q, phi, relation, ::checkValidInherit)
            }

            /** precond: all contained elements must be of a valid type */
            for (q in element.contains) {
                attemptResolveCheck(res, relem, currentContext, q, phi, relation, ::checkValidContains)
            }

            return getCheckStatus("validRelation", listOf(), "${element.name} is not a valid relation", res)

        } else {

            return getCheckStatus("validRelation", listOf(), "${element.name} is not a valid relation", res)
        }
    }
}