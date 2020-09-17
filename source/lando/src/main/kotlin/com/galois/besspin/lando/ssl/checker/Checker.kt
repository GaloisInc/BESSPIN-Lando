package com.galois.besspin.lando.ssl.checker

import com.galois.besspin.lando.ssl.ast.*

data class CheckingError(
    val pos: RawPos,
    val message: String
)
{
    fun formatError(): String = "Line: ${pos.line}, Column: ${pos.col}. $message"
}

typealias Env = MutableMap<String,RawNamed>

class RawAstChecker {

    private val errors: MutableList<CheckingError> = mutableListOf()
    // val warnings: MutableList<CheckingError> = mutableListOf()

    private fun formatErrors() : String =
        if (errors.size != 0)
            "Checking generated ${errors.size} error(s):\n" +
                    errors.joinToString(separator = "\n") { e -> e.formatError() }
        else
            ""


    private var globalEnv: Env = mutableMapOf()
    private val globalSEnv: MutableMap<Uid, Env> = mutableMapOf()
    private val globalIRel: MutableMap<Uid, MutableSet<Uid>> = mutableMapOf()

    // for now, assume that anything within double quotes is intended as a qname.
    private fun qnames(text: String): List<QName> {
        val qs = mutableListOf<QName>()
        val re = Regex("\"[^\"]*\"")
        val mr = re.findAll(text)
        for (q in mr)
            qs += q.value.trim('"').split(':').map {it.trim()}
        return qs
    }

    fun check(ast: RawSSL) : String {
        globalEnv = build(ast.body) // also builds globalSEnv
        check(globalEnv, ast.body) // also builds globalIRel and checks it is acyclic
        // check for multiple System elements
        var systemFound = false
        for (elem in ast.body)
            if (elem is RawSystem) {
                if (systemFound)
                    errors += CheckingError(elem.pos, "Multiple System elements in specification")
                systemFound = true
            }
        for (elem in ast.body)
            validToplevel(elem)
        return formatErrors()
    }

    private fun build(ast: List<RawElement>): Env {
        val env: Env = mutableMapOf()
        for (elem in ast) {
            when (elem) {
                is RawSystem -> {
                    envAdd(env, elem.name, elem, elem.pos)
                    if (elem.abbrevName != null)
                        envAdd(env, elem.abbrevName, elem, elem.pos)
                    globalSEnv[elem.uid] = build(elem.body!!)  // this !! is a bit annoying
                }
                is RawSubsystem -> {
                    envAdd(env, elem.name, elem, elem.pos)
                    if (elem.abbrevName != null)
                        envAdd(env, elem.abbrevName, elem, elem.pos)
                    globalSEnv[elem.uid] = build(elem.body!!)  // this !! is a bit annoying
                }
                is RawSubsystemImport ->
                    if (elem.abbrevName == null)
                        envAdd(env, elem.name.last(), elem, elem.pos)
                    else
                        envAdd(env, elem.abbrevName, elem, elem.pos)
                is RawComponent -> {
                    envAdd(env, elem.name, elem, elem.pos)
                    if (elem.abbrevName != null)
                        envAdd(env, elem.abbrevName, elem, elem.pos)
                }
                is RawComponentImport -> {
                    if (elem.abbrevName == null)
                        envAdd(env, elem.name.last(), elem, elem.pos)
                    else
                        envAdd(env, elem.abbrevName, elem, elem.pos)
                }
                is RawEvents -> {
                    for (r in elem.events)
                        envAdd(env, r.id, r, r.pos)
                    envAdd(env, elem.name, elem, elem.pos)
                }
                is RawScenarios -> {
                    for (r in elem.scenarios)
                        envAdd(env, r.id, r, r.pos)
                    envAdd(env, elem.name, elem, elem.pos)
                }
                is RawRequirements -> {
                    for (r in elem.requirements)
                        envAdd(env, r.id, r, r.pos)
                    envAdd(env, elem.name, elem, elem.pos)
                }
                is RawRelation -> {
                }
            }
        }
        return env
    }


    private fun check(env:Env,ast: List<RawElement>) {
        for (elem in ast) {
            when (elem) {
                is RawSystem -> {
                    check(env, elem.explanation, elem.pos)
                    for (e in elem.body!!)
                        validContains(elem,e)
                    val benv = envShadows(globalSEnv[elem.uid]!!, env)
                    check(benv, elem.body!!)
                }
                is RawSubsystem -> {
                    check(env, elem.explanation, elem.pos)
                    for (q in elem.clientOf)
                        validClient(env,elem,q,elem.pos)
                    for (e in elem.body!!)
                       validContains(elem,e)
                    val benv = envShadows(globalSEnv[elem.uid]!!, env)
                    check(benv, elem.body!!)
                }
                is RawSubsystemImport -> {
                    val ss = qlook(globalEnv,elem.name,elem.pos)
                    if (ss !=null && ss !is RawSubsystem)
                        errors += CheckingError(elem.pos, "target of import is not a subsystem")
                    for (q in elem.clientOf)
                        validClient(env,elem,q,elem.pos)
                }
                is RawComponent -> {
                    check(env, elem.explanation, elem.pos)
                    for (q in elem.inherits)
                        validInherits(env,elem,q,elem.pos)
                    for (q in elem.clientOf)
                        validClient(env,elem,q,elem.pos)
                    checkParts(env, elem.parts)
                }
                is RawComponentImport -> {
                    val c = qlook(globalEnv,elem.name,elem.pos)
                    if (c !=null && c !is RawComponent)
                        errors += CheckingError(elem.pos, "target of import is not a component")
                    for (q in elem.clientOf)
                        validClient(env,elem,q,elem.pos)
                }
                is RawEvents -> {
                    checkItems(env, elem.events)
                }
                is RawScenarios -> {
                    checkItems(env, elem.scenarios)
                }
                is RawRequirements -> {
                    checkItems(env, elem.requirements)
                }
                is RawRelation -> {
                    val e = qlook(env,elem.name,elem.pos)
                    if (e != null) {
                        for (q in elem.inherits)
                            validInherits(env, e, q, elem.pos)
                        for (q in elem.clientOf)
                            validClient(env, e, q, elem.pos)
                    }
                }
            }
        }
    }

    private fun validToplevel(elem:RawElement) {
        if (elem !is RawSubsystemImport && elem !is RawComponentImport) {
            // ok
        } else
            errors += CheckingError(elem.pos, "imports not permitted at top level")
    }

    private fun validContains(container: RawElement, contained:RawElement)  {
        if ((container is RawSystem &&
                    (contained is RawSubsystem || contained is RawSubsystemImport || contained is RawRelation))
            || (container is RawSubsystem && contained !is RawSystem)) {
            // ok
        } else
            errors += CheckingError(contained.pos, "${kind(container)} cannot contain ${kind(contained)}")
    }

    private fun validInherits(env: Env, sub: RawNamed, q: QName, pos: RawPos) {
        val sup = qlook(env,q,pos)
        if (sup != null) {
            if (sub is RawComponent && sup is RawComponent) {
                // ok; add to global inheritance relation
                registerInherit(sub,sup)
            } else
                errors += CheckingError(
                    pos,
                    "${kind(sub)} cannot inherit from ${kind(sup)}"
                )
        }
    }

    private fun validClient(env: Env, client:RawNamed, q:QName,pos: RawPos) {
        val supplier = qlook(env, q, pos)
        if (supplier != null) {
            if ((client is RawSubsystem || client is RawSubsystemImport || client is RawComponent || client is RawComponentImport)
                && (supplier is RawSubsystem || supplier is RawSubsystemImport || supplier is RawComponent || supplier is RawComponentImport)
            ) {
                // ok
            } else
                errors += CheckingError(
                     pos,
                    "${kind(client)} cannot be a client of ${kind(supplier)}"
                )
        }
    }

    private fun checkParts(env:Env,parts:List<RawComponentPart>) {
        for (p in parts)
            check(env,p.text,p.pos)
    }

    private fun checkItems(env:Env,items:List<RawItem>) {
        for (i in items)
            check(env,i.text,i.pos)
    }

    private fun check(env: Env, text: String, pos: RawPos) {
        for (q in qnames(text))
            qlook(env, q, pos)
    }


    private fun envAdd(env: Env, k: String, v: RawNamed, pos: RawPos) {
        if (k in env)
            errors += CheckingError(pos, "duplicate binding for \"$k\"")
        else
            env[k] = v
    }

    private fun envUnion(env1: Env, env2: Env, pos: RawPos) : Env {
        val env = env1.toMutableMap()
        for (k in env2.keys)
            envAdd(env, k, env2[k]!!, pos)
        return env
    }

    // env1 shadows env2
    private fun envShadows(env1:Env, env2:Env) : Env =
        (env2 + env1).toMutableMap()

    private fun look(env: Env, n: Name, pos: RawPos): RawNamed? {
        val v = env[n]
        if (v == null)
            errors += CheckingError(pos, "undefined name \"$n\"")
        return v
    }

    private fun qlook(env: Env, q: QName, pos: RawPos): RawNamed? {
        val n = q.first()     // q should never be empty
        val e = look(env, n, pos)
        val r = q.drop(1)
        if (e == null || r.isEmpty())
            return e
        when (e) {
            is RawSubsystem ->
                return qlook(globalSEnv[e.uid]!!, r, pos)
            else -> {
                errors += CheckingError(pos, "qualified name component \"$n\" is not a subsystem")
                return null
            }
        }
    }
    // a notably inefficient way to build the reflexive transitive closure
    private fun reachableInherit(uid: Uid): Set<Uid> {
        val r = mutableSetOf(uid)
        val parents = globalIRel[uid]
        if (parents != null)
            for (id in parents)
                r.addAll(reachableInherit(id))
        return r
    }

    private fun registerInherit(sub: RawComponent, sup: RawComponent) {
        if (reachableInherit(sup.uid).contains(sub.uid))
            errors += CheckingError(sub.pos, "inheritance of ${sup.name} by ${sub.name} would create inheritance cycle")
        else
            globalIRel.getOrPut(sub.uid, { mutableSetOf() }).add(sup.uid)
    }

    private fun kind(e: RawNamed): String =
        when (e) {
            is RawElement ->
                "element " + e.toString().takeWhile{ c -> c != '(' }
                    .drop(3) // hacky way to get a decent-looking element name
            is RawItem -> "item"
            else -> "" // impossible
        }

}