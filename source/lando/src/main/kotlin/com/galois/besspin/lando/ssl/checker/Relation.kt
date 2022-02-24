package com.galois.besspin.lando.ssl.checker

import com.galois.besspin.lando.ssl.ast.RawElement

/**
 * Element Relation Structure for Type Checking
 *
 * I: elem x elem is an inheritance relation between elements, where (e1, e2) in I means
 * that e1 inherits from e2. The predicate hasNoCycles(I) holds when I has no cycles.
 *
 * TODO: switch to a implementation that can do cycles detection
 */
class Relation(var rels: MutableList<Pair<RawElement, RawElement>> = mutableListOf()) {
    fun addRelations(rs: List<Pair<RawElement, RawElement>>) {
        rels.addAll(rs)
    }

    fun addRelation(r: Pair<RawElement, RawElement>) {
        rels.add(r)
    }

    fun hasNoCycles(): Boolean {
        TODO()
    }
}