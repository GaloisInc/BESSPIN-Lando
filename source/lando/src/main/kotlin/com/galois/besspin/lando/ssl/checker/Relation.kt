package com.galois.besspin.lando.ssl.checker

import com.galois.besspin.lando.ssl.ast.RawElement
import com.galois.besspin.lando.ssl.ast.Uid

import org.jgrapht.graph.DirectedPseudograph;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.alg.cycle.CycleDetector;

/**
 * Element Relation Structure for Type Checking
 *
 * I: elem x elem is an inheritance relation between elements, where (e1, e2) in I means
 * that e1 inherits from e2. The predicate hasNoCycles(I) holds when I has no cycles.
 *
 * TODO: switch to a implementation that can do cycles detection
 */
class Relation(var rels: MutableList<Pair<Uid, Uid>> = mutableListOf()) {
    var graph: DirectedPseudograph<Uid, DefaultEdge> =
        DirectedPseudograph<Uid, DefaultEdge>(DefaultEdge::class.java);

    fun addRelations(rs: List<Pair<RawElement, RawElement>>) {
        for (r in rs) {
            addRelation(r)
        }
    }

    fun addRelation(r: Pair<RawElement, RawElement>) {
        graph.addVertex(r.first.uid)
        graph.addVertex(r.second.uid)
        graph.addEdge(r.first.uid, r.second.uid)
    }

    /**
     * determine if a relation has cycles
     */
    fun hasNoCycles(): Boolean {
        return !CycleDetector(graph).detectCycles();
    }
}