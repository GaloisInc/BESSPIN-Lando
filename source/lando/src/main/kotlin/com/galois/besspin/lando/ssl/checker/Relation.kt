package com.galois.besspin.lando.ssl.checker

import com.galois.besspin.lando.ssl.ast.RawElement

import org.jgrapht.graph.SimpleDirectedGraph;
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
class Relation(var rels: MutableList<Pair<RawElement, RawElement>> = mutableListOf()) {

    var graph: SimpleDirectedGraph<RawElement, DefaultEdge> =
        SimpleDirectedGraph<RawElement, DefaultEdge>(DefaultEdge::class.java);

    fun addRelations(rs: List<Pair<RawElement, RawElement>>) {
        for (r in rs) {
            addRelation(r)
        }
    }

    fun addRelation(r: Pair<RawElement, RawElement>) {
        graph.addEdge(r.first, r.second)
    }

    fun hasNoCycles(): Boolean {
        return CycleDetector(graph).detectCycles();
    }
}