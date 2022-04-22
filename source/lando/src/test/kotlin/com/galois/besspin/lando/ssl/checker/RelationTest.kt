package com.galois.besspin.lando.ssl.checker

import com.galois.besspin.lando.ssl.ast.RawPos
import junit.framework.TestCase

class RelationTest : TestCase() {

    /** test cycles detection used for the inheritance features */
    fun testHasNoCycles() {
        // we should check self loops
        val r0 = Relation();
        r0.addRelations(listOf(
            Pair(TestElement(0, RawPos(1, 2)), TestElement(0, RawPos(2, 0)))
        ))
        assert(!r0.hasNoCycles())

        // check cycle
        val r1 = Relation();
        r1.addRelations(listOf(
            Pair(TestElement(0, RawPos(1, 2)), TestElement(1, RawPos(2, 0))),
            Pair(TestElement(1, RawPos(1, 2)), TestElement(2, RawPos(2, 0))),
            Pair(TestElement(2, RawPos(1, 2)), TestElement(0, RawPos(2, 0))),
        ))
        assert(!r1.hasNoCycles())

        // cycle free case
        val r2 = Relation();
        r2.addRelations(listOf(
            Pair(TestElement(0, RawPos(1, 2)), TestElement(1, RawPos(2, 0))),
            Pair(TestElement(1, RawPos(1, 2)), TestElement(2, RawPos(2, 0))),
            Pair(TestElement(2, RawPos(1, 2)), TestElement(3, RawPos(2, 0))),
        ))
        assert(r2.hasNoCycles())
    }
}