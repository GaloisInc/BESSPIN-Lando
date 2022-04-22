package com.galois.besspin.lando.ssl.checker

import com.galois.besspin.lando.ssl.ast.RawElement
import com.galois.besspin.lando.ssl.ast.RawPos
import com.galois.besspin.lando.ssl.ast.Uid
import junit.framework.Test
import junit.framework.TestCase

/**
 * Test Element for Example CheckStatus Error Trees
 */
class TestElement(
    override val uid: Uid,
    override val pos: RawPos
): RawElement

class CheckStatusTest : TestCase() {
    /** error string generation tests */
    fun testErrorMessage() {
        /** test example string (something of a fragile test) */
        val error = CheckStatus.Error(
            "invalidIdentifier",
            listOf(TestElement(0, RawPos(1, 4))),
            "reserved name",
            listOf(
                CheckStatus.Error(
                    "invalidCharacters",
                    listOf(TestElement(1, RawPos(2, 5))),
                    "bad character %",
                    listOf()
                ),
                CheckStatus.Ok(
                    "validB",
                    listOf()
                )
            )
        )
        val errorStr = error.getErrorString()
        assertEquals(errorStr,
            "invalidIdentifier: 'reserved name'[(Line 1, Column 4)]<[invalidCharacters: 'bad character %'[(Line 2, Column 5)]]>")
    }
}