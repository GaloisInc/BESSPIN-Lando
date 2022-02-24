package com.galois.besspin.lando.ssl.checker

import com.galois.besspin.lando.ssl.ast.RawElement
import com.galois.besspin.lando.ssl.ast.RawPos
import com.galois.besspin.lando.ssl.ast.RawSubsystem
import junit.framework.TestCase

class JudgmentsTest : TestCase() {

    public override fun setUp() {
        super.setUp()
    }

    /** subsystem generator for testing (hides some of the complexity of creating Raw AST object) */
    fun generateSubsystem(name: String, abbrev: String, explanation: String = ""): RawSubsystem {
        val subsys = RawSubsystem(
            uid = 0,
            pos = RawPos(line=1, col=0),
            name = name,
            abbrevName = abbrev,
            clientOf = listOf(),
            explanation = explanation,
            indexing = listOf(),
            body = listOf(),
            comments = listOf()
        )
        return subsys
    }

    fun testGetCheckStatus() {
        val judge = Judgments()

        /** empty preconds is OK */
        val ret0 = judge.getCheckStatus("validSubsystem", listOf(), "TODO", listOf())
        assert(ret0 is CheckStatus.Ok)

        /** error preconds is Error */
        val ret1 = judge.getCheckStatus(
            "validSubsystem",
            listOf(),
            "TODO",
            listOf(
                CheckStatus.Error("", listOf(), "", listOf()),
                CheckStatus.Ok("", listOf()),
            )
        )
        assert(ret1 is CheckStatus.Error)

        /** not errors is Ok */
        /** error preconds is Error */
        val ret2 = judge.getCheckStatus(
            "validSubsystem",
            listOf(),
            "TODO",
            listOf(
                CheckStatus.Ok("", listOf()),
                CheckStatus.Ok("", listOf()),
            )
        )
        assert(ret2 is CheckStatus.Ok)
    }

    fun testCheckNameAbbrev() {
        val judge = Judgments()

        /** name and abbrev different is Ok */
        val subsys0 = generateSubsystem("My System", "Abbrev")
        val ret0 = judge.checkNameAbbrev(subsys0.name, subsys0.abbrevName!!, subsys0)
        assert(ret0 is CheckStatus.Ok)

        /** name and abbrev same is Error */
        val subsys1 = generateSubsystem("Abbrev", "Abbrev")
        val ret1 = judge.checkNameAbbrev(subsys1.name, subsys1.abbrevName!!, subsys0)
        assert(ret1 is CheckStatus.Error)
    }

    fun testCheckValidTopLevel() {}

    fun testCheckValidContains() {}

    fun testCheckValidClient() {}

    fun testCheckIntroduceElements() {}

    fun testCheckSource() {}

    fun testCheckIntroduceSystem() {

    }

    /** subsystem introduction tests */
    fun testCheckIntroduceSubsystem() {
        val judgments =  Judgments()
        val ctx = Context()
        val phi = mutableMapOf<RawElement, Context>()

        /* check simple case is ok */
        val subsys0 = generateSubsystem("My System", "Abbrev")
        val ret0 = judgments.checkIntroduceSubsystem(ctx, phi, subsys0)
        assert(ret0 is CheckStatus.Ok)

        /* check abbreviation collision error */
        val subsys1 = generateSubsystem("Abbrev", "Abbrev")
        val ret1 = judgments.checkIntroduceSubsystem(ctx, phi, subsys1)
        assert(ret1 is CheckStatus.Error)

        /* check that explanation is added as a type */
        val expl = "this is an explanation"
        val subsys2 = generateSubsystem("My System", "Abbrev", explanation=expl)
        val ret2 = judgments.checkIntroduceSubsystem(ctx, phi, subsys2)
        assert("<TextType${subsys2.uid}-${subsys2.explanation}>" in ctx.toMap())

    }
}