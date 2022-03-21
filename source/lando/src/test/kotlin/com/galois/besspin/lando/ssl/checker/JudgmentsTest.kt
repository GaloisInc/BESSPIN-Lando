package com.galois.besspin.lando.ssl.checker

import com.galois.besspin.lando.ssl.ast.*
import junit.framework.TestCase

class JudgmentsTest : TestCase() {

    public override fun setUp() {
        super.setUp()
    }

    /** subsystem generator for testing (hides some of the complexity of creating Raw AST object) */
    fun generateSubsystem(
        name: String,
        abbrev: String,
        explanation: String = "",
        body: List<RawElement> = listOf(),
        clientOf: List<QName> = listOf()
    ): RawSubsystem {
        val subsys = RawSubsystem(
            uid = 0,
            pos = RawPos(line = 1, col = 0),
            name = name,
            abbrevName = abbrev,
            clientOf = clientOf,
            explanation = explanation,
            indexing = listOf(),
            body = body,
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

        /** no errors is Ok */
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

    fun testCheckIntroduceElements() {
        val judgments = Judgments()
        val ctx = Context()
        val rel = Relation()
        val phi = mutableMapOf<RawElement, Context>()

        /* check simple case is ok */
        val body0 = listOf(generateSubsystem("My System", "Abbrev"))
        val ret0 = judgments.checkIntroduceElements(ctx, phi, rel, body0)
        assert(ret0 is CheckStatus.Ok)

        /* check with invalid element */
        val body1 = listOf(generateSubsystem("My System", "Abbrev", body = listOf(TestElement(0, RawPos(1, 1)))))
        val ret1 = judgments.checkIntroduceElements(ctx, phi, rel, body1)
        assert(ret1 is CheckStatus.Error)
    }

    fun testCheckSource() {
        /* check that es implies a valid context */

        /* check cycles */

        /* check that two systems in top level are equivalent */

        /* check that elements can be introduced */

        /* check that body is valid-top level */
    }

    fun testCheckIntroduceSystem() {
        /* check simple case */

        /* check valid contains */

        /* check text type is introduced */

        /* check abbreviation collision error */

        /* check that body is interest in sub context */

    }

    /** subsystem introduction tests */
    fun testCheckIntroduceSubsystem() {
        val judgments = Judgments()
        val ctx = Context()
        val rel = Relation()
        val phi = mutableMapOf<RawElement, Context>()

        /* check simple case is ok */
        val subsys0 = generateSubsystem("My System", "Abbrev")
        val ret0 = judgments.checkIntroduceSubsystem(ctx, phi, rel, subsys0)
        assert(ret0 is CheckStatus.Ok)

        /* check abbreviation collision error */
        val subsys1 = generateSubsystem("Abbrev", "Abbrev")
        val ret1 = judgments.checkIntroduceSubsystem(ctx, phi, rel, subsys1)
        assert(ret1 is CheckStatus.Error)

        /* check that explanation is added as a type */
        val ctx0 = Context()
        val expl = "this is an explanation"
        val subsys2 = generateSubsystem("My System", "Abbrev", explanation = expl)
        ctx0.addSubsystem(subsys2)
        assert("<TextType${subsys2.uid}-${subsys2.explanation}>" in ctx0.toMap())

        /* check invalid contains */
        val subsys3 = generateSubsystem("My System", "Abbrev", body = listOf(TestElement(0, RawPos(1, 1))))
        val ret3 = judgments.checkIntroduceSubsystem(ctx, phi, rel, subsys3)
        assert(ret3 is CheckStatus.Error)

        /* check valid clientOf */
        val ctx1 = Context()
        ctx1.addSubsystem(subsys3)
        val subsys4 = generateSubsystem("My System2", "Abbrev", clientOf = listOf(listOf("My System")))
        val ret4 = judgments.checkIntroduceSubsystem(ctx1, phi, rel, subsys4)
        assert(ret4 is CheckStatus.Ok)

        /* check invalid clientOf */
        ctx.addSystem(RawSystem(0, RawPos(0, 0), "My Bad", null, "", listOf(), null, listOf()))
        val subsys5 = generateSubsystem("My System2", "Abbrev", clientOf = listOf(listOf("My Bad")))
        val ret5 = judgments.checkIntroduceSubsystem(ctx, phi, rel, subsys5)
        assert(ret5 is CheckStatus.Error)
    }
}