package com.galois.besspin.lando.ssl.checker

import com.galois.besspin.lando.ssl.ast.*

class RawAstChecker {
    fun check(ast: RawSSL) : String {
        val judge = Judgments().checkSource(ast.body) // also builds globalSEnv
        return when {
            (judge is CheckStatus.Error) -> judge.getErrorString()
            else -> "Success!"
        }
    }
}