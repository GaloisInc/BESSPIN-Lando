package com.galois.besspin.lando.ssl.checker

import com.galois.besspin.lando.ssl.ast.RawElement

sealed class CheckStatus(){
    class Ok(
        val identifier: String,
        val elements: List<RawElement> = listOf(),
        val message: String = "",
        val preconds: List<CheckStatus> = listOf()

    ) : CheckStatus(
    )

    class Error(
        val identifier: String,
        val elements: List<RawElement>,
        val message: String,
        val preconds: List<CheckStatus> = listOf()

    ) : CheckStatus(
    )
}