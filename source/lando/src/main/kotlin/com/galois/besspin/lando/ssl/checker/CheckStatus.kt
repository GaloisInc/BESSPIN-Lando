package com.galois.besspin.lando.ssl.checker

import com.galois.besspin.lando.ssl.ast.RawElement

/**
 * Type Checker Status ADT
 *
 * CheckStatus defines nodes for an annotated proof tree that the
 * type checker outputs. A judgment's correctness is dependent on
 * its preconditions proofs.
 */
sealed class CheckStatus {
    /** the element passed checking */
    class Ok(
        val identifier: String,
        val preconds: List<CheckStatus> = listOf()
    ) : CheckStatus()

    /** the element failed checking */
    class Error(
        val identifier: String,
        val elements: List<RawElement>,
        val message: String,
        val preconds: List<CheckStatus> = listOf()

    ) : CheckStatus()
}