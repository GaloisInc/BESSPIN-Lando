package com.galois.besspin.lando.ssl.ast

import com.galois.besspin.lando.ssl.ast.Name

data class TypedComponent(
    val name: Name,
    val abbrevName: Name?,
    val explanation: String,
    // @kiniry: This is all spelled out in the BON book and the Lando typesystem.
    // In particular, we do not permit multiple re-inheritance via subtyping
    val inherits: Set<QName>,
    val clientOf: List<QName>,
    // NOTE: In java these would resolve into getter/setter methods I think
    //    val parts: List<RawComponentPart> = arrayListOf(),
    // NOTE: this should be a doc string
    //    val comments: List<RawComment>
)





