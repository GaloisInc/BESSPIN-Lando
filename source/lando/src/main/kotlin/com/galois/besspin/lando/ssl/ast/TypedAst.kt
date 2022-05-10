package com.galois.besspin.lando.ssl.ast

import com.galois.besspin.lando.ssl.ast.Name

data class TypedComponent(
    val name: Name,
    val abbrevName: Name?,
    val explanation: String,
    // NOTE: What does `inherits` mean?
    // This should probably be resolved in some way.
    //    val inherits: List<QName>,
    // NOTE: what does `clientOf` mean?
    // This should probably be resolved in some way.
    //    val clientOf: List<QName>,
    // NOTE: In java these would resolve into getter/setter methods I think
    //    val parts: List<RawComponentPart> = arrayListOf(),
    // NOTE: this should be a doc string
    //    val comments: List<RawComment>
)





