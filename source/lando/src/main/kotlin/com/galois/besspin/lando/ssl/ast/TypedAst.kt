package com.galois.besspin.lando.ssl.ast

// @podhrmic: Is this just a different way of representing the RawAST (using ADT)
// or is this actually useful?
// See: https://medium.com/sharenowtech/kotlin-adt-74472319962a and
// https://proandroiddev.com/algebraic-data-types-in-kotlin-337f22ef230a
//
sealed class LandoType {
    // Contains:
    // explanation?
    // system | subsystem | subsystemImport | component | componentImport || events | scenarios | requirements | relations
    object System : LandoType()
    // Contains:
    // explanation?
    // system | subsystem | subsystemImport | component | componentImport || events | scenarios | requirements | relations
    object Subsystem : LandoType()
    // Contains:
    // explanation?
    // constraint | command | query
    //
    data class Component(
        val name: Name,
        val abbrevName: Name?,
        val parts: List<RawComponentPart> = arrayListOf(),
        // ...
    ): LandoType()

    // TODO
    //object Events : LandoType()
    //object Scenarios : LandoType()
}

// Typed AST should end up being a list of LandoTypes
// NOTE: does it make a difference to have a tree like structures?
typealias TypedBody = List<LandoType>