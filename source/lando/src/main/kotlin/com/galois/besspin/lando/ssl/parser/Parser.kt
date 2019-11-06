package com.galois.besspin.lando.ssl.parser

import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream


fun parseFile(fileName: String) {
    var stream = CharStreams.fromFileName(fileName)

    var lexer = SSLLexer(stream)
    var tokenStream = CommonTokenStream(lexer)

    var parsed = SSLParser(tokenStream)
    var ssl = parsed.ssl()
}

fun parseText(text: String) {
    var stream = CharStreams.fromString(text)

    var lexer = SSLLexer(stream)
    var tokenStream = CommonTokenStream(lexer)

    var parsed = SSLParser(tokenStream)
    var ssl = parsed.ssl()
}


fun toAst(cxt: SSLParser.SslContext) {

}