package com.galois.besspin.lando.ssl.parser

import com.galois.besspin.lando.ssl.ast.*
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import java.io.File
import java.lang.IllegalStateException
import org.antlr.v4.runtime.misc.ParseCancellationException
import org.antlr.v4.runtime.RecognitionException
import org.antlr.v4.runtime.Recognizer
import org.antlr.v4.runtime.BaseErrorListener

data class SyntaxError(
    val offendingSymbol: Any?,
    val line: Int,
    val column: Int,
    val message: String
)
{
    fun formatError(): String = "Line: $line, Column: $column. $message"
}

private class CollectingErrorListener : BaseErrorListener() {

    var errors: MutableList<SyntaxError> = arrayListOf() ;

    @Throws(ParseCancellationException::class)
    override fun syntaxError(
        recognizer: Recognizer<*, *>?,
        offendingSymbol: Any?,
        line: Int,
        charPositionInLine: Int,
        msg: String?,
        e: RecognitionException?
    ) {
        errors.add(SyntaxError(offendingSymbol, line, charPositionInLine, msg ?: ""))
    }

    fun formatErrors(): String = errors.map { e -> e.formatError() }.joinToString(separator = "\n")
}

fun parseFile(file: File): RawSSL {
    val errorListener = CollectingErrorListener()

    val stream = CharStreams.fromPath(file.toPath())

    val lexer = SSLLexer(stream)
    lexer.removeErrorListeners();
    lexer.addErrorListener(errorListener)

    val tokenStream = CommonTokenStream(lexer)

    val parser = SSLParser(tokenStream)
    parser.removeErrorListeners()
    parser.addErrorListener(errorListener)

    val landoSource = parser.landoSource()

    if(errorListener.errors.size != 0) {
        throw IllegalStateException("Parser Failed due to the following errors:\n${errorListener.formatErrors()}\n")
    } else {
        return RawAstBuilder(landoSource).build()
    }
}

fun parseText(text: String): RawSSL {
    val errorListener = CollectingErrorListener()

    val stream = CharStreams.fromString(text)

    val lexer = SSLLexer(stream)
    lexer.removeErrorListeners();
    lexer.addErrorListener(errorListener)

    val tokenStream = CommonTokenStream(lexer)

    val parser = SSLParser(tokenStream)
    parser.removeErrorListeners()
    parser.addErrorListener(errorListener)

    val landoSource = parser.landoSource()

    if(errorListener.errors.size != 0) {
        throw IllegalStateException("Parser Failed due to the following errors:\n${errorListener.formatErrors()}\n")
    } else {
        return RawAstBuilder(landoSource).build()
    }
}
