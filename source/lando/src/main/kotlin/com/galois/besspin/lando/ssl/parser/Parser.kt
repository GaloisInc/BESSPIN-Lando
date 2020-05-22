package com.galois.besspin.lando.ssl.parser

import com.galois.besspin.lando.ssl.ast.RawSSL
import org.antlr.v4.runtime.*
import org.antlr.v4.runtime.misc.ParseCancellationException
import java.io.File

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

    var errors: MutableList<SyntaxError> = arrayListOf()
    var warnings: MutableList<SyntaxError> = arrayListOf()

    @Throws(ParseCancellationException::class)
    override fun syntaxError(
        recognizer: Recognizer<*, *>?,
        offendingSymbol: Any?,
        line: Int,
        charPositionInLine: Int,
        msg: String?,
        e: RecognitionException?
    ) {
        if (msg?.startsWith(SSLParser.warningPrefix) == true)
            warnings.add(SyntaxError(offendingSymbol, line, charPositionInLine,
                         msg.removePrefix(SSLParser.warningPrefix)))
        else
            errors.add(SyntaxError(offendingSymbol, line, charPositionInLine, msg ?: ""))
    }

    fun formatErrors(): String = errors.map { e -> e.formatError() }.joinToString(separator = "\n")

    fun formatWarnings() : String {
        if (warnings.size != 0)
            return "Generated ${warnings.size} warning(s):\n" +
                    warnings.map { e -> e.formatError() }.joinToString(separator="\n")
        else
            return ""
    }


}

fun parseStream(stream: CharStream, debugLexer: Boolean = false): Pair<RawSSL,String> {
    val errorListener = CollectingErrorListener()

    val lexer = SSLLexer(stream)
    lexer.debug = debugLexer
    lexer.removeErrorListeners()
    lexer.addErrorListener(errorListener)

    val tokenStream = CommonTokenStream(lexer)

    val parser = SSLParser(tokenStream)
    parser.removeErrorListeners()
    parser.addErrorListener(errorListener)

    val landoSource = parser.landoSource()

    if(errorListener.errors.size != 0) {
        throw IllegalStateException("Parser Failed due to the following errors:\n${errorListener.formatErrors()}\n")
    } else {
        return Pair(RawAstBuilder(landoSource).build(), errorListener.formatWarnings())
    }
}

fun parseFile(file: File, debugLexer: Boolean = false): Pair<RawSSL,String> =
        parseStream(CharStreams.fromPath(file.toPath()), debugLexer)

fun parseText(text: String, debugLexer: Boolean = false): Pair<RawSSL,String> =
        parseStream(CharStreams.fromString(text), debugLexer)