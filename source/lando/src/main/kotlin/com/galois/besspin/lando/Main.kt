package com.galois.besspin.lando

import com.galois.besspin.lando.ssl.ast.toJSON
import com.galois.besspin.lando.ssl.parser.parseFile
import com.galois.besspin.lando.ssl.checker.RawAstChecker
import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.core.NoRunCliktCommand
import com.github.ajalt.clikt.core.subcommands
import com.github.ajalt.clikt.parameters.arguments.argument
import com.github.ajalt.clikt.parameters.arguments.optional
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.types.choice
import com.github.ajalt.clikt.parameters.types.file
import java.io.File
import kotlin.system.exitProcess


class CommandLine : NoRunCliktCommand(printHelpOnEmptyArgs = true, name = "lando") {
    override fun run() {
    }
}

class Convert : CliktCommand(
    printHelpOnEmptyArgs = true,
    help = "Read a lando SOURCE, convert it to the specified format and write to DEST"
 ) {
     val format by option("-t", "--to").choice("json","markdown").required()
     val source by argument("SOURCE").file(exists = true)
     val dest: File?  by argument("DEST").file().optional()
     val silent by option("-s", "--silent").flag()
     val debug  by option("-d", "--debug").flag()

    enum class ConvertFormat {
        JSON, MARKDOWN
    }

     override fun run() {
         when (format) {
             "json" -> convert(ConvertFormat.JSON, source, dest, debug)
             "markdown" -> convert(ConvertFormat.MARKDOWN, source, dest, debug)
             else -> println("Unknown format: $format")
         }
     }

    fun convert(format: ConvertFormat, source: File, dest: File?, debug: Boolean) {
         try {
             val (ssl, parseWarnings) = parseFile(source, debug)
             if (parseWarnings.isNotEmpty()) {
                 if (silent && (dest != null)) {
                     val destWarns = File(dest.parent, "${dest.nameWithoutExtension}.warnings")
                     printToFile(destWarns, parseWarnings)
                 } else {
                     println(parseWarnings)
                 }
             }
             val typeErrors = RawAstChecker().check(ssl)
             if (typeErrors.isNotEmpty()) {
                 if (silent && (dest != null)) {
                     val destChErrs = File(dest.parent, "${dest.nameWithoutExtension}.cherrors")
                     printToFile(destChErrs, typeErrors)
                 }
                 throw Exception(typeErrors)
             }
             val str = when (format) {
                 ConvertFormat.JSON -> ssl.toJSON()
                 ConvertFormat.MARKDOWN -> ssl.toMarkdown()
             }
             if (dest != null) {
                 printToFile(dest, str)
             } else {
                 println(str)
             }
          } catch (ex: Exception) {
             if (silent && (dest != null)) {
                 val destErrors = File(dest.parent, "${dest.nameWithoutExtension}.errors")
                 printToFile(destErrors, ex.message)
             } else {
                 println("Unable to convert $source to $format\n" + ex.message)
             }
             exitProcess(1)
         }
     }

     fun printToFile(dest: File, str: String?) {
         if (str != null)
             dest.writeText(str)
     }
 }

class Validate : CliktCommand(
    printHelpOnEmptyArgs = true,
    help = "Read a lando SOURCE and check whether it is syntactically valid and well formed"
) {
    val source by argument("SOURCE").file(exists = true)
    val silent by option("-s", "--silent").flag()
    val debug  by option("-d", "--debug").flag()

    override fun run() {
        try {
            val (ssl, warnings) = parseFile(source, debug)
            if (warnings.isNotEmpty() && !silent)
                println(warnings)
            val typeErrors = RawAstChecker().check(ssl)
            if (typeErrors.isNotEmpty()) {
                throw Exception(typeErrors)
            }
        } catch (ex: Exception) {
            if (!silent) {
                println("Unable to validate file $source\n" + ex.message)
            }
            exitProcess(1)
        }
        println("$source appears to be valid")
    }
}

fun main(args: Array<String>) {
    CommandLine().subcommands(Convert(), Validate()).main(args)
}

