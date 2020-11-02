package com.galois.besspin.lando

import com.galois.besspin.lando.ssl.ast.toJSON
import com.galois.besspin.lando.ssl.parser.parseFile
import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.core.NoRunCliktCommand
import com.github.ajalt.clikt.core.subcommands
import com.github.ajalt.clikt.parameters.arguments.argument
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.types.choice
import com.github.ajalt.clikt.parameters.types.file
import java.io.File


class CommandLine : NoRunCliktCommand(printHelpOnEmptyArgs = true, name = "lando") {
    override fun run() {
    }
}

class Convert : CliktCommand(
    printHelpOnEmptyArgs = true,
    help = "Read a lando SOURCE, convert it to the specified format and write to DEST"
) {
    val format by option("-t", "--to").choice("json").required()
    val source by argument("SOURCE").file(exists = true)
    val dest   by argument("DEST").file()
    val silent by option("-s", "--silent").flag()
    val debug  by option("-d", "--debug").flag()

    override fun run() {
        when (format) {
            "json" -> toJSON(source, dest, debug)
            else -> println("Unable to convert to format: $format")
        }
    }

    fun toJSON(source: File, dest: File, debug: Boolean) {
        try {
            val (ssl, warnings) = parseFile(source, debug)
            val str = ssl.toJSON()

            printToFile(dest, str)

            if (warnings.isNotEmpty()) {
                if (!silent) {
                    println(warnings)
                } else {
                    val destWarns = File(dest.parent, "${dest.nameWithoutExtension}.warnings")
                    printToFile(destWarns, warnings)
                }
            }
        } catch (ex: Exception) {
            if (!silent) {
                println("Unable to convert  file to JSON. " + ex.message)
            } else {
                val destErrors = File(dest.parent, "${dest.nameWithoutExtension}.errors")
                printToFile(destErrors, ex.message)
            }
            System.exit(1)
        }
    }

    fun printToFile(dest: File, str: String?) {
        if (str != null)
            dest.writeText(str)
    }
}

class Validate : CliktCommand(
    printHelpOnEmptyArgs = true,
    help = "Read a lando SOURCE and check whether it is syntactically valid"
) {
    val source by argument("SOURCE").file(exists = true)
    val silent by option("-s", "--silent").flag()
    val debug  by option("-d", "--debug").flag()

    override fun run() {
        try {
            val (_, warnings) = parseFile(source, debug)
            if (warnings.isNotEmpty() && !silent) {
                println(warnings)
            }
        } catch (ex: Exception) {
            if (!silent) {
                println("$source appears to have syntax errors. " + ex.message)
            }
            System.exit(1)
        }

        println("$source appears to be valid")
    }
}

fun main(args: Array<String>) {
    CommandLine().subcommands(Convert(), Validate()).main(args)
}

