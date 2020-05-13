package com.galois.besspin.lando

import com.galois.besspin.lando.ssl.ast.toJSON
import com.galois.besspin.lando.ssl.parser.*

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.core.NoRunCliktCommand
import com.github.ajalt.clikt.core.subcommands
import com.github.ajalt.clikt.parameters.arguments.argument
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.types.choice
import com.github.ajalt.clikt.parameters.types.file
import kotlinx.io.PrintWriter
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import java.io.File

import java.lang.System

typealias LSystem = com.galois.besspin.lando.ssl.ast.RawSystem


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
    val debug  by option("-d", "--debug").flag()

    override fun run() {
        when (format) {
            "json" -> toJSON(source, dest, debug)
            else -> println("Unable to convert to format: $format")
        }
    }

    fun toJSON(source: File, dest: File, debug: Boolean) {
        try {
            val ssl = parseFile(source, debug)
            val str = ssl.toJSON()

            val writer = PrintWriter(dest)
            writer.print(str)
            writer.close()
        } catch (ex: Exception) {
            println("Unable to convert  file to JSON. " + ex.message)
            System.exit(1)
        }
    }
}

class Validate : CliktCommand(
    printHelpOnEmptyArgs = true,
    help = "Read a lando SOURCE and check whether it is syntactically valid"
) {
    val source by argument("SOURCE").file(exists = true)
    val debug  by option("-d", "--debug").flag()

    override fun run() {
        try {
            parseFile(source, debug)
        } catch (ex: Exception) {
            println("$source appears to have syntax errors. " + ex.message)
            System.exit(1)
        }

        println("$source appears to be valid")
    }
}

fun main(args: Array<String>) {
    CommandLine().subcommands(Convert(), Validate()).main(args)
}

