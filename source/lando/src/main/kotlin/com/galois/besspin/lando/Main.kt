package com.galois.besspin.lando

import com.galois.besspin.lando.ssl.ast.toJSON
import com.galois.besspin.lando.ssl.parser.parseFile

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.core.NoRunCliktCommand
import com.github.ajalt.clikt.core.subcommands
import com.github.ajalt.clikt.parameters.arguments.argument
import com.github.ajalt.clikt.parameters.arguments.multiple
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.types.choice
import com.github.ajalt.clikt.parameters.types.file
import kotlinx.io.PrintWriter
import java.io.File

import java.lang.System

typealias LSystem = com.galois.besspin.lando.ssl.ast.System


class CommandLine : NoRunCliktCommand(printHelpOnEmptyArgs = true) {
    override fun run() {
    }
}

class Convert : CliktCommand(
    printHelpOnEmptyArgs = true,
    help = "Read a lando SOURCE, convert it to the specified format and write to DEST"
) {
    val format by option("-t", "--to").choice("json").required()
    val source by argument("SOURCE").file(exists = true)
    val dest by argument().file()

    override fun run() {
        when (format) {
            "json" -> toJSON(source, dest)
            else -> println("Unable to convert to format: $format")
        }
    }

    fun toJSON(src: File, dst: File) {
        try {
            val ssl = parseFile(source)
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

    override fun run() {
        try {
            parseFile(source)
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

