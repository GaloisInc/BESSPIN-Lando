package com.galois.besspin.lando

import com.galois.besspin.lando.ssl.parser.SSLParser ;
import com.galois.besspin.lando.ssl.parser.parseFile

fun main(args: Array<String>) {
    if (args.size == 0) {
        println("Usage: Lando <file>")
        System.exit(2)
    }
    try {
        val ssl = parseFile(args[0])
    } catch (ex: Exception) {
        println("Failed to parse file: ${args[0]}")
        ex.printStackTrace();
        System.exit(1)
    }

    println("Successfully parsed file: ${args[0]}")
    System.exit(0)
}

