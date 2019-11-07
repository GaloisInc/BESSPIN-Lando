package com.galois.besspin.lando

import com.galois.besspin.lando.ssl.parser.*

import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration
import kotlinx.serialization.modules.SerializersModule
import java.lang.System

typealias LSystem = com.galois.besspin.lando.ssl.parser.System

fun main(args: Array<String>) {
    if (args.size == 0) {
        println("Usage: Lando <file>")
        System.exit(2)
    }
    try {
        val ssl = parseFile(args[0])

        //val gson = GsonBuilder().setPrettyPrinting().create()
        //val str = gson.toJson(ssl)


        val sslModule = SerializersModule {
            polymorphic(Element::class) {
                LSystem::class with LSystem.serializer()
                Subsystem::class with Subsystem.serializer()
                Component::class with Component.serializer()
                Events::class with Events.serializer()
                Scenarios::class with Scenarios.serializer()
                Requirements::class with Requirements.serializer()
            }

            polymorphic(ComponentPart::class) {
                Query::class with Query.serializer()
                Constraint::class with Constraint.serializer()
                Command::class with Command.serializer()
            }
        }

        val config = JsonConfiguration(prettyPrint = true)
        val json = Json(context = sslModule, configuration = config)

        val str = json.stringify(SSL.serializer(), ssl)
        println(str)
    } catch (ex: Exception) {
        println("Failed to parse file: ${args[0]}")
        ex.printStackTrace();
        System.exit(1)
    }

    //println("Successfully parsed file: ${args[0]}")
    System.exit(0)
}

