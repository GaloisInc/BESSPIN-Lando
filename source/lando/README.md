# Lando

This repository contains an implementation of a parser for the Lando System Specification Sublanguage and a command-line application built around it.

## Building

The parser and application are written in a combination of Kotlin and Java and packaged to be built with Maven. To build the package you need an installation of the Java SDK, Version 11 or higher and the Kotlin compiler.

Use `mvn package` to do the honors and produce packaged jar files in the `target` directory. The jar file named `lando-1.0-SNAPSHOT-jar-with-dependencies.jar` includes all the dependencies including the Kotlin runtime and can be run on a JVM without the need for a local installation of Kotlin.

## Usage

After building the application, you can run the CLI using something like `java -jar ./target/lando-1.0-SNAPSHOT-jar-with-dependencies.jar`. It should print out a help message with various commands and options that should get your started.

Currently available commands
- `convert` to convert a lando SSL file to another format. Only `json` is supported at this point
- `validate` to validate a lando SSL file. Currently validation is purely syntactic; i.e. checking whether it conforms to the grammar.
