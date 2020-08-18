# Lando

This repository contains an implementation of a parser for the Lando
System Specification Sublanguage and a command-line application built
around it.

## Building

The parser and application are written in a combination of Kotlin and
Java and packaged to be built with Maven. To build the package you
need an installation of the Java SDK, Version 11 or higher and the
Kotlin compiler.

Run the script `lando.sh` located on the top level with argument `-r` to
build/rebuild the package. This can also be done directly by running
`mvn package`. Both produce packaged jar files in the `target` directory. The
jar file named `lando-1.0-SNAPSHOT-jar-with-dependencies.jar` includes all the
dependencies including the Kotlin runtime and can be run on a JVM without the
need for a local installation of Kotlin.

## Usage

The easiest way to run the SSL parser is to use the script `lando.sh` located
on the top level with argument `-f FILE`, where `FILE` a path to a lando file.
This parses the given file and converts it to JSON. Use the argument `-h` to
view the available options.

You can also run the CLI directly using something like
`java -jar ./target/lando-1.0-SNAPSHOT-jar-with-dependencies.jar`. It should
print out a help message with various commands and options that should get
you started.

Currently available commands
- `convert` to convert a lando SSL file to another format. Only `json` is supported at this point
- `validate` to validate a lando SSL file. Currently validation is purely syntactic; i.e. checking whether it conforms to the grammar.

Both commands support the options
- `-d`/`--debug` to show debug information while lexing
- `-s`/`--silent` to suppresses console output, and in the case of `convert`, write all errors to a file.

## Testing

Run `lando.sh` with argument `-t` to run all tests in `src/test`. This can also be done directly by running `mvn package` or `mvn surefire:test`.
