
# Parsing English-Language Leakage Specifications in Lando using GF

The code in this directory uses the
[Grammatical Framework](http://www.grammaticalframework.org/) to parse English
sentences about information and timing leaks written in Lando files and convert
them into Clafer constraints. Here are some example English sentences that the
current parser can handle:

* Timing Leakage Sentences:

    * The integer-multiplication operation of my Rocket processor doesn't leak
      input-operand values through a timing side-channel.

    * The OP instruction doesn't leak timing information.

    * The OP instruction leaks timing information.

    * No instruction leaks timing information.

    * All arithmetic instructions leak timing information.

    * Only the multiply instruction out of all the arithmetic instructions leaks timing information.

    * Only the Fmsub_s instruction leaks timing information in our P3 processor.

    * The enable signal of the MulDiv Verilog module of my Boom processor isn't
      leaked through a timing side-channel.

* Digital Information Leakage Sentences:

    * The reset signal of the ALU Verilog module of my Boom processor isn't
      leaked through an information-flow channel.

    * The secret-key register of my AES block isn't leaked outside of the
      certification boundary of my crypto module.

    * The secret-key register of my AES block isn't leaked through the ciphertext output channel.


# Running the Parser

English-language sentences such as the above can be parsed using the
`eng_to_clafer.sh` shell script. Simply run this script and pipe a sentence on
`stdin` to the script, and it will output a Clafer class with constraints that
corresponds to that sentence.

**NOTE**: The trailing period must be omitted from the English-language
sentence.
