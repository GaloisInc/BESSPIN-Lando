# Lando

This repository is organized as follows:

- `source/lobot` - A parser, type checker, and instance generator for the Lobot
  sublanguage of Lando. See `source/lobot/README.md` for more details.
  
- `source/lando` - A parser and command line tool for the Lando System
  Specification (Sub)Language (SSL).
  
  To run the SSL parser on a `.lando` file, first run `./lando.sh -r` to build
  the executable, then run:
  ```
  $ ./lando.sh -f path/to/source.lando
  ```
  Use `-h` to view the available options. See `source/lando/README.md` for more
  details.
  
- `docs` - Formal description of Lobot and the Lando SSL.
