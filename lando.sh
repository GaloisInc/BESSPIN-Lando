#!/bin/bash

# This script must be located in the top level of the lando repo
LANDO_DIR=$(dirname $0)
SSL_DIR="${LANDO_DIR}/source/lando"

cmd_prefix="java -jar ${SSL_DIR}/target/lando-1.0-SNAPSHOT-jar-with-dependencies.jar"

usage="Usage: lando.sh [-f FILE] [OPTION]
  Parses a given lando file and converts it to JSON. Options:
  -d  Show debug information while lexing
  -s  Silent - no console output, save all errors to a file
  -v  Validate only - do not save any files (JSON or errors)
  -r  Rebuild   ('-f FILE' is optional)
  -t  Run tests ('-f FILE' is optional)
  -h  Show this help message"

if [ "$#" -eq 0 ]; then
  echo "No arguments given." 1>&2;
  echo "$usage" 1>&2;
  exit 1
fi

do_rebuild=false
do_test=false
do_convert=false
do_validate=false
opt_flags=""

while getopts ":f:dsvrth" opt "$@"
do
  case $opt in
    f)
      filename=$OPTARG
      do_convert=true
      ;;
    d)
      opt_flags="$opt_flags --debug"
      ;;
    s)
      opt_flags="$opt_flags --silent"
      ;;
    v)
      do_validate=true
      ;;
    r)
      do_rebuild=true
      ;;
    t)
      do_test=true
      ;;
    h)
      echo "$usage"
      exit 0
      ;;
    ?)
      echo "Invalid option: -$OPTARG" 1>&2;
      echo "$usage";
      exit 1
      ;;
  esac
done

if [[ "$do_convert" = false && ("$opt_flags" != "" || "$do_validate" = true) ]]
then
  echo "Expected a file."
  exit 1
fi

cmd1=":"

if [[ "$do_rebuild" = true && "$do_test" = true ]]
then
  cmd1="mvn package -f ${SSL_DIR}/pom.xml"
elif [[ "$do_rebuild" = true && "$do_test" = false ]]
then
  cmd1="mvn -Dmaven.test.skip=true package -f ${SSL_DIR}/pom.xml"
elif [[ "$do_rebuild" = false && "$do_test" = true ]]
then
  cmd1="mvn surefire:test -f ${SSL_DIR}/pom.xml"
fi

cmd2=":"

if [[ "$do_convert" = true ]]
then
  filename_no_ext="${filename%.*}"
  cmd2_suffix="convert --to json ${filename} ${filename_no_ext}.json"
  if [[ "$do_validate" = true ]]
  then
    cmd2_sufix="validate ${filename}"
  fi
  cmd2="${cmd_prefix} ${cmd2_suffix} ${opt_flags}"
fi

${cmd1} && ${cmd2}
