#!/bin/bash

# This script must be located in the top level of the lando repo
LANDO_DIR=$(dirname $0)
SSL_DIR="${LANDO_DIR}/source/lando"

cmd_prefix="java -jar ${SSL_DIR}/target/lando-1.0-SNAPSHOT-jar-with-dependencies.jar"

usage="Usage: lando.sh [-f FILE] [-o FILE] [-e] [-d] [-p] [-r] [-t] [-h]
  Parses a given lando file and converts it to JSON. Options:
  -f FILE  Set input filename. This option must be present when
           using -o, -e, -p, or -d.
  -o FILE  Set output filename. If this option is not present, this
           defaults to the input filename with extension '.json'.
  -e       Redirects errors/warnings from stdout to the output
           filename with extension '.errors' or '.warnings'.
  -p 	   Parse only
  -d       Show debug information while lexing
  -r       Rebuild
  -t       Run tests
  -h       Show this help message"

if [ "$#" -eq 0 ]; then
  echo "No arguments given." 1>&2;
  echo "$usage" 1>&2;
  exit 1
fi

filename=""
filename_out=""
opt_flags=""
do_rebuild=false
do_test=false
parse_only=false

while getopts ":f:o:edprth" opt "$@"
do
  case $opt in
    f)
      filename=$OPTARG
      ;;
    o)
      filename_out=$OPTARG
      ;;
    e)
      opt_flags="$opt_flags --silent"
      ;;
    d)
      opt_flags="$opt_flags --debug"
      ;;
    p)
      parse_only=true
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

if [[ "$filename" = "" && ("$filename_out" != "" || "$opt_flags" != "") ]]
then
  echo "Expected an input file."
  exit 1
fi

if [[ "$filename" != "" ]]
then
  if [[ "$filename_out" = "" ]]
  then
    filename_no_ext="${filename%.*}"
    filename_out="${filename_no_ext}.json"
  fi
  if [[ "$parse_only" = true ]] 
  then
      cmd2_suffix="validate ${filename}"
  else
      cmd2_suffix="convert --to json ${filename} ${filename_out}"
  fi
  cmd2="${cmd_prefix} ${cmd2_suffix} ${opt_flags}"
fi

${cmd1} && ${cmd2}
