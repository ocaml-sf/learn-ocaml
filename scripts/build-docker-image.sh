#! /bin/bash

root=${PWD}
repo_dir="$root"/demo-repository
image_name="learnocaml-docker"

function print_usage() {
    printf "Usage: %s <OPTIONS>\n\
Options:\n\
  -repo-dir <directory> (default = demo-repository): \n\
\t Repository containing the exercises.\n\
  -root <directory> (default = %s): \n\
\t Root of the learn-ocaml repository.\n" "$0" "$PWD"
}

function option_error () {
    printf "Incorrect option $1\n" 1>&2
    print_usage
    exit 2
}

function option_value_error () {
    printf "Incorrect value $2 for option $1\n" 1>&2
    print_usage
    exit 2
}

function check_directory () {
    if ! [ -d "$2" ]; then
        printf "$2 is not a valid directory.\n" 1>&2
        option_value_error "$1" "$2"
    fi
}

while [[ $# -gt 0 ]]; do
  curr="$1"

  case $curr in
      -image-name)
          image_name="$2"
          shift 2
          ;;
      -repo-dir)
          check_directory "$1" "$2"
          repo_dir="$2"
          shift 2
          ;;
      -root)
          check_directory "$1" "$2"
          root="$2"
          shift 2
          ;;
      *)    # unknown option
          option_error $1
          ;;
  esac
done

exercises_repository="$root"/exercises_repository
      
if [ -d "$exercises_repository" ]; then
    rm -rf "$exercises_repository"
fi

cp -r "$repo_dir" "$exercises_repository" 

docker build -f "$root/Dockerfile" -t "$image_name" "$root"

rm -rf "$exercises_repository"

