#! /bin/sh

port="9090"
docker_image="learnocaml-docker"
container_name="learnocaml-docker-container"
verbose="0"
fg="-d"

function print_usage() {
    printf "Usage: %s COMMAND <OPTIONS>\n\
Commands:\n\
  init  \t Initializes the server and runs it in background.\n\
  start \t Starts an already initialized server.\n\
  restart\t Restarts the current server (starts it if it is not running).\n\
  stop  \t Stops the server.\n\
  remove\t Stops the server and removes it from the existing containers.\n\
Options:\n\
  -port <[0-65535]> (default = 9090): \n\
\t Port used to communicate with the server.\n\
  -docker-image <string> (default = learnocaml-docker): \n\
\t name of the generated\
  docker image.\n\
  -container-name <string> (default = learnocaml-docker-container): \n\
\t name of the container in docker container system.\n\
  -v <int> (default): verbose mode.\n\
    + 0: no output (except errors)\n\
    + 1: standard output of Docker\n\
    + 2: command executed\n\
  -fg: Runs the container in foreground.\n" "$0"
}

function print_cmd () {
    if [ "$verbose" -gt 1 ]; then
        set -x
    fi
}

function verbose_mode () {
    if [ "$verbose" -gt 0 ]; then
        output="/dev/stdout"
    else
        output="/dev/null"
    fi
}

function run_image () {
    (print_cmd;
     docker run $fg \
           -p "$port":9090 \
           --name "$container_name" "$docker_image")
    
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

function check_port () {
    if ! [[ "$2" =~ ^-?[0-9]+$ ]] ||  [ "$2" -lt 0 ] || [ "$2" -gt 65535 ]; then
        option_value_error "$1" "$2"
    fi
}

function check_verbose () {
    if ! [[ "$2" =~ ^-?[0-9]+$ ]] || [ "$2" -lt 0 ] || [ "$2" -gt 2 ]; then
        option_value_error "$1" "$2"
    fi
}
    
while [[ $# -gt 0 ]]; do
  curr="$1"
  case $curr in
      -port)
          check_port "$1" "$2"
          port="$2"
          shift 2
          ;;
      -docker-image)
          docker_image="$2"
          shift 2
          ;;
      -container-name)
          container_name="$2"
          shift 2
          ;;
      -v)
          check_verbose "$1" "$2"
          verbose="$2"
          shift 2
          ;;
      -fg)
          fg=""
          shift
          ;;
      -*)
          option_error "$1"
          ;;
      *)
          POSITIONAL+=("$1") # save it in an array for later
          shift 
          ;;
  esac
done

set -- "${POSITIONAL[@]}" # restore positional parameters

verbose_mode

case $1 in
    init)
        run_image "$port" "$container_name" "$docker_image" 1> "$output"
    ;;
    start)
        (print_cmd;
         docker container start "$container_name" 1> "$output")
    ;;
    restart)
        (print_cmd;
         docker container restart "$container_name" 1> "$output")
    ;;
    stop)
        (print_cmd;
         docker container stop "$container_name" > "$output")
    ;;    
    remove)
        (print_cmd;
         docker container stop "$container_name" 1> "$output";
         docker container rm "$container_name" 1> "$output")
    ;;
    *)
        echo "Unknown command $1"
        print_usage  
        exit 2
esac
