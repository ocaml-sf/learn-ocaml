#! /bin/sh

port="9090"
docker_image="learn-ocaml-docker"
container_name="learn-ocaml-docker-container"

function print_usage() {
    printf "Usage: %s COMMAND <OPTIONS>\n\
Commands:\n\
  init  \t Initializes the server and runs it.\n\
  start \t Starts an already initialized server.\n\
  restart\t Restarts the current server (starts it if it is not running).\n\
  stop  \t Stops the server.\n\
  remove\t Stops the server and removes it from the existing containers.\n\
Options:\n\
  -port (default = 9090): Port used to communicate with the server.\n\
  -docker-image (default = learn-ocaml-docker): name of the generated\
  docker image.\n\
  -container-name (default = learn-ocaml-docker-container): name of the\
  container in docker container system.\n" "$0"
}

function run_image () { # port, container, image
    docker run -d \
           -p "$1":9090 \
           --name "$2" "$3"
    
}

while [[ $# -gt 0 ]]; do
  curr="$1"
  case $curr in
      -port)
      port="$2"
      shift
      shift
      ;;
      -docker-image)
      docker_image="$2"
      shift
      shift
      ;;
      -container-name)
      container_name="$2"
      shift
      shift
      ;;
      -*)
      echo "Unknown option $1"
      print_usage
      exit 2
      ;;
      *)
      POSITIONAL+=("$1") # save it in an array for later
      shift 
      ;;
  esac
done

set -- "${POSITIONAL[@]}" # restore positional parameters

case $1 in
    init)
    run_image "$port" "$container_name" "$docker_image" 1> /dev/null;
    ;;
    start)
    docker container start "$container_name" 1> /dev/null
    ;;
    restart)
    docker container restart "$container_name" 1> /dev/null
    ;;
    stop)
    docker container stop "$container_name" 1> /dev/null
    ;;    
    remove)
    docker container stop "$container_name" 1> /dev/null;
    docker container rm "$container_name" 1> /dev/null
    ;;
    *)
    echo "Unknown command $1"
    print_usage  
    exit 2
esac
