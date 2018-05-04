#! /bin/sh

port="9090"
docker_image="learn-ocaml-docker"
container_name="learn-ocaml-docker-container"

function print_usage() {
    printf "Usage: %s COMMAND <OPTIONS>\n\
Commands:\n\
  init  \t Initializes the server and run it.\n\
  start \t Starts an already initialized server.\n\
  restart\t Restarts the current server (starts it if it is not running).\n\
  stop  \t Stops the server.\n\
  remove\t Remove the current server from the existing containers.\n\
  force-remove\t Stops and remove de container.\n\
  \n\
Options:\n\
  -port (default = 9090): Port used to communicate with the server.\n\
  -docker-image (default = learn-ocaml-docker): name of the generated\
  docker image.\n\
  -container-name (default = learn-ocaml-docker-container): name of the\
  container in docker container system.\n" "$0"
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
    docker run -d \
           -p "$port":9090 \
           --name "$container_name" "$docker_image"
    ;;
    start)
    docker container start "$container_name"
    ;;
    restart)
    docker container restart "$container_name"
    ;;
    stop)
    docker container stop "$container_name"
    ;;    
    remove)
    docker container rm "$container_name"
    ;;
    force-remove)
    docker container stop "$container_name" &&
    docker container rm "$container_name"
    ;;
    *)
    echo "Unknown command $1"
    print_usage  
    exit 2
esac
