#!/usr/bin/env bash
# Requirements: this script uses docker (and curl: see wait_for_it)

# Increase this timeout if ever one sub-repo build would last > 10s
build_timeout=10

# If the first argument is "set", the script will replace all the expected
# answers by the response of the server. It is useful when you need to
# initialize a set of tests.

# If the environment variable $USE_CLIENT_IMAGE is set to "true", then
# the script will use both images learn-ocaml and learn-ocaml-client,
# instead of the mere learn-ocaml image. It is useful for testing
# cross-version compatibility.

SETTER=0
srcdir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )
count=0
if [ "$1" == "set" ]; then SETTER=1; fi

use_client_image () {
    if [ "$USE_CLIENT_IMAGE" = "true" ]; then
        return 0
    else
        return 1
    fi
}

# print in green $1
green () {
    echo -e "\\e[32m$1\\e[0m"
}

# print in red $1
red () {
    echo -e "\\e[31m$1\\e[0m"
}

if use_client_image; then
    green "Running tests with BOTH images (learn-ocaml, learn-ocaml-client)"
else
    green "Running tests with single image learn-ocaml"
fi

wait_for_it () {
  local url="$1"
  local seconds="$2"
  shift 2  # "$@" => optional command to be run in case of success
  local elapsed=0
  green "waiting $seconds seconds for $url\\n"
  while :; do
      curl -fsS "$url" >/dev/null 2>/dev/null && break
      [ "$elapsed" -ge "$seconds" ] &&
          { red "timeout occurred after waiting $seconds seconds for $url\\n";
            return 124; }
      elapsed=$((elapsed + 1))
      sleep 1s
  done
  green "$url available after $elapsed seconds"
  if [ $# -gt 0 ]; then
      ( set -x; "$@" )
  fi
  return 0
}

# run a server in a docker container
run_server () {
    SYNC="$srcdir"/"$dir"/sync
    REPO="$srcdir"/"$dir"/repo
    chmod -R a+w "$REPO"

    mkdir -p "$SYNC" 2>/dev/null
    chmod o+w "$SYNC"

    # Run the server in background
    SERVERID=$(set -x; docker run -d -p 8080:8080 \
      -v "$srcdir/$dir":/home/learn-ocaml/actual \
      -v "$SYNC":/sync -v "$REPO":/repository \
      learn-ocaml)

    # Wait for the server to be initialized
    if ! wait_for_it "http://localhost:8080/version" "$build_timeout" sleep 1s ||
    [ "$(docker ps -q)" == "" ]; then
        red "PROBLEM, server is not running.\\n"

        red "LS:"
        ls -Rl "$dir"
        echo ""

        red "LOGS:"
        docker logs "$SERVERID"
        docker rm "$SERVERID" > /dev/null
        exit 1
    fi

    if use_client_image; then
        # Run the client in background
        CLIENTID=$(set -x; docker run --entrypoint '' -d -it --name=client \
          --add-host host.docker.internal:host-gateway \
          -v "$srcdir/$dir":/home/learn-ocaml/actual \
          learn-ocaml-client /bin/sh)

        if [ "$(docker ps -q -f name=client)" == "" ]; then
            red "PROBLEM, client is not running.\\n"

            red "LOGS:"
            docker logs "$CLIENTID"
            docker rm "$CLIENTID" > /dev/null
            exit 1
        fi
    fi
}

clean () {
    if popd > /dev/null; then
        chmod o-w "$SYNC"
    else
        red "popd failed"
    fi

    docker kill "$SERVERID" > /dev/null
    docker rm   "$SERVERID" > /dev/null

    if use_client_image; then
        docker kill "$CLIENTID" > /dev/null
        docker rm   "$CLIENTID" > /dev/null
    fi
}

clean_fail () {
    clean
    exit 1
}

handle_file () {
    subdir="$1"
    tosend="$(basename "$2")"
    # Grade file
    if use_client_image; then
        args=(-i "$CLIENTID")
    else
        args=(-i "$SERVERID")
    fi
    args+=(learn-ocaml-client grade --json "--id=$subdir"
           /home/learn-ocaml/actual/"$subdir"/"$tosend")
    if ! docker exec "${args[@]}" > res.json 2> stderr.txt; then
	red "NOT OK: $dir/$subdir/$tosend"
	cat stderr.txt
	clean_fail
    fi
    if [ $SETTER -eq 1 ]; then
	cp res.json "$tosend.json"
    else
	# If there isn't something to compare
	if [ ! -f "$tosend.json" ]; then
	    red "$tosend.json does not exist"
	    clean_fail
	else
	    if ! diff res.json "$tosend.json"; then
		red "DIFF FAILED: $dir/$subdir/$tosend"
		clean_fail
	    fi
	fi
    fi
    green "OK: $dir/$subdir/$tosend"
    rm res.json stderr.txt
    (( count++ ))
}

handle_subdir () {
    subdir="$(basename "$1")"
    pushd "$1" >/dev/null || { red "pushd failed"; clean_fail; }
    echo "--->---> Entering $subdir:"

    run_server

    # Get the token from the docker logs
    token=$(docker logs "$SERVERID" | \
                grep -e 'Initial teacher token created:' | \
                sed -e 's/^.*: //' | tr -d '[:space:]')
    if [ -z "$token" ]; then
        token=$(docker logs "$SERVERID" | \
                grep -A 1 -e 'Found the following teacher tokens:' | tail -n 1 | \
                sed -e 's/^.*- //' | tr -d '[:space:]')
        if [ -z "$token" ]; then
            red "Failed to get teacher token"
            clean_fail
        fi
    fi

    # init config
    args=(learn-ocaml-client init "--token=$token")
    if use_client_image; then
        docker exec -i "$CLIENTID" "${args[@]}" --server="http://host.docker.internal:8080"
    else
        docker exec -i "$SERVERID" "${args[@]}" --server="http://localhost:8080"
    fi
    # For each solution
    # shellcheck disable=SC2044
    for tosend in $(find . -name "*.ml" -type f -printf "%f\\n")
    do
	handle_file "$subdir" "$tosend"
    done

    clean
}

echo
cd "$srcdir"

# For each subdirectory (except ./corpuses/*)
while IFS= read -r dir;
do
    pushd "$dir" > /dev/null || { red "pushd failed"; clean_fail; }

    echo "---> Entering $dir:"

    # For each subdir (i.e. each exercice)
    while IFS= read -r subdir;
    do
        handle_subdir "$subdir"
    done < <(find . -maxdepth 1 -type d ! -path . ! -path ./repo ! -path ./sync)

    popd > /dev/null || { red "popd failed"; clean_fail; }
done < <(find . -maxdepth 1 -type d ! -path . ! -path ./corpuses)

#==============================================================================
echo
cd "$srcdir"

while IFS= read -r corpus;
do
    echo "---> Testing corpus $corpus:"

    chmod -R a+w "$corpus"

    if ! ( set -x; docker run --rm \
           -v "$(realpath "$corpus"):/repository" \
           learn-ocaml build ); then
        red "Failed to build $corpus"
        exit 1
    fi

    green "OK: $corpus"
    (( count++ ))
done < <(find ./corpuses -mindepth 1 -maxdepth 1 -type d)

green "\\nALL $count TESTS PASSED\\n"
