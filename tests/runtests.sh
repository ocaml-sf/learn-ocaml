#!/bin/bash

let count=0

# If the first argument is "set", the script will replace all the expected
# answers by the response of the server. It is useful when you need to
# initialize a set of tests.

# If the environment variable $USE_CLIENT_IMAGE is set to "true", then
# the script will use both images learn-ocaml and learn-ocaml-client,
# instead of the mere learn-ocaml image. It is useful for testing
# cross-version compatibility.

SETTER=0
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
    echo -e "\e[32m$1\e[0m"
}

# print in red $1
red () {
    echo -e "\e[31m$1\e[0m"
}

if use_client_image; then
    green "Running tests with BOTH images (learn-ocaml, learn-ocaml-client)"
else
    green "Running tests with single image learn-ocaml"
fi

# run a server in a docker container
run_server () {
    SYNC="$(pwd)"/"$dir"/sync
    REPO="$(pwd)"/"$dir"/repo

    mkdir "$dir"/sync 2>/dev/null
    chmod o+w "$dir"/sync

    # Run the server in background
    SERVERID=$(docker run --entrypoint '' -d -p 8080:8080 \
      -v "$(pwd)"/"$dir":/home/learn-ocaml/actual \
      -v "$SYNC":/sync -v "$REPO":/repository \
      learn-ocaml /bin/sh \
        -c "learn-ocaml --sync-dir=/sync --repo=/repository build &&
learn-ocaml --sync-dir=/sync --repo=/repository serve")

    # Wait for the server to be initialized
    ./wait-for-it.sh localhost:8080 -s -t 10 -- echo 'learn-ocaml started.'
    # in case of timeout, return exit code 124 (<> 0)

    if [ "$(docker ps -q)" == "" ]; then
	red "PROBLEM, server is not running.\n"

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
        CLIENTID=$(docker run --entrypoint '' -d -it --name=client \
          --add-host host.docker.internal:host-gateway \
          -v "$(pwd)"/"$dir":/home/learn-ocaml/actual \
          learn-ocaml-client /bin/sh)

        if [ "$(docker ps -q -f name=client)" == "" ]; then
            red "PROBLEM, client is not running.\n"

            red "LOGS:"
            docker logs "$CLIENTID"
            docker rm "$CLIENTID" > /dev/null
            exit 1
        fi
    fi
}

clean () {
    popd > /dev/null
    chmod o-w sync

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
    args=(learn-ocaml-client grade --json --id="$subdir"
          /home/learn-ocaml/actual/"$subdir"/"$tosend")
    if use_client_image; then
        docker exec -i "$CLIENTID" "${args[@]}" > res.json 2> stderr.txt
    else
        docker exec -i "$SERVERID" "${args[@]}" > res.json 2> stderr.txt
    fi
    if [ $? -ne 0 ]; then
	red "NOT OK: $dir/$tosend"
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
	    diff res.json "$tosend.json"
	    # If diff failed
	    if [ $? -ne 0 ]; then
		red "DIFF FAILED: $dir/$tosend"
		clean_fail
	    fi
	fi
    fi
    green "OK: $dir/$tosend"
    rm res.json stderr.txt
    let count++
}

handle_subdir () {
    subdir="$(basename "$1")"
    pushd "$1" >/dev/null

    # init config
    args=(learn-ocaml-client init --token="$token")
    if use_client_image; then
        docker exec -i "$CLIENTID" "${args[@]}" --server="http://host.docker.internal:8080"
    else
        docker exec -i "$SERVERID" "${args[@]}" --server="http://localhost:8080"
    fi
    # For each solution
    for tosend in $(find . -name "*.ml" -type f -printf "%f\n")
    do
	handle_file "$subdir" "$tosend"
    done

    clean
}

# For each subdirectory (except ./corpuses/*)
while IFS= read -r dir;
do
    run_server

    pushd "$dir" > /dev/null

    echo "---> Entering $dir:"

    # Get the token from the sync/ directory
    token=$(find sync -maxdepth 5 -mindepth 5 | head -n 1 | sed 's|sync/||' | sed 's|/|-|g')

    # For each subdir (i.e. each exercice)
    while IFS= read -r subdir;
    do
	handle_subdir "$subdir"
    done < <(find . -maxdepth 1 -type d ! -path . ! -path ./repo ! -path ./sync)

    popd > /dev/null
done < <(find . -maxdepth 1 -type d ! -path . ! -path ./corpuses)

#==============================================================================
while IFS= read -r corpus;
do
    echo "---> Testing corpus $corpus:"

    docker run --entrypoint '' -v "$(realpath "$corpus")":/repository \
	   learn-ocaml /bin/sh -c \
	   "learn-ocaml --repo=/repository build"
    if [ $? -ne 0 ]; then
	red "Failed to build $corpus"
	exit 1
    fi

    green "OK: $corpus"
    let count++
done < <(find ./corpuses -mindepth 1 -maxdepth 1 -type d)

green "\nALL $count TESTS PASSED\n"
