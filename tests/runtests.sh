#!/bin/bash

let count=0

# If the first argument is "set", the script will replace all the expected 
# answers by the response of the server. It is useful when you need to
# initialize a set of tests.

SETTER=0
if [ "$1" == "set" ]; then SETTER=1; fi

# print in green $1
green () {
    echo -e "\e[32m$1\e[0m"
}

# print in red $1
red () {
    echo -e "\e[31m$1\e[0m"
}

# run a server in a docker container
run_server () {
    SYNC="$(pwd)"/"$dir"/sync
    REPO="$(pwd)"/"$dir"/repo

    mkdir "$dir"/sync 2>/dev/null
    chmod o+w "$dir"/sync

    # Run the server in background
    SERVERID=$(docker run --entrypoint '' -d \
      -v "$(pwd)"/"$dir":/home/learn-ocaml/actual \
      -v "$SYNC":/sync -v "$REPO":/repository \
      learn-ocaml /bin/sh \
        -c "learn-ocaml --sync-dir=/sync --repo=/repository build && 
learn-ocaml --sync-dir=/sync --repo=/repository build serve")

    # Wait for the server to be initialized
    sleep 2

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
}

clean () {
    popd > /dev/null
    chmod o-w sync

    docker kill "$SERVERID" > /dev/null
    docker rm   "$SERVERID" > /dev/null
}

clean_fail () {
    clean
    exit 1
}

handle_file () {
    subdir="$1"
    tosend="$(basename "$2")"
    # Grade file
    docker exec -i "$SERVERID" \
	   learn-ocaml-client grade --json --id="$subdir" \
	   /home/learn-ocaml/actual/"$subdir"/"$tosend" > res.json 2> stderr.txt
    if [ $? -ne 0 ]; then
	red "NOT OK: $dir$tosend"
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
		red "DIFF FAILED: $dir$tosend"
		clean_fail
	    fi
	fi
    fi
    green "OK: $dir$tosend"
    rm res.json stderr.txt
    let count++
}

handle_subdir () {
    subdir="$(basename "$1")"
    pushd "$1" >/dev/null

    #init config
    docker exec -i "$SERVERID" \
	   learn-ocaml-client init --server=http://localhost:8080 --token="$token"
    # For each solution
    for tosend in $(find . -name "*.ml" -type f -printf "%f\n")
    do
	handle_file "$subdir" "$tosend"
    done

    clean
}

# For each subdirectory (ie. each corpus)
for dir in $(ls -d ./*/)
do
    run_server

    pushd "$dir" > /dev/null

    echo "---> Entering $dir:"

    # Get the token from the sync/ directory
    token=$(find sync -maxdepth 5 -mindepth 5 | head -n 1 | sed 's|sync/||' | sed 's|/|-|g')

    # For each subdir (ie. each exercice)
    while IFS= read -r subdir;
    do
	handle_subdir "$subdir"
    done < <(find . -maxdepth 1 -type d ! -path . ! -path ./repo ! -path ./sync)

    popd > /dev/null
done

green "\nALL $count TESTS PASSED\n"
