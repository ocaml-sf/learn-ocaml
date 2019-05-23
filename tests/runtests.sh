#!/bin/bash

let count=0

# print in green $1
green () {
    echo -e "\e[32m$1\e[0m"
}

# print in red $1
red () {
    echo -e "\e[31m$1\e[0m"
}

# run a server in a docker container
run_server (){
    SYNC=$(pwd)/$DIR/sync
    REPO=$(pwd)/$DIR/repo

    # Run the server in background
    docker run --entrypoint '' --rm -v $(pwd)/$DIR:/home/learn-ocaml/actual -v $SYNC:/sync -v $REPO:/repository learn-ocaml /bin/sh -c "learn-ocaml --sync-dir=/sync --repo=/repository build && learn-ocaml --sync-dir=/sync --repo=/repository build serve"

    if [ $? -ne 0 ]; then
        red "BUILD FAILED"
	exit 1
    fi

    # Wait for the server to be initialized
    sleep 2
}

clean (){
    popd > /dev/null

    docker kill $SERVERID > /dev/null
}

# For each subdirectory (ie. each corpus)
for DIR in `ls -d */`
do
    run_server

    pushd $DIR > /dev/null

    echo " :*: Doing $DIR :"

    # Get the token
    TOKEN=$(find sync -name \*.json -printf '%P' | sed 's|/|-|g' | sed 's|-save.json||')

    # For each subdir (ie. each exercice)
    for SUBDIR in `find .  -maxdepth 1 -type d ! -path . ! -path ./repo ! -path ./sync -printf "%f\n"`
    do
	pushd $SUBDIR > /dev/null

	for TOSEND in `find . -name "*.ml" -type f -printf "%f\n"`
	do
	    # Grade file
	    docker exec -i $SERVERID \
	      learn-ocaml-client --server http://localhost:8080 --json --token="$TOKEN" --id="$SUBDIR" /home/learn-ocaml/actual/$SUBDIR/$TOSEND > res.txt 2> stderr.txt
	    if [ $? -ne 0 ]
	    then
		red "NOT OK: $DIR$TOSEND"
		cat stderr.txt
		clean
		exit 1
	    fi
	    # If there is something to compare
	    if [ -f "$TOSEND.txt" ]
	    then
		diff res.txt "$TOSEND.txt"
		if [ $? -ne 0 ]
		then
		    red "DIFF FAILED: $DIR$TOSEND"
		    clean
		    exit 1
		fi
	    fi
            green "OK: $DIR$TOSEND"
	    rm res.txt stderr.txt
	    let count++
	done

	clean
    done
    popd > /dev/null
done

green "\nALL $count TESTS PASSED\n"
