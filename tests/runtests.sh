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

    mkdir $DIR/sync 2>/dev/null
    chmod o+w $DIR/sync

    # Run the server in background
    SERVERID=$(docker run --entrypoint '' -d -v $(pwd)/$DIR:/home/learn-ocaml/actual -v $SYNC:/sync -v $REPO:/repository learn-ocaml /bin/sh -c "learn-ocaml --sync-dir=/sync --repo=/repository build && learn-ocaml --sync-dir=/sync --repo=/repository build serve")

    # Wait for the server to be initialized
    sleep 2

    if [ "$(docker ps -q)" == "" ]; then
	red "PROBLEM, server is not running.\n"

	red "LS:"
	ls -Rl $DIR
	echo ""

	red "LOGS:"
	docker logs $SERVERID
	docker rm $SERVERID > /dev/null
	exit 1
    fi
}

clean (){
    popd > /dev/null
    chmod o-w sync

    docker kill $SERVERID > /dev/null
    docker rm   $SERVERID > /dev/null
}

# For each subdirectory (ie. each corpus)
for DIR in `ls -d */`
do
    run_server

    pushd $DIR > /dev/null

    echo "---> Doing $DIR:"

    # Get the token from the sync/ directory
    TOKEN=$(find sync -maxdepth 5 -mindepth 5 | head -n 1 | sed 's|sync/||' | sed 's|/|-|g')

    # For each subdir (ie. each exercice)
    for SUBDIR in `find .  -maxdepth 1 -type d ! -path . ! -path ./repo ! -path ./sync -printf "%f\n"`
    do
	pushd $SUBDIR > /dev/null

	for TOSEND in `find . -name "*.ml" -type f -printf "%f\n"`
	do
	    # Grade file
	    docker exec -i $SERVERID \
	      learn-ocaml-client --server http://localhost:8080 --json --token="$TOKEN" --id="$SUBDIR" /home/learn-ocaml/actual/$SUBDIR/$TOSEND > res.json 2> stderr.txt
	    if [ $? -ne 0 ]
	    then
		red "NOT OK: $DIR$TOSEND"
		cat stderr.txt
		clean
		exit 1
	    fi
	    # If there is something to compare
	    if [ -f "$TOSEND.json" ]
	    then
		diff res.json "$TOSEND.json"
		if [ $? -ne 0 ]
		then
		    red "DIFF FAILED: $DIR$TOSEND"
		    clean
		    exit 1
		fi
	    fi
            green "OK: $DIR$TOSEND"
	    rm res.json stderr.txt
	    let count++
	done

	clean
    done
    popd > /dev/null
done

green "\nALL $count TESTS PASSED\n"
