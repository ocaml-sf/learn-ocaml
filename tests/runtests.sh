#!/bin/bash

# Print in green $1
green () {
    echo -e "\e[32mOK: $1\e[0m"
}

# print in red $1
red () {
    echo -e "\e[31mNOT OK: $1\e[0m"
}

# run a server in $TMP/test-repo
run_server (){
    # Build the reporistory
    docker run --rm  -v sync:/sync -v test-repo:/repository learn-ocaml build
    
    if [ $? -ne 0 ]; then
	echo Build failed
	exit 1
    fi

    # Run the server in background
    SERVERID=$(docker run --rm -d -v $(pwd)/$DIR:/home/opam/actual -v sync:/sync -v test-repo:/repository learn-ocaml serve)

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

    echo " :*: Doing $DIR"

    #cp -r -L repo $TMP/test-repo

    # Get the token
    #TOKEN=$(find $TMP/sync -name \*.json -printf '%P' | sed 's|/|-|g' | sed 's|-save.json||')

    # For each subdir (ie. each exercice)
    for SUBDIR in `find .  -maxdepth 1 -type d ! -path . ! -path ./repo ! -path ./sync -printf "%f\n"`
    do
	pushd $SUBDIR > /dev/null

	for TOSEND in `find . -name "*.ml" -type f -printf "%f\n"`
	do
	    # Grade file
	    docker exec $SERVERID \
	    learn-ocaml-client --server http://localhost:8080 --id="$SUBDIR" /home/opam/actual/$SUBDIR/$TOSEND > res.txt
	    if [ $? -ne 0 ]
	    then
		red "$DIR$TOSEND"
		clean
		exit 1
	    fi
	    # If there is something to compare
	    if [ -f "$TOSEND.txt" ]
	    then
		diff res.txt "$TOSEND.txt"
		if [ $? -ne 0 ]
		then
		    red "$DIR$TOSEND"
		    clean
		    exit 1
		fi
	    fi
            green "$DIR$TOSEND"
	    rm res.txt
	done

	clean
    done
    popd > /dev/null
done
