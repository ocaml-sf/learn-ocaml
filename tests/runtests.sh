#!/bin/bash

# Print in green $1
green () {
    echo -e "\e[32mOK: $1\e[0m"
}

# print in red $2
red () {
    echo -e "\e[31mNOT OK: $1\e[0m"
}

# run a server in $TMP/test-repo
run_server (){
    # Build the reporistory
    pushd $TMP > /dev/null
    learn-ocaml build --repo test-repo
    if [ $? -ne 0 ]; then
	echo Build failed
	exit 1
    fi

    # Run the server in background
    learn-ocaml serve > /dev/null &
    popd > /dev/null

    # Wait for the server to be initialized
    sleep 2
}

# Temporary directory
TMP=$(mktemp -d)

# For each subdirectory (ie. each corpus)
for DIR in `ls -d */`
do
    pushd $DIR > /dev/null

    echo " :*: Doing $DIR"

    cp -r -L repo $TMP/test-repo

    run_server

    # Get the token
    TOKEN=$(find $TMP/sync -name \*.json -printf '%P' | sed 's|/|-|g' | sed 's|-save.json||')

    # For each subdir (ie. each exercice)
    for SUBDIR in `find .  -maxdepth 1 -type d ! -path . ! -path ./repo`
    do
	pushd $SUBDIR > /dev/null

	for TOSEND in `find . -name "*.ml" -type f -printf "%f\n"`
	do
	    # Grade file
	    learn-ocaml-client --server http://localhost:8080 --token "$TOKEN" --id="$SUBDIR" $TOSEND > res.txt
	    if [ $? -ne 0 ]
	    then
		red "$DIR$TOSEND"
	    fi
	    # If there is something to compare
	    if [ -f "$TOSEND.txt" ]
	    then
		diff res.txt "$TOSEND.txt"
		if [ $? -ne 0 ]
		then
		    red "$DIR$TOSEND"
		    break
		fi
	    fi
            green "$DIR$TOSEND"
	    rm res.txt
	done

	popd > /dev/null
	# Cleanup
	rm -rf $TMP/*

	kill $!
    done
    popd > /dev/null
done
