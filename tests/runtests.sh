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
    SYNC=$(pwd)/$DIR/sync
    REPO=$(pwd)/$DIR/repo

    TMP=$(mktemp -d)
    OUTDATA=/home/learn-ocaml/out

    # Build the reporistory
    docker run --user learn-ocaml -v $TMP:$OUTDATA -v $SYNC:/sync -v $REPO:/repository learn-ocaml build -o $OUTDATA

    if [ $? -ne 0 ]; then
	echo Build failed
	exit 1
    fi

    # Run the server in background
    SERVERID=$(docker run --user learn-ocaml --rm -d -v $TMP:$OUTDATA -v $(pwd)/$DIR:/home/learn-ocaml/actual -v $SYNC:/sync -v $REPO:/repository learn-ocaml serve --app-dir=$OUTDATA)

    # Wait for the server to be initialized
    sleep 2
}

clean (){
    popd > /dev/null

    rm -rf $TMP

    docker kill $SERVERID > /dev/null
}

# For each subdirectory (ie. each corpus)
for DIR in `ls -d */`
do
    run_server

    pushd $DIR > /dev/null

    echo " :*: Doing $DIR"

    # Get the token
    TOKEN=$(find sync -name \*.json -printf '%P' | sed 's|/|-|g' | sed 's|-save.json||')

    echo "TOKEN: $TOKEN"

    # For each subdir (ie. each exercice)
    for SUBDIR in `find .  -maxdepth 1 -type d ! -path . ! -path ./repo ! -path ./sync -printf "%f\n"`
    do
	pushd $SUBDIR > /dev/null

	for TOSEND in `find . -name "*.ml" -type f -printf "%f\n"`
	do
	    # Grade file
	    docker exec -i $SERVERID \
	      learn-ocaml-client --server http://localhost:8080 --token="$TOKEN" --id="$SUBDIR" /home/learn-ocaml/actual/$SUBDIR/$TOSEND > res.txt
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
