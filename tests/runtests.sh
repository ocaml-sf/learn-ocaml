#!/bin/bash

green () {
    echo -e "\e[32mOK: $1\e[0m"
}

red () {
    echo -e "\e[31mNOT OK: $1\e[0m"
}

# Temporary directory
TMP=$(mktemp -d)

cp -r ../demo-repository $TMP/test-repo

# Build the reporistory
pushd $TMP
learn-ocaml build --repo test-repo
if [ $? -ne 0 ]; then
    echo Build failed
    exit 1
fi

# Run the server in background
learn-ocaml serve > /dev/null &
popd

# Wait for the server to be initialized
sleep 2

# Get the token
TOKEN=$(find $TMP/sync -name \*.json -printf '%P' | sed 's|/|-|g' | sed 's|-save.json||')

# For each subdirectory
for DIR in `find . -type d ! -path .`
do
    pushd $DIR > /dev/null
    for TOSEND in `find . -name "*.ml" -type f -printf "%f\n"`
    do
	# Grade file
	learn-ocaml-client --server http://localhost:8080 --token "$TOKEN" $TOSEND > res.json
	if [ $? -ne 0 ]
	then
	   red "$DIR/$TOSEND"
	fi
	# If there is something to compare
	if [ -f "$TOSEND.json" ]
	then
	    diff res.json "$TOSEND.json"
	    if [ $? -ne 0 ]
	    then
	       red "$DIR/$TOSEND"
	       break 2
	    fi
	fi
        green "$DIR/$TOSEND"
	rm res.json
    done
    popd > /dev/null
done

# Cleanup
rm -rf $TMP

kill $!
