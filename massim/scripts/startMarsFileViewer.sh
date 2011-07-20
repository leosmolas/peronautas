#! /bin/bash

if [ -z $1 ]; then
	echo "Please specify a directory."
	echo "Usage: sh startMarsFileViewer.sh directory"
        exit 1
else
	directory=$1
fi

java -Xss20000k -cp ../target/agentcontest-2011-1.0.1.jar massim.competition2011.monitor.GraphFileViewer -dir $directory 
