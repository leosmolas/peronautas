#! /bin/bash

if [ -z $1 ]; then
	host=localhost
else
	host=$1
fi

if [ -z $2 ];then
	port=1099
else
	port=$2
fi

java -Xss20000k -cp ../target/agentcontest-2011-1.0.1.jar massim.competition2011.monitor.GraphMonitor -rmihost $host -rmiport $port -savexmls 
