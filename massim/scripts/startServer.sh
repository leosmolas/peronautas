#! /bin/bash

# initialize certain values
date=`date +%Y-%m-%d_%H:%M`
server=../target/agentcontest-2011-1.0.1.jar
webappPath=/home/massim/www/webapps/massim
webapp=$webappPath/WEB-INF/classes
hostname=`hostname -f`
conf=conf/

# move old result page
#mv /home/massim/www/webapps/massim/index.html /home/massim/www/webapps/massim/moved-at-$date.html

# create folder for log files
mkdir -p backup

echo "Please choose a number and then press enter:"
count=0
for i in $( ls $conf )
do
  if [ -f $conf/$i ]
  then
    echo $count: $i
    count=`expr $count + 1`
  fi
done

read number

count=0
for i in $( ls $conf )
do
  if [ -f $conf/$i ]
  then
    if [ $number -eq $count ]
    then
      conf=conf/$i
    fi
    count=`expr $count + 1`
  fi
done

echo "Starting server: $conf"

java -ea -Dcom.sun.management.jmxremote -Xss10000k -Xmx600M  -DentityExpansionLimit=1000000 -DelementAttributeLimit=1000000 -Djava.rmi.server.hostname=$hostname -jar $server --conf $conf | tee backup/$date-$hostname.log

# create results page
#cd ../target/classes
#java -cp $webapp massim.webclient.CreateResultsPage $webappPath/xslt/createResultsHTML.xslt backup/GridSimulation_report.xml
#mv Tournament.html $webappPath/index.html

# make backup of the report
cd backup
for i in $( ls *_report.xml)
do
	mv $i $date-$hostname-result.xml
done
cd ..
