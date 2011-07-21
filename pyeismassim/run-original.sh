SERVER_DIR=/home/igaray/projects-code/lidia-massim/massim-2011-1.0.1/massim/scripts/
SERVER_CMD='startServer.sh'
MONITOR_DIR=/home/igaray/projects-code/lidia-massim/massim-2011-1.0.1/massim/scripts/
MONITOR_CMD='startMarsMonitor.sh'
PERCEPT_DIR=/home/igaray/projects-code/lidia-massim/lidia-massim/pyeismassim/perceptServer/
PERCEPT_CMD='PerceptServer.py 1 10000'
AGENT_DIR=/home/igaray/projects-code/lidia-massim/lidia-massim/pyeismassim/
PYTHON=python2
TERM_CMD=urxvtc
WD=$(pwd)

echo Working dir is:  $WD
echo Server path is:  $SERVER_DIR
echo Monitor path is: $MONITOR_DIR
echo Term command is: $TERM_CMD
echo Python command:  $PYTHON

# Run servero
echo Starting MASSIM server.
cd $SERVER_DIR
$TERM_CMD -title 'MASSIM' -e $SERVER_CMD
cd $WD
sleep 3

# Run monitor
#echo Starting monitor.
#cd $MONITOR_DIR
#$TERM_CMD -name 'MONITOR' -e $MONITOR_CMD
#cd $WD
#sleep 10

# Run percept server
#echo Starting percept server.
#cd $PERCEPT_DIR
#$TERM_CMD -title 'PERCEPT' -e $PYTHON $PERCEPT_CMD
#cd $WD
#sleep 10

# Run agents
echo Starting agents.
cd $AGENT_DIR
#$TERM_CMD -title 'AGENT' -e $PYTHON Agent.py a1 1 -sh localhost -sp 10000
$TERM_CMD -title 'AGENT' -e $PYTHON Agent.py a1  1 
$TERM_CMD -title 'AGENT' -e $PYTHON Agent.py a2  1
$TERM_CMD -title 'AGENT' -e $PYTHON Agent.py a3  1
$TERM_CMD -title 'AGENT' -e $PYTHON Agent.py a4  1
$TERM_CMD -title 'AGENT' -e $PYTHON Agent.py a5  1
$TERM_CMD -title 'AGENT' -e $PYTHON Agent.py a6  1
$TERM_CMD -title 'AGENT' -e $PYTHON Agent.py a7  1
$TERM_CMD -title 'AGENT' -e $PYTHON Agent.py a8  1
$TERM_CMD -title 'AGENT' -e $PYTHON Agent.py a9  1
$TERM_CMD -title 'AGENT' -e $PYTHON Agent.py a10 1
$TERM_CMD -title 'AGENT' -e $PYTHON Agent.py b1  1
$TERM_CMD -title 'AGENT' -e $PYTHON Agent.py b2  1
$TERM_CMD -title 'AGENT' -e $PYTHON Agent.py b3  1
$TERM_CMD -title 'AGENT' -e $PYTHON Agent.py b4  1
$TERM_CMD -title 'AGENT' -e $PYTHON Agent.py b5  1
$TERM_CMD -title 'AGENT' -e $PYTHON Agent.py b6  1
$TERM_CMD -title 'AGENT' -e $PYTHON Agent.py b7  1
$TERM_CMD -title 'AGENT' -e $PYTHON Agent.py b8  1
$TERM_CMD -title 'AGENT' -e $PYTHON Agent.py b9  1
$TERM_CMD -title 'AGENT' -e $PYTHON Agent.py b10 1

