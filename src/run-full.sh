
echo Starting...

SER_DIR='/home/igaray/projects-code/lidia-massim/massim-2011-1.0.1/massim/scripts/'
MON_DIR='/home/igaray/projects-code/lidia-massim/massim-2011-1.0.1/massim/scripts/'
PER_DIR='/home/igaray/projects-code/lidia-massim/lidia-massim/pyeismassim/perceptServer/'
AGT_DIR='/home/igaray/projects-code/lidia-massim/lidia-massim/pyeismassim/'
SER_CMD='startServer.sh'
MON_CMD='startMarsMonitor.sh'
PER_CMD='PerceptServer.py 10 10000'
TRM_CMD=urxvtc
PYTHON=python2
WD=$(pwd)

echo Working dir is:  $WD
echo Server path is:  $SER_DIR
echo Monitor path is: $MON_DIR
echo Term command is: $TRM_CMD
echo Python command:  $PYTHON

# Run server
#echo Starting MASSIM server.
#cd $SER_DIR
#$TRM_CMD -title 'MASSIM' -e $SER_CMD
#cd $WD
#sleep 3

# Run monitor
#echo Starting monitor.
#cd $MON_DIR
#$TRM_CMD -title 'MONITOR' -e $MON_CMD
#sleep 3

# Run percept server
#echo Starting percept server.
#cd $PER_DIR
#$TRM_CMD -title 'PERCEPT' -e $PYTHON $PER_CMD
#sleep 3

# Run agents

echo Starting agents.
$TRM_CMD -title 'AGENT_A1' -e sh runagent.sh a1  1
$TRM_CMD -title 'AGENT_A2' -e sh runagent.sh a2  1
$TRM_CMD -title 'AGENT_A3' -e sh runagent.sh a3  1
$TRM_CMD -title 'AGENT_A4' -e sh runagent.sh a4  1
$TRM_CMD -title 'AGENT_A5' -e sh runagent.sh a5  1
$TRM_CMD -title 'AGENT_A6' -e sh runagent.sh a6  1
$TRM_CMD -title 'AGENT_A7' -e sh runagent.sh a7  1
$TRM_CMD -title 'AGENT_A8' -e sh runagent.sh a8  1
$TRM_CMD -title 'AGENT_A9' -e sh runagent.sh a9  1
$TRM_CMD -title 'AGENT_A0' -e sh runagent.sh a10 1
$TRM_CMD -title 'AGENT_B1' -e $PYTHON Agent.py b1  1 -d
$TRM_CMD -title 'AGENT_B2' -e $PYTHON Agent.py b2  1 -d
$TRM_CMD -title 'AGENT_B3' -e $PYTHON Agent.py b3  1 -d
$TRM_CMD -title 'AGENT_B4' -e $PYTHON Agent.py b4  1 -d
$TRM_CMD -title 'AGENT_B5' -e $PYTHON Agent.py b5  1 -d
$TRM_CMD -title 'AGENT_B6' -e $PYTHON Agent.py b6  1 -d
$TRM_CMD -title 'AGENT_B7' -e $PYTHON Agent.py b7  1 -d
$TRM_CMD -title 'AGENT_B8' -e $PYTHON Agent.py b8  1 -d
$TRM_CMD -title 'AGENT_B9' -e $PYTHON Agent.py b9  1 -d
$TRM_CMD -title 'AGENT_B0' -e $PYTHON Agent.py b10 1 -d
