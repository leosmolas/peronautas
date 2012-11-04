
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

echo Starting agents.
$TRM_CMD -title 'A1' -e sh runagent.sh a1  1
$TRM_CMD -title 'A2' -e sh runagent.sh a2  1
$TRM_CMD -title 'A3' -e sh runagent.sh a3  1
$TRM_CMD -title 'A4' -e sh runagent.sh a4  1
$TRM_CMD -title 'A5' -e sh runagent.sh a5  1
$TRM_CMD -title 'A6' -e sh runagent.sh a6  1
$TRM_CMD -title 'A7' -e sh runagent.sh a7  1
$TRM_CMD -title 'A8' -e sh runagent.sh a8  1
$TRM_CMD -title 'A9' -e sh runagent.sh a9  1
$TRM_CMD -title 'A0' -e sh runagent.sh a10 1
