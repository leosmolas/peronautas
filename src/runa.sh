echo Starting...
TRM_CMD=urxvtc
PYTHON=python2
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
