echo Starting...
TRM_CMD=urxvtc
PYTHON=python2
echo Term command is: $TRM_CMD
echo Python command:  $PYTHON
echo Starting agents.
$TRM_CMD -title 'B1' -e sh rundummy.sh b1  1
$TRM_CMD -title 'B2' -e sh rundummy.sh b2  1
$TRM_CMD -title 'B3' -e sh rundummy.sh b3  1
$TRM_CMD -title 'B4' -e sh rundummy.sh b4  1
$TRM_CMD -title 'B5' -e sh rundummy.sh b5  1
$TRM_CMD -title 'B6' -e sh rundummy.sh b6  1
$TRM_CMD -title 'B7' -e sh rundummy.sh b7  1
$TRM_CMD -title 'B8' -e sh rundummy.sh b8  1
$TRM_CMD -title 'B9' -e sh rundummy.sh b9  1
$TRM_CMD -title 'B0' -e sh rundummy.sh b10 1
