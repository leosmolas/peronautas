SERVER_DIR=/home/igaray/projects-code/lidia-massim/massim-2011-1.0/massim/scripts/
SERVER_CMD='startServer.sh'
MONITOR_DIR=/home/igaray/projects-code/lidia-massim/massim-2011-1.0/massim/scripts/
MONITOR_CMD='startMarsMonitor.sh'
PYTHON=python2
TERM_CMD=urxvtc

WD=$(pwd)
echo Working dir is:  $WD
echo Server path is:  $SERVER_DIR
echo Monitor path is: $MONITOR_DIR
echo Term command is: $TERM_CMD
echo Python command:  $PYTHON

# Run servero
echo Starting server.
cd $SERVER_DIR
$TERM_CMD -e $SERVER_CMD
cd $WD
sleep 10

# Run monitor
echo Starting monitor.
cd $MONITOR_DIR
$TERM_CMD -e $MONITOR_CMD
cd $WD
sleep 10

# Run agents
#$TERMCMD -e $PYTHON Agent.py a1 1
#$TERMCMD -e $PYTHON Agent.py b1 1
