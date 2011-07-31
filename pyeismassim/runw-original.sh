SERVER_DIR="c:/Users/Leo/Documents/My Dropbox/MASSIM/massim-2011-1.0.1/massim/scripts/"
SERVER_CMD='startServer.sh'
MONITOR_DIR="c:/Users/Leo/Documents/My Dropbox/MASSIM/massim-2011-1.0.1/massim/scripts/"
MONITOR_CMD='startMarsMonitor.sh'
PERCEPT_DIR="e:/Mis Documentos/Programación/lidia-massim/pyeismassim/perceptServer/"
PERCEPT_CMD='PerceptServer.py 1 10000'
AGENT_DIR=/home/igaray/projects-code/lidia-massim/lidia-massim/pyeismassim/
PYTHON=python
TERM_CMD=cmd

WD=$(pwd)
echo Working dir is:  $WD
echo Server path is:  $SERVER_DIR
echo Monitor path is: $MONITOR_DIR
echo Term command is: $TERM_CMD
echo Python command:  $PYTHON

# Run servero
echo Starting server.
cd "$SERVER_DIR"
start sh -c "$SERVER_CMD"
cd "$WD"
sleep 5

# Run monitor
echo Starting monitor.
cd "$MONITOR_DIR"
start sh -c "$MONITOR_CMD"
cd "$WD"
sleep 10

# Run percept server
echo Starting percept server.
cd "$PERCEPT_DIR"
start $TERM_CMD /K $PYTHON $PERCEPT_CMD
cd "$WD"
sleep 10

echo Starting agents.
cd $AGENT_DIR
#$TERM_CMD -title 'AGENT' -e $PYTHON Agent.py a1 1 -sh localhost -sp 10000
start $TERM_CMD /K $PYTHON Agent.py a1 1 -sh localhost -sp 10000
start $TERM_CMD /K $PYTHON Agent.py b1 1 -sh localhost -sp 10000
