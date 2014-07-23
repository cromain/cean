#! /bin/sh

[ $# -eq 0 ] && {
cat << EOF
Your new CEAN installation is ready.
Erlang shell is started with $PWD/bin/erl
CEAN node is started with $PWD/bin/ceanctl start
This CEAN directory can be moved anywhere for latter use.
Now starting Erlang, call cean:help() for instructions.

EOF
bin/erl
} || {
case $1 in
  http*) curl -s $1 | /bin/sh ;; # external postinstall
  workdir) bin/initworkdir ;;
  *) bin/erl ;;
esac
}
