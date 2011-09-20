#! /bin/sh

cat << EOF
Your new CEAN installation is ready.
Erlang shell is started with $PWD/bin/erl
This CEAN directory can be moved anywhere for latter use.
Now starting Erlang, call cean:help() for instructions.

EOF
bin/erl
