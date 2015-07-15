#!/bin/sh

# generate configure
vsn=${2//-/.}
sed -i -e "s/echo 0\.0/echo $vsn/" configure.ac
./autogen.sh

# generate static deps versions
./rebar get-deps
tools/set-dep-versions
rm -Rf deps
