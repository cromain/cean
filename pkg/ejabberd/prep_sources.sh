#!/bin/sh

# generate configure
vsn=${2//-/.}
sed -i -e "/AC_INIT/s/AC_INIT(.*/AC_INIT(ejabberd, $vsn, ejabberd@process-one.net, ejabberd)/" configure.ac
./autogen.sh

# generate static deps versions
./rebar get-deps
tools/set-dep-versions
rm -Rf deps
