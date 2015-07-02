#!/bin/sh

# generate configure
vsn=${2//-/.}
aclocal -I m4
autoconf -f -o - | sed "s/0\.0/$vsn/" > configure
chmod +x configure

# generate static deps versions
./rebar get-deps
tools/set-dep-versions
rm -Rf deps
