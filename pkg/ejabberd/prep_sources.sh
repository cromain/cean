#!/bin/sh

# cleanup
rm doc/release_notes*

# generate configure
vsn=${2//-/.}
aclocal -I m4
autoconf -f -o - | sed "s/0\.0/$vsn/" > configure
chmod +x configure

# generate guide
mkdir ebin
echo "vsn,\"$2\"" > ebin/ejabberd.app
(cd doc; rm guide.html; make)
(cd doc; rm guide.aux guide.haux guide.htoc guide.idx guide.ilg guide.ind guide.log guide.out guide.toc)
rm -Rf ebin

# generate static deps versions
./rebar get-deps
tools/set-dep-versions
rm -Rf deps

# copy README
cp README doc/README.txt
