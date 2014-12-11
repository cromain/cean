#!/bin/sh

# cleanup
rm doc/release_notes*

# generate configure
./autogen.sh
mv configure configure.old
sed "s/0\.0/$2/" configure.old > configure
chmod +x configure
rm configure.old

# generate guide
mkdir ebin
echo "vsn,\"$2\"" > ebin/ejabberd.app
(cd doc; rm guide.html; make)
(cd doc; rm guide.aux guide.haux guide.htoc guide.idx guide.ilg guide.ind guide.log guide.out guide.toc)
rm -Rf ebin

# copy README
cp README doc/README.txt
