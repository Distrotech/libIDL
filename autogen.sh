#!/bin/sh
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

PKG_NAME="libIDL"

which gnome-autogen.sh || {
    echo "You need to install gnome-common from GNOME SVN and make"
    echo "sure the gnome-autogen.sh script is in your \$PATH."
    exit 1
}

REQUIRED_AUTOMAKE_VERSION=1.9 . gnome-autogen.sh
