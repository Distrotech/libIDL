#! /bin/sh
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.
ORIGDIR=`pwd`

DIE=0

(autoconf --version) < /dev/null > /dev/null 2>&1 || {
	echo
	echo "You must have autoconf installed to compile libIDL."
	echo "Download the appropriate package for your distribution,"
	echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/"
	DIE=1
}

(libtool --version) < /dev/null > /dev/null 2>&1 || {
	echo
	echo "You must have libtool installed to compile libIDL."
	echo "Get ftp://alpha.gnu.org/gnu/libtool-1.0h.tar.gz"
	echo "(or a newer version if it is available)"
	DIE=1
}

if automake-1.9 --version < /dev/null > /dev/null 2>&1; then
        AUTOMAKE=automake-1.9
	ACLOCAL=aclocal-1.9
elif automake-1.8 --version < /dev/null > /dev/null 2>&1; then
        AUTOMAKE=automake-1.8
	ACLOCAL=aclocal-1.8
elif automake-1.7 --version < /dev/null > /dev/null 2>&1; then
        AUTOMAKE=automake-1.7
	ACLOCAL=aclocal-1.7
else
	echo
	echo "You must have automake installed to compile libIDL."
	echo "Get http://ftp.gnu.org/gnu/automake/automake-1.9.2.tar.gz"
	echo "(or a newer version if it is available)"
	DIE=1
fi

if test "$DIE" -eq 1; then
	exit 1
fi

(test -f $srcdir/include/libIDL/IDL.h.in) || {
	echo "You must run this script in the top-level libIDL directory"
	exit 1
}

if test -z "$*"; then
	echo "I am going to run ./configure with no arguments - if you wish "
        echo "to pass any to it, please specify them on the $0 command line."
fi

for i in $srcdir
do 
  echo processing $i
  cd $i
  libtoolize --copy --force || exit $?
  $ACLOCAL $ACLOCAL_FLAGS || exit $?
  autoconf || exit $?
  $AUTOMAKE --add-missing --copy || exit $?
done
cd $ORIGDIR

echo "Running ./configure --enable-maintainer-mode" "$@"
$srcdir/configure --enable-maintainer-mode "$@" || exit $?

echo "Now type 'make' to compile libIDL."
