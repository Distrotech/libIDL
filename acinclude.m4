dnl Autoconf extension macros for libIDL
AC_DEFUN(AC_PROG_PLAIN_CPP,
	[AC_CACHE_VAL(ac_prog_plain_cpp,
		[AC_ARG_WITH(plain-cpp,
			[  --with-plain-cpp=PROGRAM
                          full path to standalone C preprocessor],
			[PLAIN_CPP="$withval"
			AC_MSG_CHECKING(user specified C preprocessor)
			AC_MSG_RESULT("$PLAIN_CPP")
			ac_prog_plain_cpp="$PLAIN_CPP"],
			[AC_PATH_PROGS(PLAIN_CPP, cpp, , /lib:$PATH)
			if [[ -z "$PLAIN_CPP" ]]; then
				PLAIN_CPP="`$CC -print-prog-name=cpp 2>/dev/null`"
			fi	
			if [[ -z "$PLAIN_CPP" ]]; then
				AC_MSG_WARN([could not find a C preprocessor, specify --with-plain-cpp=PROGRAM])
			fi])
			ac_prog_plain_cpp="$PLAIN_CPP"])
	CPP_PROGRAM="$PLAIN_CPP"
	AC_DEFINE_UNQUOTED(CPP_PROGRAM, "$PLAIN_CPP")])

AC_DEFUN(AC_PROG_PLAIN_CPP_EXEC,
	[AC_MSG_CHECKING([the C preprocessor])
	AC_CACHE_VAL(ac_prog_plain_cpp_exec,
		[if [[ ! -x "$PLAIN_CPP" ]]; then
			AC_MSG_RESULT(WARNING: cannot execute "$PLAIN_CPP")
			ac_prog_plain_cpp_exec=no
		else
			AC_MSG_RESULT(executable)
			ac_prog_plain_cpp_exec=yes
		fi])])
