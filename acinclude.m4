dnl Autoconf extension macros for libIDL
AC_DEFUN(AC_CPP_ACCEPT_IDL,
	[AC_CACHE_CHECK([if C preprocessor likes IDL],
		ac_cv_cpp_accept_idl,
		[AC_TRY_CPP([
			#pragma prefix "foo.org"
			module FOO {
				typedef unsigned long long big;
				interface BAR {
					readonly attribute big number;
				};
			};
		],
		ac_cv_cpp_accept_idl=yes,
		ac_cv_cpp_accept_idl=no)])])
	
AC_DEFUN(AC_CPP_PIPE_STDIN,
	[AC_CACHE_CHECK([if C preprocessor can read from stdin],
		ac_cv_cpp_pipe_stdin,
		[AC_REQUIRE_CPP
		if echo | $CPP - 2>/dev/null 1>&2 ; then
			ac_cv_cpp_pipe_stdin=yes
		else
			ac_cv_cpp_pipe_stdin=no
		fi])
	if test $ac_cv_cpp_pipe_stdin = yes ; then
		AC_DEFINE(HAVE_CPP_PIPE_STDIN)
	fi])