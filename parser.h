typedef union {
	IDL_tree tree;
	char *str;
	long integer;
	double floatp;
	double fixedp;
} YYSTYPE;
#define	TOK_ANY	258
#define	TOK_ATTRIBUTE	259
#define	TOK_BOOLEAN	260
#define	TOK_CASE	261
#define	TOK_CHAR	262
#define	TOK_CONST	263
#define	TOK_CONTEXT	264
#define	TOK_DEFAULT	265
#define	TOK_DOUBLE	266
#define	TOK_ENUM	267
#define	TOK_EXCEPTION	268
#define	TOK_FALSE	269
#define	TOK_FIXED	270
#define	TOK_FLOAT	271
#define	TOK_IN	272
#define	TOK_INOUT	273
#define	TOK_INTERFACE	274
#define	TOK_LONG	275
#define	TOK_MODULE	276
#define	TOK_OBJECT	277
#define	TOK_OCTET	278
#define	TOK_ONEWAY	279
#define	TOK_OUT	280
#define	TOK_RAISES	281
#define	TOK_READONLY	282
#define	TOK_SEQUENCE	283
#define	TOK_SHORT	284
#define	TOK_STRING	285
#define	TOK_STRUCT	286
#define	TOK_SWITCH	287
#define	TOK_TRUE	288
#define	TOK_TYPEDEF	289
#define	TOK_UNSIGNED	290
#define	TOK_UNION	291
#define	TOK_VOID	292
#define	TOK_WCHAR	293
#define	TOK_WSTRING	294
#define	TOK_IDENT	295
#define	TOK_SQSTRING	296
#define	TOK_DQSTRING	297
#define	TOK_INTEGER	298
#define	TOK_FLOATP	299
#define	TOK_FIXEDP	300


extern YYSTYPE yylval;
