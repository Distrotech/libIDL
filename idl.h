#ifndef __IDL_H
#define __IDL_H

#ifdef __cplusplus
extern "C" {
#endif

struct _IDL_tree_node {
	struct _IDL_tree_node *_s, *_c;
	struct _IDL_tree_node *_s_tail, *_c_tail;
};

typedef struct _IDL_tree_node		IDL_tree_node;
typedef struct _IDL_tree_node *		IDL_tree;

#define IDL_SUCCESS			0
#define IDL_ERROR			1

extern int				IDL_parse_filename(const char *filename,
							   const char *cpp_args,
							   IDL_tree *tree);
extern const char *			IDL_get_last_error(void);
extern void				IDL_free(IDL_tree *tree);

#ifdef __cplusplus
}
#endif

#endif /* __IDL_H */
