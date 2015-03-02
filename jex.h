
typedef struct Jex_Context Jex_Context;


JAST_Statement *jex_context_translate (Jex_Context *context,
                                       JAST_Statement *stmt);




/* --- Constant Propagation Section --- */
JS_Boolean
jex_constprop_is_foldable_expr_type (JAST_Expr_Type type);

JAST_Expr *
jex_constprop_fold_unary_op (JAST_UnaryOp_Type op, JAST_Expr *const_sub);

JAST_Expr *
jex_constprop_fold_binary_op (JAST_BinaryOp_Type op, JAST_Expr *const_sub1, JAST_Expr *const_sub2);

/* Examples: isNaN, parseInt */
JS_Boolean jex_constprop_is_global_foldable_func (JS_String func_name);

/* Example: Math.sin. */
JS_Boolean jex_constprop_is_ns_foldable_func (JS_String namespace_name,
                                              JS_String func_name);


JAST_Expr *
