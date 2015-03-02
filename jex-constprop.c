
JS_Boolean
jex_constprop_is_foldable_expr_type (JAST_Expr_Type type)
{
  switch (type)
    {
      case JAST_EXPR_STRING_VALUE:
      case JAST_EXPR_NUMBER_VALUE:
      case JAST_EXPR_BOOLEAN_VALUE:
      case JAST_EXPR_UNDEFINED_VALUE:
      case JAST_EXPR_NULL_VALUE:
        return JS_TRUE;
      default:
        return JS_FALSE;
    }
}

JAST_Expr *
jex_constprop_fold_unary_op (JAST_UnaryOp_Type op, JAST_Expr *const_sub)
{
  switch (op)
    {
      case JAST_UNARY_OP_PLUS:
        switch (const_sub->type)
          {
            case JAST_EXPR_NUMBER_VALUE:
              return jast_expr_copy (const_sub);
            case JAST_EXPR_
      case JAST_UNARY_OP_LOGICAL_NOT:

  if (const_sub->type == JAST_EXPR_NUMBER_VALUE)
    return apply_unary_op_to_double (expr->op, sub->number_value_expr.value);
  else if (sub->type == JAST_EXPR_BOOLEAN_VALUE
        && expr->op == JAST_UNARY_OP_LOGICAL_NOT)
    return jast_expr_new_boolean_value (!sub->value);
  else if ((  sub->type == JAST_EXPR_NULL_VALUE
           || sub->type == JAST_EXPR_UNDEFINED_VALUE)
        && expr->op == JAST_UNARY_OP_LOGICAL_NOT)
    return jast_expr_new_boolean_value (JS_TRUE);
  else if (sub->type == JAST_EXPR_BOOLEAN_VALUE)
    return jast_expr_new_number_value (apply_unary_op_to_double (expr->op, sub->boolean_value_expr.value ? 1. : 0.));
  else if (sub->type == JAST_EXPR_UNDEFINED_VALUE
        && expr->op == JAST_UNARY_OP_BITWISE_NOT)
    return jast_expr_new_number_value (-1);
  else if (sub->type == JAST_EXPR_NULL_VALUE)
    return jast_expr_new_number_value (apply_unary_op_to_double (expr->op, 0.));
  else if (sub->type == JAST_EXPR_STRING_VALUE
        && expr->op == JAST_UNARY_OP_LOGICAL_NOT)
    {
      ...
    }
  else if (sub->type == JAST_EXPR_STRING_VALUE)
    {
      parse number with errors given by NaN for + -   and 0 for ~ (! handled above)
      ...
    }
  else
    {
      ...
    }
}
