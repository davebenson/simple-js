
typedef enum
{
  JV_VALUE_UNDEFINED,   /* must be 0 */
  JV_VALUE_BOOLEAN,
  JV_VALUE_NULL,
  JV_VALUE_STRING,
  JV_VALUE_NUMBER,
  JV_VALUE_ARRAY,
  JV_VALUE_OBJECT,
  JV_VALUE_FUNCTION
} JVValueType;

typedef union JV JV;

typedef struct {
  JVValueType type;
} JVValueBase;

typedef struct {
  JVValueBase base;
  int value;
} JVValueBoolean;

typedef struct {
  JVValueBase base;
  double value;
} JVValueNumber;

typedef struct {
  JVValueBase base;
  JS_String *string;
} JVValueString;
extern JVValueString jv_value_empty_string;

typedef struct {
  JVValueBase base;
  JVObjectPrototype *prototype;
  JVObjectFuncs funcs;
  void *func_data;
} JVValueObject;

typedef enum {
  JV_INVOCATION_THREW,
  JV_INVOCATION_RETURNED
} JVInvocationResultType;

typedef struct {
  JVInvocationResultType type;
  JV *rv;
} JVInvocationResult;

typedef struct {
  JVValueObject base;
  JVInvocationResult (*invoke)(size_t n_args, JV **args, void *func_data);
} JVValueFunction;

union JV {
  JVValueType type;
  JVValueBoolean v_boolean;
  JVValueString v_string;
  JVValueNumber v_number;
  JVValueArray v_array;
  JVValueObject v_object;
  JVValueFunction v_function;
};
