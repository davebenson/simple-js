

typedef enum {
  JSUS_GC_VALUE_NUMBER,
  JSUS_GC_VALUE_STRING,
  JSUS_GC_VALUE_OBJECT,
  JSUS_GC_VALUE_ARRAY,
  JSUS_GC_VALUE_NULL,
  JSUS_GC_VALUE_BOOLEAN,
  JSUS_GC_VALUE_UNDEFINED,
} JSUS_GC_Value_Type;

struct JSUS_GC_Value {
  JSUS_GC_Value_Type type;
  union {
    double v_number;
    JSUS_String *v_string;
    JSUS_GC_Object *v_object;
    JSUS_GC_Array *v_array;
    JSUS_Boolean v_boolean;
  } info;
};

struct JSUS_GC_Object {
};
void           jsus_gc_object_set_number    (JSUS_GC_Object *object,
                                             JSUS_String    *key,
                                             double          number);
void           jsus_gc_object_set_string    (JSUS_GC_Object *object,
                                             JSUS_String    *key,
                                             JSUS_String    *value);
void           jsus_gc_object_set_boolean   (JSUS_GC_Object *object,
                                             JSUS_String    *key,
                                             JSUS_Boolean    value);
void           jsus_gc_object_set_null      (JSUS_GC_Object *object,
                                             JSUS_String    *key);
void           jsus_gc_object_set_object    (JSUS_GC_Object *object,
                                             JSUS_String    *key,
                                             JSUS_GC_Object *value);
void           jsus_gc_object_set_array     (JSUS_GC_Array  *array,
                                             JSUS_String    *key,
                                             JSUS_GC_Array  *value);
void           jsus_gc_object_delete        (JSUS_GC_Object *object,
                                             JSUS_String    *key);
JSUS_GC_Value *jsus_gc_object_get           (JSUS_GC_Object *object,
                                             JSUS_String *key);


JSUS_GC_Runtime *jsus_gc_runtime_new         (void);
JSUS_GC_Object  *jsus_gc_object_new          (JSUS_GC_Runtime *runtime);
void             jsus_gc_runtime_add_root    (JSUS_GC_Runtime   *runtime,
                                              JSUS_GC_Object    *object);
void             jsus_gc_runtime_remove_root (JSUS_GC_Runtime   *runtime,
                                              JSUS_GC_Object    *object);

