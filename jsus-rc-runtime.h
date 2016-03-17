struct JSUS_RC_Value {
  JSUS_RC_Value_Type type;
  union {
    double v_number;
    JSUS_String *v_string;
    JSUS_RC_Object *v_object;
    JSUS_RC_Array *v_array;
    JSUS_Boolean v_boolean;
  } info;
};

struct JSUS_RC_BaseObject {
  unsigned ref_count;
  unsigned is_array : 1;
  JSUS_RC_WeakRef *first_weakref, *last_weakref;
};
struct JSUS_RC_Object {
  JSUS_RC_BaseObject base;
};
struct JSUS_RC_Array {
  JSUS_RC_BaseObject base;
  unsigned n_values;
  JSUS_RC_Value *values;
};

void           jsus_rc_object_set_number    (JSUS_RC_Object *object,
                                             JSUS_String    *key,
                                             double          number);
void           jsus_rc_object_set_string    (JSUS_RC_Object *object,
                                             JSUS_String    *key,
                                             JSUS_String    *value);
void           jsus_rc_object_set_boolean   (JSUS_RC_Object *object,
                                             JSUS_String    *key,
                                             JSUS_Boolean    value);
void           jsus_rc_object_set_null      (JSUS_RC_Object *object,
                                             JSUS_String    *key);
void           jsus_rc_object_set_object    (JSUS_RC_Object *object,
                                             JSUS_String    *key,
                                             JSUS_RC_Object *value);
void           jsus_rc_object_set_array     (JSUS_RC_Array  *array,
                                             JSUS_String    *key,
                                             JSUS_RC_Array  *value);
void           jsus_rc_object_delete        (JSUS_RC_Object *object,
                                             JSUS_String    *key);
JSUS_RC_Value *jsus_rc_object_get           (JSUS_RC_Object *object,
                                             JSUS_String *key);


JSUS_RC_Runtime *jsus_gc_runtime_new         (void);
JSUS_RC_Object  *jsus_rc_object_new          (JSUS_RC_Runtime *runtime);
void             jsus_gc_runtime_add_root    (JSUS_RC_Runtime   *runtime,
                                              JSUS_RC_Object    *object);
void             jsus_gc_runtime_remove_root (JSUS_RC_Runtime   *runtime,
                                              JSUS_RC_Object    *object);

