/* pointer-to-pointer map.  Not for use in public APIs. */


typedef struct Ptrmap Ptrmap;

Ptrmap *ptrmap_new ();
void ptrmap_destroy (Ptrmap *);

/* Returns old value. */
void *ptrmap_set (Ptrmap *map, void *key, void *value);

/* Returns whether the value was initially unset. */
JS_Boolean ptrmap_try_set (Ptrmap *map, void *key, void *value, void **existing_value_out);

/* Returns whether the value was initially set. */
JS_Boolean ptrmap_try_replace (Ptrmap *map, void *key, void *value, void **old_value_out);

/* Returns old value. */
void *ptrmap_get (Ptrmap *map, void *key);

/* Returns old value. */
void *ptrmap_remove (Ptrmap *map, void *key);


