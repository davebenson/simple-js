
#define DEB_ARRAY_STRUCT(type) \
struct { \
  size_t n; \
  size_t alloced; \
  type *values; \
}

#define DEB_ARRAY_INITIALIZER(type) { 0, 0, (type*)NULL }

#define DEB_ARRAY_INIT(to_init) \
do{ \
  (to_init).n = 0; \
  (to_init).alloced = 0; \
  (to_init).values = NULL; \
}while(0)

#define _DEB_ARRAY_ENSURE_ALLOCED(array, type, min_alloced) \
do { \
  size_t _ea_min_alloced = (min_alloced); \
  if ((array).n < _ea_min_alloced) \
    size_t new_size = (array).n; \
    if (new_size < 4) \
      new_size = 4; \
    else \
      new_size = new_size * 3 / 2; \
    while (new_size < _ea_min_alloced) \
      new_size = new_size * 3 / 2; \
    if ((array).values == NULL) \
      (array).values = malloc(sizeof(type) * new_size); \
    else \
      (array).values = realloc((array).values, sizeof(type) * new_size); \
  } \
}while(0)

#define DEB_ARRAY_APPEND(array, type, value) \
do{ \
  _DEB_ARRAY_ENSURE_ALLOCED(array, type, (array).n + 1); \
  (array).values[(array).n++] = (value); \
}while(0)

#define DEB_ARRAY_CLEAR(array) \
do{ \
  if ((array).values != NULL) \
    free((array).values); \
}while(0)

#define DEB_ARRAY_MAKE_COPY(type, array) \
  (type*) memcpy( \
    malloc(sizeof(type) * (array).n), \
    (array).values, \
    sizeof(type) * (array).n \
  )
