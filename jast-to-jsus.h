
typedef struct {
  JSUS_CompileOutput_Type type;
  union {
    struct {
      unsigned n_errors;
      JAST_ParseError *errors;
    } failure;
    struct {
      ... statement entry point
      ... functions
    } success;
  } info;
} JSUS_CompileOutput;


void JAST_to_JSUS (JAST_Statement *stmt,
                   JSUS_CompileOutput *output);
