// This file is only required to make intero and other tools happy.
// The variables and functions declared in this file are normally
// provided within the PostgreSQL context.

#define _POSIX_C_SOURCE 200809L
#include <stdlib.h>
#include <setjmp.h>

#include <server/postgres.h>
#include <server/executor/spi.h>
#include <server/utils/elog.h>

#define UNUSED(a) (void)(a)

ErrorContextCallback *error_context_stack = NULL;
sigjmp_buf *PG_exception_stack = NULL;

int SPI_connect(void) { return -1; }
int SPI_finish(void) { return -1; }

void pg_re_throw(void) {
    // Infinite loop with no side effects is considered a UB
    // by C standard.
    static int i = 0;
    puts("pg_re_throw");
    for(;;) i++;
}
void elog_start(const char *filename, int lineno, const char *funcname) {
    UNUSED(filename); UNUSED(lineno); UNUSED(funcname); return;
}
void elog_finish(int elevel, const char *fmt, ...) { UNUSED(elevel); UNUSED(fmt); return; }
bool errstart(int elevel, const char *filename, int lineno, const char *funcname, const char *domain) {
    UNUSED(elevel); UNUSED(filename); UNUSED(lineno); UNUSED(funcname); UNUSED(domain); return false;
}
int errmsg(const char *fmt,...) { UNUSED(fmt); return -1; }
int errcode(int e) { UNUSED(e); return -1; }
void errfinish(int d,...) { UNUSED(d); }

void *palloc(size_t s) { UNUSED(s); return NULL; }
void pfree(void* p) { UNUSED(p); return; }
char *text_to_cstring(const text *t) { UNUSED(t); return NULL; }

HeapTuple SearchSysCache(int cacheId, Datum k1, Datum k2, Datum k3, Datum k4) {
    UNUSED(cacheId); UNUSED(k1); UNUSED(k2); UNUSED(k3); UNUSED(k4); return NULL;
}
void ReleaseSysCache(HeapTuple tuple) { UNUSED(tuple); }
Oid GetUserId(void) { return -1; }
Datum SysCacheGetAttr(int cacheId, HeapTuple tup, AttrNumber attributeNumber, bool *isNull) {
    UNUSED(cacheId); UNUSED(tup); UNUSED(attributeNumber); UNUSED(isNull); return -1;
}

bool CheckFunctionValidatorAccess(Oid validatorOid, Oid functionOid) {
    UNUSED(validatorOid); UNUSED(functionOid); return false;
}

int get_func_arg_info(HeapTuple procTup,
                      Oid **p_argtypes, char ***p_argnames,
                      char **p_argmodes) {
    UNUSED(procTup); UNUSED(p_argtypes); UNUSED(p_argnames); UNUSED(p_argmodes);
    return -1;
}
