// This file is only required to make intero and other tools happy.
// The variables and functions declared in this file are normally
// provided within the PostgreSQL context.

#define _POSIX_C_SOURCE 200809L
#include <stdlib.h>
#include <setjmp.h>

#include <server/postgres.h>
#include <server/executor/spi.h>
#include <server/utils/elog.h>

ErrorContextCallback *error_context_stack = NULL;
sigjmp_buf *PG_exception_stack = NULL;

int SPI_connect(void) { return -1; }
int SPI_finish(void) { return -1; }

void pg_re_throw(void) { puts("pg_re_throw"); for(;;); }
void elog_start(const char *filename, int lineno, const char *funcname) { return; }
void elog_finish(int elevel, const char *fmt, ...) { return; }
