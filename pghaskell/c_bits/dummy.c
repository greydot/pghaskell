// This file is only required to make intero and other tools happy.
// The variables declared in this file are normally provided within the
// PostgreSQL context.

#define _POSIX_C_SOURCE 200809L
#include <stdlib.h>
#include <setjmp.h>

#include <server/postgres.h>
#include <server/executor/spi.h>
#include <server/utils/elog.h>

ErrorContextCallback *error_context_stack = NULL;
sigjmp_buf *PG_exception_stack = NULL;
