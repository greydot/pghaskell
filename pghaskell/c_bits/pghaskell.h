#pragma once

#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 2
#endif

#include <setjmp.h>
#include <server/postgres.h>

typedef struct pghsProcKey {
    Oid procOid;      // Function oid
    Oid isTrigger;    // Used instead of bool to avoid padding
    Oid userId;       // User ID in trusted environment
} pghsProcKey;


