#pragma once

#include <server/postgres.h>

typedef struct pghsProcKey {
    Oid procOid;      // Function oid
    Oid isTrigger;    // Used as boolean.
    Oid userId;       // User ID in trusted environment.
} pghsProcKey;

typedef struct pghsProcDesc {

} pghsProcDesc;

typedef struct pghsProcEntry {
    pghsProcKey key;
    pghsProcDesc *desc;
} pghsProcEntry;
