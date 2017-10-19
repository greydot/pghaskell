#pragma once

#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 2
#endif

#include <setjmp.h>
#include <server/postgres.h>

typedef struct {
    Oid procOid;      // Function oid
    Oid isTrigger;    // Used instead of bool to avoid padding
    Oid userId;       // User ID in trusted environment
} pghsProcKey;

typedef struct {
    char argName[NAMEDATALEN];
    char typeName[NAMEDATALEN];
} pghsArg;

typedef struct {
    bool isNull;
    Datum datum;
} pghsArgValue;

typedef struct {
    size_t codeSize;
    const char *code;
    size_t argsNum;
    pghsArg *args;
} pghsProcInfo;
