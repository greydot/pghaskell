#pragma once

#include <stddef.h>

#include "pghaskell/types.h"

typedef Datum (*HsIOPtr)(const pghsArgValue*, size_t);

HsIOPtr hsCompileFunction(pghsProcInfo*);
int hsValidateFunction(const char* code, size_t size);
