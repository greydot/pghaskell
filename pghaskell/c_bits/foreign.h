#pragma once

#include <stddef.h>

typedef void (*HsIOPtr)(void);

HsIOPtr hsCompileFunction(const char* code, size_t size);
int hsValidateFunction(const char* code, size_t size);
