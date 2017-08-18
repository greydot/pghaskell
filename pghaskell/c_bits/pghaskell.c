#define _POSIX_C_SOURCE 1

#include <unistd.h>
#include <fcntl.h>
#include <setjmp.h>

#include <server/postgres.h>
#include <server/access/htup_details.h>
#include <server/access/xact.h>
#include <server/catalog/pg_proc.h>
#include <server/catalog/pg_type.h>
#include <server/commands/event_trigger.h>
#include <server/commands/trigger.h>
#include <server/executor/spi.h>
#include <server/fmgr.h>
#include <server/mb/pg_wchar.h>
#include <server/miscadmin.h>
#include <server/nodes/makefuncs.h>
#include <server/parser/parse_type.h>
#include <server/tcop/tcopprot.h>
#include <server/utils/builtins.h>
#include <server/utils/elog.h>
#include <server/utils/lsyscache.h>
#include <server/utils/memutils.h>
#include <server/utils/rel.h>
#include <server/utils/syscache.h>
#include <server/utils/typcache.h>

#include <HsFFI.h>

#include "foreign.h"
#include "pghaskell.h"

#ifdef PG_MODULE_MAGIC
PG_MODULE_MAGIC;
#endif

static bool pgHaskellInitialized = false;

// Prototypes
void _PG_init(void);
static void initHsRTS(void);
static void cleanupHsRTS(void) __attribute__((destructor));

Datum pghsCallHandler(PG_FUNCTION_ARGS);
static Datum pghsFuncHandler(PG_FUNCTION_ARGS, bool);
static HsIOPtr compilePGHaskellFunction(Oid, Oid, bool, bool);


//                 C0D3

// special initialization function called by postgres

void _PG_init(void)
{
    // Initialize only once
    if(pgHaskellInitialized)
      return;

    initHsRTS();

    pgHaskellInitialized = true;
}

static void initHsRTS(void)
{
    static int argc = 0;
    static char *args[] = { "libpghaskell.so", NULL };
    static char **argv = args;

    for(argc = 0; args[argc] != NULL; argc++);

    elog(DEBUG1, "Initializing Haskell runtime...");
    hs_init(&argc, &argv);
    elog(DEBUG1, "Haskell runtime initialized.");
}

// Clean up on exit from library
static void cleanupHsRTS(void)
{
    // Note, we don't need to call hs_exit() here.
}

PG_FUNCTION_INFO_V1(pghsCallHandler);

Datum pghsCallHandler(PG_FUNCTION_ARGS)
{
    Datum ret;

    elog(DEBUG1, "Entering pghsCallHandler.");
    PG_TRY(); {
        ret = pghsFuncHandler(fcinfo, false);
    }
    PG_CATCH(); {
        // do some clean up
        PG_RE_THROW();
    }
    PG_END_TRY();

    elog(DEBUG1, "Finishing pghsCallHandler.");
    return ret;
}

static Datum pghsFuncHandler(PG_FUNCTION_ARGS, bool pltrusted)
{
    if(SPI_connect() != SPI_OK_CONNECT)
        elog(ERROR, "SPI connection failure");

    HsIOPtr fn = compilePGHaskellFunction(fcinfo->flinfo->fn_oid
                                         ,InvalidOid
                                         ,false
                                         ,pltrusted);

    if(fn) fn();

    SPI_finish();

    Datum ret = 1;
    return ret;
}


static HsIOPtr compilePGHaskellFunction( Oid fnOid
                                       , Oid tgrOid
                                       , bool isEventTrigger
                                       , bool trusted)
{
    HeapTuple procTup = SearchSysCache1(PROCOID, ObjectIdGetDatum(fnOid));

    if(!HeapTupleIsValid(procTup))
        elog(ERROR, "cache lookup failed for function oid %u", fnOid);

    Form_pg_proc procStruct = (Form_pg_proc) GETSTRUCT(procTup);
    (void) procStruct;
    pghsProcKey procKey = { .procOid = fnOid
                          , .isTrigger = OidIsValid(tgrOid)
                          , .userId = trusted ? GetUserId() : InvalidOid };

    bool isNull = false;
    Datum srcDatum = SysCacheGetAttr(PROCOID, procTup, Anum_pg_proc_prosrc, &isNull);

    if(isNull) {
        elog(ERROR, "null proc src %u", fnOid);
        // Unreachable
        return NULL;
    }

    const char *procSource = TextDatumGetCString(srcDatum);
    const size_t srcLen = strlen(procSource);

    HsIOPtr fn = hsCompileFunction(procSource, srcLen);

    ReleaseSysCache(procTup);

    return fn;
}
