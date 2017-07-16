#define _POSIX_C_SOURCE 1

#include <unistd.h>
#include <fcntl.h>
#include <setjmp.h>

#include <server/postgres.h>
#include <server/executor/spi.h>
#include <server/utils/elog.h>

#include <HsFFI.h>

#ifdef PG_MODULE_MAGIC
PG_MODULE_MAGIC;
#endif

static bool pgHaskellInitialized = false;

// Prototypes
void _PG_init(void);
static void initHsRTS(void);
static void cleanupHsRTS(void) __attribute__((destructor));

Datum pghsCallHandler(PG_FUNCTION_ARGS);
static Datum pghsFuncHandler(PG_FUNCTION_ARGS);

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
    static int argc = 1;
    static char *args[] = { "libpghaskell.so", NULL };
    static char **argv = args;

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
        ret = pghsFuncHandler(fcinfo);
    }
    PG_CATCH(); {
        // do some clean up
        PG_RE_THROW();
    }
    PG_END_TRY();

    elog(DEBUG1, "Finishing pghsCallHandler.");
    return ret;
}

static Datum pghsFuncHandler(PG_FUNCTION_ARGS)
{
    Datum ret = 1;

    (void) fcinfo;

    if(SPI_connect() != SPI_OK_CONNECT)
        elog(ERROR, "SPI connection failure");

    SPI_finish();

    return ret;
}
