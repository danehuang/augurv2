#include <HsFFI.h>

static void my_enter(void) __attribute__((constructor));
static void my_enter(void)
{
  static char *argv[] = { "libHSaugur-0.1.0.0-ghc7.8.4.dylib", 0 }, **argv_ = argv;
  static int argc = 1;
  hs_init(&argc, &argv_);
}

static void my_exit(void) __attribute__((destructor));
static void my_exit(void)
{
  hs_exit();
}
