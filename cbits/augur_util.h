/*
 * Copyright 2017 Daniel Eachern Huang
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef AUGUR_UTIL_H
#define AUGUR_UTIL_H

#ifndef AUGURCPU
#include <cuda.h>
#endif


typedef unsigned int uint_t;
typedef double real_t;


typedef enum Bool {
  FALSE = 0,
  TRUE = 1,
} Bool_t;


typedef enum AugurCode {
  AUGUR_SUCCESS = 0,
  AUGUR_ERR_TYP = 1,
} AugurCode_t;


typedef enum AugurAllocMode {
  AUGUR_DATA = 0,
  AUGUR_SKEL = 1,
} AugurAllocMode_t;


#ifdef AUGURCPU
typedef enum AugurMemLoc {
  AUGUR_CPU = 0,
} AugurMemLoc_t;
#else
typedef enum AugurMemLoc {
  AUGUR_CPU = 0,
  AUGUR_GPU = 1,
} AugurMemLoc_t;
#endif


#ifdef AUGURCPU
typedef enum AugurMemDir {
  AUGUR_H2H = 0,
} AugurMemDir_t;
#else
typedef enum AugurMemDir {
  AUGUR_H2H = 0,
  AUGUR_H2D = 1,
  AUGUR_D2H = 2,
  AUGUR_D2D = 3,
} AugurMemDir_t;
#endif




/* Debug functions */

/*
#define DEBUG 1

void dbg_printf(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
}

#define TRACE(x) do { if (DEBUG) dbg_printf x; } while (0)
*/

/* GPU functions */

#ifndef AUGURCPU
void GPU_CHK_ERR(cudaError_t code);
__device__ double atomicAdd(double* address, double val);
#endif


/* Random functions */

uint_t augur_min(uint_t i1, uint_t i2);
uint_t augur_max(uint_t i1, uint_t i2);
void augur_tab(int tab);


/* Memory functions */

void* augur_malloc(uint_t numbytes, AugurMemLoc_t loc);
void augur_free(void* ptr, AugurMemLoc_t loc);
void augur_memcpy(void* dst, void* src, uint_t numbytes, AugurMemDir_t dir);
void augur_memset(void* dst, int val, uint_t numbytes, AugurMemLoc_t loc);


/* Output functions */

void augur_arr_dumpi(int* arr, uint_t len, AugurMemLoc_t loc);
void augur_arr_dumpd(double* arr, uint_t len, AugurMemLoc_t loc);

#endif
