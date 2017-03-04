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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef AUGURCPU
#include <cuda.h>
#endif

#include "augur_util.h"


/* GPU functions */

#ifndef AUGURCPU
void GPU_CHK_ERR(cudaError_t code) {
  if (code != cudaSuccess) {
    printf("GPUassert: %s\n", cudaGetErrorString(code));
    exit(code);
  }
}

__device__ double atomicAdd(double* address, double val) {
    unsigned long long int* address_as_ull =
                             (unsigned long long int*)address;
    unsigned long long int old = *address_as_ull, assumed;
    do {
        assumed = old;
	old = atomicCAS(address_as_ull, assumed,
                        __double_as_longlong(val +
                               __longlong_as_double(assumed)));
    } while (assumed != old);
    return __longlong_as_double(old);
}
#endif


/* Random functions */

uint_t augur_min(uint_t i1, uint_t i2) {
  if (i1 < i2) return i1;
  else return i2;
}

uint_t augur_max(uint_t i1, uint_t i2) {
  if (i1 < i2) return i2;
  else return i1;
}

void augur_tab(int tab) {
  for (int i = 0; i < tab; i++) {
    printf(" ");
  }
}


/* Memory functions */

void* augur_malloc(uint_t numbytes, AugurMemLoc_t loc) {
  switch (loc) {
  case AUGUR_CPU: {
    return malloc(numbytes);
  }
#ifndef AUGURCPU
  case AUGUR_GPU: {
    void* ptr;
    GPU_CHK_ERR(cudaMalloc(&ptr, numbytes));
    return ptr;
  }
#endif
  }
}

void augur_free(void* ptr, AugurMemLoc_t loc) {
  switch (loc) {
  case AUGUR_CPU: {
    free(ptr);
    break;
  }
#ifndef AUGURCPU
  case AUGUR_GPU: {
    cudaFree(ptr);
    break;
  }
#endif
  }
}

void augur_memcpy(void* dst, void* src, uint_t numbytes, AugurMemDir_t dir) {
  switch (dir) {
  case AUGUR_H2H: {
    memcpy(dst, src, numbytes);
    break;
  }
#ifdef AUGURCPU
  default: {
    // TODO: ERROR
    break;
  }
#else
  case AUGUR_H2D: {
    GPU_CHK_ERR(cudaMemcpy(dst, src, numbytes, cudaMemcpyHostToDevice));
    break;
  }
  case AUGUR_D2H: {
    GPU_CHK_ERR(cudaMemcpy(dst, src, numbytes, cudaMemcpyDeviceToHost));
    break;
  }
  case AUGUR_D2D: {
    GPU_CHK_ERR(cudaMemcpy(dst, src, numbytes, cudaMemcpyDeviceToDevice));
    break;
  }
#endif
  }
}

void augur_memset(void* dst, int val, uint_t numbytes, AugurMemLoc_t loc) {
  switch (loc) {
  case AUGUR_CPU: {
    memset(dst, val, numbytes);
    break;
  }
#ifndef AUGURCPU
  case AUGUR_GPU: {
    GPU_CHK_ERR(cudaMemset(dst, value, numbytes));
    break;
  }
#endif
  }
}


/* Output functions */

void augur_arr_dumpi(int* arr, uint_t len, AugurMemLoc_t loc) {
  uint_t mlen = augur_min(100, len);
  switch (loc) {
  case AUGUR_CPU: {
    for (uint_t i = 0; i < mlen; i++) {
      printf("%d ", arr[i]);
    }
    break;
  }
#ifndef AUGURCPU
  case AUGUR_GPU: {
    int tmp[100];
    augur_memcpy((void*) tmp, arr, mlen * sizeof(int), AUGUR_D2H);
    for (uint_t i = 0; i < mlen; i++) {
      printf("%d ", tmp[i]);
    }
    break;
  }
#endif
  }
  printf("\n");
}

void augur_arr_dumpd(double* arr, uint_t len, AugurMemLoc_t loc) {
  uint_t mlen = augur_min(100, len);
  switch (loc) {
  case AUGUR_CPU: {
    for (uint_t i = 0; i < mlen; i++) {
      printf("%f ", arr[i]);
      // printf("%.10e ", arr[i]);
    }
    break;
  }
#ifndef AUGURCPU
  case AUGUR_GPU: {
    double tmp[100];
    augur_memcpy((void*) tmp, arr, mlen * sizeof(double), AUGUR_D2H);
    for (uint_t i = 0; i < mlen; i++) {
      printf("%f ", tmp[i]);
    }
    break;
  }
#endif
  }
  printf("\n");
}
