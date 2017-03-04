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

#include "augur_rtval.h"


/* Operations */

char* ty2str(AugurTyp_t ty) {
  switch (ty) {
  case AUGUR_INT: {
    return "INT_TY";
  }
  case AUGUR_DBL: {
    return "DBL_TY";
  }
  case AUGUR_VEC: {
    return "VEC_TY";
  }
  case AUGUR_MAT: {
    return "MAT_TY";
  }
  default: {
    // TODO: ERROR
    return 0;
  }
  }
}

uint_t ty2size(AugurTyp_t ty) {
  switch (ty) {
  case AUGUR_INT:
    return sizeof(int);
  case AUGUR_DBL:
    return sizeof(double);
  case AUGUR_VEC:
    return sizeof(AugurVec_t);
  case AUGUR_MAT:
    return sizeof(AugurMat_t);
  default:
    // TODO: ERROR
    return 0;
  }
}

void hi_augur_int_to_native(AugurMemLoc_t loc, char** dst_data, int* src, Bool_t f_cpy) {
  if (f_cpy) {
    switch (loc) {
    case AUGUR_CPU: {
      augur_memcpy(*dst_data, src, sizeof(int), AUGUR_H2H);
      break;
    }
#ifndef AUGURCPU
    case AUGUR_GPU: {
      augur_memcpy(*dst_data, src, sizeof(int), AUGUR_H2D);	
      break;
    }
#endif
    }
  }
  *dst_data += sizeof(int);
}

void h_augur_int_to_native(AugurMemLoc_t loc, int* dst, int* src, Bool_t f_cpy) {
  dst = augur_malloc(sizeof(int), loc);
  char* char_dst = (char*) dst;
  hi_augur_int_to_native(loc, &char_dst, src, f_cpy);
}

void hi_augur_int_from_native(AugurMemLoc_t loc, int* dst, char** src_data) {
  switch (loc) {
  case AUGUR_CPU: {
    augur_memcpy(dst, *src_data, sizeof(int), AUGUR_H2H);
    break;
  }
#ifndef AUGURCPU
  case AUGUR_GPU: {
    augur_memcpy(dst, *src_data, sizeof(int), AUGUR_D2H);
    break;
  }
#endif
  }
  *src_data += sizeof(int);
}

void h_augur_int_from_native(AugurMemLoc_t loc, int* dst, int* src) {
  char* char_src = (char*) src;
  hi_augur_int_from_native(loc, dst, &char_src);
}


void hi_augur_dbl_to_native(AugurMemLoc_t loc, char** dst_data, double* src, Bool_t f_cpy) {
  if (f_cpy) {
    switch (loc) {
    case AUGUR_CPU: {
      augur_memcpy(*dst_data, src, sizeof(double), AUGUR_H2H);
      break;
    }
#ifndef AUGURCPU
    case AUGUR_GPU: {
      augur_memcpy(*dst_data, src, sizeof(double), AUGUR_H2D);	
      break;
    }
#endif
    }
  }
  *dst_data += sizeof(double);
}

void h_augur_dbl_to_native(AugurMemLoc_t loc, double* dst, double* src, Bool_t f_cpy) {
  dst = augur_malloc(sizeof(double), loc);
  char* char_dst = (char*) dst;
  hi_augur_dbl_to_native(loc, &char_dst, src, f_cpy);
}

void hi_augur_dbl_from_native(AugurMemLoc_t loc, double* dst, char** src_data) {
  switch (loc) {
  case AUGUR_CPU: {
    augur_memcpy(dst, *src_data, sizeof(double), AUGUR_H2H);
    break;
  }
#ifndef AUGURCPU
  case AUGUR_GPU: {
    augur_memcpy(dst, *src_data, sizeof(double), AUGUR_D2H);
    break;
  }
#endif
  }
  *src_data += sizeof(double);
}

void h_augur_dbl_from_native(AugurMemLoc_t loc, double* dst, double* src) {
  char* char_src = (char*) src;
  hi_augur_dbl_from_native(loc, dst, &char_src);
}

void h_augur_basety_cpy_data(AugurMemLoc_t loc, void* dst, void* src, AugurTyp_t ty) {
  switch (ty) {
  case AUGUR_INT:
  case AUGUR_DBL: {
    switch (loc) {
    case AUGUR_CPU: {
      augur_memcpy(dst, src, sizeof(ty2size(ty)), AUGUR_H2H);
      break;
    }
#ifndef AUGURCPU
    case AUGUR_GPU: {
      augur_memcpy(dst, src, sizeof(ty2size(ty)), AUGUR_D2D);
      break;
    }
#endif
    }
    break;
  }
  default: {
    // TODO: ERROR
    break;
  }
  }
}
