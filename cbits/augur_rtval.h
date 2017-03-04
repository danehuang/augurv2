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

#ifndef AUGUR_RTVAL_H
#define AUGUR_RTVAL_H

#include "augur_hdr.h"
#include "augur_util.h"


typedef enum AugurTyp {
  AUGUR_INT = 0,
  AUGUR_DBL = 1,
  AUGUR_VEC = 2,
  AUGUR_MAT = 3,
} AugurTyp_t;


typedef struct AugurVec {
  AugurTyp_t ty;
  uint_t stride;
  uint_t elems;
  void* data;
} AugurVec_t;


typedef struct AugurFlatVec {
  AugurTyp_t base_ty;
  uint_t base_elems;
  void* base_data;
  AugurVec_t vec;
  // Can also store shape here
} AugurFlatVec_t;


typedef struct AugurMat {
  AugurTyp_t ty;
  uint_t row;
  uint_t col;
  void* data;
} AugurMat_t;


typedef struct AugurBlk {
  uint_t num_blks;
  AugurTyp_t* typs;
  void** blks;           // AugurVec_t* | AugurFlatVec_t* | AugurMat_t*
  Bool_t native;
  uint_t base_elems;
  double* base_data;
} AugurBlk_t;


/* Operations */

char* ty2str(AugurTyp_t ty);
uint_t ty2size(AugurTyp_t ty);

void hi_augur_int_to_native(AugurMemLoc_t loc, char** dst_data, int* src, Bool_t f_cpy);
void h_augur_int_to_native(AugurMemLoc_t loc, int* dst, int* src, Bool_t f_cpy);

void hi_augur_int_from_native(AugurMemLoc_t loc, int* dst, char** src_data);
void h_augur_int_from_native(AugurMemLoc_t loc, int* dst, int* src);
			    
void hi_augur_dbl_to_native(AugurMemLoc_t loc, char** dst_data, double* src, Bool_t f_cpy);
void h_augur_dbl_to_native(AugurMemLoc_t loc, double* dst, double* src, Bool_t f_cpy);

void hi_augur_dbl_from_native(AugurMemLoc_t loc, double* dst, char** src_data);
void h_augur_dbl_from_native(AugurMemLoc_t loc, double* dst, double* src);

void h_augur_basety_cpy_data(AugurMemLoc_t loc, void* dst, void* src, AugurTyp_t ty);

#endif
