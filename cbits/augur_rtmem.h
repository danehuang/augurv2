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

#ifndef AUGUR_RTMEM_H
#define AUGUR_RTMEM_H

#include "augur_hdr.h"
#include "augur_util.h"

typedef enum AugurDimKind {
  DIM_MAX=0,
  DIM_VAL=1,
  DIM_MAT=2,
  DIM_CPY=3,
} AugurDimKind_t;

typedef struct AugurDim {
  AugurDimKind_t kind;
  union {
    struct { AugurTyp_t objty; void* obj; uint_t dim; };
    struct { uint_t val; };
    struct { uint_t row; uint_t col; };
    struct { AugurTyp_t cpyty; void* cpyof; };
  };
} AugurDim_t;

typedef struct AugurShape {
  AugurTyp_t base_ty;
  uint_t len;
  AugurDim_t* dims;
} AugurShape_t;


/* Allocation */

AugurShape_t h_augur_shape_stk_alloc(AugurDim_t* dims, uint_t len, AugurTyp_t ty);
void h_augur_shape_stk_free(AugurShape_t* shape);


/* Printing */

void h_augur_dim_dump(AugurDim_t* dim);
void h_augur_shape_dump(AugurShape_t* shp);


/* Operations */

int* h_augur_rtval_from_int(AugurMemLoc_t loc, int* src, Bool_t f_cpy);
double* h_augur_rtval_from_dbl(AugurMemLoc_t loc, double* src, Bool_t f_cpy);
void h_augur_rtval_from_vec(AugurMemLoc_t loc, AugurFlatVec_t* dst, AugurVec_t* vec, Bool_t f_cpy);
void h_augur_rtval_from_mat(AugurMemLoc_t loc, AugurMat_t* dst, AugurMat_t* mat, Bool_t f_cpy);
void h_augur_rtval_from_shape(AugurMemLoc_t loc, void* dst, AugurShape_t* shape);

#endif
