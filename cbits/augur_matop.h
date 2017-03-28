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

#ifndef AUGUR_MATOP_H
#define AUGUR_MATOP_H

#include "augur_hdr.h"
#include "augur_util.h"
#include "augur_rtval.h"


/*
 * Assuming row major for now.
 */


/* Accessors */

#define AUGUR_MAT_GETI(mat, i, j)       (((int*) (mat)->data)[i * (mat)->row + j])
#define AUGUR_MAT_SETI(mat, i, j, v)    ({((int*) (mat)->data)[i * (mat)->row + j] = v;})

#define AUGUR_MAT_GETD(mat, i, j)       (((double*) (mat)->data)[i * (mat)->row + j])
#define AUGUR_MAT_SETD(mat, i, j, v)    ({((double*) (mat)->data)[i * (mat)->row + j] = v;})

__HOSTDEV__ AugurVec_t augur_mat_getv(AugurMat_t* mat, uint_t idx);
__HOSTDEV__ void augur_mat_setv(AugurMat_t* mat, uint_t idx, AugurVec_t* vec);


/* Metadata operations */

uint_t h_augur_mat_dim(AugurMat_t* mat, uint_t dim);
uint_t h_augur_mat_elems(AugurMat_t* mat);


/* Allocation */

void h_augur_mat_host_base_alloc(AugurMat_t* mat, void* data, AugurTyp_t ty, uint_t row, uint_t col);
void h_augur_mat_host_alloc(AugurMat_t* mat, AugurTyp_t ty, uint_t row, uint_t col, AugurAllocMode_t mode);
void h_augur_mat_host_cpy(AugurMat_t* dst, AugurMat_t* src, AugurTyp_t base_ty, AugurAllocMode_t mode);
void h_augur_mat_host_free(AugurMat_t* mat);

// AugurMat_t h_augur_mat_stk_alloc(AugurMemLoc_t loc, AugurTyp_t ty, uint_t row, uint_t col);
// AugurMat_t h_augur_mat_stk_alloc2(AugurMemLoc_t loc, AugurTyp_t ty, uint_t row, uint_t col, void* h_data);
// void h_augur_mat_stk_free(AugurMemLoc_t loc, AugurMat_t* mat);


/* Initialization */

void h_augur_mat_zero(AugurMemLoc_t loc, AugurMat_t* mat);
void h_augur_mat_cpy_data(AugurMemLoc_t loc, AugurMat_t* dst, AugurMat_t* src);


/* Viewing */

__HOSTDEV__ AugurMat_t augur_mat_view_as(AugurMat_t* src, AugurMat_t* shp);
__HOSTDEV__ AugurMat_t augur_mat_pll_view_as(AugurVec_t* src, AugurMat_t* shp, uint_t idx);
__HOSTDEV__ AugurVec_t augur_mat_view_as_row_vec(AugurMat_t* mat, uint_t row);


/* Printing */

void hi_augur_mat_dump(int tab, AugurMemLoc_t loc, AugurMat_t* mat);
void h_augur_mat_dump(AugurMemLoc_t loc, AugurMat_t* mat);


/* Matrix operations */

__HOSTDEV__ void augur_mat_cpy(AugurMat_t* dst, AugurMat_t* src);
__HOSTDEV__ void augur_mat_plus(AugurMat_t* dst, AugurMat_t* M1, AugurMat_t* M2);
__HOSTORDEV__ void augur_mat_atm_plus(AugurMat_t* dst, AugurMat_t* src);
__HOSTDEV__ void augur_mat_cholesky(AugurMat_t* A);
__HOSTDEV__ void augur_mat_lower_trmi(AugurMat_t* L);
__HOSTDEV__ void augur_mat_lower_trtcpy(AugurMat_t* L);
__HOSTDEV__ void augur_mat_cholesky_invert(AugurMat_t* LLT);
__HOSTDEV__ void augur_mat_trtrmm(AugurMat_t* L1, AugurMat_t* L2);
__HOSTDEV__ void augur_mat_trvm(AugurMat_t* L, AugurVec_t* bv);
__HOSTDEV__ void augur_mat_mvm(AugurVec_t* dst, AugurMat_t* A, AugurVec_t* v);
__HOSTDEV__ void augur_mat_vmm(AugurVec_t* dst, AugurVec_t* v, AugurMat_t* A);
__HOSTDEV__ void augur_mat_ms(double a, AugurMat_t* A);

__HOSTORDEV__ void augur_mat_atm_inc_vtmt(AugurMat_t* mat, AugurVec_t* vec);

/* Host matrix operations */

void h_augur_mat_plus(AugurMat_t* dst, AugurMat_t* m1, AugurMat_t* m2);


/* Transfer operations */

void hi_augur_mat_to_native(AugurMemLoc_t loc, AugurMat_t* dst, char** dst_data, AugurMat_t* src, Bool_t f_cpy);
void h_augur_mat_to_native(AugurMemLoc_t loc, AugurMat_t* dst, AugurMat_t* src, Bool_t f_cpy);

void hi_augur_mat_from_native(AugurMemLoc_t loc, AugurMat_t* dst, char** src_data);
void h_augur_mat_from_native(AugurMemLoc_t loc, AugurMat_t* dst, AugurMat_t* src);

void h_augur_mat_basis_add(AugurMemLoc_t loc, AugurMat_t* mat, int idx, double v);
void h_augur_mat_getd_idx(AugurMemLoc_t loc, double* dst, AugurMat_t* mat, int idx);
void h_augur_mat_setd_idx(AugurMemLoc_t loc, AugurMat_t* mat, int idx, double* src);

#endif
