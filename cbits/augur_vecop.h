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

#ifndef AUGUR_VECOP_H
#define AUGUR_VECOP_H

#include "augur_hdr.h"
#include "augur_rtval.h"


/* MOVE ME */

#define AUGUR_SETI(dst, i)        ({*(dst) = i;})
#define AUGUR_SETD(dst, d)        ({*(dst) = d;})

#ifdef AUGURCPU
#define AUGUR_ATMINCI(dst, i)     ({*(dst) = *(dst) + (i);})
#define AUGUR_ATMINCD(dst, d)     ({*(dst) = *(dst) + (d);})
#else
#define AUGUR_ATMINCI(dst, i)     ({atomicAdd(dst, i);})
#define AUGUR_ATMINCD(dst, d)     ({atomicAdd(dst, d);})
#endif

#define AUGUR_INCI(dst, i)        ({*(dst) = *(dst) + (i);})
#define AUGUR_INCD(dst, d)        ({*(dst) = *(dst) + (d);})

#define AUGUR_ATMMINCI(dst, i)    ({*(dst) = *(dst) + (i);})
#define AUGUR_ATMMINCD(dst, d)    ({*(dst) = *(dst) + (d);})

#define AUGUR_MINCI(dst, i)       ({*(dst) = *(dst) + (i);})
#define AUGUR_MINCD(dst, d)       ({*(dst) = *(dst) + (d);})


/* Accessors */

#define AUGUR_VEC_GETI(vec, idx)         (((int*) (vec)->data)[idx])
#define AUGUR_VEC_SETI(vec, idx, i)      ({((int*) (vec)->data)[idx] = i;})

#define AUGUR_VEC_GETD(vec, idx)         (((double*) (vec)->data)[idx])
#define AUGUR_VEC_SETD(vec, idx, d)      ({((double*) (vec)->data)[idx] = d;})

#define AUGUR_VEC_GETV(vec, idx)         (((AugurVec_t*) ((vec)->data)) + idx)
#define AUGUR_VEC_SETV(vec, idx, v)      ({((AugurVec_t*) ((vec)->data))[idx] = *v;})

#define AUGUR_VEC_GETM(vec, idx)         ((AugurMat_t*) (vec)->data + idx)
#define AUGUR_VEC_SETM(vec, idx, m)      ({((AugurMat_t*) (vec)->data)[idx] = *m;})


/* Incrementing */

#define AUGUR_VEC_INCI(vec, idx, i)      (AUGUR_INCI((int*) ((vec)->data) + idx, i))
#define AUGUR_VEC_INCD(vec, idx, d)      (AUGUR_INCD((double*) ((vec)->data) + idx, d))

#define AUGUR_VEC_ATMINCI(vec, idx, i)   (AUGUR_ATMINCI((int*) ((vec)->data) + idx, i))
#define AUGUR_VEC_ATMINCD(vec, idx, d)   (AUGUR_ATMINCD((double*) ((vec)->data) + idx, d))

#define AUGUR_VEC_ATMMINCI(vec, idx, i)  (AUGUR_ATMMINCI((int*) ((vec)->data) + idx, i))
#define AUGUR_VEC_ATMMINCD(vec, idx, d)  (AUGUR_ATMMINCD((double*) ((vec)->data) + idx, d))
  
#define AUGUR_VEC_MINCI(vec, idx, i)     (AUGUR_MINCI((int*) ((vec)->data) + idx, i))
#define AUGUR_VEC_MINCD(vec, idx, d)     (AUGUR_MINCD((double*) ((vec)->data) + idx, d))


/* Metadata operations */

#define AUGUR_VEC_ELEMS(vec)             ((vec)->elems)

AugurTyp_t h_augur_vec_base_ty(AugurVec_t* vec);
uint_t h_augur_vec_deep_elems(AugurVec_t* vec);
uint_t h_augur_vec_maxdim(AugurVec_t* vec, uint_t dim);


/* Allocation */

AugurVec_t h_augur_idx_setup(AugurMemLoc_t loc, uint_t elems);
void h_augur_vec_host_base_alloc(AugurVec_t* vec, void* data, AugurTyp_t ty, uint_t elems);
void h_augur_vec_host_alloc(AugurVec_t* vec, AugurTyp_t ty, uint_t elems, AugurAllocMode_t mode);
void h_augur_vec_host_cpy(AugurVec_t* dst, AugurVec_t* src, AugurTyp_t base_ty, AugurAllocMode_t mode);
void h_augur_vec_host_free(AugurVec_t* vec);

// void h_augur_flat_vec_host_cpy(AugurVec_t* dst, AugurVec_t* src);

// AugurVec_t h_augur_vec_stk_alloc(AugurMemLoc_t loc, AugurTyp_t ty, uint_t elems);
// AugurVec_t h_augur_vec_stk_alloc2(AugurMemLoc_t loc, AugurTyp_t ty, uint_t elems, void* h_data);
// void h_augur_vec_stk_free(AugurMemLoc_t loc, AugurVec_t* vec);


/* Initialization */

void h_augur_vec_zero(AugurMemLoc_t loc, AugurVec_t* vec);
// void h_augur_vec_cpy_data(AugurVec_t* dst, void* src, AugurMemDir_t dir);

void h_augur_flat_vec_zero(AugurMemLoc_t loc, AugurFlatVec_t* fvec);
void h_augur_flat_vec_cpy_data(AugurMemLoc_t loc, AugurFlatVec_t* dst, AugurFlatVec_t* src);


/* Views */

__HOSTDEV__ AugurVec_t augur_vec_view_as(AugurVec_t* src, AugurVec_t* shp);
__HOSTDEV__ AugurVec_t augur_vec_pll_view_as(AugurVec_t* src, AugurVec_t* shp, uint_t idx);
__HOSTDEV__ AugurVec_t augur_arr_view_as_vec(AugurTyp_t ty, void* data, uint_t elems);


/* Printing */

void h_augur_vec_dump(AugurMemLoc_t loc, AugurVec_t* vec);
void h_augur_flat_vec_dump(AugurMemLoc_t loc, AugurFlatVec_t* fvec);


/* Base vector operations */

__HOSTDEV__ double augur_vec_dot(AugurVec_t* v1, AugurVec_t* v2);
__HOSTDEV__ void augur_vec_scale(double a, AugurVec_t* v);
__HOSTDEV__ void augur_vec_plus(AugurVec_t* dst, AugurVec_t* v1, AugurVec_t* v2);
__HOSTDEV__ void augur_vec_minus(AugurVec_t* dst, AugurVec_t* v1, AugurVec_t* v2);
__HOSTDEV__ void augur_vec_times(AugurVec_t* dst, AugurVec_t* v1, AugurVec_t* v2);
// __HOSTDEV__ void augur_vec_div(AugurVec_t* dst, AugurVec_t* v1, AugurVec_t* v2);

__HOSTORDEV__ void augur_vec_atm_plus(AugurVec_t* dst, AugurVec_t* vec);
__HOSTDEV__ void augur_vec_cpy(AugurVec_t* dst, AugurVec_t* src);

/* Host vector operations */

void h_augur_vec_plus(AugurVec_t* dst, AugurVec_t* v1, AugurVec_t* v2);
void h_augur_flat_vec_plus(AugurFlatVec_t* dst, AugurFlatVec_t* v1, AugurFlatVec_t* v2);
void h_augur_flat_vec_basis_add(AugurMemLoc_t loc, AugurFlatVec_t* fvec, int idx, double v);
void h_augur_flat_vec_getd_idx(AugurMemLoc_t loc, double* dst, AugurFlatVec_t* fvec, int idx);

/* Transfer operations */

void hi_augur_vec_to_native(AugurMemLoc_t loc, AugurVec_t* dst, char** dst_data, AugurVec_t* src, Bool_t f_cpy);
void h_augur_vec_to_native(AugurMemLoc_t loc, AugurFlatVec_t* dst, AugurVec_t* src, Bool_t f_cpy);

void hi_augur_vec_from_native(AugurMemLoc_t loc, AugurVec_t* dst, char** src_data);
void h_augur_vec_from_native(AugurMemLoc_t loc, AugurVec_t* dst, AugurFlatVec_t* src);

#endif
