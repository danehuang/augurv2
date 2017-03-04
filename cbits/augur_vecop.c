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

#include "augur_hdr.h"
#include "augur_util.h"
#include "augur_rtval.h"
#include "augur_vecop.h"
#include "augur_matop.h"


/* Metadata operations */


/**
 * Computes the base type of a vector (assumes all vectors have same base type)
 */
AugurTyp_t h_augur_vec_base_ty(AugurVec_t* vec) {
  switch(vec->ty) {
  case AUGUR_INT:
  case AUGUR_DBL: {
    return vec->ty;
  }
  case AUGUR_VEC: {
    return h_augur_vec_base_ty((AugurVec_t*) vec->data);
  }
  case AUGUR_MAT: {
    return ((AugurMat_t*) vec->data)->ty;
  }
  default: {
    // TODO: ERROR
    return AUGUR_INT;
  }
  }
}

/**
 * Deep traversal of vector to compute the total number of elements.
 */
uint_t h_augur_vec_deep_elems(AugurVec_t* vec) {
  switch (vec->ty) {
  case AUGUR_INT:
  case AUGUR_DBL: {
    return vec->elems;
  }
  case AUGUR_VEC: {
    uint_t elems = 0;
    for (uint_t i = 0; i < vec->elems; i++) {
      elems += h_augur_vec_deep_elems(AUGUR_VEC_GETV(vec, i));
    }
    return elems;
  }
  case AUGUR_MAT: {
    uint_t elems = 0;
    for (uint_t i = 0; i < vec->elems; i++) {
      elems += h_augur_mat_elems(AUGUR_VEC_GETM(vec, i));
    }
    return elems;
  }
  default: {
    // TODO: ERROR
    return 0;
  }
  }
}

/**
 * Deep traversal of vector to compute dimenion dim's maximal length.
 *
 * vec: input vector
 * dim: dimension at which we want the maximal length
 */
uint_t h_augur_vec_maxdim(AugurVec_t* vec, uint_t dim) {
  if (dim == 0) {
    return vec->elems;
  }
  else {
    switch (vec->ty) {      
    case AUGUR_VEC: {
      uint_t maxdim = 0;
      for (uint_t i = 0; i < vec->elems; i++) {
	maxdim = augur_max(maxdim, h_augur_vec_maxdim(AUGUR_VEC_GETV(vec, i), dim-1));
      }
      return maxdim;
    }
    case AUGUR_MAT: {
      uint_t maxdim = 0;
      for (uint_t i = 0; i < vec->elems; i++) {
	maxdim = augur_max(maxdim, h_augur_mat_dim(AUGUR_VEC_GETM(vec, i), dim-1));
      }
      return maxdim;
    }
    default: {
      // TODO: ERROR
      return 0;
    }
    }
  }
}


/* Allocation */

/**
 * Helper function for allocating data
 *
 * ty: the type that the vector holds
 * elems: length of vector
 * mode: AUGUR_DATA allocates data, AUGUR_SKEL does not allocate data
 */
void hi_augur_vec_host_alloc_data(AugurVec_t* vec, AugurTyp_t ty, uint_t elems, AugurAllocMode_t mode) {
  switch (mode) {
  case AUGUR_DATA: {
    uint_t numbytes = ty2size(ty) * elems;
    vec->data = augur_malloc(numbytes, AUGUR_CPU);
    break;
  }
  case AUGUR_SKEL: {
    vec->data = NULL;
    break;
  }
  }
}

void h_augur_vec_host_base_alloc(AugurVec_t* vec, void* data, AugurTyp_t ty, uint_t elems) {
  h_augur_vec_host_alloc(vec, ty, elems, AUGUR_DATA);
  uint_t numbytes = ty2size(ty) * elems;
  augur_memcpy(vec->data, data, numbytes, AUGUR_H2H);
}

/**
 * Allocates a vector on the (CPU) host.
 *
 * ty: the type that the vector holds
 * elems: length of vector
 * mode: AUGUR_DATA allocates data, AUGUR_SKEL does not allocate data
 */
void h_augur_vec_host_alloc(AugurVec_t* vec, AugurTyp_t ty, uint_t elems, AugurAllocMode_t mode) {
  vec->ty = ty;
  vec->stride = 0;      // TODO??
  vec->elems = elems;
  hi_augur_vec_host_alloc_data(vec, ty, elems, mode);
}

/**
 * Copies a vector on the (CPU) host.
 */
void h_augur_vec_host_cpy(AugurVec_t* dst, AugurVec_t* src, AugurTyp_t base_ty, AugurAllocMode_t mode) {
  dst->stride = src->stride;
  dst->elems = src->elems;
  switch (src->ty) {
  case AUGUR_INT:
  case AUGUR_DBL: {
    dst->ty = base_ty;
    hi_augur_vec_host_alloc_data(dst, base_ty, src->elems, mode);
    // Copy if mode is AUGUR_DATA
    switch (mode) {
    case AUGUR_DATA: {
      uint_t numbytes = ty2size(base_ty) * src->elems;
      if (src->ty == base_ty) {
	augur_memcpy(dst->data, src->data, numbytes, AUGUR_H2H);
      }
      else {
	// TODO: ERROR
      }
      break;
    }
    case AUGUR_SKEL: {
      break;
    }
    }
    break;
  }
  case AUGUR_VEC: {
    dst->ty = src->ty;
    uint_t numbytes = ty2size(src->ty) * src->elems;
    dst->data = augur_malloc(numbytes, AUGUR_CPU);
    for (uint_t i = 0; i < src->elems; i++) {
      h_augur_vec_host_cpy(AUGUR_VEC_GETV(dst, i), AUGUR_VEC_GETV(src, i), base_ty, mode);
    }					     
    break;
  }
  case AUGUR_MAT: {
    dst->ty = src->ty;
    uint_t numbytes = ty2size(src->ty) * src->elems;
    dst->data = augur_malloc(numbytes, AUGUR_CPU);
    for (uint_t i = 0; i < src->elems; i++) {
      h_augur_mat_host_cpy(AUGUR_VEC_GETM(dst, i), AUGUR_VEC_GETM(src, i), base_ty, mode);
    }
    break;
  }
  }
}

/**
 * Helper to recursively free a vector on the (CPU) host.
 */
void hi_augur_vec_host_free(AugurVec_t* vec) {
  switch (vec->ty) {
  case AUGUR_INT:
  case AUGUR_DBL: {
    if (vec->data) {
      augur_free(vec->data, AUGUR_CPU);
    }
    break;
  }
  case AUGUR_VEC: {
    for (uint_t i = 0; i < vec->elems; i++) {
      hi_augur_vec_host_free(AUGUR_VEC_GETV(vec, i));
    }
    augur_free(vec->data, AUGUR_CPU);
    break;
  }
  case AUGUR_MAT: {
    for (uint_t i = 0; i < vec->elems; i++) {
      h_augur_mat_host_free(AUGUR_VEC_GETM(vec, i));
    }
    augur_free(vec->data, AUGUR_CPU);
    break;
  }
  }
}

/**
 * Recursively free a vector on the (CPU) host.
 */
void h_augur_vec_host_free(AugurVec_t* vec) {
  hi_augur_vec_host_free(vec);
  // augur_free(vec, AUGUR_CPU);
}


/*
void h_augur_flat_vec_host_cpy(AugurMemLoc_t loc, AugurFlatVec_t* dst, AugurFlatVec_t* src) {
  AugurFlatVec_t* fvec = (AugurFlatVec_t*) augur_malloc(sizeof(AugurFlatVec_t), loc);
  h_augur_vec_to_native(loc, &(dst->vec), &(src->vec), TRUE);
  dst->vec = src->vec;
}
*/

/* Initialization */

void h_augur_vec_zero(AugurMemLoc_t loc, AugurVec_t* vec) {
  switch (vec->ty) {
  case AUGUR_INT:
  case AUGUR_DBL: {
    augur_memset(vec->data, 0, ty2size(vec->ty) * vec->elems, loc);
    break;
  }
  case AUGUR_VEC: {
    for (uint_t i = 0; i < vec->elems; i++) {
      h_augur_vec_zero(loc, AUGUR_VEC_GETV(vec, i));
    }
    break;
  }
  default:
    // TODO: ERROR
    break;
  }
}

void h_augur_flat_vec_zero(AugurMemLoc_t loc, AugurFlatVec_t* fvec) {
  switch (fvec->base_ty) {
  case AUGUR_INT:
  case AUGUR_DBL: {
    augur_memset(fvec->base_data, 0, ty2size(fvec->base_ty) * fvec->base_elems, loc);
    break;
  }
  default: {
    // TODO: ERROR
    break;
  }
  }
}

void  h_augur_flat_vec_cpy_data(AugurMemLoc_t loc, AugurFlatVec_t* dst, AugurFlatVec_t* src) {
  uint_t numbytes = ty2size(dst->base_ty) * dst->base_elems;
  switch (loc) {
  case AUGUR_CPU: {
    augur_memcpy(dst->base_data, src->base_data, numbytes, AUGUR_H2H);
    break;
  }
#ifndef AUGURCPU
  case AUGUR_GPU: {
    augur_memcpy(dst->base_data, src->base_data, numbytes, AUGUR_D2D);
    break;
  }
#endif
  }
}

/*
void h_augur_vec_cpy_data(AugurVec_t* dst, void* src, AugurMemDir_t dir) {
  uint_t numbytes = ty2size(dst->ty) * dst->elems;
  augur_memcpy(dst->data, src, numbytes, dir);
}
*/

/* Views */

AugurVec_t augur_vec_view_as(AugurVec_t* src, AugurVec_t* shp) {
  AugurVec_t view;

  view.ty = shp->ty;
  view.stride = shp->stride;
  view.elems = shp->elems;
  view.data = src->data;
  
  return view;
}

AugurVec_t augur_arr_view_as_vec(AugurTyp_t ty, void* data, uint_t elems) {
  AugurVec_t view;

  view.ty = ty;
  view.stride = 0; // TODO
  view.elems = elems;
  view.data = data;

  return view;
}


/* Printing */

void hi_augur_vec_dump(int tab, AugurMemLoc_t loc, AugurVec_t* vec) {
  if (!vec->data) {
    augur_tab(tab);
    printf("base[len=%d,ty=%s]: unallocated\n", vec->elems, ty2str(vec->ty));
    return;
  }
  
  switch (vec->ty) {
  case AUGUR_INT: {
    augur_tab(tab);
    printf("base[len=%d,ty=%s]: ", vec->elems, ty2str(vec->ty));
    augur_arr_dumpi(vec->data, vec->elems, loc);
    break;
  }
  case AUGUR_DBL: {
    augur_tab(tab);
    printf("base[len=%d,ty=%s]: ", vec->elems, ty2str(vec->ty));
    augur_arr_dumpd(vec->data, vec->elems, loc);
    break;
  }
  case AUGUR_VEC: {
    switch (loc) {
    case AUGUR_CPU: {
      for (uint_t i = 0; i < vec->elems; i++) {
	augur_tab(tab);
	printf("vec[idx=%d]\n", i);
	hi_augur_vec_dump(tab+2, loc, AUGUR_VEC_GETV(vec, i));
      }
      break;
    }
#ifndef AUGURCPU
    case AUGUR_GPU: {
      for (uint_t i = 0; i < vec->elems; i++) {
	AugurVec_t tmp;
	augur_memcpy(&tmp, AUGUR_VEC_GETV(vec, i), AUGUR_D2H);
	augur_tab(tab);
	printf("vec[idx=%d]\n", i);
	hi_augur_vec_dump(tab+2, &tmp, loc);
      }
      break;
    }
#endif
    }
    break;
  }
  case AUGUR_MAT: {
    switch (loc) {
    case AUGUR_CPU: {
      for (uint_t i = 0; i < vec->elems; i++) {
	augur_tab(tab);
	printf("mat[idx=%d]\n", i);
	hi_augur_mat_dump(tab+2, loc, AUGUR_VEC_GETM(vec, i));
      }
      break;
    }
#ifndef AUGURCPU
    case AUGUR_GPU: {
      for (uint_t i = 0; i < vec->elems; i++) {
	AugurMat_t tmp;
	augur_memcpy(&tmp, AUGUR_VEC_GETM(vec, i), AUGUR_D2H);
	augur_tab(tab);
	printf("mat[idx=%d]\n", i);
	hi_augur_mat_dump(tab+2, loc, &tmp);
      }
      break;
    }
#endif
    }
    break;
  }
  }
}

void h_augur_vec_dump(AugurMemLoc_t loc, AugurVec_t* vec) {
  printf("elems: %d\n", vec->elems);
  hi_augur_vec_dump(0, loc, vec);
}

void h_augur_flat_vec_dump(AugurMemLoc_t loc, AugurFlatVec_t* fvec) {
  printf("base typ: %s, base elems: %d\n", ty2str(fvec->base_ty), fvec->base_elems);
  switch (fvec->base_ty) {
  case AUGUR_INT: {
    augur_arr_dumpi(fvec->base_data, fvec->base_elems, loc);
    break;
  }
  case AUGUR_DBL: {
    augur_arr_dumpd(fvec->base_data, fvec->base_elems, loc);
    break;
  }
  default: {
    // TODO: ERROR
    break;
  }
  }
}


/* Base vector operations */

__HOSTDEV__ double augur_vec_dot(AugurVec_t* v1, AugurVec_t* v2) {
  if (v1->ty == AUGUR_DBL && v2->ty == AUGUR_DBL) {
    double acc = 0.0;
    double* p1 = (double*) v1->data;
    double* p2 = (double*) v2->data;
    for (uint_t i = 0; i < v1->elems; i++) {
      acc += p1[i] * p2[i];
    }
    return acc;
  }
  else if (v1->ty == AUGUR_INT && v2->ty == AUGUR_INT) {
    double acc = 0.0;
    int* p1 = (int*) v1->data;
    int* p2 = (int*) v2->data;
    for (uint_t i = 0; i < v1->elems; i++) {
      acc += p1[i] * p2[i];
    }
    return acc;
  }
  else if (v1->ty == AUGUR_INT && v2->ty == AUGUR_DBL) {
    double acc = 0.0;
    int* p1 = (int*) v1->data;
    double* p2 = (double*) v2->data;
    for (uint_t i = 0; i < v1->elems; i++) {
      acc += p1[i] * p2[i];
    }
    return acc;
  }
  else if (v1->ty == AUGUR_DBL && v2->ty == AUGUR_INT) {
    double acc = 0.0;
    double* p1 = (double*) v1->data;
    int* p2 = (int*) v2->data;
    for (uint_t i = 0; i < v1->elems; i++) {
      acc += p1[i] * p2[i];
    }
    return acc;
  }
  return 0.0;
}

__HOSTDEV__ void augur_vec_scale(double a, AugurVec_t* v) {
  for (int i = 0; i < v->elems; i++) {
    AUGUR_VEC_SETD(v, i, a * AUGUR_VEC_GETD(v, i));
  }
}

__HOSTDEV__ void augur_vec_plus(AugurVec_t* dst, AugurVec_t* v1, AugurVec_t* v2) {
  if (dst->ty == AUGUR_DBL) {
    if (v1->ty == AUGUR_DBL && v2->ty == AUGUR_DBL) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETD(dst, i, AUGUR_VEC_GETD(v1, i) + AUGUR_VEC_GETD(v2, i));
      }
    }
    else if (v1->ty == AUGUR_INT && v2->ty == AUGUR_INT) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETD(dst, i, AUGUR_VEC_GETI(v1, i) + AUGUR_VEC_GETI(v2, i));
      }
    }
    else if (v1->ty == AUGUR_INT && v2->ty == AUGUR_DBL) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETD(dst, i, AUGUR_VEC_GETI(v1, i) + AUGUR_VEC_GETD(v2, i));
      }
    }
    else if (v1->ty == AUGUR_DBL && v2->ty == AUGUR_INT) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETD(dst, i, AUGUR_VEC_GETD(v1, i) + AUGUR_VEC_GETI(v2, i));
      }
    }
  }
  else if (dst->ty == AUGUR_INT) {
    if (v1->ty == AUGUR_DBL && v2->ty == AUGUR_DBL) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETI(dst, i, AUGUR_VEC_GETD(v1, i) + AUGUR_VEC_GETD(v2, i));
      }
    }
    else if (v1->ty == AUGUR_INT && v2->ty == AUGUR_INT) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETI(dst, i, AUGUR_VEC_GETI(v1, i) + AUGUR_VEC_GETI(v2, i));
      }
    }
    else if (v1->ty == AUGUR_INT && v2->ty == AUGUR_DBL) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETI(dst, i, AUGUR_VEC_GETI(v1, i) + AUGUR_VEC_GETD(v2, i));
      }
    }
    else if (v1->ty == AUGUR_DBL && v2->ty == AUGUR_INT) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETI(dst, i, AUGUR_VEC_GETD(v1, i) + AUGUR_VEC_GETI(v2, i));
      }
    }
  }
}

__HOSTDEV__ void augur_vec_minus(AugurVec_t* dst, AugurVec_t* v1, AugurVec_t* v2) {
  if (dst->ty == AUGUR_DBL) {
    if (v1->ty == AUGUR_DBL && v2->ty == AUGUR_DBL) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETD(dst, i, AUGUR_VEC_GETD(v1, i) - AUGUR_VEC_GETD(v2, i));
      }
    }
    else if (v1->ty == AUGUR_INT && v2->ty == AUGUR_INT) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETD(dst, i, AUGUR_VEC_GETI(v1, i) - AUGUR_VEC_GETI(v2, i));
      }
    }
    else if (v1->ty == AUGUR_INT && v2->ty == AUGUR_DBL) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETD(dst, i, AUGUR_VEC_GETI(v1, i) - AUGUR_VEC_GETD(v2, i));
      }
    }
    else if (v1->ty == AUGUR_DBL && v2->ty == AUGUR_INT) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETD(dst, i, AUGUR_VEC_GETD(v1, i) - AUGUR_VEC_GETI(v2, i));
      }
    }
  }
  else if (dst->ty == AUGUR_INT) {
    if (v1->ty == AUGUR_DBL && v2->ty == AUGUR_DBL) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETI(dst, i, AUGUR_VEC_GETD(v1, i) - AUGUR_VEC_GETD(v2, i));
      }
    }
    else if (v1->ty == AUGUR_INT && v2->ty == AUGUR_INT) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETI(dst, i, AUGUR_VEC_GETI(v1, i) - AUGUR_VEC_GETI(v2, i));
      }
    }
    else if (v1->ty == AUGUR_INT && v2->ty == AUGUR_DBL) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETI(dst, i, AUGUR_VEC_GETI(v1, i) - AUGUR_VEC_GETD(v2, i));
      }
    }
    else if (v1->ty == AUGUR_DBL && v2->ty == AUGUR_INT) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETI(dst, i, AUGUR_VEC_GETD(v1, i) - AUGUR_VEC_GETI(v2, i));
      }
    }
  }
}

__HOSTDEV__ void augur_vec_times(AugurVec_t* dst, AugurVec_t* v1, AugurVec_t* v2) {
  if (dst->ty == AUGUR_DBL) {
    if (v1->ty == AUGUR_DBL && v2->ty == AUGUR_DBL) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETD(dst, i, AUGUR_VEC_GETD(v1, i) * AUGUR_VEC_GETD(v2, i));
      }
    }
    else if (v1->ty == AUGUR_INT && v2->ty == AUGUR_INT) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETD(dst, i, AUGUR_VEC_GETI(v1, i) * AUGUR_VEC_GETI(v2, i));
      }
    }
    else if (v1->ty == AUGUR_INT && v2->ty == AUGUR_DBL) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETD(dst, i, AUGUR_VEC_GETI(v1, i) * AUGUR_VEC_GETD(v2, i));
      }
    }
    else if (v1->ty == AUGUR_DBL && v2->ty == AUGUR_INT) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETD(dst, i, AUGUR_VEC_GETD(v1, i) * AUGUR_VEC_GETI(v2, i));
      }
    }
  }
  else if (dst->ty == AUGUR_INT) {
    if (v1->ty == AUGUR_DBL && v2->ty == AUGUR_DBL) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETI(dst, i, AUGUR_VEC_GETD(v1, i) * AUGUR_VEC_GETD(v2, i));
      }
    }
    else if (v1->ty == AUGUR_INT && v2->ty == AUGUR_INT) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETI(dst, i, AUGUR_VEC_GETI(v1, i) * AUGUR_VEC_GETI(v2, i));
      }
    }
    else if (v1->ty == AUGUR_INT && v2->ty == AUGUR_DBL) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETI(dst, i, AUGUR_VEC_GETI(v1, i) * AUGUR_VEC_GETD(v2, i));
      }
    }
    else if (v1->ty == AUGUR_DBL && v2->ty == AUGUR_INT) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETI(dst, i, AUGUR_VEC_GETD(v1, i) * AUGUR_VEC_GETI(v2, i));
      }
    }
  }
}

__HOSTDEV__ void augur_vec_div(AugurVec_t* dst, AugurVec_t* v1, AugurVec_t* v2) {
  if (dst->ty == AUGUR_DBL) {
    if (v1->ty == AUGUR_DBL && v2->ty == AUGUR_DBL) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETD(dst, i, AUGUR_VEC_GETD(v1, i) / AUGUR_VEC_GETD(v2, i));
      }
    }
    else if (v1->ty == AUGUR_INT && v2->ty == AUGUR_INT) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETD(dst, i, AUGUR_VEC_GETI(v1, i) / AUGUR_VEC_GETI(v2, i));
      }
    }
    else if (v1->ty == AUGUR_INT && v2->ty == AUGUR_DBL) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETD(dst, i, AUGUR_VEC_GETI(v1, i) / AUGUR_VEC_GETD(v2, i));
      }
    }
    else if (v1->ty == AUGUR_DBL && v2->ty == AUGUR_INT) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETD(dst, i, AUGUR_VEC_GETD(v1, i) / AUGUR_VEC_GETI(v2, i));
      }
    }
  }
  else if (dst->ty == AUGUR_INT) {
    if (v1->ty == AUGUR_DBL && v2->ty == AUGUR_DBL) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETI(dst, i, AUGUR_VEC_GETD(v1, i) / AUGUR_VEC_GETD(v2, i));
      }
    }
    else if (v1->ty == AUGUR_INT && v2->ty == AUGUR_INT) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETI(dst, i, AUGUR_VEC_GETI(v1, i) / AUGUR_VEC_GETI(v2, i));
      }
    }
    else if (v1->ty == AUGUR_INT && v2->ty == AUGUR_DBL) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETI(dst, i, AUGUR_VEC_GETI(v1, i) / AUGUR_VEC_GETD(v2, i));
      }
    }
    else if (v1->ty == AUGUR_DBL && v2->ty == AUGUR_INT) {
      for (uint_t i = 0; i < dst->elems; i++) {
	AUGUR_VEC_SETI(dst, i, AUGUR_VEC_GETD(v1, i) / AUGUR_VEC_GETI(v2, i));
      }
    }
  }
}

__HOSTORDEV__ void augur_vec_atm_plus(AugurVec_t* dst, AugurVec_t* vec) {
  double* p_data = (double*) dst->data;
  for (uint_t i = 0; i < dst->elems; i++) {
    AUGUR_ATMINCD(p_data + i, AUGUR_VEC_GETD(vec, i));
  }
}

__HOSTDEV__ void augur_vec_cpy(AugurVec_t* dst, AugurVec_t* src) {
  switch (dst->ty) {
  case AUGUR_INT: {
    for (uint_t i = 0; i < dst->elems; i++) {
      AUGUR_VEC_SETI(dst, i, AUGUR_VEC_GETI(src, i));
    }
    break;
  }
  case AUGUR_DBL: {
    for (uint_t i = 0; i < dst->elems; i++) {
      AUGUR_VEC_SETD(dst, i, AUGUR_VEC_GETD(src, i));
    }
    break;
  }
  case AUGUR_VEC: {
    for (uint_t i = 0; i < dst->elems; i++) {
      AUGUR_VEC_SETV(dst, i, AUGUR_VEC_GETV(src, i));
    }
    break;
  }
  case AUGUR_MAT: {
    for (uint_t i = 0; i < dst->elems; i++) {
      AUGUR_VEC_SETM(dst, i, AUGUR_VEC_GETM(src, i));
    }
    break;
  }
  }
}


/* Host vector operations */

void h_augur_vec_plus(AugurVec_t* dst, AugurVec_t* v1, AugurVec_t* v2) {
  for (uint_t i = 0; i < dst->elems; i++) {
    double sum = AUGUR_VEC_GETD(v1, i) + AUGUR_VEC_GETD(v2, i);
    AUGUR_VEC_SETD(dst, i, sum);
  }
}

void h_augur_flat_vec_plus(AugurFlatVec_t* dst, AugurFlatVec_t* fv1, AugurFlatVec_t* fv2) {
  switch (dst->base_ty) {
  case AUGUR_DBL: {
#ifdef AUGURCPU
    double* dst_data = (double*) dst->base_data;
    double* fv1_data = (double*) fv1->base_data;
    double* fv2_data = (double*) fv2->base_data;
    for (uint_t i = 0; i < dst->base_elems; i++) {
      dst_data[i] = fv1_data[i] + fv2_data[i];
    }
#else
    // TODO: IMPLEMENT ME
#endif
    break;
  }
  default: {
    // TODO: ERROR
    break;
  }
  }
}

void h_augur_flat_vec_getd_idx(AugurMemLoc_t loc, double* dst, AugurFlatVec_t* fvec, int idx) {
  switch (loc) {
  case AUGUR_CPU: {
    augur_memcpy(dst, (double*) fvec->base_data + idx, sizeof(double), AUGUR_H2H);
    break;
  }
#ifndef AUGURCPU
  case AUGUR_GPU: {
    augur_memcpy(dst, (double*) fvec->base_data + idx, sizeof(double), AUGUR_D2H);
    break;
  }
#endif
  }
}

void h_augur_flat_vec_setd_idx(AugurMemLoc_t loc, AugurFlatVec_t* fvec, int idx,  double* src) {
  switch (loc) {
  case AUGUR_CPU: {
    augur_memcpy((double*) fvec->base_data + idx, src, sizeof(double), AUGUR_H2H);
    break;
  }
#ifndef AUGURCPU
  case AUGUR_GPU: {
    augur_memcpy((double*) fvec->base_data + idx, src, sizeof(double), AUGUR_H2D);
    break;
  }
#endif
  }
}

void h_augur_flat_vec_basis_add(AugurMemLoc_t loc, AugurFlatVec_t* fvec, int idx, double v) {
  switch (fvec->base_ty) {
  case AUGUR_DBL: {
    double tmp;
    h_augur_flat_vec_getd_idx(loc, &tmp, fvec, idx);
    tmp += v;
    h_augur_flat_vec_setd_idx(loc, fvec, idx, &tmp);
    break;
  }
  default: {
    // TODO: ERROR 
    break;
  }
  }
}

/* Transfer operations */

/**
 * Allocates dst and copies data from src to dst. 
 *
 * dst: at host, underlying data is dst_data
 * dst_data: pointer to data at loc
 * src: at host
 */
void hi_augur_vec_to_native(AugurMemLoc_t loc, AugurVec_t* dst, char** dst_data, AugurVec_t* src, Bool_t f_cpy) {
  switch (src->ty) {
  case AUGUR_INT: 
  case AUGUR_DBL: {
    uint_t numbytes = ty2size(src->ty) * src->elems;
    if (f_cpy) {
      switch (loc) {
      case AUGUR_CPU: {
	augur_memcpy(*dst_data, src->data, numbytes, AUGUR_H2H);
	break;
      }
#ifndef AUGURCPU
      case AUGUR_GPU: {
	augur_memcpy(*dst_data, src->data, numbytes, AUGUR_H2D);
	break;
      }
#endif
      }
    }
    else {
      augur_memset(*dst_data, 0, numbytes, loc);
    }
    dst->ty = src->ty;
    dst->elems = src->elems;
    dst->data = *dst_data;
    *dst_data += numbytes;
    break;
  }
  case AUGUR_VEC: {
    uint_t numbytes = sizeof(AugurVec_t) * src->elems;
    AugurVec_t* h_vecs = (AugurVec_t*) augur_malloc(numbytes, AUGUR_CPU);
    for (uint_t i = 0; i < src->elems; i++) {
      hi_augur_vec_to_native(loc, h_vecs + i, dst_data, AUGUR_VEC_GETV(src, i), f_cpy);
    }
    dst->ty = src->ty;
    dst->elems = src->elems;
    switch (loc) {
    case AUGUR_CPU: {
      dst->data = h_vecs;
      break;
    }
#ifndef AUGURCPU
    case AUGUR_GPU: {
      AugurVec_t* d_vecs = augur_malloc(numbytes, AUGUR_GPU);
      augur_memcpy(d_vecs, h_vecs, numbytes, AUGUR_H2D);
      dst->data = d_vecs;
      augur_free(h_vecs, AUGUR_CPU);      
      break;
    }
#endif
    }
    break;
  }
  case AUGUR_MAT: {
    uint_t numbytes = sizeof(AugurMat_t) * src->elems;
    AugurMat_t* h_mats = (AugurMat_t*) augur_malloc(numbytes, AUGUR_CPU);
    for (uint_t i = 0; i < src->elems; i++) {
      AugurMat_t* mat_src = AUGUR_VEC_GETM(src, i);
      hi_augur_mat_to_native(loc, h_mats + i, dst_data, mat_src, f_cpy);
    }
    dst->ty = src->ty;
    dst->elems = src->elems;
    switch (loc) {
    case AUGUR_CPU: {
      dst->data = h_mats;
      break;
    }
#ifndef AUGURCPU
    case AUGUR_GPU: {
      AugurMat_t* d_mats = augur_malloc(numbytes, AUGUR_GPU);
      augur_memcpy(d_mats, h_vecs, numbytes, AUGUR_H2D);
      dst->data = d_mats;
      augur_free(h_mats, AUGUR_CPU);
      break;
    }
#endif
    }
    break;
  }
  }
}

/**
 * Allocates dst and copies data from src to dst.
 *
 * dst: at host, underlying data at loc
 * src: at host
 */
void h_augur_vec_to_native(AugurMemLoc_t loc, AugurFlatVec_t* dst, AugurVec_t* src, Bool_t f_cpy) {
  AugurTyp_t ty = h_augur_vec_base_ty(src);
  uint_t base_elems = h_augur_vec_deep_elems(src);

  uint_t numbytes = ty2size(ty) * base_elems;
  char* dst_data = (char*) augur_malloc(numbytes, loc);
  
  dst->base_ty = ty;
  dst->base_elems = base_elems;
  dst->base_data = dst_data;
  hi_augur_vec_to_native(loc, &(dst->vec), &dst_data, src, f_cpy);
}

/**
 * Copies data from src_data to dst.
 * 
 * dst: at host, underlying data at host
 * src_data: at loc
 */
void hi_augur_vec_from_native(AugurMemLoc_t loc, AugurVec_t* dst, char** src_data) {
  switch (dst->ty) {
  case AUGUR_INT:
  case AUGUR_DBL: {
    uint_t numbytes = ty2size(dst->ty) * dst->elems;
    switch (loc) {
    case AUGUR_CPU: {
      augur_memcpy(dst->data, *src_data, numbytes, AUGUR_H2H);
      break;
    }
#ifndef AUGURCPU
    case AUGUR_GPU: {
      augur_memcpy(dst->data, *src_data, numbytes, AUGUR_H2D);
      break;
    }
#endif
    }
    *src_data += numbytes;
    break;
  }
  case AUGUR_VEC: {
    for (uint_t i = 0; i < dst->elems; i++) {
      hi_augur_vec_from_native(loc, AUGUR_VEC_GETV(dst, i), src_data);
    }
    break;
  }
  case AUGUR_MAT: {
    for (uint_t i = 0; i < dst->elems; i++) {
      hi_augur_mat_from_native(loc, AUGUR_VEC_GETM(dst, i), src_data);
    }
    break;
  }
  }
}

/**
 * Copies data from src to dst.
 * 
 * dst: at host, underlying data at host
 * src_data: at loc
 */
void h_augur_vec_from_native(AugurMemLoc_t loc, AugurVec_t* dst, AugurFlatVec_t* src) {
  // h_augur_flat_vec_dump(AUGUR_CPU, src);
  // h_augur_vec_dump(AUGUR_CPU, &(src->vec));
  char* cpy = (char*) src->base_data;
  hi_augur_vec_from_native(loc, dst, &cpy);
}




 /*
AugurVec_t h_augur_vec_stk_alloc(AugurMemLoc_t loc, AugurTyp_t ty, uint_t elems) {
  AugurVec_t vec;
  vec.ty = ty;
  vec.stride = 0; // TODO??
  vec.elems = elems;
  vec.data = augur_malloc(ty2size(ty) * elems, loc);
  return vec;
}

AugurVec_t h_augur_vec_stk_alloc2(AugurMemLoc_t loc, AugurTyp_t ty, uint_t elems, void* h_data) {
  AugurVec_t vec = h_augur_vec_stk_alloc(loc, ty, elems);
  uint_t numbytes = ty2size(ty) * elems;
  switch (loc) {
  case AUGUR_CPU: {
    augur_memcpy(vec.data, h_data, numbytes, AUGUR_H2H);
    break;
  }
#ifndef AUGURCPU
  case AUGUR_GPU: {
    augur_memcpy(vec.data, h_data, numbytes, AUGUR_H2D);
    break;
  }
#endif
  }
  return vec;
}

void h_augur_vec_stk_free(AugurMemLoc_t loc, AugurVec_t* vec) {
  switch (vec->ty) {
  case AUGUR_INT:
  case AUGUR_DBL: {
    if (!vec->data) {
      augur_free(vec->data, loc);
    }
    break;
  }
  case AUGUR_VEC: {
    switch (loc) {
    case AUGUR_CPU: {
      for (uint_t i = 0; i < vec->elems; i++) {
	h_augur_vec_stk_free(loc, AUGUR_VEC_GETV(vec, i));
      }
      augur_free(vec->data, loc);
      break;
    }
#ifndef AUGURCPU
    case AUGUR_GPU: {
      for (uint_t i = 0; i < vec->elems; i++) {
	AugurVec_t tmp;
	augur_memcpy(&tmp, AUGUR_VEC_GETV(vec, i), sizeof(AugurVec_t), AUGUR_D2H);
	h_augur_vec_stk_free(&tmp, loc);
      }
      augur_free(vec->data, loc);
      break;
    }
#endif
    }
    break;
  }
  case AUGUR_MAT: {
    switch (loc) {
    case AUGUR_CPU: {
      for (uint_t i = 0; i < vec->elems; i++) {
	// TODO: Should this be a matrix operation? or not to avoid circular ...
	augur_free(AUGUR_VEC_GETM(vec, i)->data, loc);
      }
      augur_free(vec->data, loc);
      break;
    }
#ifndef AUGURCPU
    case AUGUR_GPU: {
      for (uint_t i = 0; i < vec->elems; i++) {
	AugurMat_t mat;
	augur_memcpy(&mat, AUGUR_VEC_GETM(vec, i), sizeof(AugurMat_t), AUGUR_D2H);
	augur_free(mat->data, loc);
      }
      augur_free(vec->data, loc);
      break;
    }
#endif
    }
    break;
  }
  }
}
*/
