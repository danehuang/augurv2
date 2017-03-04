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

#include <math.h>

#include "augur_hdr.h"
#include "augur_util.h"
#include "augur_math.h"
#include "augur_rtval.h"
#include "augur_vecop.h"
#include "augur_matop.h"


/* Accessors */

__HOSTDEV__ AugurVec_t augur_mat_getv(AugurMat_t* mat, uint_t idx) {
  AugurVec_t vec;
  vec.ty = mat->ty;
  vec.stride = 0; // TODO???
  vec.elems = mat->col;
  vec.data = mat->data + (mat->row * idx);
  return vec;
}

__HOSTDEV__ void augur_mat_setv(AugurMat_t* mat, uint_t idx, AugurVec_t* vec) {
  switch (vec->ty) {
  case AUGUR_INT: {
    for (uint_t i = 0; i < vec->elems; i++) {
      AUGUR_MAT_SETI(mat, idx, i, AUGUR_VEC_GETI(vec, i));
    }
    break;
  }
  case AUGUR_DBL: {
    for (uint_t i = 0; i < vec->elems; i++) {
      AUGUR_MAT_SETD(mat, idx, i, AUGUR_VEC_GETD(vec, i));
    }
    break;
  }
  default: {
    // TODO: ERROR
    break;
  }
  }
}

  
/* Metadata operations */

uint_t h_augur_mat_dim(AugurMat_t* mat, uint_t dim) {
  switch (dim) {
  case 0: {
    return mat->row;
  }
  case 1: {
    return mat->col;
  }
  default: {
    // TODO: ERROR
    return 0;
  }
  }
}

uint_t h_augur_mat_elems(AugurMat_t* mat) {
  return mat->row * mat->col;
}

  
/* Allocation */

void hi_augur_mat_host_alloc_data(AugurMat_t* mat, AugurTyp_t ty, uint_t row, uint_t col, AugurAllocMode_t mode) {
  switch (mode) {
  case AUGUR_DATA: {
    uint_t numbytes = ty2size(ty) * row * col;
    mat->data = augur_malloc(numbytes, AUGUR_CPU);
    break;
  }
  case AUGUR_SKEL: {
    mat->data = NULL;
    break;
  }
  }
}

void h_augur_mat_host_base_alloc(AugurMat_t* mat, void* data, AugurTyp_t ty, uint_t row, uint_t col) {
  h_augur_mat_host_alloc(mat, ty, row, col, AUGUR_DATA);
  uint_t numbytes = ty2size(ty) * row * col;
  augur_memcpy(mat->data, data, numbytes, AUGUR_H2H);
}

void h_augur_mat_host_alloc(AugurMat_t* mat, AugurTyp_t ty, uint_t row, uint_t col, AugurAllocMode_t mode) {
  mat->ty = ty;
  mat->row = row;
  mat->col = col;
  hi_augur_mat_host_alloc_data(mat, ty, row, col, mode);
}

void h_augur_mat_host_cpy(AugurMat_t* dst, AugurMat_t* src, AugurTyp_t base_ty, AugurAllocMode_t mode) {
  dst->ty = base_ty;
  dst->row = src->row;
  dst->col = src->col;
  hi_augur_mat_host_alloc_data(dst, base_ty, src->row, src->col, mode);
  switch (mode) {
  case AUGUR_DATA: {
    uint_t numbytes = ty2size(base_ty) * src->row * src->col;
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
}

void h_augur_mat_host_free(AugurMat_t* mat) {
  if (mat->data) {
    free(mat->data);
  }
}


/* Initialization */

void h_augur_mat_zero(AugurMemLoc_t loc, AugurMat_t* mat) {
  switch (mat->ty) {
  case AUGUR_INT:
  case AUGUR_DBL: {
    uint_t numbytes = ty2size(mat->ty) * mat->row * mat->col;
    augur_memset(mat->data, 0, numbytes, loc);
    break;
  }
  default: {
    // TODO: ERROR
    break;
  }
  }
}

void h_augur_mat_cpy_data(AugurMemLoc_t loc, AugurMat_t* dst, AugurMat_t* src) {
  switch (dst->ty) {
  case AUGUR_INT:
  case AUGUR_DBL: {
    uint_t numbytes = ty2size(dst->ty) * dst->row * dst->col;
    switch (loc) {
    case AUGUR_CPU: {
      augur_memcpy(dst->data, src->data, numbytes, AUGUR_H2H);
      break;
    }
#ifndef AUGURCPU
    case AUGUR_GPU: {
      augur_memcpy(dst->data, src->data, numbytes, AUGUR_D2D);
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

/* Viewing */

AugurMat_t augur_mat_view_as(AugurMat_t* src, AugurMat_t* shp) {
  AugurMat_t view;

  view.ty = shp->ty;
  view.row = shp->row;
  view.col = shp->col;
  view.data = src->data;
  
  return view;
}

AugurVec_t augur_mat_view_as_row_vec(AugurMat_t* mat, uint_t row) {
  AugurVec_t view;

  view.ty = mat->ty;
  view.stride = 0; // For row vec??
  view.elems = mat->col;
  view.data = mat->data + row * mat->col;
  
  return view;
}


/* Printing */

void hi_augur_mat_dump(int tab, AugurMemLoc_t loc, AugurMat_t* mat) {
  augur_tab(tab);
  printf("Matrix: %d x %d\n", mat->row, mat->col);
  if (!mat->data) {
    augur_tab(tab+2);
    printf("base[elems=%d]: unallocated\n", mat->row * mat->col);
    return;
  }
  
  switch (mat->ty) {
  case AUGUR_INT: {    
    int* data = (int*) mat->data;
    for (uint_t i = 0; i < mat->row; i++) {
      augur_tab(tab+2);
      augur_arr_dumpi(data + i * mat->col, mat->col, loc);
    }
    break;
  }
  case AUGUR_DBL: {
    double* data = (double*) mat->data;
    for (uint_t i = 0; i < mat->row; i++) {
      augur_tab(tab+2);
      augur_arr_dumpd(data + i * mat->col, mat->col, loc);
    }
    break;
  }
  default: {
    // TODO: ERROR
    break;
  }
  }
}

void h_augur_mat_dump(AugurMemLoc_t loc, AugurMat_t* mat) {
  hi_augur_mat_dump(0, loc, mat);
}


/* Matrix operations */

__HOSTDEV__ void augur_mat_cpy(AugurMat_t* dst, AugurMat_t* src) {
  switch (dst->ty) {
  case AUGUR_INT: {
    for (int i = 0; i < dst->row * dst->col; i++) {
      ((int*) dst->data)[i] = ((int*) src->data)[i];
    }
    break;
  }
  case AUGUR_DBL: {
    for (int i = 0; i < dst->row * dst->col; i++) {
      ((double*) dst->data)[i] = ((double*) src->data)[i];
    }
    break;
  }
  default: {
    // TODO: ERROR
    break;
  }
  }
}

__HOSTDEV__ void augur_mat_plus(AugurMat_t* dst, AugurMat_t* M1, AugurMat_t* M2) {
  for (int i = 0; i < dst->row; i++) {
    for (int j = 0; j < dst->col; j++) {
      AUGUR_MAT_SETD(dst, i, j, AUGUR_MAT_GETD(M1, i, j) + AUGUR_MAT_GETD(M2, i, j));
    }
  }
}

__HOSTORDEV__ void augur_mat_atm_plus(AugurMat_t* dst, AugurMat_t* src) {
  double* p_data = (double*) dst->data;
  double* p_data_src = (double*) src->data;
  for (uint_t i = 0; i < dst->row * dst->col; i++) {
    AUGUR_ATMINCD(p_data + i, p_data_src[i]);
  }
}

/* 
 * based off of https://rosettacode.org/wiki/Cholesky_decomposition#C 
 * Input:   positive semidefinite matrix A
 * Output:  L lower triangular where A = L L^T 
 *          the upper triangular part is unchanged
 */
__HOSTDEV__ void augur_mat_cholesky(AugurMat_t* A) {
  int n = A->col;
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < i + 1; j++) {
      double s = 0.0;
      for (int k = 0; k < j; k++) {
	s += AUGUR_MAT_GETD(A, i, k) * AUGUR_MAT_GETD(A, j, k);
      }
      if (i == j) {
	AUGUR_MAT_SETD(A, i, j, augur_sqrt(AUGUR_MAT_GETD(A, i, i) - s));
      }
      else {
	AUGUR_MAT_SETD(A, i, j, 1.0 / AUGUR_MAT_GETD(A, j, j) * (AUGUR_MAT_GETD(A, i, j) - s));
      }
    }
  }
}

/*
 * Input:   L lower triangular matrix
 * Output:  L^{-1} in-place
 *          This can be optimized to not touch upper triangular part
 */
__HOSTDEV__ void augur_mat_lower_trmi(AugurMat_t* L) {
  for (int c = 0; c < L->col; c++) {
    for (int m = 0; m < L->row; m++) { // could be started at c?
      double b = (c == m) ? 1.0 : 0.0;
      for (int i = 0; i < m; i++) {
	b -= AUGUR_MAT_GETD(L, m, i) * AUGUR_MAT_GETD(L, i, c);
      }
      AUGUR_MAT_SETD(L, m, c, b / AUGUR_MAT_GETD(L, m, m));
    }
  }
}

/*
 * Input:   L lower triangular matrix
 * Output:  [L L^T] in-place
 */
__HOSTDEV__ void augur_mat_lower_trtcpy(AugurMat_t* L) {
  for (int c = 0; c < L->col; c++) {
    for (int r = c+1; r < L->row; r++) {
      AUGUR_MAT_SETD(L, c, r, AUGUR_MAT_GETD(L, r, c));
    }
  }
}

/*
 * Input:   A = L L^T in cholesky form (upper part not used)
 * Output:  A{-1} = L^{-T} L^{-1}
 */
__HOSTDEV__ void augur_mat_cholesky_invert(AugurMat_t* LLT) {
  augur_mat_lower_trmi(LLT);
  for (int c = 0; c < LLT->col; c++) {
    for (int r = c; r < LLT->row; r++) {
      double acc = 0.0;
      for (int k = c; k < LLT->row; k++) {
	acc += AUGUR_MAT_GETD(LLT, k, r) * AUGUR_MAT_GETD(LLT, k, c);
      }
      AUGUR_MAT_SETD(LLT, r, c, acc);
    }
  }
  augur_mat_lower_trtcpy(LLT);
}

/*
 * Input:   L1, L2 lower triangular (upper part not used)
 * Output:  L2 = L1 L2
 */
__HOSTDEV__ void augur_mat_trtrmm(AugurMat_t* L1, AugurMat_t* L2) {
  for (int i = L2->row; i > 0 && i--;) {
    for (int j = 0; j < L2->row; j++) {
      double tmp = 0.0;
      for (int k = 0; k < i; k++) {
	tmp += AUGUR_MAT_GETD(L1, i, k) * AUGUR_MAT_GETD(L2, k, j);
      }
      tmp += AUGUR_MAT_GETD(L1, i, i) * AUGUR_MAT_GETD(L2, i, j);
      AUGUR_MAT_SETD(L2, i, j, tmp);
    }
  }
}

/*
 * Input:   L lower triangular (upper part not used), v vector
 * Output:  v = L v
 */
__HOSTDEV__ void augur_mat_trvm(AugurMat_t* L, AugurVec_t* bv) {
  for (int i = bv->elems - 1; i >= 0; i--) {
    double tmp = 0.0;
    for (int j = 0; j <= i; j++) {
      tmp += AUGUR_MAT_GETD(L, i, j) * AUGUR_VEC_GETD(bv, j);
    }
    AUGUR_VEC_SETD(bv, i, tmp);
  }
}

/*
 * Input:   A matrix, v vector, dst vector
 * Output:  dst = A v
 */
__HOSTDEV__ void augur_mat_mvm(AugurVec_t* dst, AugurMat_t* A, AugurVec_t* v) {
  for (int i = 0; i < A->row; i++) {
    double tmp = 0.0;
    for (int j = 0; j < A->col; j++) {
      tmp += AUGUR_MAT_GETD(A, i, j) * AUGUR_VEC_GETD(v, j);
    }
    AUGUR_VEC_SETD(dst, i, tmp);
  }
}

/*
 * Input:   A matrix, v vector, dst vector
 * Output:  dst = v' A
 */
__HOSTDEV__ void augur_mat_vmm(AugurVec_t* dst, AugurVec_t* v, AugurMat_t* A) {
  for (uint_t i = 0; i < A->col; i++) {
    real_t tmp = 0.0;
    for (uint_t j = 0; j < A->row; j++) {
      tmp += AUGUR_MAT_GETD(A, j, i) * AUGUR_VEC_GETD(v, j);
    }
    AUGUR_VEC_SETD(dst, i, tmp);
  }
}

/*
 * Input:   A matrix, v vector, dst vector
 * Output:  A = a A 
 */
__HOSTDEV__ void augur_mat_ms(double a, AugurMat_t* A) {
  for (int i = 0; i < A->row; i++) {
    for (int j = 0; j < A->col; j++) {
      double tmp = AUGUR_MAT_GETD(A, i, j);
      AUGUR_MAT_SETD(A, i, j, a * tmp);
    }
  }
}

/*
 * Input:   A matrix, v vector
 * Output:  A += A v'v
 */
__HOSTDEV__ void augur_mat_atm_inc_vtmt(AugurMat_t* A, AugurVec_t* v) {
  for (uint_t i = 0; i < v->elems; i++) {
    for (uint_t j = 0; j < v->elems; j++) {
      double* p = ((double*) A->data) + (i * v->elems + j);
      double tmp = AUGUR_VEC_GETD(v, i) * AUGUR_VEC_GETD(v, j);
      AUGUR_ATMINCD(p, tmp);
    }
  }
}


/* Host matrix operations */

void h_augur_mat_plus(AugurMat_t* dst, AugurMat_t* m1, AugurMat_t* m2) {
  switch (dst->ty) {
  case AUGUR_DBL: {
#ifdef AUGURCPU
    double* dst_data = (double*) dst->data;
    double* m1_data = (double*) m1->data;
    double* m2_data = (double*) m2->data;
    for (uint_t i = 0; i < dst->row * dst->col; i++) {
      dst_data[i] = m1_data[i] + m2_data[i];
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



/* Transfer operations */

/*
 * Allocates dst and copies data from src to dst. 
 *
 * dst: at host, underlying data is dst_data
 * dst_data: pointer to data at loc
 * src: at host
 */
void hi_augur_mat_to_native(AugurMemLoc_t loc, AugurMat_t* dst, char** dst_data, AugurMat_t* src, Bool_t f_cpy) {
  dst->ty = src->ty;
  dst->row = src->row;
  dst->col = src->col;
  uint_t numbytes = ty2size(src->ty) * src->row * src->col;
  dst->data = *dst_data; 
  if (f_cpy) {
    switch (loc) {
    case AUGUR_CPU: {
      augur_memcpy(dst->data, src->data, numbytes, AUGUR_H2H);
      break;
    }
#ifndef AUGURCPU
    case AUGUR_GPU: {
      augur_memcpy(dst->data, src->data, numbytes, AUGUR_H2D);
      break;
    }
#endif
    }
  }
  else {
    augur_memset(dst->data, 0, numbytes, loc);
  }
  *dst_data += numbytes;
}

/*
 * Allocates dst and copies data from src to dst.
 *
 * dst: at host, underlying data at loc
 * src: at host
 */
void h_augur_mat_to_native(AugurMemLoc_t loc, AugurMat_t* dst, AugurMat_t* src, Bool_t f_cpy) {
  uint_t numbytes = ty2size(src->ty) * src->row * src->col;
  char* dst_data = augur_malloc(numbytes, loc);
  hi_augur_mat_to_native(loc, dst, &dst_data, src, f_cpy);
}

/*
 * Copies data from src_data to dst.
 * 
 * dst: at host, underlying data at host
 * src_data: at loc
 */
void hi_augur_mat_from_native(AugurMemLoc_t loc, AugurMat_t* dst, char** src_data) {
  uint_t numbytes = ty2size(dst->ty) * dst->row * dst->col;
  switch (loc) {
  case AUGUR_CPU: {
    augur_memcpy(dst->data, *src_data, numbytes, AUGUR_H2H);
    break;
  }
#ifndef AUGURCPU
  case AUGUR_GPU: {
    augur_memcpy(dst->data, *src_data, numbytes, AUGUR_D2H);
    break;
  }
#endif
  }
  *src_data += numbytes;
}

/*
 * Copies data from src to dst.
 * 
 * dst: at host, underlying data at host
 * src_data: at loc
 */
void h_augur_mat_from_native(AugurMemLoc_t loc, AugurMat_t* dst, AugurMat_t* src) {
  char* src_data = (char*) src->data;
  hi_augur_mat_from_native(loc, dst, &src_data);
}


void h_augur_mat_getd_idx(AugurMemLoc_t loc, double* dst, AugurMat_t* mat, int idx) {
  switch (loc) {
  case AUGUR_CPU: {
    augur_memcpy(dst, (double*) mat->data + idx, sizeof(double), AUGUR_H2H);
    break;
  }
#ifndef AUGURCPU
  case AUGUR_GPU: {
    augur_memcpy(dst, (double*) mat->data + idx, sizeof(double), AUGUR_D2H);
    break;
  }
#endif
  }
}

void h_augur_mat_setd_idx(AugurMemLoc_t loc, AugurMat_t* mat, int idx, double* src) {
  switch (loc) {
  case AUGUR_CPU: {
    augur_memcpy((double*) mat->data + idx, src, sizeof(double), AUGUR_H2H);
    break;
  }
#ifndef AUGURCPU
  case AUGUR_GPU: {
    augur_memcpy((double*) mat->data + idx, src, sizeof(double), AUGUR_H2D);
    break;
  }
#endif
  }
}

void h_augur_mat_basis_add(AugurMemLoc_t loc, AugurMat_t* mat, int idx, double v) {
  switch (mat->ty) {
  case AUGUR_DBL: {
    double tmp;
    h_augur_mat_getd_idx(loc, &tmp, mat, idx);
    tmp += v;
    h_augur_mat_setd_idx(loc, mat, idx, &tmp);
    break;
  }
  default: {
    // TODO: ERROR
    break;
  }
  }
}



/*
AugurMat_t h_augur_mat_stk_alloc(AugurMemLoc_t loc, AugurTyp_t ty, uint_t row, uint_t col) {
  AugurMat_t mat;
  mat.ty = ty;
  mat.row = row;
  mat.col = col;
  mat.data = augur_malloc(ty2size(ty) * row * col, loc);
  return mat;
}

AugurMat_t h_augur_mat_stk_alloc2(AugurMemLoc_t loc, AugurTyp_t ty, uint_t row, uint_t col, void* h_data) {
  AugurMat_t mat = h_augur_mat_stk_alloc(loc, ty, row, col);
  uint_t numbytes = ty2size(ty) * row * col;
  switch (loc) {
  case AUGUR_CPU: {
    augur_memcpy(mat.data, h_data, numbytes, AUGUR_H2H);
    break;
  }
#ifndef AUGURCPU
  case AUGUR_GPU: {
    augur_memcpy(mat.data, h_data, numbytes, AUGUR_H2D);
    break;
  }
#endif
  }
  return mat;
}

void h_augur_mat_stk_free(AugurMemLoc_t loc, AugurMat_t* mat) {
  augur_free(mat->data, loc);
}
*/
