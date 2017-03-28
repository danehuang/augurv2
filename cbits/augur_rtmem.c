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
#include "augur_rtmem.h"


/* Allocation */

AugurShape_t h_augur_shape_stk_alloc(AugurDim_t* dims, uint_t len, AugurTyp_t ty) {
  AugurShape_t shape;
  shape.base_ty = ty;
  shape.len = len;
  uint_t numbytes = sizeof(AugurDim_t) * len;
  shape.dims = (AugurDim_t*) augur_malloc(numbytes, AUGUR_CPU);
  augur_memcpy(shape.dims, dims, numbytes, AUGUR_H2H);
  return shape;
}

void h_augur_shape_stk_free(AugurShape_t* shape) {
  augur_free(shape->dims, AUGUR_CPU);
}


/* Printing */

void h_augur_dim_dump(AugurDim_t* dim) {
  switch (dim->kind) {
  case DIM_MAX: {
    uint_t val;
    switch (dim->objty) {
    case AUGUR_VEC: {
      val = h_augur_vec_maxdim((AugurVec_t*) dim->obj, dim->dim);
      break;
    }
    case AUGUR_MAT: {
      val = h_augur_mat_dim((AugurMat_t*) dim->obj, dim->dim);
      break;
    }
    default: {
      // TODO: ERROR
      val = 0;
      break;
    }
    }
    printf("max: %d\n", val);
    break;
  }
  case DIM_VAL: {
    printf("val: %d\n", dim->val);
    break;
  }
  case DIM_MAT: {
    printf("mat: %d x %d\n", dim->row, dim->col);
    break;
  }
  case DIM_CPY: {
    printf("cpy: %p\n", dim->cpyof);
    break;
  }
  }
}

void h_augur_shape_dump(AugurShape_t* shp) {
  printf("Shape base typ: %s\n", ty2str(shp->base_ty));
  for (int i = 0; i < shp->len; i++) {
    printf("shp[%d]: ", i);
    h_augur_dim_dump(shp->dims + i);
  }
}


/* Operations */

AugurTyp_t hi_augur_dim_to_typ(AugurDim_t* dim) {
  switch (dim->kind) {
  case DIM_MAX: {
    return AUGUR_VEC;
    // return dim->objty;
  }
  case DIM_VAL: {
    return AUGUR_VEC;
  }
  case DIM_MAT: {
    return AUGUR_MAT;
  }
  case DIM_CPY: {
    return dim->cpyty;
  }
  default: {
    // TODO: ERROR
    return AUGUR_VEC;
  }
  }
}

uint_t hi_augur_dim_to_elems(AugurDim_t* dim) {
  switch (dim->kind) {
  case DIM_MAX: {
    switch (dim->objty) {
    case AUGUR_VEC: {
      AugurVec_t* vec = (AugurVec_t*) dim->obj;
      return h_augur_vec_maxdim(vec, dim->dim);
    }
    case AUGUR_MAT: {
      AugurMat_t* mat = (AugurMat_t*) dim->obj;
      return h_augur_mat_dim(mat, dim->dim);
    }
    default: {
      // TODO: ERROR
      return 0;
    }
    }
  }
  case DIM_VAL: {
    return dim->val;
  }
  case DIM_MAT: {
    return dim->row * dim->col;
  }
  case DIM_CPY: {
    switch (dim->cpyty) {
    case AUGUR_VEC: {
      return h_augur_vec_deep_elems((AugurVec_t*) dim->cpyof);
    }
    case AUGUR_MAT: {
      return h_augur_mat_elems((AugurMat_t*) dim->cpyof);
    }
    default: {
      // TODO: ERROR
      return 0;
    }
    }
  }
  default: {
    // TODO: ERROR
    return 0;
  }
  }
}

void hi_augur_dim_alloc_obj(void* obj, AugurTyp_t base_ty, AugurDim_t* dim) {
  switch (dim->kind) {
  case DIM_MAX: {
    switch (dim->objty) {
    case AUGUR_VEC: {
      AugurVec_t* vec = (AugurVec_t*) dim->obj;
      uint_t elems = h_augur_vec_maxdim(vec, dim->dim);
      h_augur_vec_host_alloc((AugurVec_t*) obj, base_ty, elems, AUGUR_SKEL);
      break;
    }
    case AUGUR_MAT: {
      AugurMat_t* mat = (AugurMat_t*) dim->obj;
      uint_t elems = h_augur_mat_dim(mat, dim->dim);
      h_augur_vec_host_alloc((AugurVec_t*) obj, base_ty, elems, AUGUR_SKEL);
      break;
    }
    default: {
      // TODO: ERROR
      break;
    }
    }
    break;
  }
  case DIM_VAL: {
    h_augur_vec_host_alloc((AugurVec_t*) obj, base_ty, dim->val, AUGUR_SKEL);
    break;
  }
  case DIM_MAT: {
    h_augur_mat_host_alloc((AugurMat_t*) obj, base_ty, dim->row, dim->col, AUGUR_SKEL);
    break;
  }
  case DIM_CPY: {
    switch (dim->cpyty) {
    case AUGUR_VEC: {
      AugurVec_t* src = (AugurVec_t*) dim->cpyof;
      h_augur_vec_host_cpy((AugurVec_t*) obj, src, base_ty, AUGUR_SKEL);
      break;
    }
    case AUGUR_MAT: {
      AugurMat_t* src = (AugurMat_t*) dim->cpyof;
      h_augur_mat_host_cpy((AugurMat_t*) obj, src, base_ty, AUGUR_SKEL);
      break;
    }
    default: {
      // TODO: ERROR
      break;
    }
    }
    break;
  }
  }
}

void hi_augur_dims_alloc_obj(void* obj, AugurTyp_t base_ty, AugurDim_t* dims, uint_t depth) {
  if (depth == 1) {
    hi_augur_dim_alloc_obj(obj, base_ty, dims);
  }
  else if (depth > 1) {
    uint_t elems = hi_augur_dim_to_elems(dims);
    AugurTyp_t ty = hi_augur_dim_to_typ(dims+1);
    AugurVec_t* vec = (AugurVec_t*) obj;
    h_augur_vec_host_alloc(vec, ty, elems, AUGUR_DATA);
    switch (ty) {
    case AUGUR_VEC: {
      for (uint_t i = 0; i < elems; i++) {
	hi_augur_dims_alloc_obj(AUGUR_VEC_GETV(vec, i), base_ty, dims+1, depth-1);
      }
      break;
    }
    case AUGUR_MAT: {
      for (uint_t i = 0; i < elems; i++) {
        hi_augur_dims_alloc_obj(AUGUR_VEC_GETM(vec, i), base_ty, dims+1, depth-1);
      }
      break;
    }
    default: {
      // TODO: ERROR
      break;
    }
    }
  }
  else {
    
  }
}

int* h_augur_rtval_from_int(AugurMemLoc_t loc, int* src, Bool_t f_cpy) {
  int* dst = (int*) augur_malloc(sizeof(int), loc);
  switch (f_cpy) {
  case TRUE: {
    switch (loc) {
    case AUGUR_CPU: {
      augur_memcpy(dst, src, sizeof(int), AUGUR_H2H);
      break;
    }
#ifndef AUGURCPU
    case AUGUR_GPU: {
      augur_memcpy(dst, src, sizeof(int), AUGUR_H2D);
      break;
    }
#endif
    }
    break;
  }
  case FALSE: {
    augur_memset(dst, 0, sizeof(int), loc);
    break;
  }
  }
  return dst;
}

double* h_augur_rtval_from_dbl(AugurMemLoc_t loc, double* src, Bool_t f_cpy) {
  double* dst = (double*) augur_malloc(sizeof(double), loc);
  switch (f_cpy) {
  case TRUE: {
    switch (loc) {
    case AUGUR_CPU: {
      augur_memcpy(dst, src, sizeof(double), AUGUR_H2H);
      break;
    }
#ifndef AUGURCPU
    case AUGUR_GPU: {
      augur_memcpy(dst, src, sizeof(double), AUGUR_H2D);
      break;
    }
#endif
    }
    break;
  }
  case FALSE: {
    augur_memset(dst, 0, sizeof(double), loc);
    break;
  }
  }
  return dst;
}

void h_augur_rtval_from_vec(AugurMemLoc_t loc, AugurFlatVec_t* dst, AugurVec_t* vec, Bool_t f_cpy) {
  h_augur_vec_to_native(loc, dst, vec, f_cpy);
}

void h_augur_rtval_from_mat(AugurMemLoc_t loc, AugurMat_t* dst, AugurMat_t* mat, Bool_t f_cpy) {
  h_augur_mat_to_native(loc, dst, mat, f_cpy);
}

/**
 * 1) Convert shape into host object (AugurShape_t -> AugurVec_t + AugurMat_t)
 * 2) Convert host object to native object (AugurVec_t + AugurMat_t -> AugurFlatVec_t + AugurMat_t)
 * 3) Free host object 
 */
void h_augur_rtval_from_shape(AugurMemLoc_t loc, void* dst, AugurShape_t* shp) {
  switch (hi_augur_dim_to_typ(shp->dims)) {
  case AUGUR_VEC: {
    AugurVec_t tmp;
    hi_augur_dims_alloc_obj(&tmp, shp->base_ty, shp->dims, shp->len);
    h_augur_vec_dump(AUGUR_CPU, &tmp);
    h_augur_rtval_from_vec(loc, (AugurFlatVec_t*) dst, &tmp, FALSE);    
    h_augur_vec_host_free(&tmp);
    break;
  }
  case AUGUR_MAT: {
    AugurMat_t tmp;
    hi_augur_dims_alloc_obj(&tmp, shp->base_ty, shp->dims, shp->len);
    h_augur_rtval_from_mat(loc, (AugurMat_t*) dst, &tmp, FALSE);
    h_augur_mat_host_free(&tmp);
    break;
  }
  default: {
    // TODO: ERROR 
    break;
  }
  }
}
