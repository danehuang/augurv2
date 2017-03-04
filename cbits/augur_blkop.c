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


/* Block helper functions */

uint_t h_augur_blk_obj2elems(AugurTyp_t ty, void* obj) {
  switch (ty) {
  case AUGUR_DBL: {
    return 1;
  }
  case AUGUR_VEC: {
    AugurFlatVec_t* fvec = (AugurFlatVec_t*) obj;
    return fvec->base_elems;
  }
  case AUGUR_MAT: {
    AugurMat_t* mat = (AugurMat_t*) obj;
    return mat->row * mat->col;
  }
  default: {
    // TODO: ERROR 
    return 0;
  }
  }
}

AugurTyp_t augur_blk_obj2basety(AugurTyp_t ty, void* obj) {
  switch (ty) {
  case AUGUR_DBL: {
    return AUGUR_DBL;
  }
  case AUGUR_VEC: {
    AugurFlatVec_t* fvec = (AugurFlatVec_t*) obj;
    return fvec->base_ty;
  }
  case AUGUR_MAT: {
    AugurMat_t* mat = (AugurMat_t*) obj;
    return mat->ty;
  }
  default: {
    // TODO: ERROR
    return AUGUR_DBL;
  }
  }
}

uint_t h_augur_blk_obj2numbytes(AugurTyp_t ty, void* obj) {
  return ty2size(AUGUR_DBL) * h_augur_blk_obj2elems(ty, obj);
}

void* h_augur_blk_obj_get_data(AugurTyp_t ty, void* obj) {
  switch (ty) {
  case AUGUR_DBL: {
    return obj;
  }
  case AUGUR_VEC: {
    AugurFlatVec_t* fvec = (AugurFlatVec_t*) obj;
    return fvec->base_data;
  }
  case AUGUR_MAT: {
    AugurMat_t* mat = (AugurMat_t*) obj;
    return mat->data;
  }
  default: {
    // TODO: ERROR
    return NULL;
  }
  }
}


/* Allocation */

void h_augur_blk_cpy(AugurMemLoc_t loc, AugurBlk_t* dst, AugurBlk_t* src) {
  for (uint_t i = 0; i < dst->num_blks; i++) {
    void* dst_data = h_augur_blk_obj_get_data(dst->typs[i], dst->blks[i]);
    void* src_data = h_augur_blk_obj_get_data(src->typs[i], src->blks[i]);
    uint_t numbytes = h_augur_blk_obj2numbytes(dst->typs[i], dst->blks[i]);
    switch (loc) {
    case AUGUR_CPU: {
      augur_memcpy(dst_data, src_data, numbytes, AUGUR_H2H);
      break;
    }
#ifndef AUGURCPU
    case AUGUR_GPU: {
      augur_memcpy(dst_data, src_data, numbytes, AUGUR_D2D);
      break;
    }
#endif
    }
  }
}


/* Printing */

void h_augur_blk_dump(AugurMemLoc_t loc, AugurBlk_t* blk) {
  printf("blk[numblks=%d]\n", blk->num_blks);
  for (uint_t i = 0; i < blk->num_blks; i++) {
    printf("blk[idx=%d, ty=%s]\n", i, ty2str(blk->typs[i]));
    switch (blk->typs[i]) {
    case AUGUR_DBL: {
      // TODO: IMPLEMENT ME
      printf("%f\n", * ((double*) blk->blks[i]));
      break;
    }
    case AUGUR_VEC: {
      switch (blk->native) {
      case FALSE: {
	h_augur_vec_dump(loc, (AugurVec_t*) blk->blks[i]);
	break;
      }
      case TRUE: {
	h_augur_flat_vec_dump(loc, (AugurFlatVec_t*) blk->blks[i]);
	break;
      }
      }
      break;
    }
    case AUGUR_MAT: {
      h_augur_mat_dump(loc, (AugurMat_t*) blk->blks[i]);
      break;
    }
    default: {
      // TODO: ERROR
      break;
    }
    }
  }
}


/* Block operations */

void h_augur_blk_zero(AugurMemLoc_t loc, AugurBlk_t* blk) {
  h_augur_blk_dump(AUGUR_CPU, blk);
  for (uint_t i = 0; i < blk->num_blks; i++) {
    void* blk_data = h_augur_blk_obj_get_data(blk->typs[i], blk->blks[i]);
    uint_t numbytes = h_augur_blk_obj2numbytes(blk->typs[i], blk->blks[i]);
    augur_memset(blk_data, 0, numbytes, loc);
  }
}

void h_augur_blk_scale(AugurMemLoc_t loc, double a, AugurBlk_t* blk) {
  for (uint_t i = 0; i < blk->num_blks; i++) {
    double* blk_data = (double*) h_augur_blk_obj_get_data(blk->typs[i], blk->blks[i]);
    uint_t elems = h_augur_blk_obj2elems(blk->typs[i], blk->blks[i]);
#ifdef AUGURCPU
    for (uint_t j = 0; j < elems; j++) {
      blk_data[j] = a * blk_data[j];
    }
#else
    // TODO: IMPLEMENT ME
#endif
  }
}

void h_augur_blk_plus(AugurMemLoc_t loc, AugurBlk_t* dst, AugurBlk_t* b1, AugurBlk_t* b2) {
  for (uint_t i = 0; i < dst->num_blks; i++) {
    double* dst_data = (double*) h_augur_blk_obj_get_data(dst->typs[i], dst->blks[i]);
    double* b1_data = (double*) h_augur_blk_obj_get_data(b1->typs[i], b1->blks[i]);
    double* b2_data = (double*) h_augur_blk_obj_get_data(b2->typs[i], b2->blks[i]);
    uint_t elems = h_augur_blk_obj2elems(dst->typs[i], dst->blks[i]);
    switch (loc) {
    case AUGUR_CPU: {
      for (uint_t j = 0; j < elems; j++) {
	dst_data[j] = b1_data[j] + b2_data[j];
      } 
      break;
    }
#ifndef AUGURCPU
    case AUGUR_GPU: {
      // TODO: IMPLEMENT ME
      break;
    }
#endif
    }
  }
}

void h_augur_blk_scale_plus(AugurMemLoc_t loc, AugurBlk_t* dst, AugurBlk_t* b1, real_t scale, AugurBlk_t* b2) {
  for (uint_t i = 0; i < dst->num_blks; i++) {
    double* dst_data = (double*) h_augur_blk_obj_get_data(dst->typs[i], dst->blks[i]);
    double* b1_data = (double*) h_augur_blk_obj_get_data(b1->typs[i], b1->blks[i]);
    double* b2_data = (double*) h_augur_blk_obj_get_data(b2->typs[i], b2->blks[i]);
    uint_t elems = h_augur_blk_obj2elems(dst->typs[i], dst->blks[i]);
    switch (loc) {
    case AUGUR_CPU: {
      for (uint_t j = 0; j < elems; j++) {
	dst_data[j] = b1_data[j] + scale * b2_data[j];
      } 
      break;
    }
#ifndef AUGURCPU
    case AUGUR_GPU: {
      // TODO: IMPLEMENT ME
      break;
    }
#endif
    }
  }
}

void h_augur_blk_minus(AugurMemLoc_t loc, AugurBlk_t* dst, AugurBlk_t* b1, AugurBlk_t* b2) {
  h_augur_blk_scale_plus(loc, dst, b1, -1.0, b2);
}

real_t h_augur_blk_dot(AugurMemLoc_t loc, AugurBlk_t* b1, AugurBlk_t* b2) {
  double acc = 0.0;
  for (uint_t i = 0; i < b1->num_blks; i++) {
    double* b1_data = (double*) h_augur_blk_obj_get_data(b1->typs[i], b1->blks[i]);
    double* b2_data = (double*) h_augur_blk_obj_get_data(b2->typs[i], b2->blks[i]);
    uint_t elems = h_augur_blk_obj2elems(b1->typs[i], b1->blks[i]);
    switch (loc) {
    case AUGUR_CPU: {
      for (uint_t j = 0; j < elems; j++) {
	acc += b1_data[j] * b2_data[j];
      } 
      break;
    }
#ifndef AUGURCPU
    case AUGUR_GPU: {
      // TODO: IMPLEMENT ME
      break;
    }
#endif
    }
  }
  return acc;
}


/* Transfer operations */

/**
 * Group objects into a blk.
 *
 * dst: destination block
 * num_blks: number of objects in the block 
 * typs: the types of the objects in the block
 * blks: the objects in the block (assumed to already 
 *       be allocated at the appropriate location)
 */
void h_augur_blk_mk_group(AugurBlk_t* dst, uint_t num_blks, AugurTyp_t* typs, void** blks) {
  uint_t numbytes;
    
  dst->native = TRUE;
  dst->num_blks = num_blks;

  numbytes = sizeof(AugurTyp_t) * num_blks;
  dst->typs = (AugurTyp_t*) augur_malloc(numbytes, AUGUR_CPU);
  augur_memcpy(dst->typs, typs, numbytes, AUGUR_H2H);

  numbytes = sizeof(void*) * num_blks;
  dst->blks = (void**) augur_malloc(numbytes, AUGUR_CPU);
  augur_memcpy(dst->blks, blks, numbytes, AUGUR_H2H);

  uint_t base_elems = 0;
  for (uint_t i = 0; i < num_blks; i++) {
    switch (typs[i]) {
    case AUGUR_DBL: {
      base_elems += 1;
      break;
    }
    case AUGUR_VEC: {
      base_elems += ((AugurFlatVec_t*) blks[i])->base_elems;
      break;
    }
    case AUGUR_MAT: {
      AugurMat_t* mat = (AugurMat_t*) blks[i];
      base_elems += mat->row * mat->col;
      break;
    }
    default: {
      // TODO: ERROR
      break;
    }
    }
  }
  dst->base_elems = base_elems;
}

/**
 * Copy a block given a block that is on host. 
 *
 * dst: destination block
 * num_blks: number of objects in the block 
 * typs: the types of the objects in the block
 * blks: the objects in the block (gives the shape, assumed to be on HOST)
 */
void h_augur_blk_mk_cpy(AugurMemLoc_t loc, AugurBlk_t* dst, uint_t num_blks, AugurTyp_t* typs, void** blks) {
  uint_t numbytes;
  dst->native = TRUE;
  dst->num_blks = num_blks;

  numbytes = sizeof(AugurTyp_t) * num_blks;
  dst->typs = (AugurTyp_t*) augur_malloc(numbytes, AUGUR_CPU);  
  augur_memcpy(dst->typs, typs, numbytes, AUGUR_H2H);

  numbytes = sizeof(void*) * num_blks;
  dst->blks = (void**) augur_malloc(numbytes, AUGUR_CPU);

  uint_t base_elems = 0;
  for (uint_t i = 0; i < num_blks; i++) {
    switch (typs[i]) {
    case AUGUR_DBL: {
      double* d = (double*) augur_malloc(sizeof(double), AUGUR_CPU);
      dst->blks[i] = d;
      base_elems += 1;
      break;
    }
    case AUGUR_VEC: {
      AugurFlatVec_t* fvec = (AugurFlatVec_t*) augur_malloc(sizeof(AugurFlatVec_t), AUGUR_CPU);
      h_augur_vec_to_native(loc, fvec, (AugurVec_t*) blks[i], FALSE);
      dst->blks[i] = fvec;
      base_elems += fvec->base_elems;
      break;
    }
    case AUGUR_MAT: {
      AugurMat_t* mat = (AugurMat_t*) augur_malloc(sizeof(AugurMat_t), AUGUR_CPU);
      h_augur_mat_to_native(loc, mat, (AugurMat_t*) blks[i], FALSE);
      dst->blks[i] = mat;
      base_elems += mat->row * mat->col;
      break;
    }
    default: {
      // TODO: ERROR
      break;
    }
    }
  }
  dst->base_elems = base_elems;
}

void h_augur_blk_to_native(AugurMemLoc_t loc, AugurBlk_t* dst, AugurBlk_t* src, Bool_t f_cpy) {
  dst->num_blks = src->num_blks;
  dst->typs = (AugurTyp_t*) augur_malloc(sizeof(AugurTyp_t) * src->num_blks, AUGUR_CPU);
  dst->blks = (void**) augur_malloc(sizeof(void*) * src->num_blks, AUGUR_CPU);
  dst->base_elems = src->base_elems;
  dst->base_data = (double*) augur_malloc(sizeof(double) * src->base_elems, loc);

  char* dst_data = (char*) dst->base_data;
  for (uint_t i = 0; i < dst->num_blks; i++) {
    switch (dst->typs[i]) {
    case AUGUR_DBL: {
      double* dbl_src = (double*) ((src->blks)[i]);
      hi_augur_dbl_to_native(loc, &dst_data, dbl_src, f_cpy);
      break;
    }
    case AUGUR_VEC: {
      AugurVec_t* vec_dst = (AugurVec_t*) ((dst->blks)[i]);
      AugurVec_t* vec_src = (AugurVec_t*) ((src->blks)[i]);
      hi_augur_vec_to_native(loc, vec_dst, &dst_data, vec_src, f_cpy);
      break;
    }
    case AUGUR_MAT: {
      AugurMat_t* mat_dst = (AugurMat_t*) ((dst->blks)[i]);
      AugurMat_t* mat_src = (AugurMat_t*) ((src->blks)[i]);
      hi_augur_mat_to_native(loc, mat_dst, &dst_data, mat_src, f_cpy);
      break;
    }
    default:
      // TODO: ERROR
      break;
    }
  }
}


void hi_augur_blk_from_native(AugurMemLoc_t loc, AugurBlk_t* dst, char** src_data) {
  for (uint_t i = 0; i < dst->num_blks; i++) {
    switch (dst->typs[i]) {
    case AUGUR_DBL: {
      double* dbl_dst = (double*) ((dst->blks)[i]);
      hi_augur_dbl_from_native(loc, dbl_dst, src_data);
      break;
    }
    case AUGUR_VEC: {
      AugurVec_t* vec_dst = (AugurVec_t*) ((dst->blks)[i]);
      hi_augur_vec_from_native(loc, vec_dst, src_data);
      break;
    }
    case AUGUR_MAT: {
      AugurMat_t* mat_dst = (AugurMat_t*) ((dst->blks)[i]);
      hi_augur_mat_from_native(loc, mat_dst, src_data);
      break;
    }
    default: {
      // TODO: ERROR
      break;
    }      
    }
  }
}

void h_augur_blk_from_native(AugurMemLoc_t loc, AugurBlk_t* dst, AugurBlk_t* src) {
  char* src_data = (char*) src->base_data;
  hi_augur_blk_from_native(loc, dst, &src_data);
}
