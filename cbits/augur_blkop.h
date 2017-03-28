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

#ifndef AUGUR_BLKOP_H
#define AUGUR_BLKOP_H

#include "augur_hdr.h"
#include "augur_util.h"
#include "augur_rtval.h"


/* Accessors */

uint_t h_augur_blk_obj2elems(AugurTyp_t ty, void* obj);
AugurTyp_t augur_blk_obj2basety(AugurTyp_t ty, void* obj);
uint_t h_augur_blk_obj2numbytes(AugurTyp_t ty, void* obj);
void* h_augur_blk_obj_get_data(AugurTyp_t ty, void* obj);


/* Metadata operations */


/* Allocation */

// void h_augur_blk_stk_alloc();
// void h_augur_blk_stk_free(AugurMemLoc_t loc, AugurBlk_t* blk);
void h_augur_blk_cpy(AugurMemLoc_t loc, AugurBlk_t* dst, AugurBlk_t* src);

/* Initialization */


/* Printing */

void h_augur_blk_dump(AugurMemLoc_t loc, AugurBlk_t* blk);


/* Block operations */

void h_augur_blk_zero(AugurMemLoc_t loc, AugurBlk_t* blk);
void h_augur_blk_scale(AugurMemLoc_t loc, real_t a, AugurBlk_t* blk);
void h_augur_blk_plus(AugurMemLoc_t loc, AugurBlk_t* dst, AugurBlk_t* b1, AugurBlk_t* b2);
void h_augur_blk_minus(AugurMemLoc_t loc, AugurBlk_t* dst, AugurBlk_t* b1, AugurBlk_t* b2);
void h_augur_blk_scale_plus(AugurMemLoc_t loc, AugurBlk_t* dst, AugurBlk_t* b1, real_t scale, AugurBlk_t* b2);
real_t h_augur_blk_dot(AugurMemLoc_t loc, AugurBlk_t* blk1, AugurBlk_t* blk2);


/* Transfer operations */

void h_augur_blk_mk_group(AugurBlk_t* dst, uint_t num_blks, AugurTyp_t* typs, void** blks);
void h_augur_blk_mk_cpy(AugurMemLoc_t loc, AugurBlk_t* dst, uint_t num_blks, AugurTyp_t* typs, void** blks);

void h_augur_blk_to_native(AugurMemLoc_t loc, AugurBlk_t* dst, AugurBlk_t* src, Bool_t f_cpy);
void h_augur_blk_from_native(AugurMemLoc_t loc, AugurBlk_t* dst, AugurBlk_t* src);


#endif
