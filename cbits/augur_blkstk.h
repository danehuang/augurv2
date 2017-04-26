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

#ifndef AUGUR_BLKSTK_H
#define AUGUR_BLKSTK_H

#include "augur_hdr.h"
#include "augur_util.h"
#include "augur_rtval.h"
#include "augur_blkop.h"


#define AUGUR_BLK_STK_MAX 16

typedef struct AugurBlkStk {
  AugurBlk_t blks[AUGUR_BLK_STK_MAX];
  int hd;
  uint_t alloc;
} AugurBlkStk_t;


/* Allocate */
AugurBlkStk_t augur_blk_stk_stk_alloc();
void augur_blk_stk_reset(AugurBlkStk_t* stk);

/* Printing */
void augur_blk_stk_dump(AugurMemLoc_t loc, AugurBlkStk_t* stk);

/* Operations */
int augur_blk_stk_push(AugurMemLoc_t loc, AugurBlkStk_t* stk, AugurBlk_t* shape, AugurBlk_t* blk);
AugurBlk_t* augur_blk_stk_pop(AugurBlkStk_t* stk);
AugurBlk_t* augur_blk_stk_peek(AugurBlkStk_t* stk);

#endif
