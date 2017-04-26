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
#include "augur_blkop.h"
#include "augur_blkstk.h"


/* Allocate */

AugurBlkStk_t augur_blk_stk_stk_alloc() {
  AugurBlkStk_t stk;
  stk.hd = -1;
  stk.alloc = 0;
  return stk;
}

void augur_blk_stk_reset(AugurBlkStk_t* stk) {
  stk->hd = -1;
}


/* Printing */

void augur_blk_stk_dump(AugurMemLoc_t loc, AugurBlkStk_t* stk) {
  printf("Head: %d, Alloc: %d\n", stk->hd, stk->alloc);
  for (int i = stk->hd; i >= 0; i--) {
    printf("STK[%d]\n", i);
    h_augur_blk_dump(loc, stk->blks + i);
  }
}


/* Operations */

int augur_blk_stk_push(AugurMemLoc_t loc, AugurBlkStk_t* stk, AugurBlk_t* shape, AugurBlk_t* blk) {
  stk->hd++;
  if (stk->hd >= stk->alloc) {
    if (stk->alloc >= AUGUR_BLK_STK_MAX) {
      return -1;
    }
    h_augur_blk_mk_cpy2(loc, stk->blks + stk->hd, shape);
    stk->alloc++;
  }
  h_augur_blk_cpy(loc, stk->blks + stk->hd, blk);
  return 0;
}

AugurBlk_t* augur_blk_stk_pop(AugurBlkStk_t* stk) {
  AugurBlk_t* blk = stk->blks + stk->hd;
  stk->hd--;
  return blk;
}

AugurBlk_t* augur_blk_stk_peek(AugurBlkStk_t* stk) {
  AugurBlk_t* blk = stk->blks + stk->hd;
  return blk;
}
