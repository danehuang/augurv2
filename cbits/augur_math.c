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
#include "augur_vecop.h"


/* Numeric operations */

__HOSTDEV__ real_t augur_pow(real_t base, real_t expon) {
  return pow(base, expon);
}

__HOSTDEV__ real_t augur_sqrt(real_t x) {
  return sqrt(x);
}

__HOSTDEV__ real_t augur_exp(real_t x) {
  return exp(x);
}

__HOSTDEV__ real_t augur_log(real_t x) {
  return log(x);
}

__HOSTDEV__ real_t augur_cos(real_t x) {
  return cos(x);
}

__HOSTDEV__ real_t augur_sin(real_t x) {
  return sin(x);
}


__HOSTDEV__ real_t augur_expit(real_t x) {
  return 1.0 / (1.0 + augur_exp(-x)); 
}

__HOSTDEV__ real_t augur_expit_grad_0(real_t x) {
  double t = augur_expit(x);
  return t * (1.0 - t);
}


__HOSTDEV__ real_t augur_logit(real_t x) {
  return augur_log(x) - augur_log(1.0 - x);
}

// Taken from http://www.machinedlearnings.com/2011/06/faster-lda.html
__HOSTDEV__ real_t augur_digamma(real_t x) {
  real_t twopx = 2.0 + x;
  real_t logterm = augur_log(twopx);
 
  return - (1.0 + 2.0 * x) / (x * (1.0 + x))
         - (13.0 + 6.0 * x) / (12.0 * twopx * twopx)
         + logterm;
}

__HOSTDEV__ real_t augur_lgamma(real_t x) {
  return lgamma(x);
}

__HOSTDEV__ real_t augur_multi_lgamma(int p, real_t x) {
  real_t res = (p * (p-1) * 0.25) * log(3.1415);
  for (int i = 1; i < p+1; i++) {
    res += augur_lgamma(x - (i - 1.0) / 2.0);
  }
  return res;
}


/* Derivatives */

__HOSTDEV__ real_t augur_dotpt_exp(real_t x) {
  return augur_dotpt_exp(x);
}

__HOSTDEV__ real_t augur_dotpt_log(real_t x) {
  return 1.0 / x;
}

__HOSTDEV__ real_t augur_dotpt_cos(real_t x) {
  return -augur_sin(x);
}

__HOSTDEV__ real_t augur_dotpt_sin(real_t x) {
  return augur_cos(x);
}

__HOSTDEV__ real_t augur_dotpt_expit(real_t x) {
  double t = augur_expit(x);
  return t * (1.0 - t);
}

__HOSTDEV__ real_t augur_dotpt_logit(real_t x) {
  return 1.0 / (x * (1.0 - x));
}


/* Vector operations */

__HOSTDEV__ real_t augur_dotprod(AugurVec_t* v1, AugurVec_t* v2) {
  real_t acc = 0.0;
  if (v1->ty == AUGUR_DBL && v2->ty == AUGUR_DBL) {
    for (uint_t i = 0; i < v1->elems; i++) {
      acc += AUGUR_VEC_GETD(v1, i) * AUGUR_VEC_GETD(v2, i);
    }
  }
  else if (v1->ty == AUGUR_DBL && v2->ty == AUGUR_INT) {
    for (uint_t i = 0; i < v1->elems; i++) {
      acc += AUGUR_VEC_GETD(v1, i) * AUGUR_VEC_GETI(v2, i);
    }
  }
  else if (v1->ty == AUGUR_INT && v2->ty == AUGUR_DBL) {
    for (uint_t i = 0; i < v1->elems; i++) {
      acc += AUGUR_VEC_GETI(v1, i) * AUGUR_VEC_GETD(v2, i);
    }
  }
  else {
    for (uint_t i = 0; i < v1->elems; i++) {
      acc += AUGUR_VEC_GETI(v1, i) * AUGUR_VEC_GETI(v2, i);
    }
  }
  return acc;
}

__HOSTDEV__ void augur_dotprod_grad_0(AugurVec_t* dst, real_t adj, AugurVec_t* v1, AugurVec_t* v2) {
  switch (v2->ty) {
  case AUGUR_INT: {
    for (uint_t i = 0; i < dst->elems; i++) {
      AUGUR_VEC_SETD(dst, i, adj * AUGUR_VEC_GETI(v2, i));
    }
    break;
  }
  case AUGUR_DBL: {
    for (uint_t i = 0; i < dst->elems; i++) {
      AUGUR_VEC_SETD(dst, i, adj * AUGUR_VEC_GETD(v2, i));
    }
    break;
  }
  default: {
    // TODO: ERROR 
    break;
  }
  }
}
