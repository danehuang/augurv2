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

#ifndef AUGUR_MATH_H
#define AUGUR_MATH_H

#include "augur_hdr.h"
#include "augur_rtval.h"

#define AUGUR_PI 3.14159265358979323846264338327950288
#define LARGE_POS_NUM (1e8)


/* Numeric operations */

#define augur_plus_grad_0(x, y)      (1)
#define augur_plus_grad_1(x, y)      (1)

#define augur_minus_grad_0(x, y)     (1)
#define augur_minus_grad_1(x, y)     (-1)

#define augur_times_grad_0(x, y)     (y)
#define augur_times_grad_1(x, y)     (x)

#define augur_div_grad_0(x, y)       (1.0/(y))
#define augur_div_grad_1(x, y)       ((x) / ((y) * (y)))


__HOSTDEV__ real_t augur_pow(real_t base, real_t expon);


__HOSTDEV__ real_t augur_sqrt(real_t x);


__HOSTDEV__ real_t augur_exp(real_t x);


__HOSTDEV__ real_t augur_log(real_t x);


__HOSTDEV__ real_t augur_cos(real_t x);


__HOSTDEV__ real_t augur_sin(real_t x);


__HOSTDEV__ real_t augur_expit(real_t x);
__HOSTDEV__ real_t augur_expit_grad_0(real_t x);


__HOSTDEV__ real_t augur_logit(real_t x);


__HOSTDEV__ real_t augur_digamma(real_t x);


__HOSTDEV__ real_t augur_lgamma(real_t x);


__HOSTDEV__ real_t augur_multi_lgamma(int p, real_t x);


/* Derivatives */

__HOSTDEV__ real_t augur_dotpt_exp(real_t x);
__HOSTDEV__ real_t augur_dotpt_log(real_t x);
__HOSTDEV__ real_t augur_dotpt_cos(real_t x);
__HOSTDEV__ real_t augur_dotpt_sin(real_t x);
__HOSTDEV__ real_t augur_dotpt_expit(real_t x);
__HOSTDEV__ real_t augur_dotpt_logit(real_t x);


/* Vector operations */

__HOSTDEV__ real_t augur_dotprod(AugurVec_t* v1, AugurVec_t* v2);
__HOSTDEV__ void augur_dotprod_grad_0(AugurVec_t* dst, real_t adj, AugurVec_t* v1, AugurVec_t* v2);

// __HOSTDEV__ void augur_vec_logit(AugurVec_t* dst, AugurVec_t* src);


#endif
