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
#include "augur_blkop.h"
#include "augur_math.h"
#include "augur_dist.h"


#ifndef AUGURCPU
__global__ void rngSetupKernel(unsigned int seed, curandState_t* states) {
  curand_init(seed, blockIdx.x, 0, &states[blockIdx.x]);
}
#endif

augur_rng* augur_rng_setup(uint_t seed) {
#ifdef AUGURCPU
  const gsl_rng_type * T;
  gsl_rng * r;
  gsl_rng_env_setup();
  T = gsl_rng_default;
  r = gsl_rng_alloc (T);
  return r;
#else
  curandState_t* states;
  cudaMalloc((void**) &states, 30000 * sizeof(curandState_t));
  rngSetupKernel<<<30000, 1>>>(seed, states);
  return states;
#endif
}


/* Univariate discrete distributions */


/* Bernoulli */

__HOSTDEV__ real_t augur_bernoulli_pdf(int pt, real_t bias) {
  return (pt * bias + (1 - pt) * (1.0 - bias));
}

__HOSTDEV__ real_t augur_bernoulli_ll(int pt, real_t bias) {
  if (bias >= 0.0 && bias <= 1.0) {
    return augur_log(augur_bernoulli_pdf(pt, bias));
  }
  else {
    return -LARGE_POS_NUM;
  }
}

__HOSTDEV__ real_t augur_bernoulli_dotarg1(int pt, real_t bias) {
  real_t denom = (pt * bias + (1 - pt) * (1.0 - bias));
  return (pt + pt - 1) / denom;
}

__HOSTORDEV__ int augur_bernoulli_sample(augur_rng* rng, real_t bias) {
  real_t u = augur_std_uniform_sample(rng);
  if (u < bias) {
    return 1;
  }
  else {
    return 0;
  }
}


/* Categorical */

__HOSTDEV__ real_t augur_categorical_pdf(int pt, AugurVec_t* pmf) {
  return AUGUR_VEC_GETD(pmf, pt);
}

__HOSTDEV__ real_t augur_categorical_ll(int pt, AugurVec_t* pmf) {
  return augur_log(augur_categorical_pdf(pt, pmf));
}

__HOSTORDEV__ int augur_categorical_sample(augur_rng* rng, AugurVec_t* pmf) {
  real_t u = augur_std_uniform_sample(rng);
  real_t acc = AUGUR_VEC_GETD(pmf, 0);
  for (int i = 1; i < pmf->elems; i++) {
    if (u < acc) {
      return i - 1;
    }
    acc += AUGUR_VEC_GETD(pmf, i);
  }
  return pmf->elems - 1;
}


/* Poisson */

__HOSTDEV__ real_t augur_poisson_pdf(int pt, real_t rate) {
  // TODO: Can be optimized
  return augur_exp(augur_poisson_ll(pt, rate));
}

__HOSTDEV__ real_t augur_poisson_ll(int pt, real_t rate) {
  real_t acc = 0.0;
  for (int k = 1; k <= pt; k++) {
    acc += augur_log(k);
  }
  return pt * augur_log(rate) - rate - acc;
}

__HOSTDEV__ real_t augur_poisson_dotarg1(int pt, real_t rate) {
  if (rate > 0.0) {
    return pt / rate - 1.0;
  }
  else {
    return -LARGE_POS_NUM;
  }
}

__HOSTORDEV__ int augur_poisson_sample(augur_rng* rng, real_t rate) {
  real_t L = augur_exp(-rate);
  int k = 0;
  int p = 1;
  do {
    k++;
    p *= augur_std_uniform_sample(rng);
  } while ( p > L);
  return k - 1;
}


/* Univariate continuous distributions */


/* Beta */

__HOSTDEV__ real_t augur_beta_pdf(real_t pt, real_t alpha, real_t beta) {
  return augur_exp(augur_lgamma(alpha + beta)) / (augur_exp(augur_lgamma(alpha) + augur_lgamma(beta))) * augur_pow(pt, alpha - 1.0) * augur_pow(1.0 - pt, beta - 1.0);
}

__HOSTDEV__ real_t augur_beta_ll(real_t pt, real_t alpha, real_t beta) {
  // TODO: Can be optimized
  return augur_log(augur_beta_pdf(pt, alpha, beta));
}

__HOSTDEV__ real_t augur_beta_dotpt(real_t pt, real_t alpha, real_t beta) {
  return (alpha - 1.0) / pt - (beta - 1.0) / (1.0 - pt);
}

__HOSTDEV__ real_t augur_beta_dotarg1(real_t pt, real_t alpha, real_t beta) {
  return augur_digamma(alpha + beta) - augur_digamma(alpha) + augur_log(pt);
}

__HOSTDEV__ real_t augur_beta_dotarg2(real_t pt, real_t alpha, real_t beta) {
  return augur_digamma(alpha + beta) - augur_digamma(beta) + augur_log(1.0 - pt);
}

__HOSTORDEV__ real_t augur_beta_sample(augur_rng* rng, real_t alpha, real_t beta) {
  real_t x = augur_gamma_sample(rng, alpha, 1.0);
  real_t y = augur_gamma_sample(rng, beta, 1.0);
  return x / (x + y);
}

__HOSTORDEV__ real_t augur_beta_bernoulli_conj(augur_rng* rng, int sum, int N, real_t alpha, real_t beta) {
  real_t _alpha = alpha + (real_t) sum;
  real_t _beta = beta + ((real_t) (N - sum));
  return augur_beta_sample(rng, _alpha, _beta);
}

__HOSTORDEV__ real_t augur_beta_geometric_conj(augur_rng* rng, int sum, int N, real_t alpha, real_t beta) {
  real_t _alpha = alpha + (real_t) N;
  real_t _beta = beta + (real_t) sum;
  return augur_beta_sample(rng, _alpha, _beta);
}


/* Chi-squared */

__HOSTDEV__ real_t augur_chisquared_pdf(real_t pt, int df) {
  // TODO: Can be optimized
  return augur_exp(augur_chisquared_ll(pt, df));
}

__HOSTDEV__ real_t augur_chisquared_ll(real_t pt, int df) {
  real_t half_df = df / 2.0;
  return (-half_df) * augur_log(2.0) - augur_lgamma(half_df) + (half_df - 1.0) * pt - pt / 2.0;
}

__HOSTDEV__ real_t augur_chisquared_dotpt(real_t pt, int df) {
  // TODO: IMPLEMENT ME
  return 0.0;
}

__HOSTDEV__ real_t augur_chisquared_sample(augur_rng* rng, int df) {
  real_t chi = 0.0;
  for (int i = 0; i < df; i++) {
    double tmp = augur_std_normal_sample(rng);
    chi += tmp * tmp;
  }
  return chi;
}


/* Exponential */

__HOSTDEV__ real_t augur_exponential_pdf(real_t pt, real_t rate) {
  return rate * augur_exp(-rate * pt);
}

__HOSTDEV__ real_t augur_exponential_ll(real_t pt, real_t rate) {
  if (rate > 0.0 && pt > 0.0) {
    return augur_log(rate) - rate * pt;
  }
  else {
    return -LARGE_POS_NUM;
  }
}

__HOSTDEV__ real_t augur_exponential_dotpt(real_t pt, real_t rate) {
  if (rate > 0.0 && pt > 0.0) {
    return -rate;
  }
  else {
    return 0.0;
  }
}

__HOSTDEV__ real_t augur_exponential_dotarg1(real_t pt, real_t rate) {
  if (rate > 0.0 && pt > 0.0) {
    return 1.0 / rate - pt;
  }
  else {
    return 0.0;
  }
}

__HOSTORDEV__ real_t augur_exponential_sample(augur_rng* rng, real_t rate) {
  real_t u = augur_std_uniform_sample(rng);
  return -augur_log(u) / rate;
}


/* Gamma */

__HOSTDEV__ real_t augur_gamma_pdf(real_t pt, real_t shape, real_t rate) {
  return augur_pow(rate, shape) / augur_exp(augur_lgamma(shape)) * augur_pow(pt, shape - 1.0) * augur_exp(-rate * pt);
}

__HOSTDEV__ real_t augur_gamma_ll(real_t pt, real_t shape, real_t rate) {
  // TODO: Can be optimized
  return augur_log(augur_gamma_pdf(pt, shape, rate));
}

__HOSTDEV__ real_t augur_gamma_dotpt(real_t pt, real_t shape, real_t rate) {
  return ((shape - 1.0) / pt) - rate;
}

__HOSTDEV__ real_t augur_gamma_dotarg1(real_t pt, real_t shape, real_t rate) {
  return -augur_digamma(shape) + augur_log(rate) + augur_log(pt);
}

__HOSTDEV__ real_t augur_gamma_dotarg2(real_t pt, real_t shape, real_t rate) {
  return (shape / rate) - pt;
}

__HOSTORDEV__ real_t augur_gamma_ge1_1_sample(augur_rng* rng, real_t alpha) {
  // Marsaglia-Tang Method
  real_t d = alpha - 1.0 / 3.0;
  real_t c = 1.0 / augur_sqrt(9 * d);
  while (1) {
    real_t z = augur_std_normal_sample(rng);
    real_t u = augur_std_uniform_sample(rng);
    real_t V = (1.0 + c * z) * (1.0 + c * z) * (1.0 + c * z);
    
    if (z > -1.0 / c && augur_log(u) < (0.5 * z * z) + d - (d * V) + (d * augur_log(V))) {
      return d * V;
    }
  }
}

__HOSTORDEV__ real_t augur_gamma_sample(augur_rng* rng, real_t shape, real_t rate) {
  real_t scale = 1.0 / rate;
  if (shape < 1.0) {
    real_t x = augur_gamma_ge1_1_sample(rng, shape + 1.0);
    real_t u = augur_std_uniform_sample(rng);
    return (x * augur_pow(u, 1.0 / shape)) * scale;
  }
  else {
    return augur_gamma_ge1_1_sample(rng, shape) * scale;
  }
}

__HOSTORDEV__ real_t augur_gamma_poisson_conj(augur_rng* rng, int sum, int N, real_t shape, real_t rate) {
  real_t _shape = shape + (real_t) sum;
  real_t _rate = rate + (real_t) N;
  return augur_gamma_sample(rng, _shape, _rate);
}

__HOSTORDEV__ real_t augur_gamma_exponential_conj(augur_rng* rng, real_t sum, int N, real_t shape, real_t rate) {
  real_t _shape = shape + (real_t) N;
  real_t _rate = rate + sum;
  return augur_gamma_sample(rng, _shape, _rate);
}

__HOSTORDEV__ real_t augur_gamma_gammashape_conj(augur_rng* rng, real_t sum, real_t shapeN, real_t shape, real_t rate) {
  real_t _shape = shape + shapeN;
  real_t _rate = rate + sum;
  return augur_gamma_sample(rng, _shape, _rate);
}

__HOSTORDEV__ real_t augur_gamma_invgamma_conj(augur_rng* rng, real_t invsum, real_t shapeN, real_t shape, real_t rate) {
  real_t _shape = shape + shapeN;
  real_t _rate = rate + invsum;
  return augur_gamma_sample(rng, _shape, _rate);  
}


/* Inverse Gamma */

__HOSTDEV__ real_t augur_invgamma_pdf(real_t pt, real_t shape, real_t scale) {
  return (augur_pow(scale, shape) / augur_exp(augur_lgamma(shape))) * augur_pow(pt, -shape - 1.0) * augur_exp(-scale / pt);
}

__HOSTDEV__ real_t augur_invgamma_ll(real_t pt, real_t shape, real_t scale) {
  return shape * augur_log(scale) - augur_lgamma(shape) - (shape + 1.0) * augur_log(pt) - scale / pt;
}

__HOSTDEV__ real_t augur_invgamma_dotpt(real_t pt, real_t shape, real_t scale) {
  return -(shape + 1.0) / pt + scale / (pt * pt);
}

__HOSTDEV__ real_t augur_invgamma_dotarg1(real_t pt, real_t shape, real_t scale) {
  return augur_log(scale) - augur_digamma(shape) - augur_log(pt);
}

__HOSTDEV__ real_t augur_invgamma_dotarg2(real_t pt, real_t shape, real_t scale) {
  return shape / scale - 1.0 / pt;
}

__HOSTORDEV__ real_t augur_invgamma_sample(augur_rng* rng, real_t shape, real_t scale) {
  return 1.0 / augur_gamma_sample(rng, shape, 1.0 / scale);
}


/* Normal */

__HOSTDEV__ real_t augur_normal_pdf(real_t pt, real_t mean, real_t var) {
  return 1.0 / (augur_sqrt(2.0 * AUGUR_PI * var)) * augur_exp(-(pt - mean) * (pt - mean) / (2.0 * var)); 
}

__HOSTDEV__ real_t augur_normal_ll(real_t pt, real_t mean, real_t var) {
  // TODO: normalizing constant?
  if (var > 0.0) {
    real_t t = pt - mean;
    return (-augur_log(var)) - (0.5 * t * t / var);
  }
  else {
    return -LARGE_POS_NUM;
  }
}

__HOSTDEV__ real_t augur_normal_dotpt(real_t pt, real_t mean, real_t var) {
  return -(pt - mean) / var;
}

__HOSTDEV__ real_t augur_normal_dotarg1(real_t pt, real_t mean, real_t var) {
  return (pt - mean) / var;
}

__HOSTDEV__ real_t augur_normal_dotarg2(real_t pt, real_t mean, real_t var) {
  real_t t = pt - mean;
  return (-0.5 * var) + (0.5 * (t * t) / (var * var));
}

__HOSTORDEV__ real_t augur_std_normal_sample(augur_rng* rng) {
#ifdef AUGURCPU
  return gsl_ran_gaussian_ziggurat(rng, 1.0);
#else
  return curand_normal_double(rng);
#endif
}

__HOSTORDEV__ real_t augur_normal_sample(augur_rng* rng, real_t mean, real_t var) {
  return mean + augur_sqrt(var) * augur_std_normal_sample(rng);
}

__HOSTORDEV__ real_t augur_normal_normalmean_conj(augur_rng* rng, real_t sum, int cnt, real_t mean0, real_t var0, real_t var) {
  real_t _var = 1.0 / (1.0 / var0 + cnt / var);
  real_t _mean = (mean0 / var0 + sum / var) * _var;
  return augur_normal_sample(rng, _mean, _var);
}


/* Uniform */

__HOSTDEV__ real_t augur_uniform_pdf(real_t pt, real_t l, real_t u) {
  if (l < pt && pt < u) {
    return 1.0 / (u - l);
  }
  else {
    return 0.0;
  }
}

__HOSTDEV__ real_t augur_uniform_ll(real_t pt, real_t l, real_t u) {
  if (l < pt && pt < u) {
    return -augur_log(u - l);
  }
  else {
    return -LARGE_POS_NUM;
  }
}

__HOSTDEV__ real_t augur_uniform_dotpt(real_t pt, real_t l, real_t u) {
  return 0.0;
}

__HOSTDEV__ real_t augur_uniform_dotarg1(real_t pt, real_t l, real_t u) {
  if (l < pt && pt < u) {
    return 1.0 / (u - l);
  }
  else {
    return 0.0;
  }
}

__HOSTDEV__ real_t augur_uniform_dotarg2(real_t pt, real_t l, real_t u) {
  if (l < pt && pt < u) {
    return -1.0 / (u - l);
  }
  else {
    return 0.0;
  }
}

__HOSTORDEV__ real_t augur_std_uniform_sample(augur_rng* rng) {
#ifdef AUGURCPU
  return gsl_rng_uniform(rng);
#else
  return curand_uniform_double(rng);
#endif  
}

__HOSTORDEV__ real_t augur_uniform_sample(augur_rng *rng, real_t l, real_t u) {
  real_t u2 = augur_std_uniform_sample(rng);
  return l + u2 * (u - l);
}




/* Dirichlet */

__HOSTDEV__ real_t augur_dirichlet_pdf(AugurVec_t* pt, AugurVec_t* alpha) {
  real_t sum = 0.0;
  real_t lgamma_sum = 0.0;
  real_t acc = 0.0;
  for (int i = 0; i < alpha->elems; i++) {
    real_t a = AUGUR_VEC_GETD(alpha, i);
    sum += a;
    lgamma_sum += augur_lgamma(a);
    acc *= augur_pow(AUGUR_VEC_GETD(pt, i), a - 1.0);
  }
  return acc * augur_exp(augur_lgamma(sum)) / augur_exp(lgamma_sum);
}

__HOSTDEV__ real_t augur_dirichlet_ll(AugurVec_t* pt, AugurVec_t* alpha) {
  // TODO: Does not give normalizing constant
  real_t acc = 0.0;
  for (int i = 0; i < alpha->elems; i++) {
    real_t a = AUGUR_VEC_GETD(alpha, i);
    acc += (a - 1.0) * augur_log(AUGUR_VEC_GETD(pt, i));
  }
  return acc;
}

__HOSTDEV__ void augur_dirichlet_dotpt(AugurVec_t* dst, AugurVec_t* pt, AugurVec_t* alpha) {
  for (int i = 0; i < pt->elems; i++) {
    AUGUR_VEC_SETD(dst, i, (AUGUR_VEC_GETD(alpha, i) - 1.0) / (AUGUR_VEC_GETD(pt, i)));
  }
}

__HOSTDEV__ void augur_dirichlet_dotarg1(AugurVec_t* dst, AugurVec_t* pt, AugurVec_t* alpha) {
  real_t acc = 0.0;
  for (int i = 0; i < alpha->elems; i++) {
    acc += AUGUR_VEC_GETD(alpha, i);
  }
  for (int i = 0; i < alpha->elems; i++) {
    AUGUR_VEC_SETD(dst, i, augur_digamma(acc) - augur_digamma(AUGUR_VEC_GETD(alpha, i)) + augur_log(AUGUR_VEC_GETD(pt, i)));
  }
}

__HOSTORDEV__ void dirichlet_sample(augur_rng* rng, AugurVec_t* dst, AugurVec_t* alpha) {
  real_t acc = 0.0;
  for (int i = 0; i < dst->elems; i++) {
    real_t g = augur_gamma_sample(rng, AUGUR_VEC_GETD(alpha, i), 1.0);
    AUGUR_VEC_SETD(dst, i, g);
    acc += g;
  }
  for (int i = 0; i < dst->elems; i++) {
    AUGUR_VEC_SETD(dst, i, AUGUR_VEC_GETD(dst, i) / acc);
  }
}

__HOSTORDEV__ void augur_dirichlet_categorical_conj(augur_rng* rng, AugurVec_t* dst, AugurVec_t* alpha, AugurVec_t* cnts) {
  // Sample Gamma's
  real_t acc = 0.0;
  for (int i = 0; i < dst->elems; i++) {
    real_t tmp = augur_gamma_sample(rng, AUGUR_VEC_GETD(alpha, i) + (real_t) AUGUR_VEC_GETI(cnts, i), 1.0);
    AUGUR_VEC_SETD(dst, i, tmp);
    acc += tmp;
  }
  // Normalize
  for (int i = 0; i < dst->elems; i++) {
    AUGUR_VEC_SETD(dst, i, AUGUR_VEC_GETD(dst, i) / acc);
  }
}


/* Multivariate Normal */

__HOSTDEV__ real_t augur_mvnormal_pdf(AugurVec_t* pt, AugurVec_t* mu, AugurMat_t* cov, AugurMat_t* L, AugurVec_t* work) {
  // TODO: Can be optimized
  return augur_exp(augur_mvnormal_ll(pt, mu, cov, L, work));
}

__HOSTDEV__ real_t augur_mvnormal_ll(AugurVec_t* pt, AugurVec_t* mean, AugurMat_t* cov, AugurMat_t* L, AugurVec_t* work) {
  augur_vec_minus(work, pt, mean);
   
  augur_mat_cpy(L, cov);
  augur_mat_cholesky(L);
  
  real_t det_cov = 1.0;
  for (int i = 0; i < L->col; i++) {
    det_cov *= AUGUR_MAT_GETD(L, i, i);
  }
  det_cov *= det_cov;

  augur_mat_lower_trmi(L);
  augur_mat_trvm(L, work);
  real_t expon = augur_vec_dot(work, work);

  // WARNING: L->col is unsigned int ... 
  real_t c = -0.5 * ((double) L->col) * augur_log(2.0 * AUGUR_PI);
  
  return c - 0.5 * augur_log(det_cov) - 0.5 * expon;
}

__HOSTDEV__ void augur_mvnormal_dotpt(AugurVec_t* dst, AugurVec_t* pt, AugurVec_t* mu, AugurMat_t* cov, AugurMat_t* L, AugurVec_t* work) {
  // -cov^{-1} * (x - mu) = cov^{-1} * (mu - x)
  augur_vec_minus(work, mu, pt);
  augur_mat_cpy(L, cov);
  augur_mat_cholesky(L);
  augur_mat_cholesky_invert(L);
  augur_mat_mvm(dst, L, work);
}

__HOSTDEV__ void augur_mvnormal_dotarg1(AugurVec_t* dst, AugurVec_t* pt, AugurVec_t* mu, AugurMat_t* cov, AugurMat_t* L, AugurVec_t* work) {
  // cov^{-1} * (x - mu)
  augur_vec_minus(work, pt, mu);
  augur_mat_cpy(L, cov);
  augur_mat_cholesky(L);
  augur_mat_cholesky_invert(L);
  augur_mat_mvm(dst, L, work);
}

__HOSTDEV__ void augur_mvnormal_dotarg2(AugurMat_t* dst, AugurVec_t* pt, AugurVec_t* mu, AugurMat_t* cov, AugurMat_t* L, AugurVec_t* work) {
  // dst contains cov^{-1}
  augur_mat_cpy(dst, cov);
  augur_mat_cholesky(dst);
  augur_mat_cholesky_invert(dst);

  AugurVec_t view_work2 = augur_mat_view_as_row_vec(L, 0);
  AugurVec_t* work2 = &view_work2;

  // work = pt - mu
  augur_vec_minus(work, pt, mu);

  // work2 = cov^{-1} * work
  augur_mat_mvm(work2, dst, work);

  // work = work' * cov^{-1}
  augur_mat_vmm(work, work, dst);

  // cov^{-1} - work2 * work
  for (uint_t i = 0; i < dst->row; i++) {
    for (uint_t j = 0; j < dst->col; j++) {
      real_t tmp = AUGUR_MAT_GETD(dst, i, j) - AUGUR_VEC_GETD(work2, i) * AUGUR_VEC_GETD(work, j);
      AUGUR_MAT_SETD(dst, i, j, -0.5 * tmp);
    }
  }
}


__HOSTORDEV__ void augur_std_mvnormal_sample(augur_rng* rng, AugurVec_t* result) {
  for (int i = 0; i < result->elems; i++) {
    AUGUR_VEC_SETD(result, i, augur_std_normal_sample(rng));
  }
}

__HOSTORDEV__ void augur_mvnormal_sample(augur_rng* rng, AugurVec_t* result, AugurVec_t* mean, AugurMat_t* cov, AugurMat_t* L, AugurVec_t* work) {
  augur_std_mvnormal_sample(rng, result);
  augur_mat_cpy(L, cov);
  augur_mat_cholesky(L);
  augur_mat_trvm(L, result);
  for (int i = 0; i < result->elems; i++) {
    AUGUR_VEC_SETD(result, i, AUGUR_VEC_GETD(result, i) + AUGUR_VEC_GETD(mean, i));
  }
}


__HOSTORDEV__ void augur_mvnormal_mvnormal_conj(augur_rng* rng, AugurVec_t* result, AugurVec_t* mu0, AugurMat_t* cov0, AugurVec_t* conjmean, int cnt, AugurMat_t* cov, AugurMat_t* L0, AugurVec_t* work0, AugurMat_t* L) {
  // cov0^{-1}
  augur_mat_cpy(L0, cov0);
  augur_mat_cholesky(L0);
  augur_mat_cholesky_invert(L0);
  
  // n cov^{-1}
  augur_mat_cpy(L, cov);
  augur_mat_cholesky(L);
  augur_mat_cholesky_invert(L);
  augur_mat_ms(cnt, L);
  
  // cov0^{-1} mu_0
  augur_mat_mvm(result, L0, mu0);

  // n cov^{-1} mu
  if (cnt == 0) {
    augur_vec_scale(1.0, conjmean);
  }
  else {
    augur_vec_scale(1.0 / cnt, conjmean);
  }
  augur_mat_mvm(work0, L, conjmean);
  
  // v = cov0^{-1} mu_0 + n cov^{-1} mu
  augur_vec_plus(result, result, work0);
  
  // A = (n cov^{-1} + cov0^{-1})^{-1}
  augur_mat_plus(L0, L0, L);
  augur_mat_cholesky(L0);
  augur_mat_cholesky_invert(L0);
  
  // A v
  augur_mat_mvm(work0, L0, result);

  augur_mvnormal_sample(rng, result, work0, L0, L, work0);
}


/* Inverse Wishart */

__HOSTDEV__ real_t augur_invwishart_pdf(AugurMat_t* pt, int df, AugurMat_t* scale, AugurMat_t* L, AugurMat_t* work) {
  // TODO: Can be optimized
  return augur_exp(augur_invwishart_ll(pt, df, scale, L, work));
}

__HOSTDEV__ real_t augur_invwishart_ll(AugurMat_t* pt, int df, AugurMat_t* scale, AugurMat_t* L, AugurMat_t* work) {
  int d = L->col;

  augur_mat_cpy(L, scale);
  augur_mat_cholesky(L);
  real_t detPsi = 1.0;
  for (int i = 0; i < d; i++) {
    detPsi *= AUGUR_MAT_GETD(L, i, i);
  }
  detPsi *= detPsi;

  augur_mat_cpy(work, pt);
  augur_mat_cholesky(work);
  real_t detX = 1.0;
  for (int i = 0; i < d; i++) {
    detX *= AUGUR_MAT_GETD(work, i, i);
  }
  detX *= detX;

  augur_mat_cholesky_invert(work);
  real_t tr = 0.0;
  for (int i = 0; i < d; i++) {
    real_t tmp = 0.0;
    for (int j = 0; j < d; j++) {
      tmp += AUGUR_MAT_GETD(scale, i, j) * AUGUR_MAT_GETD(work, j, i);
    }
    tr += tmp;
  }

  real_t c = -df * d / 2.0 * augur_log(2.0) - augur_multi_lgamma(d, df / 2.0);

  return c - (df + d + 1.0) / 2.0 * augur_log(detX) + df / 2.0 * augur_log(detPsi) - 0.5 * tr;
}

__HOSTDEV__ void augur_invwishart_dotpt(AugurMat_t* dst, AugurMat_t* pt, int df, AugurMat_t* scale, AugurMat_t* L, AugurMat_t* work) {
  // TODO: IMPLEMENT ME
}

__HOSTDEV__ void augur_invwishart_dotarg2(AugurMat_t* dst, AugurMat_t* pt, int df, AugurMat_t* scale, AugurMat_t* L, AugurMat_t* work) {
  // TODO: IMPLEMENT ME
}


__HOSTORDEV__ void bartlett_decomp(augur_rng* rng, int n, AugurMat_t* result) {
  int d = result->col;
  for (int i = 0; i < d; i++) {
    for (int j = 0; j < d; j++) {
      if (j < i) {
	AUGUR_MAT_SETD(result, i, j, augur_std_normal_sample(rng));
      }
      else if (j > i) {
	AUGUR_MAT_SETD(result, i, j, 0.0);
      }
      else {
	// WARNING: not n - i + 1 because i starts at 0, not 1
	AUGUR_MAT_SETD(result, i, j, augur_sqrt(augur_chisquared_sample(rng, n - i)));
      }
    }
  }
}

__HOSTORDEV__ void augur_invwishart_sample(augur_rng* rng, AugurMat_t* result, int df, AugurMat_t* scale, AugurMat_t* L) {
  augur_mat_cpy(L, scale);
  augur_mat_cholesky(L);
  augur_mat_cholesky_invert(L);
  augur_mat_cholesky(L);

  bartlett_decomp(rng, df, result);
  augur_mat_trtrmm(L, result);
  augur_mat_cholesky_invert(result);
}

__HOSTORDEV__ void augur_invwishart_mvnormal_conj(augur_rng* rng, AugurMat_t* result, int df, AugurMat_t* scale, AugurMat_t* conj, int cnt, AugurVec_t* diff, AugurMat_t* L, AugurMat_t* work) {
  int d = scale->col * scale->col;
  for (int i = 0; i < d; i++) {
    ((real_t*) conj->data)[i] += ((real_t*) scale->data)[i];
  }
  augur_invwishart_sample(rng, result, cnt + df, conj, L);
}


/* Extra */

real_t h_augur_uniform_sample(gsl_rng* rng, real_t lower, real_t upper) {
  real_t u = gsl_rng_uniform(rng);
  return lower + u * (upper - lower);
}

#ifndef AUGURCPU
__global__ void hRandNormVecKernel(augur_rng *rng, AugurBlk_t b) {
  size_t idx = blockIdx.x * blockDim.x + threadIdx.x;
  int size = b.dataLen;
  if (idx < size) {
    ((double*) b.base_data)[idx] = augur_std_normal_sample(rng + idx);
    return;
  }
}
#endif

void hi_augur_arrd_randn(augur_rng *rng, double* arr, uint_t elems) {
  for (uint_t i = 0; i < elems; i++) {
    arr[i] = augur_std_normal_sample(rng);
  }
}

void h_augur_blk_randn(augur_rng *rng, AugurBlk_t* blk) {
#ifdef AUGURCPU
  for (uint_t i = 0; i < blk->num_blks; i++) {
    uint_t elems = h_augur_blk_obj2elems(blk->typs[i], blk->blks[i]);
    void* data = h_augur_blk_obj_get_data(blk->typs[i], blk->blks[i]);
    hi_augur_arrd_randn(rng, (double*) data, elems);
  }
#else
  // TODO: IMPLEMENT ME
  // hRandNormVecKernel<<<BLKS(b->num_blks), THRDS >>>(rng, *b);
#endif
}

__HOSTORDEV__ int augur_unnorm_disc_samp(augur_rng *rng, AugurVec_t* pmf) {
  real_t u = augur_std_uniform_sample(rng);
  real_t norm = 0.0;
  for (int i = 0; i < pmf->elems; i++) {
    norm += AUGUR_VEC_GETD(pmf, i);
  }

  real_t acc = AUGUR_VEC_GETD(pmf, 0) / norm; 
  for (int i = 1; i < pmf->elems; i++) {
    if (u < acc) {
      return i - 1;
    }
    acc += AUGUR_VEC_GETD(pmf, i) / norm;
  }
  return pmf->elems - 1;
}
