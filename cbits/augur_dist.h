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

#ifndef AUGUR_DIST_H
#define AUGUR_DIST_H

#include "augur_hdr.h"
#include "augur_util.h"


/* Random number generator */

augur_rng* augur_rng_setup(uint_t seed);



/* Univariate discrete distributions */


/* Bernoulli */

__HOSTDEV__ real_t augur_bernoulli_pdf(int pt, real_t bias);
__HOSTDEV__ real_t augur_bernoulli_ll(int pt, real_t bias);
__HOSTDEV__ real_t augur_bernoulli_dotarg1(int pt, real_t bias);
__HOSTORDEV__ int augur_bernoulli_sample(augur_rng *rng, real_t bias);


/* Categorical */

__HOSTDEV__ real_t augur_categorical_pdf(int pt, AugurVec_t* pmf);
__HOSTDEV__ real_t augur_categorical_ll(int pt, AugurVec_t* pmf);
__HOSTORDEV__ int augur_categorical_sample(augur_rng *rng, AugurVec_t* pmf);


/* Poisson */

__HOSTDEV__ real_t augur_poisson_pdf(int pt, real_t rate);
__HOSTDEV__ real_t augur_poisson_ll(int pt, real_t rate);
__HOSTDEV__ real_t augur_poisson_dotarg1(int pt, real_t rate);
__HOSTORDEV__ int augur_poisson_sample(augur_rng *rng, real_t rate);



/* Univariate continuous distributions */


/* Beta */

__HOSTDEV__ real_t augur_beta_pdf(real_t pt, real_t alpha, real_t beta);
__HOSTDEV__ real_t augur_beta_ll(real_t pt, real_t alpha, real_t beta);
__HOSTDEV__ real_t augur_beta_dotpt(real_t pt, real_t alpha, real_t beta);
__HOSTDEV__ real_t augur_beta_dotarg1(real_t pt, real_t alpha, real_t beta);
__HOSTDEV__ real_t augur_beta_dotarg2(real_t pt, real_t alpha, real_t beta);
__HOSTORDEV__ real_t augur_beta_sample(augur_rng* rng, real_t alpha, real_t beta);
__HOSTORDEV__ real_t augur_beta_bernoulli_conj(augur_rng* rng, int sum, int N, real_t alpha, real_t beta);
__HOSTORDEV__ real_t augur_beta_geometric_conj(augur_rng* rng, int sum, int N, real_t alpha, real_t beta);


/* Chi-squared */

__HOSTDEV__ real_t augur_chisquared_pdf(real_t pt, int df);
__HOSTDEV__ real_t augur_chisquared_ll(real_t pt, int df);
__HOSTDEV__ real_t augur_chisquared_dotpt(real_t pt, int df);
__HOSTORDEV__ real_t augur_chisquared_sample(augur_rng* rng, int df);
  

/* Exponential */

__HOSTDEV__ real_t augur_exponential_pdf(real_t pt, real_t rate);
__HOSTDEV__ real_t augur_exponential_ll(real_t pt, real_t rate);
__HOSTDEV__ real_t augur_exponential_dotpt(real_t pt, real_t rate);
__HOSTDEV__ real_t augur_exponential_dotarg1(real_t pt, real_t rate);
__HOSTORDEV__ real_t augur_exponential_sample(augur_rng* rng, real_t rate);


/* Gamma */

__HOSTDEV__ real_t augur_gamma_pdf(real_t pt, real_t shape, real_t rate);
__HOSTDEV__ real_t augur_gamma_ll(real_t pt, real_t shape, real_t rate);
__HOSTDEV__ real_t augur_gamma_dotpt(real_t pt, real_t shape, real_t rate);
__HOSTDEV__ real_t augur_gamma_dotarg1(real_t pt, real_t shape, real_t rate);
__HOSTDEV__ real_t augur_gamma_dotarg2(real_t pt, real_t shape, real_t rate);
__HOSTORDEV__ real_t augur_gamma_ge1_1_sample(augur_rng* rng, real_t alpha);
__HOSTORDEV__ real_t augur_gamma_sample(augur_rng* rng, real_t shape, real_t rate);
__HOSTORDEV__ real_t augur_gamma_poisson_conj(augur_rng* rng, int sum, int N, real_t shape, real_t rate);
__HOSTORDEV__ real_t augur_gamma_exponential_conj(augur_rng* rng, real_t sum, int N, real_t shape, real_t rate);
__HOSTORDEV__ real_t augur_gamma_gammashape_conj(augur_rng* rng, real_t sum, real_t shapeN, real_t shape, real_t rate);
__HOSTORDEV__ real_t augur_gamma_invgamma_conj(augur_rng* rng, real_t invsum, real_t shapeN, real_t shape, real_t rate);


/* Inverse Gamma */

__HOSTDEV__ real_t augur_invgamma_pdf(real_t pt, real_t shape, real_t scale);
__HOSTDEV__ real_t augur_invgamma_ll(real_t pt, real_t shape, real_t scale);
__HOSTDEV__ real_t augur_invgamma_dotpt(real_t pt, real_t shape, real_t scale);
__HOSTDEV__ real_t augur_invgamma_dotarg1(real_t pt, real_t shape, real_t scale);
__HOSTDEV__ real_t augur_invgamma_dotarg2(real_t pt, real_t shape, real_t scale);
__HOSTORDEV__ real_t augur_invgamma_sample(augur_rng* rng, real_t shape, real_t scale);



/* Normal */

__HOSTDEV__ real_t augur_normal_pdf(real_t pt, real_t mean, real_t var);
__HOSTDEV__ real_t augur_normal_ll(real_t pt, real_t mean, real_t var);
__HOSTDEV__ real_t augur_normal_dotpt(real_t pt, real_t mean, real_t var);
__HOSTDEV__ real_t augur_normal_dotarg1(real_t pt, real_t mean, real_t var);
__HOSTDEV__ real_t augur_normal_dotarg2(real_t pt, real_t mean, real_t var);
__HOSTORDEV__ real_t augur_std_normal_sample(augur_rng* rng);
__HOSTORDEV__ real_t augur_normal_sample(augur_rng* rng, real_t mean, real_t var);
__HOSTORDEV__ real_t augur_normal_normalmean_conj(augur_rng* rng, real_t sum, int cnt, real_t mean0, real_t var0, real_t var);


/* Uniform */

__HOSTDEV__ real_t augur_uniform_pdf(real_t pt, real_t l, real_t u);
__HOSTDEV__ real_t augur_uniform_ll(real_t pt, real_t l, real_t u);
__HOSTDEV__ real_t augur_uniform_dotpt(real_t pt, real_t l, real_t u);
__HOSTDEV__ real_t augur_uniform_dotarg1(real_t pt, real_t l, real_t u);
__HOSTDEV__ real_t augur_uniform_dotarg2(real_t pt, real_t l, real_t u);
__HOSTORDEV__ real_t augur_std_uniform_sample(augur_rng* rng);
__HOSTORDEV__ real_t augur_uniform_sample(augur_rng *rng, real_t l, real_t u);



/* Multivariate distributions */


/* Dirichlet */

__HOSTDEV__ real_t augur_dirichlet_pdf(AugurVec_t* pt, AugurVec_t* alpha);
__HOSTDEV__ real_t augur_dirichlet_ll(AugurVec_t* pt, AugurVec_t* alpha);
__HOSTDEV__ void augur_dirichlet_dotpt(AugurVec_t* dst, AugurVec_t* pt, AugurVec_t* alpha);
__HOSTDEV__ void augur_dirichlet_dotarg1(AugurVec_t* dst, AugurVec_t* pt, AugurVec_t* alpha);
__HOSTORDEV__ void dirichlet_sample(augur_rng* rng, AugurVec_t* dst, AugurVec_t* alpha);
__HOSTORDEV__ void augur_dirichlet_categorical_conj(augur_rng* rng, AugurVec_t* dst, AugurVec_t* alpha, AugurVec_t* cnts);



/* Multivariate Normal */

__HOSTDEV__ real_t augur_mvnormal_pdf(AugurVec_t* pt, AugurVec_t* mu, AugurMat_t* cov, AugurMat_t* L, AugurVec_t* work);
__HOSTDEV__ real_t augur_mvnormal_ll(AugurVec_t* pt, AugurVec_t* mean, AugurMat_t* cov, AugurMat_t* L, AugurVec_t* work);
__HOSTDEV__ void augur_mvnormal_dotpt(AugurVec_t* dst, AugurVec_t* pt, AugurVec_t* mu, AugurMat_t* cov, AugurMat_t* L, AugurVec_t* work);
__HOSTDEV__ void augur_mvnormal_dotarg1(AugurVec_t* dst, AugurVec_t* pt, AugurVec_t* mu, AugurMat_t* cov, AugurMat_t* L, AugurVec_t* work);
__HOSTDEV__ void augur_mvnormal_dotarg2(AugurMat_t* dst, AugurVec_t* pt, AugurVec_t* mu, AugurMat_t* cov, AugurMat_t* L, AugurVec_t* work);
__HOSTORDEV__ void augur_std_mvnormal_sample(augur_rng* rng, AugurVec_t* result);
__HOSTORDEV__ void augur_mvnormal_sample(augur_rng* rng, AugurVec_t* result, AugurVec_t* mean, AugurMat_t* cov, AugurMat_t* L, AugurVec_t* work);
__HOSTORDEV__ void augur_mvnormal_mvnormal_conj(augur_rng* rng, AugurVec_t* result, AugurVec_t* mu0, AugurMat_t* cov0, AugurVec_t* conjmean, int cnt, AugurMat_t* cov, AugurMat_t* L0, AugurVec_t* work0, AugurMat_t* L);
// __HOSTORDEV__ void augur_mvnormal_mvnormal_conj(augur_rng* rng, AugurVec_t* result, AugurVec_t* conjmean, int cnt, AugurMat_t* L, AugurVec_t* mu0, AugurMat_t* cov0, AugurMat_t* L0, AugurVec_t* work0, AugurMat_t* cov);


/* Inverse Wishart */

__HOSTDEV__ real_t augur_invwishart_pdf(AugurMat_t* pt, int df, AugurMat_t* scale, AugurMat_t* L, AugurMat_t* work);
__HOSTDEV__ real_t augur_invwishart_ll(AugurMat_t* pt, int df, AugurMat_t* scale, AugurMat_t* L, AugurMat_t* work);
__HOSTDEV__ void augur_invwishart_dotpt(AugurMat_t* dst, AugurMat_t* pt, int df, AugurMat_t* scale, AugurMat_t* L, AugurMat_t* work);
__HOSTDEV__ void augur_invwishart_dotarg2(AugurMat_t* dst, AugurMat_t* pt, int df, AugurMat_t* scale, AugurMat_t* L, AugurMat_t* work);
__HOSTORDEV__ void augur_invwishart_sample(augur_rng* rng, AugurMat_t* result, int df, AugurMat_t* scale, AugurMat_t* L);
__HOSTORDEV__ void augur_invwishart_mvnormal_conj(augur_rng* rng, AugurMat_t* result, int df, AugurMat_t* scale, AugurMat_t* conj, int cnt, AugurVec_t* diff, AugurMat_t* L, AugurMat_t* work);



/* Host functions */

real_t h_augur_uniform_sample(gsl_rng* rng, real_t lower, real_t upper);
void h_augur_blk_randn(augur_rng *rng, AugurBlk_t* b);

/* where to put this */

__HOSTORDEV__ int augur_unnorm_disc_samp(augur_rng *rng, AugurVec_t* pmf);

#endif
