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

#ifndef AUGUR_MCMCLIB_H
#define AUGUR_MCMCLIB_H

#include "augur_util.h"
#include "augur_math.h"
#include "augur_vecop.h"
#include "augur_blkop.h"
#include "augur_dist.h"
#include <augur_iface.h>
#include <math.h>
#include <gsl/gsl_rng.h>


typedef struct mcmc {
  AugurMod_t curr;
  AugurMod_t prop;
  AugurAux_t aux;
  int accept;
  double currLL;
  double propLL;
  double eps;
  double L;
  double leapLen;
  double deltaMax;
  int nutsalloc;
} mcmc_t;

mcmc_t MCMC;
gsl_rng* h_rng;

void dump_state(mcmc_t state) {
  printf("currLL: %f\n", state.currLL);
  printf("propLL: %f\n", state.propLL);
  printf("eps: %f\n", state.eps);
  printf("leapLen: %f\n", state.leapLen);
  printf("L: %f\n", state.L);
  
}


// double modObjFn(AugurMod_t curr, AugurAux_t aux);
EXTERNC void mcmcStep();

/*
void acceptAlways() {
  MCMC.currLL = modObjFn(MCMC.curr, MCMC.aux);
}
*/


/**
 * Returns TRUE if we should accept, FALSE if we should reject.
 */
Bool_t augur_mcmc_ar( double auxll
		    , double (*objFn)(AugurAux_t, AugurMod_t)
		    ) {
  double currll = objFn(MCMC.aux, MCMC.curr);
  double propll = objFn(MCMC.aux, MCMC.prop);
  double u = gsl_rng_uniform(h_rng);

  if (augur_log(u) < propll - currll + auxll) {
    return TRUE;
  }
  else {
    return FALSE;
  }
}


/**
 * Returns TRUE if we should accept, FALSE if we should reject.
 */
Bool_t augur_mcmc_ar_idx( double auxll
			, AugurVec_t* idxs
			, double (*objFn)(AugurAux_t, AugurMod_t, AugurVec_t* idxs)
		        ) {
  double currll = objFn(MCMC.aux, MCMC.curr, idxs);
  double propll = objFn(MCMC.aux, MCMC.prop, idxs);
  double u = gsl_rng_uniform(h_rng);

  if (augur_log(u) < propll - currll + auxll) {
    return TRUE;
  }
  else {
    return FALSE;
  }
}


int acceptReject(double auxLL, double (*objFn)(AugurAux_t, AugurMod_t)) {
  MCMC.propLL = objFn(MCMC.aux, MCMC.prop);
  double u = gsl_rng_uniform(h_rng);
  // printf("currLL: %f  propLL: %f  auxLL: %f   ar: %d\n", MCMC.currLL, MCMC.propLL, auxLL, log(u) < MCMC.propLL - MCMC.currLL + auxLL);
  if (log(u) < MCMC.propLL - MCMC.currLL + auxLL) {
    MCMC.accept += 1;
    AugurMod_t tmp = MCMC.curr;
    MCMC.curr = MCMC.prop;
    MCMC.prop = tmp;
    // double tmp2 = MCMC.currLL;
    MCMC.currLL = MCMC.propLL;
    // MCMC.propLL = tmp2;
    return 1;
  }
  else {
    return 0;
  }
}

void swapCurrProp(double (*objFn)(AugurAux_t, AugurMod_t)) {
  MCMC.propLL = objFn(MCMC.aux, MCMC.prop);
  AugurMod_t tmp = MCMC.curr;
  MCMC.curr = MCMC.prop;
  MCMC.prop = tmp;
  double tmp2 = MCMC.currLL;
  MCMC.currLL = MCMC.propLL;
  MCMC.propLL = tmp2;
}

void initMcmc() {
  const gsl_rng_type * T;
  gsl_rng_env_setup();
  T = gsl_rng_default;
  h_rng = gsl_rng_alloc (T);
  
  // MCMC.curr = curr;
  // MCMC.prop = prop;
  // MCMC.aux = aux;
  MCMC.accept = 0;
  // MCMC.currLL = objFn(MCMC.aux, MCMC.curr);
  MCMC.propLL = MCMC.currLL;
  MCMC.eps = 1.0;
  MCMC.L = 20;
  MCMC.leapLen = 10.0;
  MCMC.deltaMax = 1000.0;
  MCMC.nutsalloc = 0;
  return;
}

EXTERNC void setHmcParams(double eps, double leapLen) {
  MCMC.eps = eps;
  MCMC.leapLen = leapLen;
}

EXTERNC void setHmcParams2(double eps, int L) {
  MCMC.eps = eps;
  MCMC.L = L;
}

#define max(x, y) ( x < y ? y : x)

double h_sample_uniform(gsl_rng* rng, double left, double right) {
  double u = gsl_rng_uniform(rng);
  return left + u * (right - left);
}


/**
 * Metropololis-within-gibbs Kernel.
 */
void h_augur_mcmc_mwg
      ( AugurAux_t aux, AugurMod_t curr, AugurMod_t prop
      , double* llCorrect, AugurVec_t* idxs
      , void (*proposal)(AugurAux_t, AugurMod_t, AugurMod_t, AugurVec_t*)
      , void (*swap)(AugurAux_t, AugurMod_t, AugurMod_t, AugurVec_t*, int)
      , double (*objFn)(AugurAux_t, AugurMod_t, AugurVec_t*) ) {
  proposal(aux, curr, prop, idxs);
  if (augur_mcmc_ar_idx(*llCorrect, idxs, objFn)) {
    // Accept, so copy proposal theta into current theta (maintain invariant)
    swap(aux, curr, prop, idxs, 0);
  }
  else {
    // Reject, so copy current theta into proposal theta (maintain invariant)
    swap(aux, curr, prop, idxs, 1);
  }
}
  

/**
 * Gradient checking. (Call from host code)
 */
void h_augur_chk_grad
      ( AugurMemLoc_t loc, AugurBlk_t* pt, AugurBlk_t* grad
      , AugurAux_t aux, AugurMod_t curr
      , double (*objFn)(AugurAux_t, AugurMod_t) ) {
  double eps = 10e-8;
  printf("Evaluating gradient at point:\n");
  h_augur_blk_dump(loc, pt);
  
  for (uint_t i = 0; i < pt->num_blks; i++) {
    switch (pt->typs[i]) {
    case AUGUR_DBL: {
      // TODO 
      break;
    }
    case AUGUR_VEC: {
      AugurFlatVec_t* fvec = (AugurFlatVec_t*) pt->blks[i];
      for (int j = 0; j < fvec->base_elems; j++) {
	h_augur_flat_vec_basis_add(loc, fvec, j, eps);
	double plus = objFn(aux, curr);
	h_augur_flat_vec_basis_add(loc, fvec, j, -2.0 * eps);
	double minus = objFn(aux, curr);
	double fin_diff = (plus - minus) / (2.0 * eps);
	double ad;
	h_augur_flat_vec_getd_idx(loc, &ad, (AugurFlatVec_t*) (grad->blks[i]), j);
	printf("plus: %f, minus: %f\n", plus, minus);
	printf("auto-diff: %f, finite-diff: %f, error: %f\n", ad, fin_diff, ad - fin_diff);
	h_augur_flat_vec_basis_add(loc, fvec, j, eps);
      }
      break;
    }
    case AUGUR_MAT: {
      AugurMat_t* mat = (AugurMat_t*) pt->blks[i];
      for (int j = 0; j < mat->row * mat->col; j++) {
	h_augur_mat_basis_add(loc, mat, j, eps);
	double plus = objFn(aux, curr);
	h_augur_mat_basis_add(loc, mat, j, -2.0 * eps);
	double minus = objFn(aux, curr);
	double fin_diff = (plus - minus) / (2.0 * eps);
	double ad;
	h_augur_mat_getd_idx(loc, &ad, (AugurMat_t*) (grad->blks[i]), j);	
	printf("auto-diff: %f, finite-diff: %f, error: %f\n", ad, fin_diff, ad - fin_diff);
	h_augur_mat_basis_add(loc, mat, j, eps);
      }
      break;
    }
    default: {
      // TODO
      break;
    }
    }
  }
}


/**
 * Calls a gradient function. (Call from host code)
 *
 * The struct curr contains "the point" we are evaluating gradfn at.
 * The result of the gradient computation is in grad, which is part of
 * the struct aux.
 */
void h_augur_mcmc_call_grad
      ( AugurMemLoc_t loc
      , AugurAux_t aux, AugurMod_t curr
      , AugurBlk_t* grad
      , void (*gradfn)(AugurAux_t, AugurMod_t)
      ) {
  h_augur_blk_zero(loc, grad);
  gradfn(aux, curr);
}


/**
 * The struct curr contains pt, "the point" we are evaluating gradfn at.
 */
void h_augur_mcmc_chkcall_grad
      ( AugurMemLoc_t loc
      , AugurAux_t aux, AugurMod_t curr
      , AugurBlk_t* grad 
      , void (*gradfn)(AugurAux_t, AugurMod_t)
      , AugurBlk_t* pt
      , double (*objfn) (AugurAux_t, AugurMod_t)
      , Bool_t f_chkgrad
      ) {
  h_augur_mcmc_call_grad(loc, aux, curr, grad, gradfn);
  if (f_chkgrad) {
    h_augur_chk_grad(loc, pt, grad, aux, curr, objfn);
  }
}

/**
 * Leapfrog simulation. (Call from host code)
 * 
 * Leapfrog with (POSITIVE) gradient of log-likelihood of objfn. 
 * (Signs are important!!)
 *
 * theta0 / thetaStar: initial / proposal point
 * grad: result of gradient
 * p0 / pStar: initial / proposal momentum
 * 
 * curr.theta0 = theta0, prop.theta0 = thetaStar, aux.grad = grad
 * 
 * gradfn(aux, curr) evaluates gradient at theta0, putting result in grad
 * gradfn(aux, prop) evaluates gradient at thetaStar, putting result in grad
 *
 */
void h_augur_mcmc_leapfrog
      ( AugurMemLoc_t loc
      , AugurAux_t aux, AugurMod_t curr, AugurMod_t prop      
      , AugurBlk_t* theta0, AugurBlk_t* thetaStar
      , AugurBlk_t* grad, AugurBlk_t* p0, AugurBlk_t* pStar
      , void (*gradfn)(AugurAux_t, AugurMod_t)
      , double (*objfn) (AugurAux_t, AugurMod_t)
      , double stepsize, int simsteps
      ) {
  // Simulate Hamiltonian dynamics first half step of momentum
  // pStar = p0 + stepsize / 2.0 * d_like(x, theta0)
  h_augur_mcmc_call_grad(loc, aux, curr, grad, gradfn);
  h_augur_blk_scale_plus(loc, pStar, p0, stepsize / 2.0, grad);   
  
  // Simulate full steps
  for (uint_t l = 0; l < simsteps; l++) {
    // Full step for position
    // Position: thetaStar = thetaStar + stepsize * pStar
    h_augur_blk_scale_plus(loc, thetaStar, thetaStar, stepsize, pStar);

    // Move momentum except at last step    
    if (l != simsteps-1) {
      // Momentum: pStar = pStar + stepsize * d_like(x, thetaStar)
      h_augur_mcmc_call_grad(loc, aux, prop, grad, gradfn);   
      h_augur_blk_scale_plus(loc, pStar, pStar, stepsize, grad);
    }
  }

  // Simulate last half step of momentum
  // pStar = pStar + stepsize / 2.0 * d_like(x, thetaStar)
  h_augur_mcmc_call_grad(loc, aux, prop, grad, gradfn);
  h_augur_blk_scale_plus(loc, pStar, pStar, stepsize / 2.0, grad);
}


/**
 * Hamiltonian Monte Carlo Kernel. (Call from host code)
 *
 * Suppose this kernel moves blk = [p_1, .., p_n]. 
 * Then we have the following equalities on the inputs:
 * 
 * curr.blk = theta0
 * prop.blk = thetaStar
 *
 * For compositionality, we maintain the invariant that:
 * curr.blk = prop.blk
 */
void h_augur_mcmc_hmc
     ( AugurMemLoc_t loc
     , AugurAux_t aux, AugurMod_t curr, AugurMod_t prop      
     , AugurBlk_t* theta0, AugurBlk_t* thetaStar
     , AugurBlk_t* grad, AugurBlk_t* p0, AugurBlk_t* pStar
     , double simlen, double eps
     , void (*computeGrad)(AugurAux_t, AugurMod_t)
     , double (*objFn)(AugurAux_t, AugurMod_t)
     ) {
  // Initialize momentum
  h_augur_blk_randn(aux.rng, p0);

  // Randomize simsteps
  double scale_eps = h_augur_uniform_sample(h_rng, 0.8, 1.2);
  double stepsize = scale_eps * eps;
  double s = h_augur_uniform_sample(h_rng, 0.9, 1.1);
  uint_t simsteps = augur_max(1, round(s * simlen / stepsize));

  // Simulate Hamiltonian dynamics with Leapfrog method
  h_augur_mcmc_leapfrog(loc, aux, curr, prop, theta0, thetaStar, grad, p0, pStar, computeGrad, objFn, stepsize, simsteps);
  
  // Compute correction 
  double initMomLL = h_augur_blk_dot(loc, p0, p0) / 2.0;
  double propMomLL = h_augur_blk_dot(loc, pStar, pStar) / 2.0;
  double auxll = initMomLL - propMomLL;

  // After trajectory, compute whether we should accept or reject
  if (augur_mcmc_ar(auxll, objFn)) {
    // Accept, so copy proposal theta into current theta (for invariant)
    h_augur_blk_cpy(loc, theta0, thetaStar);
  }
  else {
    // Reject, so copy current theta into proposal theta (for invariant)
    h_augur_blk_cpy(loc, thetaStar, theta0);
  }
}


/**
 * Elliptical Slice Sampling Kernel. (Note it is host/dev code)
 *
 * Suppose this kernel moves param = currTheta. 
 * Then we have the following equalities on the inputs:
 * 
 * curr.param = currTheta
 * prop.param = propTheta
 *
 * For compositionality, we maintain the invariant that:
 * curr.param = prop.param
 */
__HOSTORDEV__ void augur_mcmc_eslice
      ( AugurAux_t aux, AugurMod_t curr, AugurMod_t prop
      , AugurVec_t* curr_theta, AugurVec_t* prop_theta
      , AugurVec_t* mean, AugurMat_t* cov
      , AugurMat_t* L, AugurVec_t* nu
      , AugurVec_t* idxs
      , double (*objfn)(AugurAux_t, AugurMod_t, AugurVec_t*) ) {
  augur_mvnormal_sample(aux.rng, nu, mean, cov, L, nu);
  double u = augur_std_uniform_sample(aux.rng);
  double log_y = objfn(aux, curr, idxs) + augur_log(u);
  
  double theta = augur_uniform_sample(aux.rng, 0.0, 2.0 * AUGUR_PI);
  double theta_min = theta - 2.0 * AUGUR_PI;
  double theta_max = theta;

  while (TRUE) {
    double diff = theta_max - theta_min;
    if (diff < 1e-8) {
      // Maintain invariant that curr == prop
      for (uint_t i = 0; i < prop_theta->elems; i++) {
	AUGUR_VEC_SETD(prop_theta, i, AUGUR_VEC_GETD(curr_theta, i));
      }
      break;
    }

    // propSt = (currSt - mean) * cos(theta) + (nu - mean) * sin(theta) + mean;
    for (uint_t i = 0; i < prop_theta->elems; i++) {
      double mean_i = AUGUR_VEC_GETD(mean, i);
      double a = (AUGUR_VEC_GETD(curr_theta, i) - mean_i) * augur_cos(theta);
      double b = (AUGUR_VEC_GETD(nu, i) - mean_i) * augur_sin(theta);
      AUGUR_VEC_SETD(prop_theta, i, a + b + mean_i);
    }

    double prop_ll = objfn(aux, prop, idxs);
    if (prop_ll > log_y) {      
      // Maintain invariant that curr == prop
      for (uint_t i = 0; i < curr_theta->elems; i++) {
	AUGUR_VEC_SETD(curr_theta, i, AUGUR_VEC_GETD(prop_theta, i));
      }
      break;
    }

    // Shrink bracket
    if (theta < 0) {
      theta_min = theta;
    }
    else {
      theta_max = theta;
    }
    theta = augur_uniform_sample(aux.rng, theta_min, theta_max);
  }
}





/************************************************************ 
Reflective slice sampling
*************************************************************/

/*
double reflective_slice
      ( AugurMod_t curr
      , AugurMod_t prop
      , AugurAux_t aux
      , AugurBlk_t* theta0
      , AugurBlk_t* thetaProp
      , AugurBlk_t* p0
      , AugurBlk_t* grad
      , double (*objFn)(AugurAux_t, AugurMod_t)
      , void (*computeGrad)(AugurAux_t, AugurMod_t) ) {
  double eps = MCMC.eps;
  int L = (int) round(MCMC.leapLen / MCMC.eps);
  
  // Initialize momentum
  h_augur_vec_randn(aux.rng, p0);

  // Compute slice level
  double z = objFn(aux, curr) - gsl_ran_exponential(h_rng, 1.0);
  int cond = 0;
  
  // thetaProp starts at theta0
  // TODO: Do we need to cpy?
  
  for (int i = 0 ; i < L; i++) {
    // Take scaled-step in direction of momentum
    h_augur_blk_scale_plus(thetaProp, thetaProp, eps, p0);

    cond = objFn(aux, prop) < z;
    if (cond) {
      // Reflect (outside version) if we leave slice
      
      // Compute grad at outside point
      h_augur_blk_zero(grad);
      computeGrad(aux, prop);

      // Compute reflection and update direction
      double scale = h_augur_blk_dot(p0, grad) / h_augur_blk_dot(grad, grad);
      h_augur_blk_scale_plus(p0, p0, -2.0 * scale, grad);

    }
  }
  
  if (!cond) {
    // Copy if we are in slice
    h_augur_blk_cpy(theta0, thetaProp);
  }

  return 0.0;
}
*/


#endif
