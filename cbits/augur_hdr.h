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

#ifndef AUGUR_HDR_H
#define AUGUR_HDR_H

#ifdef AUGURCPU


#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

typedef gsl_rng augur_rng;

#ifndef EXTERNC
#define EXTERNC
#endif

#ifndef __HOSTORDEV__
#define __HOSTORDEV__
#endif

#ifndef __HOSTDEV__
#define __HOSTDEV__
#endif






#else



#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

#include <math.h>

#include <curand.h>
#include <curand_kernel.h>

#include <thrust/transform_reduce.h>
#include <thrust/inner_product.h>
#include <thrust/device_ptr.h>
#include <thrust/functional.h>
#include <thrust/execution_policy.h>

typedef curandState_t augur_rng;

#ifndef EXTERNC
#define EXTERNC extern "C"
#endif

#ifndef __HOSTORDEV__
#define __HOSTORDEV__ EXTERNC __device__
#endif

#ifndef __HOSTDEV__
#define __HOSTDEV__ EXTERNC __host__ __device__
#endif

#define THRDS        256
#define BLKS(x)      ((int) ceil(((double) x) / THRDS))

#endif







#endif
