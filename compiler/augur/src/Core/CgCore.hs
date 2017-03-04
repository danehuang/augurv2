{- 
 - Copyright 2017 Daniel Eachern Huang
 -
 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at
 - 
 -    http://www.apache.org/licenses/LICENSE-2.0
 -
 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License.
 -}

module Core.CgCore where

import AstUtil.Var
import Comm.Prim
import Low.LowpPrimSyn
import Core.DensSyn
import qualified Low.LowSyn as L

    
-----------------------------------
-- == Transformations

cgLit :: Lit -> L.Lit
cgLit (Int i) = L.Int i
cgLit (Real d) = L.Real d
                 
                 
cgCallExp :: CallExp -> L.CallExp
cgCallExp (FnId name) = L.FnId name
cgCallExp (PrimId prim) = L.PrimId DM_Fn PM_Fn prim

                          
cgExp :: (TypedVar b t) => Exp b -> L.Exp b
cgExp (Var x) = L.Var x
cgExp (Lit lit) = L.Lit (cgLit lit)
cgExp (DistOp dop dist es) = L.DistOp dop DM_Fn dist (map cgExp es)
cgExp (Call ce es) = L.Call (cgCallExp ce) (map cgExp es)
cgExp (Proj e es) = L.Proj (cgExp e) (map cgExp es)
                  

cgGen :: (TypedVar b t) => Gen b -> L.Gen b
cgGen (Until e1 e2) = L.Until (cgExp e1) (cgExp e2)

                      
cgIndCond :: (TypedVar b t) => IndCond b -> L.Exp b
cgIndCond (CatCond x e) = L.Call (L.PrimId DM_Fn PM_Fn EqEq) [ L.Var x, cgExp e ]
                          

andExp :: (TypedVar b t) => [L.Exp b] -> L.Exp b
andExp (hd : tl) = foldl (\acc e -> L.Call (L.PrimId DM_Fn PM_Fn LAnd) [ acc, e ]) hd tl
andExp [] = 0
