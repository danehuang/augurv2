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

{-# LANGUAGE FlexibleInstances, FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module CudaC.CudaCSyn where

import Data.Maybe
import Text.PrettyPrint
    
import AstUtil.Pretty
import AstUtil.Var


----------------------------------------------------------------------
-- = CudaCSyn Description
{-| [Note]

Contains Cuda/C syntax.

-}   

    
-----------------------------------
-- == Syntax
    
data Typ = VoidTy
         | IntTy
         | DblTy
         | PtrTy Typ
         | NameTy String
         | VecTy Typ
         | FlatVecTy Typ
         | MatTy Typ
           deriving Show

data Attrib = Extern
            | Global
            | Host
            | Device
              deriving Show

newtype Prog b = Prog { unProg :: [Decl b] }
                       
data Decl b = Fun [Attrib] Name [(b, Typ)] (Stmt b) (Maybe (Exp b)) Typ
            | Struct String [(b, Typ)] (Maybe String)
            | ThrustFunc Name [(b, Typ)] [(b, Typ)] (Stmt b) (Maybe (Exp b)) Typ
            deriving (Show, Functor, Foldable, Traversable)
                    
data Stmt b = Exp (Exp b)
            | Declare (Declare b) (Maybe (Exp b))
            | Seq (Stmt b) (Stmt b)
            | If (Exp b) (Stmt b) (Maybe (Stmt b))
            | For (Exp b) (Exp b) (Exp b) (Stmt b)
              deriving (Show, Functor, Foldable, Traversable)

data Declare b = Fwd b
               | ConstArr b Int
                 deriving (Show, Functor, Foldable, Traversable)
                       
data Exp b = Var b
           | Lit (Lit b)
           | Arr [Exp b]
           | Assign (Exp b) Aop (Exp b)
           | Call (Exp b) [Exp b]
           | LibCall String [Exp b]
           | KernelCall String (Exp b) (Exp b) [Exp b]
           | Unary Uop (Exp b)
           | Binop (Exp b) Bop (Exp b)
           | Cast Typ (Exp b)
             deriving (Show, Functor, Foldable, Traversable)
                      
data Lit b = Int Int
           | Dbl Double
           | CString String
           | Array [Exp b]             -- ughh, fuck compound lit bullshit
           | Strct [(String, Exp b)]   -- ughh, fuck compound lit bullshit
           | Literally String
           deriving (Show , Functor, Foldable, Traversable)
                  
data Aop = EqAss
         | EqAssStaticArr
         | PlusEqAss
         | TimesEqAss
           deriving Show
                    
data Uop = Neg
         | Deref
         | Addr
         | Not
           deriving Show

data Bop = Plus
         | Minus
         | Times
         | Div
         | EqEq
         | And
         | Lt
         | Lte
         | Proj
         | RArr
         | Idx
           deriving Show

instance Num (Exp b) where
    (+) e1 e2 = Binop e1 Plus e2
    (-) e1 e2 = Binop e1 Minus e2
    (*) e1 e2 = Binop e1 Times e2                
    abs e = error "Shouldn't use..."
    signum e = error "Shouldn't use..."
    fromInteger n = Lit (Int (fromInteger n))
    negate e = Unary Neg e

               
-----------------------------------
-- == Instances

instance Pretty Typ where
    ppr VoidTy = text "void"
    ppr IntTy = text "int"
    ppr DblTy = text "double"
    ppr (PtrTy t) = ppr t <> text "*"
    ppr (NameTy str) = text str
    ppr (VecTy _) = text "AugurVec_t"
    ppr (FlatVecTy _) = text "AugurFlatVec_t"
    ppr (MatTy _) = text "AugurMat_t"
                              
instance Pretty Attrib where
    ppr Extern = text "EXTERNC"
    ppr Global = text "__global__"
    ppr Host = text "__host__"
    ppr Device = text "__device__"

instance Pretty (Prog (TVar Typ)) where
    ppr (Prog decls) =
        vcat (map ppr decls)
                 
instance Pretty (Decl (TVar Typ)) where
    ppr (Fun attribs name params body retExp retTy) =
        vcat [ hang (sepBy space attribs <+> ppr retTy <+> ppr name <> parens (sepBy' commasp pprParam params) <+> lbrace) 2 (vcat [ ppr body, pprRet retExp ]), rbrace ]
        where
          pprParam (x, ty) = ppr ty <+> ppr x

          pprRet (Just e) = text "return" <+> ppr e <+> semi
          pprRet Nothing = empty
    ppr (Struct name fields typeDef) =
        case typeDef of
          Nothing ->
              vcat [ hang (text "struct" <+> text name <+> lbrace) 2 (vcat (map pprField fields)), rbrace <+> semi ]
          Just typeDef' ->
              vcat [ hang (text "typedef" <+> text "struct" <+> text name <+> lbrace) 2 (vcat (map pprField fields)), rbrace <+> text typeDef' <> semi ]
        where
          pprField (x, ty) = ppr ty <+> ppr x <> semi
                             
    ppr (ThrustFunc name sparams params body retExp retTy) =
        vcat [ hang (text "struct" <+> ppr name <+> lbrace) 2 func, rbrace <> semi ]
        where
          func =
              vcat [ vcat (map pprSParam sparams)
                   , ctor
                   , fun ]

          pprSParam (x, ty) =
              text "const" <+> ppr ty <+> ppr x <> semi
                            
          ctor =
              text "__host__ __device__" <+> ppr name <> parens (sepBy' commasp pprSParam' sparams) <+> colon <+> (sepBy' commasp pprSParam'' sparams) <+> braces empty

          pprSParam' (x, ty) =
              ppr ty <+> text "_" <> ppr x

          pprSParam'' (x, _) =
              ppr x <> parens (text "_" <> ppr x)
                   
          fun =
              vcat [ hang (text "__host__ __device__" <+> ppr retTy <+> text "operator()" <> parens (sepBy' commasp pprParam params) <+> text "const" <+> lbrace) 2 (vcat [ ppr body, pprRet retExp ]), rbrace ]

          pprParam (x, ty) =
              text "const" <+> ppr ty <> text "&" <+> ppr x

          pprRet (Just ret) = text "return" <+> ppr ret <+> semi
          pprRet Nothing = text "return" <+> semi
        -- error "TODO | Thrust"

{-
rep tab ' ' ++ "struct " ++ emitC 0 name ++ " {\n" ++
            sepByStr "\n" (map (emitSParam (tab + 2)) sparams) ++ "\n" ++
            rep (tab + 2) ' ' ++ "__host__ __device__ " ++ emitC 0 name ++ "(" ++ sepByStr ", " (map emitSParam' sparams) ++ ") : " ++ sepByStr ", " (map emitSParam'' sparams) ++ " {} " ++ "\n" ++
            rep (tab + 2) ' ' ++ "__host__ __device__ " ++ emitC 0 retTy ++ " operator()(" ++ sepByStr "," (map emitParam params) ++ ") const {\n" ++
            emitC (tab + 4) body ++ "\n" ++ emitRet (tab + 4) ret ++ "\n" ++
            rep (tab + 2) ' ' ++ "}\n" ++
            rep tab ' ' ++ "};"
        where          
          emitSParam tab (x, ty) =
              rep tab ' ' ++ "const " ++ emitC 0 ty ++ " " ++ emitC 0 x ++ ";"

          emitSParam' (x, ty) =
              emitC 0 ty ++ " _" ++ emitC 0 x

          emitSParam'' (x, _) =
              emitC 0 x ++ "(_" ++ emitC 0 x ++ ")"
                    
          emitParam (x, ty) =
              "const " ++ emitC 0 ty ++ "& " ++ emitC 0 x
                  
          emitRet tab (Just ret) = rep tab ' ' ++ "return " ++ emitC 0 ret ++ ";"
          emitRet tab Nothing = rep tab ' ' ++ "return;"
-}

                                                  
instance Pretty (Stmt (TVar Typ)) where
    ppr (Exp e) =
        case e of
          Lit (Int 0) -> empty
          _ -> ppr e <+> semi
    ppr (Declare d me) = ppr d <+> pprRhs me
        where
          pprRhs (Just e) = text "=" <+> ppr e <+> semi
          pprRhs Nothing = semi
    ppr (Seq s1 s2) = vcat [ ppr s1, ppr s2 ]
    ppr (If e s1 s2') =
        case s2' of
          Just (Exp (Lit (Int 0))) -> pprBranch (text "if" <> parens (ppr e)) s1
          Just s2 -> vcat [ pprBranch (text "if" <> parens (ppr e)) s1, pprBranch (text "else") s2 ]
          Nothing -> pprBranch (text "if" <> parens (ppr e)) s1
        where
          pprBranch hdr s = vcat [ hang (hdr <+> lbrace) 2 (ppr s), rbrace ]
                     
    ppr (For e1 e2 e3 s) =
        vcat [ hang (text "for" <> parens (sepBy (semi <> space) [ e1, e2, e3 ]) <+> lbrace) 2 (ppr s), rbrace ]
     
instance Pretty (Declare (TVar Typ)) where
    ppr (Fwd x) =
        ppr (fromJust (getType x)) <+> ppr x
    ppr (ConstArr x len) =
        ppr (fromJust (getType x)) <+> ppr x <> brackets (int len)
             
instance Pretty (Exp (TVar Typ)) where
    ppr (Var x) = ppr x
    ppr (Lit lit) = ppr lit
    ppr (Arr es) = braces (sepBy commasp es)
    ppr (Assign e1 aop e2) =
        case aop of
          EqAss -> 
              case e1 of
                Var x -> ppr (fromJust (getType x)) <+> ppr e1 <+> ppr aop <+> ppr e2
                _ -> ppr e1 <+> ppr aop <+> ppr e2         
          _ -> ppr e1 <+> ppr aop <+> ppr e2
    ppr (Call e es) = ppr e <> parens (sepBy commasp es)
    ppr (LibCall f es) = text f <> parens (sepBy commasp es)
    ppr (KernelCall k eBlk eThrd es) = text k <> text "<<<" <> sepBy commasp [ eBlk, eThrd ] <> text ">>>" <> parens (sepBy commasp es)
    ppr (Unary uop e) = parens (ppr uop <> ppr e)
    ppr (Binop e1 bop e2) =
        case bop of
          Proj -> parens (ppr e1 <> ppr bop <> ppr e2)
          RArr -> parens (ppr e1 <> ppr bop <> ppr e2)
          Idx -> ppr e1 <> brackets (ppr e2)
          _ -> ppr e1 <+> ppr bop <+> ppr e2
    ppr (Cast ty e) = parens (parens (ppr ty) <+> ppr e)
            
instance Pretty (Lit (TVar Typ)) where
    ppr (Int i) = int i
    ppr (Dbl d) = double d
    ppr (CString str) = text "\"" <> text str <> text "\""
    ppr (Array lits) = braces (sepBy commasp lits)
    ppr (Strct fields) = braces (sepBy' commasp pprField fields)
        where
          pprField (name, lit) = text "." <> text name <+> text "=" <+> ppr lit
          -- pprField (_, lit) = ppr lit
    ppr (Literally str) = text str

instance Pretty Aop where
    ppr EqAss = text "="
    ppr EqAssStaticArr = text "="
    ppr PlusEqAss = text "+="
    ppr TimesEqAss = text "*="
            
instance Pretty Uop where
    ppr Neg = text "-"
    ppr Deref = text "*"
    ppr Addr = text "&"
    ppr Not = text "!"

instance Pretty Bop where
    ppr Plus = text "+"
    ppr Minus = text "-"
    ppr Times = text "*"
    ppr Div = text "/"
    ppr EqEq = text "=="
    ppr And = text "&&"
    ppr Lt = text "<"
    ppr Lte = text "<="
    ppr Proj = text "."
    ppr RArr = text "->"


-----------------------------------
-- == Functions on syntax
               
seqStmt :: [Stmt b] -> Stmt b
seqStmt [] = Exp (Lit (Int 0))
seqStmt stmts = foldl (\acc s -> Seq acc s) (head stmts) (tail stmts)


assign :: b -> Exp b -> Exp b
assign x e = Assign (Var x) EqAss e

             
assign' :: Exp b -> Exp b -> Exp b
assign' elhs erhs = Assign elhs EqAss erhs

                    
assignStmt :: b -> Exp b -> Stmt b
assignStmt x e = Exp (assign x e)


assignStmt' :: Exp b -> Exp b -> Stmt b
assignStmt' elhs erhs = Exp (assign' elhs erhs)
                 

derefStrct :: b -> b -> Exp b
derefStrct strct field = Binop (Var strct) RArr (Var field)

derefStrct' :: Exp b -> Exp b -> Exp b
derefStrct' strct field = Binop strct RArr field

                         
strctProj :: b -> b -> Exp b
strctProj strct field = Binop (Var strct) Proj (Var field)

strctProj' :: Exp b -> String -> Exp b
strctProj' e field = Binop e Proj (Lit (Literally field))
                        
strctProj'' :: Exp b -> Exp b -> Exp b
strctProj'' strct field = Binop strct Proj field
                     
addrOf :: Exp b -> Exp b
addrOf e = Unary Addr e

           
deref :: Exp b -> Exp b
deref e = Unary Deref e

           
mkLiterally :: String -> Exp b
mkLiterally = Lit . Literally


mkInt :: Int -> Exp b
mkInt = Lit . Int


mkLibCall :: String -> [Exp b] -> Stmt b
mkLibCall fn es = Exp (LibCall fn es)


mkSkip :: Stmt b
mkSkip = Exp (mkInt 0)



mkLt :: Exp b -> Exp b -> Exp b
mkLt e1 e2 = Binop e1 Lt e2

             
mkLte :: Exp b -> Exp b -> Exp b
mkLte e1 e2 = Binop e1 Lte e2

              
mkAnd :: Exp b -> Exp b -> Exp b
mkAnd e1 e2 = Binop e1 And e2
