--{-# OPTIONS_GHC -Wall #-}
--module Kryvosheienko09 where
--
--import Data.Maybe
---- розглядаємо лише цілі дані: скаляри  і масиви
----------------------------------------------------------------------
--type Id    = String
--data Value = I Int | A [(Int, Int)]  deriving (Eq, Show)
--data Op    = Add | Minus | Mul | Less | Equal | Index  deriving (Eq, Show)
--
--data Exp = Const Int
--         | Var Id
--         | OpApp Op Exp Exp
--         | Cond Exp Exp Exp
--         | FunApp Id [Exp]
--         deriving (Eq, Show)
--
--data Stmt = Assign Id Exp
--          | AssignA Id Exp Exp
--          | If Exp Stmt Stmt
--          | While Exp Stmt
--          | Call Id [Exp]
--          | Block [VarDef] [Stmt]
--          deriving (Eq, Show)
--
--data VarDef  =  Arr Id | Int Id deriving (Eq, Show)
--
--type FunDef  =  (Id, ([VarDef], Exp))
---- функції повертають лише цілі скалярні дані, не використовують глобальні дані (чисті!!)
--type ProcDef = (Id, ([VarDef], Stmt))
--type Program = ([VarDef], [FunDef], [ProcDef])
--
--type StateP  = [(Id, Value)]  -- стек даних
--
--data Type    = At | It  deriving (Eq, Show)
--type FunEnv  = [(Id,[Type])]
--type ProcEnv = [(Id,[Type])]
--type VarEnv  = [(Id,Type)]
--
---- Задача 1 ------------------------------------
--updateValue :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
--updateValue a b []= [(a,b)]
--updateValue a b ((a1, b1):abs) =
--  if a1 == a
--    then (a, b) : abs
--    else (a1, b1) : updateValue a b abs
--
---- Задача 2 ------------------------------------
--updateArray :: Value -> Value -> Value -> Value
--updateArray (A a) (I i) (I v)  = updateArrayR (A []) (A a) (I i) (I v)
--
--updateArrayR :: Value -> Value -> Value -> Value-> Value
--updateArrayR (A res) (A []) (I i) (I v)  = A (res++[(i,v)])
--updateArrayR (A res) (A ((a1,b1):a)) (I i) (I v)  = if a1 == i
--                                               then A (res++(i,v):a)
--                                               else  updateArrayR (A ((a1, b1) : res)) (A a) (I i) (I v)
--
---- Задача 3 ------------------------------------
--applyOp :: Op -> Value -> Value -> Value
--applyOp Add (I v1) (I v2) = I(v1+v2)
--applyOp Minus (I v1) (I v2) = I(v1-v2)
--applyOp Mul (I v1) (I v2) = I(v1*v2)
--applyOp Less (I v1) (I v2) = I(if v1<v2 then 1 else 0)
--applyOp Equal (I v1) (I v2) = I(if v1==v2 then 1 else 0)
--applyOp Index(A []) (I _)  = I 0
--applyOp Index(A ((a1,b1):a)) (I i) =if a1 == i then I b1 else applyOp Index(A a) (I i)
--
--
---- Задача 4 ------------------------------------
--evExp ::  Exp -> [FunDef] -> StateP -> Value
--evExp (Const c) _ _= I c
--evExp (Var y) _ st= getVarValue (Var y) st
--evExp (OpApp op exp1 exp2) dfx st= applyOp op (evExp exp1 dfx st) (evExp exp2 dfx st)
--evExp (Cond exp1 exp2 exp3) dfx st =if evExp exp1 dfx st == I 0
--                                    then evExp exp3 dfx st
--                                    else evExp exp2 dfx st
--evExp (FunApp f expAr) fAr st = evExp (snd function) fAr (args++st)
--                                where  function = findFunction f fAr
--                                       args = concatIdVal (toIdsRec (fst function) []) (evArgs expAr fAr st)
--
--evArgs :: [Exp] -> [FunDef] -> StateP -> [Value]
--evArgs ex dfx st = fmap (\e -> evExp e dfx st) ex
--
--findFunction::Id -> [FunDef]->([VarDef], Exp)
--findFunction id f = snd (head (filter (\(x, _) -> x == id) f))
--
--concatIdVal :: [Id] -> [Value] -> [(Id, Value)]
--concatIdVal (id:ids) (v:val) = (id, v):concatIdVal ids val
--concatIdVal _ _ = []
--
--toIdsRec :: [VarDef] -> [Id] -> [Id]
--toIdsRec [] ids = ids
--toIdsRec (Arr i:vs) ids = toIdsRec vs (ids++[i])
--toIdsRec (Int i:vs) ids = toIdsRec vs (ids++[i])
--
--getVarValue::Exp->StateP -> Value
--getVarValue(Var _) [] = I 0
--getVarValue (Var y) ((a,b):st) = if a==y then b else getVarValue (Var y) st
--
---- Задача 5 ------------------------------------
--evStmt :: Stmt -> [FunDef] -> [ProcDef] -> StateP -> StateP
--evStmt (Assign id exp) dfx _ st = updateValue id (evExp exp dfx st) st
--evStmt (AssignA id exp1 exp2) dfx _ st = updateValue id (updateArray (lookUp id st) (evExp exp1 dfx st) (evExp exp2 dfx st)) st
--evStmt (If exp stmt1 stmt2) dfx dpx st = if evExp exp dfx st == I 1 then evStmt stmt1 dfx dpx st else evStmt stmt2 dfx dpx st
--evStmt (While exp stmt) dfx dpx st = if evExp exp dfx st /= I 0 then evStmt stmt dfx dpx st else st
--evStmt (Call i exs) dfx dpx st = let (v,s)= findProc i dpx
--                                     fs = expStEx v exs dfx
--                                     ss = evStmt s dfx dpx
--                                 in unexpSt v ss
--evStmt (Block vardefAr stmtAr) dfx dpx st = undefined
--
--findProc:: Id->[ProcDef]->([VarDef], Stmt)
--findProc i ((d,v):ps) | i == d= v
--                     |otherwise = findProc i ps
--
--expStEx::[VarDef] -> [Exp] -> [FunDef]->StateP->StateP
--expStEx var ex dfx st = let ve = evArgs ex dfx st
--                        in (createState var ve)++st
--
--unexpSt::[VarDef] ->StateP->StateP
--unexpSt [] st = st
--unexpSt [Arr i] st = delVar i st
--unexpSt [Int i] st = delVar i st
--unexpSt ((Arr i):ss) st = unexpSt ss (delVar i st)
--unexpSt ((Int i):ss) st = unexpSt ss (delVar i st)
--
--
--delVar::Id->StateP->StateP
--delVar _ [] = []
--delVar i ((d,s):st) |i ==d = st
--                    |otherwise = (d,s):(delVar i st)
--
---- Задача 6 ------------------------------------
--iswfExp :: Exp -> VarEnv -> FunEnv -> Maybe Type
--iswfExp (Const _) _ _          = Just It
--iswfExp (Var id) ve _           = lookup id ve
--iswfExp (Cond exp1 exp2 exp3) ve fe
--                                    | typ == Nothing = Nothing
--                                    | otherwise =
--                                      if typ1 /= Nothing && typ1 == typ2
--                                         then typ1
--                                         else Nothing
--                                    where typ = iswfExp exp1 ve fe
--                                          typ1 = iswfExp exp2 ve fe
--                                          typ2 = iswfExp exp3 ve fe
--iswfExp (OpApp op exp1 exp2) ve fe  | typ/=Nothing && typ1/=Nothing = iswfOp op [fromJust typ , fromJust typ1]
--                                    | otherwise = Nothing
--                                    where typ = iswfExp exp1 ve fe
--                                          typ1 = iswfExp exp2 ve fe
--
--iswfExp (FunApp id expAr) ve fe
--                                | typ == Nothing = Nothing
--                                | otherwise =
--                                  if ars /= ars2
--                                    then Nothing
--                                    else Just It
--                                where
--                                  typ = lookup id fe
--                                  ars = map (\x -> iswfExp x ve fe) expAr
--                                  ars2 = [Just a | a <- fromJust typ]
--
---- Задача 7 ------------------------------------
--
--iswfStmt :: Stmt -> VarEnv -> FunEnv -> ProcEnv -> Bool
--iswfStmt (Assign n ex) ve fe _ = typ ==  iswfExp ex ve fe && typ == Just It
--                                 where typ = Just (lookUp n ve)
--iswfStmt (AssignA n ex1 ex2) ve fe _ =  iswfAssignA [type1, type2, type3]
--                                         where  Just type1 = Just (lookUp n ve)
--                                                Just type2 = iswfExp ex1 ve fe
--                                                Just type3 = iswfExp ex2 ve fe
--
--iswfStmt (If ex st1 st2) ve fe pe = Just It == iswfExp ex ve fe &&  iswfStmt st1 ve fe pe && iswfStmt st2 ve fe pe
--iswfStmt (While ex st) ve fe pe = Just It == iswfExp ex ve fe && iswfStmt st ve fe pe
--iswfStmt (Call n vs) ve fe pe = map (\x -> iswfExp x ve fe) vs == map Just (lookUp n pe)
--iswfStmt (Block vd sts) ve fe pe = check (Block vd sts) ve fe pe
--
--check::Stmt -> VarEnv -> FunEnv -> ProcEnv -> Bool
--check (Block _ _) _ _ _ =  True
--check (Block vd (x:sts)) ve fe pe = if iswfStmt x varE fe pe then check (Block vd sts) ve fe pe else False
--                                 where varE = map getType vd ++ ve
--
--
--getType :: VarDef -> (Id, Type)
--getType (Arr v) = (v, At)
--getType (Int v) = (v, It)
--
---- Задача 8 ------------------------------------
--iswfFunDef :: FunDef -> FunEnv -> Bool
--iswfFunDef = undefined
--
--iswfProcDef :: ProcDef -> VarEnv -> FunEnv -> ProcEnv -> Bool
--iswfProcDef = undefined
--
---- Задача 9 ------------------------------------
--iswfProgram :: Program -> Bool
--iswfProgram = undefined
--
--
----- Допоміжні функції -----------------------------
--lookUp :: Eq a => a -> [(a,b)] -> b
---- Передумова: Пара з ключом a є в списку пар abx
--lookUp a abx = maybe (error "lookUp") id (lookup a abx)
--
---- формує початкове значення змінної
--initv :: VarDef -> (Id, Value)
--initv (Arr v) = (v, A [])
--initv (Int v) = (v, I 0)
--
---- Реалізація виконання програми
--evProgram :: Program -> StateP
--evProgram (dvx, dfx, dpx) =
--   let sb = map initv dvx
--       ( _, s) = lookUp "main" dpx
--   in  evStmt s dfx dpx sb
--
----  iswfOp o ts - перевіряє коректність типів операндів ts
----     бінарної операції o і формує тип результату Just t або Nothing
--iswfOp :: Op -> [Type] -> Maybe Type
--iswfOp Add   [It,It] = Just It
--iswfOp Minus [It,It] = Just It
--iswfOp Mul   [It,It] = Just It
--iswfOp Less  [It,It] = Just It
--iswfOp Equal [It,It] = Just It
--iswfOp Index [At,It] = Just It
--iswfOp _      _      = Nothing
--
----  iswfCond ts - перевіряє коректність  типів операндів ts
----     умовного виразу і формує тип результату Just t або Nothing
--iswfCond :: [Type] -> Maybe Type
--iswfCond [It,It,It] = Just It
--iswfCond [It,At,At] = Just At
--iswfCond _          = Nothing
--
---- iswfAssignA ts перевіряє коректність  типів операндів ts
----   операції присвоювання значення елементу масива
--iswfAssignA :: [Type] -> Bool
--iswfAssignA [At,It,It] = True
--iswfAssignA _          = False
--
------ Дані для тестування  -----------------------
---- Стан для тестування
--sampleState :: StateP
--sampleState = [("x",I 5),("y",I 2),("a", A [(2,3),(0,4), (1,2)])]
--
--varEnv :: VarEnv
--varEnv = [("x",It), ("y",It), ("a",At)]
--
---- Функція максимум двох чисел
---- func biggest(m,n)= (m<n ? n : m)
--biggest :: FunDef
--biggest =("biggest",
--          ([Int "m", Int "n"],
--           Cond (OpApp  Less (Var "m") (Var "n"))  (Var "n")  (Var "m")
--           )
--         )
---- Функція, що обчислює число Фібоначчі
---- func fib(n) = (n<3 ? 1 : fib(n-1) + fib(n-2))
--fib :: FunDef
--fib = ("fib",
--       ([Int "n"],
--        Cond (OpApp Less (Var "n") (Const 3))
--             (Const 1)
--             (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const 1)])
--                        (FunApp "fib" [OpApp Minus (Var "n") (Const 2)]))
--       )
--      )
--
---- Функція - сума елементів масиву 0..n ...
---- func sumA(a[],n) = (n<0 ? 0 : a[n] + sumA (a,n-1))
--sumA :: FunDef
--sumA = ("sumA",
--        ([Arr "a", Int "n"],
--         Cond (OpApp Less (Var "n") (Const 0))
--              (Const 0)
--              (OpApp Add (OpApp Index (Var "a") (Var "n"))
--                         (FunApp "sumA" [Var "a", OpApp Minus (Var "n")(Const 1)])
--              )
--        )
--       )
--
--funEnv :: FunEnv
--funEnv = [("biggest",[It,It]),("fib", [It]),("sumA",[At,It])]
--
---- Приклад оператору - блоку
--sampleBlock :: Stmt
--sampleBlock = Block [Arr "b"]
--                 [AssignA "b" (Const 0) (Const 9), AssignA "b" (Const 2) (Const 5),
--                  AssignA "b" (Const 3) (Const 7), AssignA "b" (Const 5) (Const 1),
--                  Call "sumA1" [Var "b", Const 5]
--                 ]
--
---- Процедура - додавання двох чисел...
---- proc gAdd(x,y) gSum = x + y
--gAdd :: ProcDef
--gAdd = ("gAdd",
--        ([Int "x", Int "y"],
--         Assign "gSum" (OpApp Add (Var "x") (Var "y"))
--        )
--       )
--
---- Процедура - сума елементів масиву 0..n ...
---- proc sumA1(a[],n) {i;limit;
----      sA=0; i=0; limit=n+1;
----      while (i<limit){sA=sA+a[i]; i=i+1}
----                   }
--sumA1 :: ProcDef
--sumA1 = ("sumA1",
--         ([Arr "a", Int "n"],
--          Block [Int "i", Int "limit"]
--            [Assign "sA" (Const 0), Assign "i" (Const 0),
--             Assign "limit" (OpApp Add (Var "n") (Const 1)),
--             While (OpApp Less (Var "i") (Var "limit"))
--                   (Block []
--                     [Assign "sA" (OpApp Add (Var "sA")
--                                  (OpApp Index (Var "a") (Var "i"))),
--                      Assign "i" (OpApp Add (Var "i") (Const 1))
--                     ]
--                   )
--            ]
--         )
--        )
--
--procEnv :: ProcEnv
--procEnv = [("gAdd",[It,It]),("sumA1",[At,It])]
--
---- Повні програми
---- gSum;
---- proc gAdd(x,y) gSum = x + y
---- proc main() call gAdd(5,10)
--pr1 :: Program
--pr1 = ([Int "gSum"], [], [gAdd, ("main",([],Call "gAdd" [Const  5, Const 10]))])
--
---- sA
---- proc sumA1(a[],n) {i;limit; .... }
---- proc main() {b[]; b[0]=9; b[2]=5; b[3]=7; b[5]=1;
----                   call sumA1 (b,5)
----             }
--
--pr2 :: Program
--pr2 = ([Int "sA"], [],
--       [sumA1,
--        ("main",([], sampleBlock))
--       ])
