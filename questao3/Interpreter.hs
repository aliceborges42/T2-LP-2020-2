module Interpreter where

import AbsLI
import Prelude hiding (lookup)


type ErrorMessage = String

{- Dica: somente o tipo de executeP precisa mudar conforme a sugestao abaixo, 
   mas a sua definicao (corpo) pode ficar a mesma
   executeP :: RContext -> Program  -> Either ErrorMessage RContext
-}
executeP :: RContext -> Program  -> Either ErrorMessage RContext
executeP context (Prog stm) = execute context stm
   

{- Dica: o tipo de execute deve mudar para 
 execute :: RContext -> Stm -> Either ErrorMessage RContext   
 Alem disso, o corpo dessa funcao deve ser modificado, pois eval
 retorna tipo diferente agora, ou seja, a avaliacao pode falhar
 e, consequentemente o execute tambem. Assim, todos tipos de comandos 
 serao afetados
-}
--rafatoração para o caso de dar erro(divisão por 0)
execute :: RContext -> Stm -> Either ErrorMessage RContext
execute context x = case x of
   SAss id exp -> case (eval context exp) of
                     Left error -> Left error
                     Right i -> Right (update context (getStr id) i)
   SBlock [] -> Right context
   SBlock (s:stms) -> case (execute context s) of
                        Left error -> Left error
                        Right cont->  execute cont (SBlock stms) 
   SWhile exp stm -> case (eval context exp) of
                     Left error -> Left error
                     Right i -> if ( i /= 0) 
                                 then  case (execute context stm) of
                                          Left error -> Left error
                                          Right cont -> execute cont (SWhile exp stm)
                                 else Right context


{- Dica: o tipo de eval deve mudar para
 eval :: RContext -> Exp -> Either ErrorMessage Integer
-}
-- Ao mudar o retorno implica na refatoração dos daemais casos, de forma a parar o programa repassando o erro(divisão por 0)
eval :: RContext -> Exp -> Either ErrorMessage Integer
eval context x = case x of
    EAdd exp0 exp  ->  case eval context exp0 of
                     Left error -> Left error
                     Right i -> case eval context exp of
                                 Left error2 -> Left error2
                                 Right i2 -> Right (i + i2)--eval context exp0 + eval context exp
    ESub exp0 exp  ->  case eval context exp0 of
                     Left error -> Left error
                     Right i -> case eval context exp of
                                 Left error2 -> Left error2
                                 Right i2 -> Right (i - i2)--eval context exp0 - eval context exp
    EMul exp0 exp  ->  case eval context exp0 of
                     Left error -> Left error
                     Right i -> case eval context exp of
                                 Left error2 -> Left error2
                                 Right i2 -> Right (i * i2)
    EDiv exp0 exp  -> case eval context exp0 of
                     Left error -> Left error
                     Right i -> case eval context exp of
                                 Left error2 -> Left error2
                                 Right i2 -> if (i2 /= 0)
                                             then Right (i `div` i2)
                                             else Left "divisao por 0"
    EInt n  -> Right n
    EVar id  -> Right (lookup context (getStr id))
{-  algumas dicas abaixo...para voce adaptar o codigo acima
    EDiv e1 e2 -> case eval context e1 of 
                    Right ve1 -> case eval context e2 of 
                                   Right ve2 -> if (ve2 == 0)
                                                 then Left ("divisao por 0 na expressao: " 
                                                            ++ show (EDiv e1 e2))
                                                 else Right (ve1 `div` ve2)
                                  Left msg -> Left msg  
                    Left msg -> Left msg  
    EInt n  ->  Right n 
-}                


-- Dica: voce nao precisa mudar o codigo a partir daqui
type RContext = [(String,Integer)]

getStr :: Ident -> String
getStr (Ident s) = s

lookup :: RContext -> String -> Integer
lookup ((i,v):cs) s
   | i == s = v
   | otherwise = lookup cs s

update :: RContext -> String -> Integer -> RContext
update [] s v = [(s,v)]
update ((i,v):cs) s nv
  | i == s = (i,nv):cs
  | otherwise = (i,v) : update cs s nv
