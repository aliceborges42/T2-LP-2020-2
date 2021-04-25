module Interpreter where

import AbsLI
import Prelude hiding (lookup)


{- Mudando o tipo do RContext, aceitamos variaveis do tipo Valor(int, string e bool)
pegando os codigos feitos nas questoes anteriores e os adaptando para que suportem esse novo RContext
basicamente gerenciando os tipos de retorno
-}

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
                     Right (ValorInt i) -> if ( i /= 0) 
                                 then  case (execute context stm) of
                                          Left error -> Left error
                                          Right cont -> execute cont (SWhile exp stm)
                                 else Right context
   SdoWhile stm exp -> case (execute context stm) of
                        Left error-> Left error
                        Right cont -> execute cont (SWhile exp stm)
   STry [] _ finaly -> execute context (SBlock finaly)
   STry (tStm:tStms) catch finaly -> case execute context tStm of
                                       Left error -> execute context (SBlock (catch  ++ finaly))
                                       Right cont -> execute cont (STry tStms catch finaly) 
                        


data Valor = ValorStr String |
             ValorInt Integer |
             ValorBool Bool
-- note que ja foi adicionado um novo contrutor de tipo para valor booleano

s :: Valor -> String             
s (ValorStr str) = str
i :: Valor -> Integer
i (ValorInt vint) = vint 
-- a funcao "b" abaixo recupera o valor booleano dentro de um valor
b (ValorBool vbool) = vbool


instance Show Valor where
 show (ValorInt vint) = show vint
 show (ValorStr vstr) = vstr
 show (ValorBool vb) = show vb

-- precisamos que Valor esteja em Eq para podermos especificar os casos de teste em Testes.hs
instance Eq Valor where
 (ValorInt i1) == (ValorInt i2) =  i1 == i2
 (ValorStr s1) == (ValorStr s2) =  s1 == s2
 (ValorBool b1) == (ValorBool b2) = b1 == b2




{- Dica: o tipo de eval deve mudar para
 eval :: RContext -> Exp -> Either ErrorMessage Integer
-}
eval :: RContext -> Exp -> Either ErrorMessage Valor
eval context x = case x of
    EAdd exp0 exp  ->  case eval context exp0 of
                     Left error -> Left error
                     Right (ValorInt i) -> case eval context exp of
                                 Left error2 -> Left error2
                                 Right (ValorInt i2) -> Right (ValorInt(i + i2))--eval context exp0 + eval context exp
    ESub exp0 exp  ->  case eval context exp0 of
                     Left error -> Left error
                     Right (ValorInt i) -> case eval context exp of
                                 Left error2 -> Left error2
                                 Right (ValorInt i2) -> Right (ValorInt(i - i2))--eval context exp0 - eval context exp
    EMul exp0 exp  ->  case eval context exp0 of
                     Left error -> Left error
                     Right (ValorInt i) -> case eval context exp of
                                 Left error2 -> Left error2
                                 Right (ValorInt i2) -> Right (ValorInt(i * i2))
    EDiv exp0 exp  -> case eval context exp0 of
                     Left error -> Left error
                     Right (ValorInt i) -> case eval context exp of
                                 Left error2 -> Left error2
                                 Right (ValorInt i2) -> if (i2 /= 0)
                                             then Right (ValorInt(i `div` i2))
                                             else Left "divisao por 0"
    EInt n  -> Right (ValorInt n)
    EVar id  -> Right (lookup context (getStr id))
    ECon exp0 exp -> case (eval context exp0) of 
                        Left error -> Left error
                        Right (ValorStr valor) -> case eval context exp of 
                                       Left error -> Left error
                                       Right (ValorStr valor2) -> Right( ValorStr ( valor ++ valor2))
    EStr str -> Right (ValorStr str)
    -- adicione abaixo um padrao e comportamento associado a expressao Or
    EOr exp0 exp -> case (eval context exp0) of 
                     Left error -> Left error
                     Right (ValorBool valor1)-> case (eval context exp) of
                                    Left error -> Left error
                                    Right (ValorBool valor2)-> Right (ValorBool ( valor1 || valor2))
    -- adicione abaixo um padrao e comportamento associado a expressao And
    EAnd exp0 exp -> case (eval context exp0) of 
                     Left error -> Left error
                     Right (ValorBool valor1)-> case (eval context exp) of
                                    Left error -> Left error
                                    Right (ValorBool valor2)-> Right (ValorBool ( valor1 && valor2))
    -- adicione abaixo um padrao e comportamento associado a expressao Not
    ENot exp -> case(eval context exp) of
                  Left error -> Left error
                  Right valor->  Right( ValorBool(not (b valor)))
    -- adicione abaixo um padrao e comportamento associado ao literal true
    ETrue -> Right (ValorBool (True))
    -- adicione abaixo um padrao e comportamento associado ao literal false
    EFalse -> Right (ValorBool (False))




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



type RContext = [(String,Valor)]

getStr :: Ident -> String
getStr (Ident s) = s

lookup :: RContext -> String -> Valor
lookup ((i,v):cs) s
   | i == s = v
   | otherwise = lookup cs s

update :: RContext -> String -> Valor -> RContext
update [] s v = [(s,v)]
update ((i,v):cs) s nv
  | i == s = (i,nv):cs
  | otherwise = (i,v) : update cs s nv
