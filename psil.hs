-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# HLINT ignore "Use id" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Replace case with maybe" #-}
--
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
--------------------------------------------------------a-------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr

---------------------------------------------------------------------------
-- 1ère représentation interne des expressions de notre language         --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3)  ==  (((() . +) . 2) . 3)
--          ==>  Scons (Scons (Scons Snil (Ssym "+"))
--                            (Snum 2))
--                     (Snum 3)
--                   
-- (/ (* (- 68 32) 5) 9)
--     ==  (((() . /) . (((() . *) . (((() . -) . 68) . 32)) . 5)) . 9)
--     ==>
-- Scons (Scons (Scons Snil (Ssym "/"))
--              (Scons (Scons (Scons Snil (Ssym "*"))
--                            (Scons (Scons (Scons Snil (Ssym "-"))
--                                          (Snum 68))
--                                   (Snum 32)))
--                     (Snum 5)))
--       (Snum 9)

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                (pChar '\n' <|> eof); return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(shorthand-quote E)"
-- La notation "`E" est équivalente à "(shorthand-backquote E)"
-- La notation ",E" est équivalente à "(shorthand-comma E)"
pQuote :: Parser Sexp
pQuote = do { c <- satisfy (\c -> c `elem` "'`,"); pSpaces; e <- pSexp;
              return (Scons
                      (Scons Snil
                             (Ssym (case c of
                                     ',' -> "shorthand-comma"
                                     '`' -> "shorthand-backquote"
                                     _   -> "shorthand-quote")))
                      e) }

-- Une liste (Tsil) est de la forme ( [e .] {e} )
pTsil :: Parser Sexp
pTsil = do _ <- char '('
           pSpaces
           (do { _ <- char ')'; return Snil }
            <|> do hd <- (do e <- pSexp
                             pSpaces
                             (do _ <- char '.'
                                 pSpaces
                                 return e
                              <|> return (Scons Snil e)))
                   pLiat hd)
    where pLiat :: Sexp -> Parser Sexp
          pLiat hd = do _ <- char ')'
                        return hd
                 <|> do e <- pSexp
                        pSpaces
                        pLiat (Scons hd e)

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pTsil <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _ s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) = showHead (Scons e1 e2) . showString ")"
    where showHead (Scons Snil e') = showString "(" . showSexp' e'
          showHead (Scons e1' e2')
            = showHead e1' . showString " " . showSexp' e2'
          showHead e = showString "(" . showSexp' e . showString " ."

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire "Lambda"                                 --
---------------------------------------------------------------------------

type Var = String

-- Type Haskell qui décrit les types Psil.
data Ltype = Lint
           | Larw Ltype Ltype   -- Type "arrow" des fonctions.
           deriving (Show, Eq)

-- Type Haskell qui décrit les expressions Psil.
data Lexp = Lnum Int            -- Constante entière.
          | Lvar Var            -- Référence à une variable.
          | Lhastype Lexp Ltype -- Annotation de type.
          | Lapp Lexp Lexp      -- Appel de fonction, avec un argument.
          | Llet Var Lexp Lexp  -- Déclaration de variable locale.
          | Lfun Var Lexp       -- Fonction anonyme.
          deriving (Show, Eq)

-- Type Haskell qui décrit les déclarations Psil.
data Ldec = Ldec Var Ltype      -- Déclaration globale.
          | Ldef Var Lexp       -- Définition globale.
          deriving (Show, Eq)
          

-- Conversion de Sexp à Lambda --------------------------------------------
--conversion de sexp en liste pour cas currying
sexptolist :: Sexp -> [Sexp]
sexptolist sexp = listloop sexp []
    where
        listloop (Scons heads tails) acc = listloop heads (tails : acc)
        --base case
        listloop Snil acc = acc
        listloop _ _ = error ("Liste incorrecte: " ++ show sexp)

s2t :: Sexp -> Ltype
s2t (Ssym "Int") = Lint
s2t se@(Scons _ _) =
    let selist = sexptolist se
    in s2t' se selist
s2t se = error ("Type Psil inconnu: " ++ (showSexp se))

s2t' :: Sexp -> [Sexp] -> Ltype
s2t' se selist =
    case selist of
        (ta : Ssym "->" : tr : []) -> Larw (s2t ta) (s2t tr)
        _ | (last (init selist)) == Ssym "->" ->
              Larw (s2t (head selist)) (s2t' se (tail selist))
          | otherwise -> error ("Expression de type invalide:" ++ showSexp se)

s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s
s2l se@(Scons _ _) = 
    let selist = sexptolist se 
    in case selist of
        _ | length selist < 2 -> error (showSexp se)
          | otherwise -> go selist
    where
        go (Ssym ":" : e : t : []) = Lhastype (s2l e) (s2t t) 
        go (Ssym "let" : es) = case es of
            [Scons Snil (Scons (Scons Snil (Ssym v)) e1), e2] -> Llet v (s2l e1) (s2l e2)
            _ -> error "Expression de type invalide"

        go (Ssym "fun" : Ssym v : es) = case es of
            [] -> error "Expression manquante dans la définition de la fonction"
            [e] -> Lfun v (s2l e)
            _  -> Lfun v (go es)
            
        go (Ssym s : es) = case es of
            [] -> Lvar s
            _  -> foldl (\acc e -> Lapp acc (s2l e)) (Lvar s) es
        go (Snum n : []) = Lnum n
        go (e1 : es) = foldl (\acc e -> Lapp acc (s2l e)) (s2l e1) es
        go _ = error ("Expression invalide: " ++ showSexp se)


s2l se = error ("Expression Psil inconnue: " ++ (showSexp se))


s2d :: Sexp -> Ldec
s2d (Scons (Scons (Scons Snil (Ssym "def")) (Ssym v)) e) = Ldef v (s2l e)
s2d (Scons (Scons (Scons Snil (Ssym "dec")) (Ssym v)) t) = Ldec v (s2t t)
s2d se = error ("Déclaration Psil inconnue: " ++ showSexp se)


---------------------------------------------------------------------------
-- Vérification des types                                                --
---------------------------------------------------------------------------

-- Type des tables indexées par des `α` qui contiennent des `β`.
-- Il y a de bien meilleurs choix qu'une liste de paires, mais
-- ça suffit pour notre prototype.
type Map α β = [(α, β)]

-- Transforme une `Map` en une fonctions (qui est aussi une sorte de "Map").
mlookup :: Map Var β -> (Var -> β)
mlookup [] x = error ("Uknown variable: " ++ show x)
mlookup ((x,v) : xs) x' = if x == x' then v else mlookup xs x'

minsert :: Map Var β -> Var -> β -> Map Var β
minsert m x v = (x,v) : m

type TEnv = Map Var Ltype
type TypeError = String

-- L'environment de typage initial.
tenv0 :: TEnv
tenv0 = [("+", Larw Lint (Larw Lint Lint)),
         ("-", Larw Lint (Larw Lint Lint)),
         ("*", Larw Lint (Larw Lint Lint)),
         ("/", Larw Lint (Larw Lint Lint)),
         ("if0", Larw Lint (Larw Lint (Larw Lint Lint)))]

-- `check Γ e τ` vérifie que `e` a type `τ` dans le contexte `Γ`.
check :: TEnv -> Lexp -> Ltype -> Maybe TypeError
-- ¡¡COMPLÉTER ICI!!
check tenv (Lfun x body) (Larw t1 t2) = check (minsert tenv x t1) body t2
check _ (Lfun _ _) t = Just ("Type de fonction attendu: " ++ show t)

check tenv e t
  -- Essaie d'inférer le type et vérifie alors s'il correspond au
  -- type attendu.
  = let t' = synth tenv e
    in if t == t' then Nothing
       else Just ("Erreur de type: " ++ show t ++ " ≠ " ++ show t')

-- `synth Γ e` vérifie que `e` est typé correctement et ensuite "synthétise"
-- et renvoie son type `τ`.
synth :: TEnv -> Lexp -> Ltype
synth _    (Lnum _) = Lint
synth tenv (Lvar v) = mlookup tenv v
synth tenv (Lhastype e t) =
    case check tenv e t of
      Nothing -> t
      Just err -> error err

synth tenv (Lapp e1 e2) = 
  case synth tenv e1 of
    Larw t1 t2 -> 
      case check tenv e2 t1 of
        Nothing -> t2
        Just msg -> error msg
    _ -> error "Type de fonction attendu"

synth tenv (Llet v e1 e2) =
     let t1 = synth tenv e1
         tenv' = minsert tenv v t1
     in synth tenv' e2
    
synth _tenv e = error ("Incapable de trouver le type de: " ++ (show e))

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

-- Type des valeurs renvoyées par l'évaluateur.
data Value = Vnum Int
           | Vfun VEnv Var Lexp
           | Vop (Value -> Value)

type VEnv = Map Var Value

instance Show Value where
    showsPrec p  (Vnum n) = showsPrec p n
    showsPrec _p (Vfun _ _ _) = showString "<fermeture>"
    showsPrec _p (Vop _) = showString "<fonction>"

-- L'environnement initial qui contient les fonctions prédéfinies.
venv0 :: VEnv
venv0 = [("+", Vop (\ (Vnum x) -> Vop (\ (Vnum y) -> Vnum (x + y)))),
         ("-", Vop (\ (Vnum x) -> Vop (\ (Vnum y) -> Vnum (x - y)))),
         ("*", Vop (\ (Vnum x) -> Vop (\ (Vnum y) -> Vnum (x * y)))),
         ("/", Vop (\ (Vnum x) -> Vop (\ (Vnum y) -> Vnum (x `div` y)))),
         ("if0", Vop (\ (Vnum x) ->
                       case x of
                         0 -> Vop (\ v1 -> Vop (\ _ -> v1))
                         _ -> Vop (\ _ -> Vop (\ v2 -> v2))))]

-- La fonction d'évaluation principale.
eval :: VEnv -> Lexp -> Value
eval _venv (Lnum n) = Vnum n
eval venv (Lvar x) = mlookup venv x
-- ¡¡COMPLÉTER ICI!!

eval venv (Lhastype e _) = eval venv e
eval venv (Lfun x e) = Vop x' where x' val = eval (minsert venv x val) e 
eval venv (Llet x e1 e2) =
  let x' = eval venv e1 in 
    let venv' = minsert venv x x' in 
       eval venv' e2 

eval venv (Lapp e1 e2) =     
  case eval venv e1 of
        Vfun env' x body -> eval (minsert env' x (eval venv e2)) body 
        Vop x -> x (eval venv e2) 
        _ -> error "pas de type de fonction " 

-- État de l'évaluateur.
type EState = ((TEnv, VEnv),       -- Contextes de typage et d'évaluation.
               Maybe (Var, Ltype), -- Déclaration en attente d'une définition.
               [(Value, Ltype)])   -- Résultats passés (en ordre inverse).

-- Évalue une déclaration, y compris vérification des types.
process_decl :: EState -> Ldec -> EState
process_decl (env, Nothing, res) (Ldec x t) = (env, Just (x,t), res)
process_decl (env, Just (x', _), res) (decl@(Ldec _ _)) =
    process_decl (env, Nothing,
                  error ("Manque une définition pour: " ++ x') : res)
                 decl
process_decl ((tenv, venv), Nothing, res) (Ldef x e) =
    -- Le programmeur n'a *pas* fourni d'annotation de type pour `x`.
    let ltype = synth tenv e
        tenv' = minsert tenv x ltype
        val = eval venv e
        venv' = minsert venv x val
    in ((tenv', venv'), Nothing, (val, ltype) : res)
-- ¡¡COMPLÉTER ICI!!

process_decl ((tenv, venv), Just (x', t), res) (Ldef x e) =
  case check tenv e t of
    Just err -> error ("Erreur de type: " ++ err)
    Nothing -> let tenv' = minsert tenv x t
                   val = eval venv e
                   venv' = minsert venv x val
               in if x /= x'
                  then error ("on declare sans definir la variable ")
                  else ((tenv', venv'), Nothing, (val, t) : res)
  
---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

process_sexps :: EState -> [Sexp] -> IO ()
process_sexps _ [] = return ()
process_sexps es (sexp : sexps) =
    let decl = s2d sexp
        (env', pending, res) = process_decl es decl
    in do (hPutStr stdout)
            (concat
             (map (\ (val, ltyp) ->
                   "  " ++ show val ++ " : " ++ show ltyp ++ "\n")
              (reverse res)))
          process_sexps (env', pending, []) sexps

-- Lit un fichier contenant plusieurs Sexps, les évalue l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename
  = do filestring <- readFile filename
       let sexps = case parse pSexps filename filestring of
                     Left err -> error ("Parse error: " ++ show err)
                     Right es -> es
       process_sexps ((tenv0, venv0), Nothing, []) sexps

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

typeOf :: String -> Ltype
typeOf = synth tenv0 . lexpOf

valOf :: String -> Value
valOf = eval venv0 . lexpOf