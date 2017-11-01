module RealAnalysis (
    library, printingData
) where

import Prelude hiding ((/))

import Control.Arrow
import Control.Applicative
import qualified Data.Map as Map
import Data.Map (Map)

import Types
import Expansion
import TexBase
import Parser
import Tex
import RobotM
import Library
import Writeup
import Printing

        
expansionTableSource :: [(String, String)]
expansionTableSource = [
    ("sequencein(an,intersect(A,B))", "sequencein(an,A) & sequencein(an,B)"),
    ("in(x,intersect(A,B))", "in(x,A) & in(x,B)"),
    ("in(x,union(A,B))", "in(x,A) | in(x,B)"),
    ("subsetof(A,intersect(B,C))","subsetof(A,B) & subsetof(A,C)"),
    ("subsetof(A,B)", "forall x.(in(x,A) => in(x,B))"),
    ("in(x,preimage(f,U))", "in(applyfn(f,x),U)"),
    ("sequencein(x,preimage(f,U))", "sequencein(applyfnpointwise(f,x),U)"), 
    ("in(x,image(f,A))", "exists y.(in(y,A) & equals(applyfn(f,y),x))"),
    ("in(x,complement(A))", "notin(x,A)"),
    ("notin(x,A)","~in(x,A)"),
    --("setequals(A,B)", "subsetof(A,B) & subsetof(B,A)"),--new | clones issameas()
    ("injection(f)", "forall x y z.(equals(applyfn(f,x),z) & equals(applyfn(f,y),z) => equals(x,y))"),
    ("sequencein(an,A)", "forall n.(in(kthterm(an,n),A))"),
    ("open(A)", "forall x.(in(x, A) => exists delta.(forall y.(lessthan(d(x, y), delta) => in(y, A))))"),
    ("continuous(f)", "forall x epsilon.(exists delta.(forall y.(lessthan(d(x, y), delta) => lessthan(d(applyfn(f,x), applyfn(f,y)), epsilon))))"),
    ("tendsto(an,a)", "forall epsilon.(exists N.(forall n.(atleast(n,N) => lessthan(d(a,kthterm(an,n)),epsilon))))"),
    ("completespace(X)", "forall an.(cauchy(an) => converges(an))"),
    ("complete(A)", "forall an.(cauchy(an) & sequencein(an,A) => convergesin(an,A))"),
    ("converges(an)", "exists a.(tendsto(an,a))"),
    ("convergesin(an,A)", "exists a.(in(a,A) & tendsto(an,a))"),
    ("closed(A)", "forall an a.(sequencein(an,A) & tendsto(an,a) => in(a,A))"),
    ("cauchy(an)", "forall epsilon.(exists N.(forall m n.(atleast(m,N) & atleast(n,N) => lessthan(d(kthterm(an,m),kthterm(an,n)),epsilon))))"),
    --- NEW DEFS ---
    ("surjection(f, A, B)", "forall y.(in(y, B) => (exists x.(in(x, A) & equals(applyfn(f,x),y) & in(applyfn(f,x), B))))"), --new
--    ("surjection(f, A, B)", "forall y.(in(y, B) => in(y, image(f, A)))"),--new
    ("islimitpoint(x, A)", "exists an.(sequencein(an, A) & tendsto(an, x) & forall n.(~equals(x, kthterm(an, n))))"), --new
    ("bounded(an)", "exists N.(forall n.(lessthan(d(0,kthterm(an,n)), N)))"),--new
    ("bounded(A)", "exists N.(forall a.(in(a,A) => lessthan(a, N)))"),--new
    ("compact(A)", "closed(A) & bounded(A)"),--new
    ("increasing(an)", "forall m n.(lessthan(m,n) => lessthan(kthterm(an, m), kthterm(an, n)))"), --new
    ("decreasing(an)", "forall m n.(lessthan(m,n) => lessthan(kthterm(an, n), kthterm(an, m)))"), --new
    ("monotone(an)", "increasing(an) | decreasing(an)"),--new
    ("sup(s, A)", "forall a.(in(a, A) => lessthan(a, s)) & forall v.(lessthan(v, s) => exists b.(in(b, A) & lessthan(v, b)))"), -- new
    ("inf(s, A)", "forall a.(in(a, A) => lessthan(s, A)) & forall v.(lessthan(s, v) => exists b.(in(b, A) & lessthan(b, v)))") -- new
    
    --("surjection(f)","forall y.(exists x.(equals(applyfn(f,x), y)))"),
--    ("isEmptySet(A)", "forall x.(notin(x, A))")
    ]

expansionTable :: [(FormulaWithSlots, Formula)]
expansionTable = [(f / allVariablesInFormula f, f') |
                  (f, f') <- (parse formula *** parse formula) <$> expansionTableSource]


--NB: the term rewriting code does not use renameFormula  -- so DO NOT ever  put quantifiers on RHS
--     of the rewrite table.
--  (This is only relevant if we introduce sets, etc., so that formulae can be inside terms.)

rewriteTableSource = [
    ("applyfn(compose(f,g),x)", "applyfn(f,applyfn(g,x))"),
    ("kthterm(applyfnpointwise(f,an),n)", "applyfn(f,kthterm(an,n))")
  ]


rewriteTable :: [(Term, [Variable], Term)]
rewriteTable = [(t, allVariablesInTerm t, t') |
                  (t, t') <- (parse term *** parse term) <$> rewriteTableSource ]

----------------------------------------------------------------------------------------------------


termPatterns' :: Map String Pattern
termPatterns' = Map.fromList [
    ("intersect", "%\\cap %"),
    ("union", "%\\cup %"),
    ("compose", "%\\circ %"),
    ("applyfn", "%(%)"),
    ("image", "%(%)"),
    ("preimage", "%^{-1}(%)"),
    ("complement", "(%)^c"),
    ("product", "%%"),
    ("inverse", "%^{-1}"),
    ("e", "e"),
    ("ball", "B_{%}(%)")
  ]

formulaPatterns' :: Map String Pattern
formulaPatterns' = Map.fromList [
    ("in", "$%\\in %$"),
    ("notin", "$%\\notin %$"),
    ("subsetof", "$%\\subset %$"),
    ("equals", "$% = %$"),
    ("issameas", "$% = %$"),
    ("lessthan", "$% < %$"),
    ("atleast", "$%\\geqslant %$"),
    ("atmost", "$%\\leqslant %$"),
    ("open", "$%$ is open"),
    ("closed", "$%$ is closed"),
    ("complete", "$%$ is a complete space"),
    ("completespace", "$%$ is complete"),
    ("closedin", "$%$ is closed in $%$"),
    ("sequencein", "$%$ is a sequence in $%$"),
    ("injection", "$%$ is an injection"),
    ("continuous", "$%$ is continuous"),
    ("cauchy", "$%$ is Cauchy"),
    ("converges", "$%$ converges"),
    ("convergesin", "$%$ converges in $%$"),
    ("mapsto", "$%:%\\mapsto %$"),
    ("subgroup", "$%$ is a subgroup"),
    ("closedunderinverse", "$%$ is closed under taking inverses"),
    ("closedundermultiplication", "$%$ is closed under multiplication"),
    ("surjection", "$%$ from $%$ to $%$ is a surjection"),
    ("isEmptySet", "$%$ is an empty set"),
    ("bounded", "$%$ is bounded"),
    ("monotone", "$%$ is monotone")
  ]

nounPatterns' :: Map String Pattern
nounPatterns' = Map.fromList [
    ("in", "element of $%$"),
    ("subsetof", "subset of $%$"),
    ("sequencein", "sequence in $%$"),
    ("injection", "injection")
  ]

adjectivePatterns' :: Map String Pattern
adjectivePatterns' = Map.fromList [
    ("open", "open"),
    ("closed", "closed"),
    ("complete", "complete"),
    ("continuous", "continuous"),
    ("cauchy", "Cauchy")
  ]

printingData = PrintingData termPatterns' formulaPatterns' nounPatterns' adjectivePatterns'

library = Library [
    Result "transitivity" [
        parse formula "lessthan(alpha,beta)",
        parse formula "atmost(beta,gamma)"]
        (parse formula "lessthan(alpha,gamma)"),
    Result "" [
        parse formula "subsetof(A,B)",
        parse formula "subsetof(B,C)"]
        (parse formula "subsetof(A,C)"),
    Result "a closed set contains its limit points" [
        parse formula "closedin(F,X)",
        parse formula "sequencein(an,F)",
        parse formula "tendsto(an,a)"]
        (parse formula "in(a,F)"),
    Result "Monotone Convergence Theorem" [
        parse formula "bounded(an)",
        parse formula "monotone(an)"]
        (parse formula "converges(an)"),
    Result "every convergent sequence is bounded" [
        parse formula "converges(an)"]
        (parse formula "bounded(an)"),
    Result "" [
        parse formula "in(x, complement(A))",
        parse formula "in(x, complement(B))"]
        (parse formula "notin(x, union(A, B))")
    ]
-- library = Library [
--     Result "" [
--         parse formula "subset(A,ball(x,beta))",
--         parse formula "atmost(beta,gamma)"]
--         (parse formula "subset(A,ball(x,gamma))"),
--     Result "transitivity" [
--         parse formula "atmost(alpha,beta)"]
--         (parse formula "subsetof(ball(x,alpha),ball(x,beta))"),
--     Result "" [
--         parse formula "closedunderinverse(H)",
--         parse formula "in(x,H)"]
--         (parse formula "in(inverse(x),H)"),
--     Result "" [
--         parse formula "closedundermultiplication(H)",
--         parse formula "in(x,H)",
--         parse formula "in(y,H)"]
--         (parse formula "in(product(x,y),H)"),
    -- Result "" [parse formula "issameas(A,complement(B))"] (parse formula "equals(twoBack(f,A),complement(preimage(f,B)))")
--  ]
    [
     Solution [parse variable "eta"] [
        parse formula "atmost(eta, alpha)",
        parse formula "atmost(eta, beta)"]
        [parse term "min(alpha, beta)"]
    ]
 expansionTable
 rewriteTable


