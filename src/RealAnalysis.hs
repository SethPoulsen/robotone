module RealAnalysis (
    problems, library, printingData
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

-- negativeEx1 = Problem
--     "If $f$ is a car then $f(A)\\cap f(B)\\subset f(A\\cap B)$"
--         ["car(f)"]
--         "subsetof(intersect(image(f,A),image(f,B)),image(f,intersect(A,B)))"

-- ifGoFisOntoThenGisOnto = Problem
--     "If $g . f$ is onto, then $g$ is onto."
--     ["notsurjection(g)"]
--     "notsurjection(compose(g,f))"
-- ifFsurjectiveThenfAUBsubsetfAUfB = Problem
--     "If f is surjective, then $f(A\\cup B) \\subset f(A)\\cup f(B)$"
--     ["surjection(f)"]
--     "subsetof(image(f, union(A,B)), union(image(f, A), image(f,B)))"


-- surjectionDef = Problem
--     "If f is a surjection then for all y in D there exists x in R such that f x = y."
--     ["surjection(f, D, C)"]
--     "forall y.(in(y, C) => (exists x.(in(x, D) & equals(applyfn(f,x),y))))"
      
-- ifFsurjectiveThenH1equalsH2 = Problem
--     "If $f$ is a surjection and $h o f = g o f$, then $h = g$"
--     ["surjection(f, D, C)",
--     "forall x.(equals(applyfn(compose(h,f), x), applyfn(compose(g,f), x)))"]
--     "forall x.(equals(applyfn(h, x), applyfn(g, x)))"

iffInjectionThenfAcapfBsubsetfAcapB = Problem
    "If $f$ is an injection then $f(A)\\cap f(B)\\subset f(A\\cap B)$"
        ["injection(f)"]
        "subsetof(intersect(image(f,A),image(f,B)),image(f,intersect(A,B)))"

ifGandFareSurjectionsThenGoFisSurjection = Problem
    "If g,f are surjections then (g o f) is a surjection."
    ["surjection(f, A, B)",
     "surjection(g, B, C)"]
    "surjection(compose(g,f), A, C)"

-- ifSurjectionThenCompfASubsetfCompA = Problem
--     "If $f$ is a surjection then $f(A)^c \\subset f(A^c)$"
--         ["surjection(f, A, C)"]
--         "subsetof(complement(image(f, A)), image(f, complement(A)))"

-- injectionDef = Problem
--                "injectionDef"
--                ["injection(f)"]
--                "forall x y z.(equals(applyfn(f,x),z) & equals(applyfn(f,y),z) => equals(x,y))"
    
problems = [--ifGoFisOntoThenGisOnto,
            --ifFsurjectiveThenfAUBsubsetfAUfB,
--            surjectionDef,
--            ifFsurjectiveThenH1equalsH2,
          ifGandFareSurjectionsThenGoFisSurjection,
--            ifSurjectionThenCompfASubsetfCompA,
--             negativeEx1,
             iffInjectionThenfAcapfBsubsetfAcapB]
        
expansionTableSource :: [(String, String)]
expansionTableSource = [
    ("in(x,intersect(A,B))", "in(x,A) & in(x,B)"),
    ("in(x,union(A,B))", "in(x,A) | in(x,B)"),
    ("in(x,preimage(f,U))", "in(applyfn(f,x),U)"),
    ("in(x,image(f,A))", "exists y.(in(y,A) & equals(applyfn(f,y),x))"),
    ("in(x,complement(A))", "notin(x,A)"),
    ("notin(x,A)","¬in(x,A)"),
    ("subsetof(A,intersect(B,C))","subsetof(A,B) & subsetof(A,C)"),
    ("subsetof(A,B)", "forall x.(in(x,A) => in(x,B))"),
    ("injection(f)", "forall x y z.(equals(applyfn(f,x),z) & equals(applyfn(f,y),z) => equals(x,y))"),
    ("converges(an)", "exists a.(tendsto(an,a))"),
    ("cauchy(an)", "forall epsilon.(exists N.(forall m n.(atleast(m,N) & atleast(n,N) => lessthan(d(kthterm(an,m),kthterm(an,n)),epsilon))))"),
--    ("surjection(f)", "forall x y.(equals(applyfn(f,x),z) => equals(x,y))"),
--    ("car(f)", "injection(f)"),
    ("surjection(f, A, B)", "forall y.(in(y, B) => (exists x.(in(x, A) & equals(applyfn(f,x),y))))")
--    ("notsurjection(f)","¬surjection(f)"),
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
    ("complement", "%^c"),
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
--    ("notsurjection", "$%$ is not a surjection"),
    ("isEmptySet", "$%$ is an empty set")
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
    Result "transitivity" [
        parse formula "lessthan(alpha,beta)",
        parse formula "atmost(beta,gamma)"]
        (parse formula "lessthan(alpha,gamma)")
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


