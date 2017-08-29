module Problems (
  problems
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
