module Problems (
  problems
  ) where

import Prelude hiding ((/))

import Control.Arrow
import Control.Applicative
import qualified Data.Map as Map
import Data.Map (Map)

import Types

problems = [ifGandFareInjectionsThenGoFisInjection,
            fAintersectBsubsetfAintersectfB,
            preimAcapBequalsPreimAcapPreimB1,
            preimAcapBequalsPreimAcapPreimB2,
            preimAcupBequalsPreimAcupPreimB1,
            preimAcupBequalsPreimAcupPreimB2
            ]


----------------------------------------------------------------------
-- INJECTION & SURJECTION FUNCTIONS PROOFS ------------------------------
-- Surjection is NOT working for most cases
----------------------------------------------------------------------

iffInjectionThenfAcapfBsubsetfAcapB = Problem
    "If $f$ is an injection then $f(A)\\cap f(B)\\subset f(A\\cap B)$"
        ["injection(f)"]
        "subsetof(intersect(image(f,A),image(f,B)),image(f,intersect(A,B)))"

ifGandFareInjectionsThenGoFisInjection = Problem --New
    "If g,f are injections then (g o f) is an injection."
    ["injection(f)",
     "injection(g)"]
    "injection(compose(g,f))"

ifGandFareSurjectionsThenGoFisSurjection = Problem --Not working
    "If g,f are surjections then (g o f) is a surjection."
    ["surjection(f, A, B)",
     "surjection(g, B, C)"]
    "surjection(compose(g,f), A, C)"

ifSurjectionThenCompfASubsetfCompA = Problem --Not working
    "If $f$ is a surjection then $f(A)^c \\subset f(A^c)$"
        ["surjection(f, A, C)"]
        "subsetof(complement(image(f, A)), image(f, complement(A)))"


----------------------------------------------------------------------
-- SEQUENCE PROOFS ------------------------------
----------------------------------------------------------------------

convergenceIsCauchy = Problem -- not working
    "If $x_n$ is convergent then $x_n$ is Cauchy."
    ["exists a.(forall epsilon.(exists N.(forall n.(atleast(n,N) => lessthan(d(a,kthterm(an,n)),epsilon)))))"]
    "cauchy(an)"

boundedMonotoneImpliesConvergence = Problem -- not working/cause error
    "If $a_n$ is bounded and monotone, then $a_n$ converges."
    ["exists N.(forall n.(lessthan(d(0,kthterm(an,n)), N)))",
     "forall m n.(lessthan(m,n) => lessthan(kthterm(an, m), kthterm(an, n))) | forall m n.(lessthan(m,n) => lessthan(kthterm(an, n), kthterm(an, m)))"]
    "exists a.(forall epsilon.(exists N.(forall n.(atleast(n,N) => lessthan(d(a,kthterm(an,n)),epsilon)))))"
    

----------------------------------------------------------------------
-- IMAGE & PREIMAGE OF FUNCTIONS PROOFS ------------------------------
----------------------------------------------------------------------

setAisSubsetOfPreimageOfImageOfA = Problem
    "Prove that $A \\subseteq f^{-1}(f(A))$"
    []
    "subsetof(A, preimage(f, image(f, A)))"

ffminusoneAsubsetA = Problem "Prove that $f(f^{-1}(A))\\subset A$"
    []
    "subsetof(image(f,preimage(f,A)),A)"

fAintersectBsubsetfAintersectfB = Problem -- New
    "Prove that $f(A \\cap B) \\subset f(A) \\cap f(B)$"
    ["in(y, image(f,intersect(A,B)))"]
    "in(y, intersect(image(f,A), image(f,B)))"

preimAcapBequalsPreimAcapPreimB1 = Problem -- New
    "Prove that $f^{-1}(A \\cap B) \\subset f^{-1}(A) \\cap f^{-1}(B)$"
    ["in(x, preimage(f, intersect(A,B)))"]
    "in(x,intersect(preimage(f,A), preimage(f,B)))"

preimAcapBequalsPreimAcapPreimB2 = Problem -- New
    "Prove that $f^{-1}(A) \\cap f^{-1}(B) \\subset f^{-1}(A \\cap B)$"
    []
    "subsetof(intersect(preimage(f,A),preimage(f,B)), preimage(f, intersect(A,B)))"
    
preimAcupBequalsPreimAcupPreimB1 = Problem -- New
    "Prove that $f^{-1}(A \\cup B) \\subset f^{-1}(A) \\cup f^{-1}(B)$"
    []
    "subsetof(preimage(f, union(A,B)), union(preimage(f,A), preimage(f,B)))"

preimAcupBequalsPreimAcupPreimB2 = Problem -- New
    "Prove that $f^{-1}(A) \\cup f^{-1}(B) \\subset f^{-1}(A \\cup B)$"
    []
    "subsetof(union(preimage(f,A), preimage(f,B)), preimage(f, union(A,B)))"

----------------------------------------------------------------------
-- SETS PROOFS ------------------------------
----------------------------------------------------------------------
compOfIntersectionEqualsUnionOfComps1 = Problem -- not working
    "Prove that $(A \\cap B)^c \\subset A^c \\cup B^c$"
    []
    "subsetof(complement(intersect(A,B)), union(complement(A), complement(B)))"

compOfIntersectionEqualsUnionOfComps2 = Problem -- not working
    "Prove that $A^c \\cup B^c \\subset (A \\cap B)^c $"
    []
    "subsetof( union(complement(A), complement(B)), complement(intersect(A,B)))"


compOfUnionEqualsIntersectionOfComps = Problem -- not working
    "Prove that $(A \\cup B)^c = A^c \\cap B^c$"
    []
    "issameas(complement(union(A,B)), intersect(complement(A), complement(B)))"


union3OpenSets = Problem
    "If $A$, $B$,and $C$ are open sets, then $A \\cup (B \\cup C)$ is also open." --"The union of three open sets is open."
   ["open(A)",
    "open(B)",
    "open(C)"]
    "open(union(A,union(B,C)))"

intersection3OpenSets = Problem
    "If $A$, $B$,and $C$ are open sets, then $A \\cap (B \\cap C)$ is also open." --"The intersection of three open sets is open."
   ["open(A)",
    "open(B)",
    "open(C)"]
    "open(intersect(A,intersect(B,C)))"

unionOpenSets = Problem
    "If $A$ and $B$ are open sets, then $A \\cup B$ is also open." --"The union of two open sets is open."
   ["open(A)",
    "open(B)"]
    "open(union(A,B))"

intersectionOpenSets = Problem
    "If $A$ and $B$ are open sets, then $A \\cap B$ is also open." --"The intersection of two open sets is open."
   ["open(A)",
    "open(B)"]
    "open(intersect(A,B))"

unionClosedSets = Problem
    "If $A$ and $B$ are closed sets, then $A \\cup B$ is also closed." --"The union of two closed sets is closed."
   ["closed(A)",
    "closed(B)"]
    "closed(union(A,B))"

intersectionClosedSets = Problem
    "If $A$ and $B$ are closed sets, then $A \\cap B$ is also closed." --"The intersection of two closed sets is closed."
   ["closed(A)",
    "closed(B)"]
    "closed(intersect(A,B))"

----------------------------------------------------------------------
-- CONTINUITY PROOFS ------------------------------
----------------------------------------------------------------------


continuousPreimageClosed = Problem
    "The pre-image of a closed set $U$ under a continuous function $f$ is closed."
   ["continuous(f)",
    "closed(U)"]
    "closed(preimage(f,U))"

continuousPreimageOpen = Problem
    "The pre-image of an open set $U$ under a continuous function $f$ is open."
   ["continuous(f)",
    "open(U)"]
    "open(preimage(f,U))"

compositionContinuousFunctions = Problem
    "If $f$ and $g$ are continuous functions, then $g \\circ f$ is continuous." --"A composition of continuous functions is continuous."
   ["continuous(f)",
    "continuous(g)"]
    "continuous(compose(g,f))"

continuousFunctionsPreserveLimits = Problem
    "If $f$ is a continuous function and $(a_n) \\to a$, then $(f(a_n)) \\to f(a)$"-- "A continuous function preserves limits."
   ["continuous(f)",
    "tendsto(an,a)"]
    "tendsto(applyfnpointwise(f,an),applyfn(f,a))"

