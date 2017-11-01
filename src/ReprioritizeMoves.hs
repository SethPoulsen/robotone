module ReprioritizeMoves (
    newPriority
) where

import Prelude hiding ((/))

import Types
import Rename
import Expansion
import Match
import Move
import Library

import DeletionMoves
import TidyingMoves
import ApplyingMoves
import Suspension

defaultPriority :: [MoveType]
defaultPriority = [
  --Deletion
    deleteDone,
    deleteDoneDisjunct,
--    deleteRedundantHypothesis, --move 1
    deleteDangling, --move 2
    deleteUnmatchable, -- move 3
  --Tidying (4-9)
    peelAndSplitUniversalConditionalTarget, --move 4
--    flipNegativeTarget, --move 5
--    flipNegativeHypothesis, --move 6
    splitDisjunctiveHypothesis, --move 7
--    splitConjunctiveTarget, --move 8
    splitDisjunctiveTarget,
    peelBareUniversalTarget, -- move 9
    removeTarget, --move 10
    collapseSubtableauTarget,
  --Applying
    forwardsReasoning, --move 11
    forwardsLibraryReasoning, --move 11
    expandPreExistentialHypothesis, --move 13
    elementaryExpansionOfHypothesis, --move 12
    backwardsReasoning, --move 14
    backwardsLibraryReasoning, --move 14
    elementaryExpansionOfTarget, --move 15
    expandPreUniversalTarget, --move 16
    solveBullets,
    automaticRewrite,
  --Suspension
    unlockExistentialUniversalConditionalTarget, --move 17
    unlockExistentialTarget,
    expandPreExistentialTarget,
    convertDiamondToBullet,
    rewriteVariableVariableEquality,
    rewriteVariableTermEquality
  ]

newPriority :: [MoveType]
newPriority = defaultPriority
