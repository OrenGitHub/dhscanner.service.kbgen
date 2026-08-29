module Factify

where

-- general imports
import Data.Set ( Set )
import Data.Maybe ( mapMaybe )
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import System.FilePath ( takeDirectory )

-- project imports
import Cfg
import Callable
import Location
import qualified Fqn
import qualified Token
import qualified Kbgen
import qualified Bitcode

factify :: Callable -> Set Kbgen.Fact
factify (Method m) = factifyMethod m
factify (Lambda l) = factifyLambda l
factify (Script s) = factifyScript s
factify (Function f) = factifyFunc f

factifyScript :: Callable.ScriptContent -> Set Kbgen.Fact
factifyScript s = List.foldl' Set.union Set.empty (factifyScript' s)

factifyScript' :: Callable.ScriptContent -> [ Set Kbgen.Fact ]
factifyScript' s = [
        getCallsRelatedFacts (Callable.scriptBody s),
        getConstStringsRelatedFacts (Callable.scriptBody s),
        getConstIntegersRelatedFacts (Callable.scriptBody s),
        getConstNullsRelatedFacts (Callable.scriptBody s),
        getGatedReturnFacts (Callable.scriptBody s),
        getComparisonFacts (Callable.scriptBody s),
        getAssignmentsRelatedFacts (Callable.scriptBody s)
    ]

factifyLambda :: Callable.LambdaContent -> Set Kbgen.Fact
factifyLambda l = List.foldl' Set.union Set.empty (factifyLambda' l)

factifyLambda' :: Callable.LambdaContent -> [ Set Kbgen.Fact ]
factifyLambda' l = [
        getDataflowFacts (Callable.lambdaBody l),
        getCallsRelatedFacts (Callable.lambdaBody l),
        getParamsRelatedFacts (Kbgen.Callable (Callable.lambdaLocation l)) (Callable.lambdaBody l),
        getConstStringsRelatedFacts (Callable.lambdaBody l),
        getConstIntegersRelatedFacts (Callable.lambdaBody l),
        getConstNullsRelatedFacts (Callable.lambdaBody l),
        getGatedReturnFacts (Callable.lambdaBody l),
        getComparisonFacts (Callable.lambdaBody l),
        getCallableReturnsFacts (Kbgen.Callable (Callable.lambdaLocation l)) (Callable.lambdaBody l)
    ]

factifyFunc :: Callable.FunctionContent -> Set Kbgen.Fact
factifyFunc f = List.foldl' Set.union Set.empty (factifyFunc' f)

factifyFunc' :: Callable.FunctionContent -> [ Set Kbgen.Fact ]
factifyFunc' f = [
        getCallsRelatedFacts (Callable.funcBody f),
        getDataflowFacts (Callable.funcBody f),
        getCallableRelatedFacts (Callable.funcName f) (Callable.funcLocation f),
        getParamsRelatedFacts (Kbgen.Callable (Callable.funcLocation f)) (Callable.funcBody f),
        getConstStringsRelatedFacts (Callable.funcBody f),
        getConstIntegersRelatedFacts (Callable.funcBody f),
        getConstNullsRelatedFacts (Callable.funcBody f),
        getGatedReturnFacts (Callable.funcBody f),
        getComparisonFacts (Callable.funcBody f),
        getCallableReturnsFacts (Kbgen.Callable (Callable.funcLocation f)) (Callable.funcBody f)
    ]

factifyMethod :: Callable.MethodContent -> Set Kbgen.Fact
factifyMethod m = List.foldl' Set.union Set.empty (factifyMethod' m)

factifyMethod' :: Callable.MethodContent -> [ Set Kbgen.Fact ]
factifyMethod' m = [
        getClassRelatedFacts m,
        getDataflowFacts (Callable.methodBody m),
        getCallsRelatedFacts (Callable.methodBody m),
        getParamsRelatedFacts (Kbgen.Callable (Callable.methodLocation m)) (Callable.methodBody m),
        getConstStringsRelatedFacts (Callable.methodBody m),
        getConstIntegersRelatedFacts (Callable.methodBody m),
        getConstNullsRelatedFacts (Callable.methodBody m),
        getGatedReturnFacts (Callable.methodBody m),
        getComparisonFacts (Callable.methodBody m),
        getCallableReturnsFacts (Kbgen.Callable (Callable.methodLocation m)) (Callable.methodBody m)
    ]

getCallableRelatedFacts :: Token.FuncName -> Location -> Set Kbgen.Fact
getCallableRelatedFacts name loc = let
    f = Location.filename loc
    f' = Kbgen.FuncDefinedInFile f
    d = Kbgen.FuncDefinedInDir (takeDirectory f)
    in Set.singleton (Kbgen.FuncDefCtor (Kbgen.FuncDef (Kbgen.Func loc) name f' d))

getConstStringsRelatedFacts :: Cfg -> Set Kbgen.Fact
getConstStringsRelatedFacts = getConstStringsRelatedFacts' . instructions

getConstStringsRelatedFacts' :: Set Bitcode.Instruction -> Set Kbgen.Fact
getConstStringsRelatedFacts' = Foldable.foldl' getConstStringsRelatedFacts'' Set.empty

getConstStringsRelatedFacts'' :: Set Kbgen.Fact -> Bitcode.Instruction -> Set Kbgen.Fact
getConstStringsRelatedFacts'' acc i = Set.union acc (getConstStringsRelatedFacts''' i)

getConstStringsRelatedFacts''' :: Bitcode.Instruction -> Set Kbgen.Fact
getConstStringsRelatedFacts''' (Bitcode.Instruction _ i) = getConstStringsRelatedFacts'''' i

getConstStringsRelatedFacts'''' :: Bitcode.InstructionContent -> Set Kbgen.Fact
getConstStringsRelatedFacts'''' (Bitcode.Call c) = getConstStringsRelatedFactsFromCall c
getConstStringsRelatedFacts'''' (Bitcode.Unop u) = getConstStringsRelatedFactsFromUnop u
getConstStringsRelatedFacts'''' (Bitcode.Binop b) = getConstStringsRelatedFactsFromBinop b
getConstStringsRelatedFacts'''' (Bitcode.Return r) = getConstStringsRelatedFactsFromReturn r
getConstStringsRelatedFacts'''' (Bitcode.Assign a) = getConstStringsRelatedFactsFromAssign a
getConstStringsRelatedFacts'''' (Bitcode.FieldWrite f) = getConstStringsRelatedFactsFromFieldWrite f
getConstStringsRelatedFacts'''' (Bitcode.SubscriptRead s) = getConstStringsRelatedFactsFromSubscriptRead s
getConstStringsRelatedFacts'''' (Bitcode.SubscriptWrite s) = getConstStringsRelatedFactsFromSubscriptWrite s
getConstStringsRelatedFacts'''' _ = Set.empty

getConstStringsRelatedFactsFromCall :: Bitcode.CallContent -> Set Kbgen.Fact
getConstStringsRelatedFactsFromCall = getConstStringsFromValues . Bitcode.args

getConstStringsRelatedFactsFromUnop :: Bitcode.UnopContent -> Set Kbgen.Fact
getConstStringsRelatedFactsFromUnop = getConstStringsFromValue . Bitcode.unopLhs

getConstStringsRelatedFactsFromBinop :: Bitcode.BinopContent -> Set Kbgen.Fact
getConstStringsRelatedFactsFromBinop b = let
    lhs = getConstStringsFromValue (Bitcode.binopLhs b)
    rhs = getConstStringsFromValue (Bitcode.binopRhs b)
    in Set.union lhs rhs

getConstStringsRelatedFactsFromReturn :: Bitcode.ReturnContent -> Set Kbgen.Fact
getConstStringsRelatedFactsFromReturn (Bitcode.ReturnContent Nothing) = Set.empty
getConstStringsRelatedFactsFromReturn (Bitcode.ReturnContent (Just v)) = getConstStringsFromValue v

getConstStringsRelatedFactsFromAssign :: Bitcode.AssignContent -> Set Kbgen.Fact
getConstStringsRelatedFactsFromAssign = getConstStringsFromValue . Bitcode.assignInput

getConstStringsRelatedFactsFromFieldWrite :: Bitcode.FieldWriteContent -> Set Kbgen.Fact
getConstStringsRelatedFactsFromFieldWrite = getConstStringsFromValue . Bitcode.fieldWriteInput

getConstStringsRelatedFactsFromSubscriptRead :: Bitcode.SubscriptReadContent -> Set Kbgen.Fact
getConstStringsRelatedFactsFromSubscriptRead = getConstStringsFromValue . Bitcode.subscriptReadIdx

getConstStringsRelatedFactsFromSubscriptWrite :: Bitcode.SubscriptWriteContent -> Set Kbgen.Fact
getConstStringsRelatedFactsFromSubscriptWrite s = let
    index = getConstStringsFromValue (Bitcode.subscriptWriteIdx s)
    value = getConstStringsFromValue (Bitcode.subscriptWriteInput s)
    in Set.union index value

getConstStringsFromValues :: [ Bitcode.Value ] -> Set Kbgen.Fact
getConstStringsFromValues = List.foldl' Set.union Set.empty . map getConstStringsFromValue

getConstStringsFromValue :: Bitcode.Value -> Set Kbgen.Fact
getConstStringsFromValue (Bitcode.ConstValueCtor (Bitcode.ConstStrValue s)) = getConstStringsFromValue' s
getConstStringsFromValue _ = Set.empty

getConstStringsFromValue' :: Token.ConstStr -> Set Kbgen.Fact
getConstStringsFromValue' value = let
    location = Kbgen.ConstStr (Token.constStrLocation value)
    constString = Kbgen.ConstString location value
    in Set.singleton (Kbgen.ConstStringCtor constString)

getConstIntegersRelatedFacts :: Cfg -> Set Kbgen.Fact
getConstIntegersRelatedFacts = getConstIntegersRelatedFacts' . instructions

getConstIntegersRelatedFacts' :: Set Bitcode.Instruction -> Set Kbgen.Fact
getConstIntegersRelatedFacts' = Foldable.foldl' getConstIntegersRelatedFacts'' Set.empty

getConstIntegersRelatedFacts'' :: Set Kbgen.Fact -> Bitcode.Instruction -> Set Kbgen.Fact
getConstIntegersRelatedFacts'' acc i = Set.union acc (getConstIntegersRelatedFacts''' i)

getConstIntegersRelatedFacts''' :: Bitcode.Instruction -> Set Kbgen.Fact
getConstIntegersRelatedFacts''' (Bitcode.Instruction _ i) = getConstIntegersRelatedFacts'''' i

getConstIntegersRelatedFacts'''' :: Bitcode.InstructionContent -> Set Kbgen.Fact
getConstIntegersRelatedFacts'''' (Bitcode.Call c) = getConstIntegersRelatedFactsFromCall c
getConstIntegersRelatedFacts'''' (Bitcode.Unop u) = getConstIntegersRelatedFactsFromUnop u
getConstIntegersRelatedFacts'''' (Bitcode.Binop b) = getConstIntegersRelatedFactsFromBinop b
getConstIntegersRelatedFacts'''' (Bitcode.Return r) = getConstIntegersRelatedFactsFromReturn r
getConstIntegersRelatedFacts'''' (Bitcode.Assign a) = getConstIntegersRelatedFactsFromAssign a
getConstIntegersRelatedFacts'''' (Bitcode.FieldWrite f) = getConstIntegersRelatedFactsFromFieldWrite f
getConstIntegersRelatedFacts'''' (Bitcode.SubscriptRead s) = getConstIntegersRelatedFactsFromSubscriptRead s
getConstIntegersRelatedFacts'''' (Bitcode.SubscriptWrite s) = getConstIntegersRelatedFactsFromSubscriptWrite s
getConstIntegersRelatedFacts'''' _ = Set.empty

getConstIntegersRelatedFactsFromCall :: Bitcode.CallContent -> Set Kbgen.Fact
getConstIntegersRelatedFactsFromCall = getConstIntegersFromValues . Bitcode.args

getConstIntegersRelatedFactsFromUnop :: Bitcode.UnopContent -> Set Kbgen.Fact
getConstIntegersRelatedFactsFromUnop = getConstIntegersFromValue . Bitcode.unopLhs

getConstIntegersRelatedFactsFromBinop :: Bitcode.BinopContent -> Set Kbgen.Fact
getConstIntegersRelatedFactsFromBinop b = let
    lhs = getConstIntegersFromValue (Bitcode.binopLhs b)
    rhs = getConstIntegersFromValue (Bitcode.binopRhs b)
    in Set.union lhs rhs

getConstIntegersRelatedFactsFromReturn :: Bitcode.ReturnContent -> Set Kbgen.Fact
getConstIntegersRelatedFactsFromReturn (Bitcode.ReturnContent Nothing) = Set.empty
getConstIntegersRelatedFactsFromReturn (Bitcode.ReturnContent (Just v)) = getConstIntegersFromValue v

getConstIntegersRelatedFactsFromAssign :: Bitcode.AssignContent -> Set Kbgen.Fact
getConstIntegersRelatedFactsFromAssign = getConstIntegersFromValue . Bitcode.assignInput

getConstIntegersRelatedFactsFromFieldWrite :: Bitcode.FieldWriteContent -> Set Kbgen.Fact
getConstIntegersRelatedFactsFromFieldWrite = getConstIntegersFromValue . Bitcode.fieldWriteInput

getConstIntegersRelatedFactsFromSubscriptRead :: Bitcode.SubscriptReadContent -> Set Kbgen.Fact
getConstIntegersRelatedFactsFromSubscriptRead = getConstIntegersFromValue . Bitcode.subscriptReadIdx

getConstIntegersRelatedFactsFromSubscriptWrite :: Bitcode.SubscriptWriteContent -> Set Kbgen.Fact
getConstIntegersRelatedFactsFromSubscriptWrite s = let
    index = getConstIntegersFromValue (Bitcode.subscriptWriteIdx s)
    value = getConstIntegersFromValue (Bitcode.subscriptWriteInput s)
    in Set.union index value

getConstIntegersFromValues :: [ Bitcode.Value ] -> Set Kbgen.Fact
getConstIntegersFromValues = List.foldl' Set.union Set.empty . map getConstIntegersFromValue

getConstIntegersFromValue :: Bitcode.Value -> Set Kbgen.Fact
getConstIntegersFromValue (Bitcode.ConstValueCtor (Bitcode.ConstIntValue i)) = getConstIntegersFromValue' i
getConstIntegersFromValue _ = Set.empty

getConstIntegersFromValue' :: Token.ConstInt -> Set Kbgen.Fact
getConstIntegersFromValue' value = let
    location = Kbgen.ConstInt (Token.constIntLocation value)
    constInt = Kbgen.ConstInteger location value
    in Set.singleton (Kbgen.ConstIntegerCtor constInt)

getConstNullsRelatedFacts :: Cfg -> Set Kbgen.Fact
getConstNullsRelatedFacts = getConstNullsRelatedFacts' . instructions

getConstNullsRelatedFacts' :: Set Bitcode.Instruction -> Set Kbgen.Fact
getConstNullsRelatedFacts' = Foldable.foldl' getConstNullsRelatedFacts'' Set.empty

getConstNullsRelatedFacts'' :: Set Kbgen.Fact -> Bitcode.Instruction -> Set Kbgen.Fact
getConstNullsRelatedFacts'' acc i = Set.union acc (getConstNullsRelatedFacts''' i)

getConstNullsRelatedFacts''' :: Bitcode.Instruction -> Set Kbgen.Fact
getConstNullsRelatedFacts''' (Bitcode.Instruction _ i) = getConstNullsRelatedFacts'''' i

getConstNullsRelatedFacts'''' :: Bitcode.InstructionContent -> Set Kbgen.Fact
getConstNullsRelatedFacts'''' (Bitcode.Call c) = getConstNullsRelatedFactsFromCall c
getConstNullsRelatedFacts'''' (Bitcode.Unop u) = getConstNullsRelatedFactsFromUnop u
getConstNullsRelatedFacts'''' (Bitcode.Binop b) = getConstNullsRelatedFactsFromBinop b
getConstNullsRelatedFacts'''' (Bitcode.Return r) = getConstNullsRelatedFactsFromReturn r
getConstNullsRelatedFacts'''' (Bitcode.Assign a) = getConstNullsRelatedFactsFromAssign a
getConstNullsRelatedFacts'''' (Bitcode.FieldWrite f) = getConstNullsRelatedFactsFromFieldWrite f
getConstNullsRelatedFacts'''' (Bitcode.SubscriptRead s) = getConstNullsRelatedFactsFromSubscriptRead s
getConstNullsRelatedFacts'''' (Bitcode.SubscriptWrite s) = getConstNullsRelatedFactsFromSubscriptWrite s
getConstNullsRelatedFacts'''' _ = Set.empty

getConstNullsRelatedFactsFromCall :: Bitcode.CallContent -> Set Kbgen.Fact
getConstNullsRelatedFactsFromCall = getConstNullsFromValues . Bitcode.args

getConstNullsRelatedFactsFromUnop :: Bitcode.UnopContent -> Set Kbgen.Fact
getConstNullsRelatedFactsFromUnop = getConstNullsFromValue . Bitcode.unopLhs

getConstNullsRelatedFactsFromBinop :: Bitcode.BinopContent -> Set Kbgen.Fact
getConstNullsRelatedFactsFromBinop b = let
    lhs = getConstNullsFromValue (Bitcode.binopLhs b)
    rhs = getConstNullsFromValue (Bitcode.binopRhs b)
    in Set.union lhs rhs

getConstNullsRelatedFactsFromReturn :: Bitcode.ReturnContent -> Set Kbgen.Fact
getConstNullsRelatedFactsFromReturn (Bitcode.ReturnContent Nothing) = Set.empty
getConstNullsRelatedFactsFromReturn (Bitcode.ReturnContent (Just v)) = getConstNullsFromValue v

getConstNullsRelatedFactsFromAssign :: Bitcode.AssignContent -> Set Kbgen.Fact
getConstNullsRelatedFactsFromAssign = getConstNullsFromValue . Bitcode.assignInput

getConstNullsRelatedFactsFromFieldWrite :: Bitcode.FieldWriteContent -> Set Kbgen.Fact
getConstNullsRelatedFactsFromFieldWrite = getConstNullsFromValue . Bitcode.fieldWriteInput

getConstNullsRelatedFactsFromSubscriptRead :: Bitcode.SubscriptReadContent -> Set Kbgen.Fact
getConstNullsRelatedFactsFromSubscriptRead = getConstNullsFromValue . Bitcode.subscriptReadIdx

getConstNullsRelatedFactsFromSubscriptWrite :: Bitcode.SubscriptWriteContent -> Set Kbgen.Fact
getConstNullsRelatedFactsFromSubscriptWrite s = let
    index = getConstNullsFromValue (Bitcode.subscriptWriteIdx s)
    value = getConstNullsFromValue (Bitcode.subscriptWriteInput s)
    in Set.union index value

getConstNullsFromValues :: [ Bitcode.Value ] -> Set Kbgen.Fact
getConstNullsFromValues = List.foldl' Set.union Set.empty . map getConstNullsFromValue

getConstNullsFromValue :: Bitcode.Value -> Set Kbgen.Fact
getConstNullsFromValue (Bitcode.ConstValueCtor (Bitcode.ConstNullValue n)) = getConstNullsFromValue' n
getConstNullsFromValue _ = Set.empty

getConstNullsFromValue' :: Token.ConstNull -> Set Kbgen.Fact
getConstNullsFromValue' value = let
    location = Token.constNullLocation value
    constNull = Kbgen.ConstNull location
    in Set.singleton (Kbgen.ConstNullCtor constNull)

-- |
-- Extract @kb_callable_returns_value( Callable, ReturnedValue )@ and
-- @kb_callable_returns_without_value( Callable, ReturnStmtLocation )@
-- facts by walking every 'Bitcode.Return' in the callable\'s CFG.
--
--     * If the return carries an expression, fire the value-carrying
--       fact anchored at the returned value\'s location. Downstream
--       predicates ( @kb_const_null@, @kb_call_resolved@, ... ) add
--       semantic meaning to that location.
--     * If the return is bare ( @return;@ ), fire the void fact
--       anchored at the return statement\'s own location.
--
-- Parser-injected fall-through returns ( see
-- @ensureCallableBodyEndsWithReturn@ in the TS parser ) flow through
-- the first branch with the returned-value location coinciding with
-- the callable\'s header location, so downstream analyses can still
-- distinguish parser-injected returns from source-level returns.
getCallableReturnsFacts :: Kbgen.Callable -> Cfg -> Set Kbgen.Fact
getCallableReturnsFacts callable = getCallableReturnsFacts' callable . instructions

getCallableReturnsFacts' :: Kbgen.Callable -> Set Bitcode.Instruction -> Set Kbgen.Fact
getCallableReturnsFacts' callable = Foldable.foldl' (getCallableReturnsFacts'' callable) Set.empty

getCallableReturnsFacts'' :: Kbgen.Callable -> Set Kbgen.Fact -> Bitcode.Instruction -> Set Kbgen.Fact
getCallableReturnsFacts'' callable acc i = Set.union acc (getCallableReturnsFacts''' callable i)

getCallableReturnsFacts''' :: Kbgen.Callable -> Bitcode.Instruction -> Set Kbgen.Fact
getCallableReturnsFacts''' callable (Bitcode.Instruction loc (Bitcode.Return r)) = mkCallableReturnsFact callable loc r
getCallableReturnsFacts''' _        _                                             = Set.empty

mkCallableReturnsFact :: Kbgen.Callable -> Location -> Bitcode.ReturnContent -> Set Kbgen.Fact
mkCallableReturnsFact callable _ (Bitcode.ReturnContent (Just v)) = let
    rv = Kbgen.ReturnedValue (Bitcode.locationValue v)
    in Set.singleton (Kbgen.CallableReturnsValueCtor (Kbgen.CallableReturnsValue callable rv))
mkCallableReturnsFact callable retLoc (Bitcode.ReturnContent Nothing) =
    Set.singleton (Kbgen.CallableReturnsWithoutValueCtor (Kbgen.CallableReturnsWithoutValue callable retLoc))

-- |
-- Extract @kb_gated_return( Cond, ReturnedValue )@ facts for the
-- \"single-return in an empty-else then-block\" idiom.
--
-- __Two tightened invariants__
--
--     1. Empty else       ( the if-statement has no else branch ).
--     2. Exactly one       @return e;@ anywhere inside the then-body
--                          -- possibly nested inside a further @if@
--                          ( e.g. @if (X) { if (Y) return e; }@ ).
--
-- Any @if@ that violates either invariant emits nothing. Rejected
-- shapes :
--
--     * @if (X) return a; else return b;@       ( non-empty else )
--     * @if (X) { if (Y) return a; return b; }@ ( two returns )
--     * @while (X) { return e; }@               ( loop back-edge : the
--                                                 sibling\'s single
--                                                 successor is not a
--                                                 Nop-at-if-loc )
--
-- __How the codegen makes this cheap__
--
-- 'codeGenStmtIf' anchors both paired @Assume@ nodes at
-- @Ast.stmtIfLocation stmtIf@, and 'Cfg.parallelNormalCfgs' now anchors
-- the diamond\'s /join Nop/ at that same location. This gives us an
-- O(1) empty-else test :
--
--     empty else  <=>  the sibling Assume\'s single CFG successor is a
--                      Nop whose Location equals the Assume\'s Location.
--
-- With the join Nop known we then do a /bounded/ forward BFS from the
-- current Assume that stops at ( and does not expand past ) the join
-- Nop. So we never even visit the post-@if@ tail of the function.
--
-- Per-Assume cost drops from O( |reach(Assume)| ) to O( |then-block| ).
--
-- __Nested-if is welcome__
--
-- For @if (X) { if (Y) return e; }@ both the outer and the inner Assume
-- pass all the invariants, so /two/ facts are emitted :
--
-- @
-- kb_gated_return( loc(X), loc(e) ).
-- kb_gated_return( loc(Y), loc(e) ).
-- @
--
-- __Polarity__
--
-- Captured implicitly : the @Assume(_, False)@ side of an empty-else
-- diamond has the join Nop as its only successor, so its bounded BFS
-- visits nothing beyond itself and no fact is emitted from that side.
getGatedReturnFacts :: Cfg -> Set Kbgen.Fact
getGatedReturnFacts Cfg.Empty = Set.empty
getGatedReturnFacts (Cfg.Normal content) = getGatedReturnFactsFromContent content

getGatedReturnFactsFromContent :: Cfg.Content -> Set Kbgen.Fact
getGatedReturnFactsFromContent content = let
    allEdges = Cfg.actualEdges (Cfg.edges content)
    allAssumes = collectAssumeNodes allEdges
    in Foldable.foldl' (accumGatedReturnFactsForAssume allEdges allAssumes) Set.empty allAssumes

collectAssumeNodes :: Set Cfg.Edge -> Set Cfg.Node
collectAssumeNodes edges' = let
    fromNodes = Set.map Cfg.from edges'
    toNodes = Set.map Cfg.to edges'
    in Set.filter isAssumeNode (Set.union fromNodes toNodes)

isAssumeNode :: Cfg.Node -> Bool
isAssumeNode (Cfg.Node i) = isAssumeInstruction i

isAssumeInstruction :: Bitcode.Instruction -> Bool
isAssumeInstruction (Bitcode.Instruction _ (Bitcode.Assume _)) = True
isAssumeInstruction _ = False

accumGatedReturnFactsForAssume :: Set Cfg.Edge -> Set Cfg.Node -> Set Kbgen.Fact -> Cfg.Node -> Set Kbgen.Fact
accumGatedReturnFactsForAssume allEdges allAssumes acc a = Set.union acc (gatedReturnFactsForAssume allEdges allAssumes a)

gatedReturnFactsForAssume :: Set Cfg.Edge -> Set Cfg.Node -> Cfg.Node -> Set Kbgen.Fact
gatedReturnFactsForAssume allEdges allAssumes a = gatedReturnFactsForAssume' allEdges allAssumes a (getAssumeConditionValueFromNode a)

gatedReturnFactsForAssume' :: Set Cfg.Edge -> Set Cfg.Node -> Cfg.Node -> Maybe Bitcode.Value -> Set Kbgen.Fact
gatedReturnFactsForAssume' _ _ _ Nothing = Set.empty
gatedReturnFactsForAssume' allEdges allAssumes a (Just condVal) = gatedReturnFactsForAssume'' allEdges a condVal (findSibling allAssumes a)

-- | No sibling ==> we cannot verify the \"empty else\" invariant, so we
-- refuse to emit anything ( conservative for this tightened tier ).
gatedReturnFactsForAssume'' :: Set Cfg.Edge -> Cfg.Node -> Bitcode.Value -> Maybe Cfg.Node -> Set Kbgen.Fact
gatedReturnFactsForAssume'' _ _ _ Nothing = Set.empty
gatedReturnFactsForAssume'' allEdges a condVal (Just sibling) = gatedReturnFactsForAssume''' allEdges a condVal (findEmptyElseJoinNop allEdges a sibling)

-- | If we could not identify a valid empty-else join Nop, no fact is
-- emitted ( it is not an @if@ with an empty else, or the CFG shape is
-- otherwise not what 'codeGenStmtIf' produces ).
gatedReturnFactsForAssume''' :: Set Cfg.Edge -> Cfg.Node -> Bitcode.Value -> Maybe Cfg.Node -> Set Kbgen.Fact
gatedReturnFactsForAssume''' _ _ _ Nothing = Set.empty
gatedReturnFactsForAssume''' allEdges a condVal (Just joinNop) = let
    thenRegion = boundedForwardReach allEdges a joinNop
    in singleReturnFact condVal (returnedValuesInRegion thenRegion)

-- | O(1) empty-else check. The sibling Assume\'s single CFG successor
-- must be a Nop whose Location equals the sibling Assume\'s own
-- Location -- guaranteed to hold for empty-else if-statements by the
-- 'codeGenStmtIf' + 'Cfg.parallelNormalCfgs' construction. When the
-- check passes we return that join Nop; otherwise 'Nothing'.
findEmptyElseJoinNop :: Set Cfg.Edge -> Cfg.Node -> Cfg.Node -> Maybe Cfg.Node
findEmptyElseJoinNop allEdges a sibling = findEmptyElseJoinNop' (assumeLocation a) (Set.toList (successorsOf allEdges sibling))

findEmptyElseJoinNop' :: Maybe Location -> [ Cfg.Node ] -> Maybe Cfg.Node
findEmptyElseJoinNop' (Just ifLoc) [ n ] = findEmptyElseJoinNop'' ifLoc n (isNopAt ifLoc n)
findEmptyElseJoinNop' _ _ = Nothing

findEmptyElseJoinNop'' :: Location -> Cfg.Node -> Bool -> Maybe Cfg.Node
findEmptyElseJoinNop'' _ n True = Just n
findEmptyElseJoinNop'' _ _ False = Nothing

assumeLocation :: Cfg.Node -> Maybe Location
assumeLocation (Cfg.Node (Bitcode.Instruction loc (Bitcode.Assume _))) = Just loc
assumeLocation _ = Nothing

isNopAt :: Location -> Cfg.Node -> Bool
isNopAt loc (Cfg.Node (Bitcode.Instruction l Bitcode.Nop)) = l == loc
isNopAt _ _ = False

returnedValuesInRegion :: Set Cfg.Node -> [ Bitcode.Value ]
returnedValuesInRegion = mapMaybe returnedValueOfNode . Set.toList

returnedValueOfNode :: Cfg.Node -> Maybe Bitcode.Value
returnedValueOfNode (Cfg.Node i) = getReturnedValueMaybe i

singleReturnFact :: Bitcode.Value -> [ Bitcode.Value ] -> Set Kbgen.Fact
singleReturnFact condVal [ retVal ] = Set.singleton (mkGatedReturnFact condVal retVal)
singleReturnFact _ _ = Set.empty

findSibling :: Set Cfg.Node -> Cfg.Node -> Maybe Cfg.Node
findSibling allAssumes a = List.find (areSiblingAssumes a) (Set.toList (Set.delete a allAssumes))

areSiblingAssumes :: Cfg.Node -> Cfg.Node -> Bool
areSiblingAssumes (Cfg.Node x) (Cfg.Node y) = areSiblingAssumeInstructions x y

areSiblingAssumeInstructions :: Bitcode.Instruction -> Bitcode.Instruction -> Bool
areSiblingAssumeInstructions (Bitcode.Instruction l1 (Bitcode.Assume a1)) (Bitcode.Instruction l2 (Bitcode.Assume a2)) = l1 == l2 && Bitcode.assumeValue a1 == Bitcode.assumeValue a2 && Bitcode.assumeTruthy a1 /= Bitcode.assumeTruthy a2
areSiblingAssumeInstructions _ _ = False

-- | Forward BFS from @start@ that treats @boundary@ as a fence : nodes
-- are visited, but @boundary@ is never expanded ( its successors are
-- not explored ) and it is dropped from the returned visited set. On
-- CFGs without a reachable @boundary@ ( e.g. all paths from @start@
-- return before joining ) this degenerates to a plain forward BFS.
boundedForwardReach :: Set Cfg.Edge -> Cfg.Node -> Cfg.Node -> Set Cfg.Node
boundedForwardReach allEdges start boundary = boundedForwardReach' allEdges boundary (Set.singleton start) (Set.singleton start)

boundedForwardReach' :: Set Cfg.Edge -> Cfg.Node -> Set Cfg.Node -> Set Cfg.Node -> Set Cfg.Node
boundedForwardReach' allEdges boundary frontier visited = boundedForwardReach'' allEdges boundary frontier visited (Set.null frontier)

boundedForwardReach'' :: Set Cfg.Edge -> Cfg.Node -> Set Cfg.Node -> Set Cfg.Node -> Bool -> Set Cfg.Node
boundedForwardReach'' _ boundary _ visited True = Set.delete boundary visited
boundedForwardReach'' allEdges boundary frontier visited False = let
    stepped = Foldable.foldl' (accumSuccessors allEdges) Set.empty frontier
    newlyReached = Set.difference stepped visited
    newVisited = Set.union visited newlyReached
    nextFrontier = Set.delete boundary newlyReached
    in boundedForwardReach' allEdges boundary nextFrontier newVisited

accumSuccessors :: Set Cfg.Edge -> Set Cfg.Node -> Cfg.Node -> Set Cfg.Node
accumSuccessors allEdges acc n = Set.union acc (successorsOf allEdges n)

successorsOf :: Set Cfg.Edge -> Cfg.Node -> Set Cfg.Node
successorsOf allEdges n = Set.map Cfg.to (Set.filter (isOutgoingFrom n) allEdges)

isOutgoingFrom :: Cfg.Node -> Cfg.Edge -> Bool
isOutgoingFrom n e = Cfg.from e == n

mkGatedReturnFact :: Bitcode.Value -> Bitcode.Value -> Kbgen.Fact
mkGatedReturnFact condVal retVal = let
    cond = Kbgen.Cond (Bitcode.locationValue condVal)
    rv = Kbgen.ReturnedValue (Bitcode.locationValue retVal)
    in Kbgen.GatedReturnCtor (Kbgen.GatedReturn cond rv)

-- | Extract @kb_comparison( Cond, Lhs, Rhs, Op )@ facts from the CFG.
--
-- Every @Bitcode.Binop@ whose operator tag is 'Bitcode.EqOp' or
-- 'Bitcode.NeqOp' fires a fact keyed by the binop's *output* variable
-- location ( so it composes with 'GatedReturn' whose @Cond@ slot uses
-- the same location for an @if@ that early-returns on the comparison
-- result ). Operators outside the equality family collapse to
-- 'Bitcode.OtherOp' and produce no fact.
getComparisonFacts :: Cfg -> Set Kbgen.Fact
getComparisonFacts = getComparisonFacts' . instructions

getComparisonFacts' :: Set Bitcode.Instruction -> Set Kbgen.Fact
getComparisonFacts' = Foldable.foldl' getComparisonFacts'' Set.empty

getComparisonFacts'' :: Set Kbgen.Fact -> Bitcode.Instruction -> Set Kbgen.Fact
getComparisonFacts'' acc i = Set.union acc (getComparisonFactsFromInstruction i)

getComparisonFactsFromInstruction :: Bitcode.Instruction -> Set Kbgen.Fact
getComparisonFactsFromInstruction (Bitcode.Instruction _ (Bitcode.Binop b)) = comparisonFactFromBinop b
getComparisonFactsFromInstruction _ = Set.empty

comparisonFactFromBinop :: Bitcode.BinopContent -> Set Kbgen.Fact
comparisonFactFromBinop b = case Bitcode.binopOperator b of
    Bitcode.EqOp    -> Set.singleton (mkComparisonFact b "eq")
    Bitcode.NeqOp   -> Set.singleton (mkComparisonFact b "neq")
    Bitcode.OtherOp -> Set.empty

mkComparisonFact :: Bitcode.BinopContent -> String -> Kbgen.Fact
mkComparisonFact b op = let
    cond = Kbgen.Cond (Bitcode.locationVariable (Bitcode.binopOutput b))
    lhs  = Kbgen.Lhs  (Bitcode.locationValue    (Bitcode.binopLhs    b))
    rhs  = Kbgen.Rhs  (Bitcode.locationValue    (Bitcode.binopRhs    b))
    in Kbgen.ComparisonCtor (Kbgen.Comparison cond lhs rhs (Kbgen.ComparisonOp op))

getAssumeConditionValueFromNode :: Cfg.Node -> Maybe Bitcode.Value
getAssumeConditionValueFromNode (Cfg.Node i) = getAssumeConditionValue i

getAssumeConditionValue :: Bitcode.Instruction -> Maybe Bitcode.Value
getAssumeConditionValue (Bitcode.Instruction _ (Bitcode.Assume a)) = Just (Bitcode.assumeValue a)
getAssumeConditionValue _ = Nothing

getReturnedValueMaybe :: Bitcode.Instruction -> Maybe Bitcode.Value
getReturnedValueMaybe (Bitcode.Instruction _ (Bitcode.Return (Bitcode.ReturnContent (Just v)))) = Just v
getReturnedValueMaybe _ = Nothing

getAssignmentsRelatedFacts :: Cfg -> Set Kbgen.Fact
getAssignmentsRelatedFacts = getAssignmentsRelatedFacts' . instructions

getAssignmentsRelatedFacts' :: Set Bitcode.Instruction -> Set Kbgen.Fact
getAssignmentsRelatedFacts' = Foldable.foldMap' getAssignmentRelatedFacts

getAssignmentRelatedFacts :: Bitcode.Instruction -> Set Kbgen.Fact
getAssignmentRelatedFacts (Bitcode.Instruction _ (Bitcode.Assign assign)) = getAssignmentRelatedFacts' assign
getAssignmentRelatedFacts _ = Set.empty

getAssignmentRelatedFacts' :: Bitcode.AssignContent -> Set Kbgen.Fact
getAssignmentRelatedFacts' (Bitcode.AssignContent var value) = getAssignmentRelatedFacts'' var value

getAssignmentRelatedFacts'' :: Bitcode.Variable -> Bitcode.Value -> Set Kbgen.Fact
getAssignmentRelatedFacts'' (Bitcode.SrcVariableCtor var) value = getAssignmentRelatedFacts''' var value
getAssignmentRelatedFacts'' _ _ = Set.empty

getAssignmentRelatedFacts''' :: Bitcode.SrcVariable -> Bitcode.Value -> Set Kbgen.Fact
getAssignmentRelatedFacts''' (Bitcode.SrcVariable _ varName) value = let
    assignedValue = Kbgen.AssignedValue (Bitcode.locationValue value)
    fact = Kbgen.AssignValueToToplevelVarName assignedValue varName
    in Set.singleton (Kbgen.AssignValueToToplevelVarNameCtor fact)

getParamsRelatedFacts :: Kbgen.Callable -> Cfg -> Set Kbgen.Fact
getParamsRelatedFacts callable body = getParamsRelatedFacts' callable (justParams (instructions body))

getParamsRelatedFacts' :: Kbgen.Callable -> Set Bitcode.ParamDeclContent -> Set Kbgen.Fact
getParamsRelatedFacts' callable = Foldable.foldl' (getParamsRelatedFacts'' callable) Set.empty

getParamsRelatedFacts'' :: Kbgen.Callable -> Set Kbgen.Fact -> Bitcode.ParamDeclContent -> Set Kbgen.Fact
getParamsRelatedFacts'' c facts p = Set.union facts (Set.fromList (getParamsRelatedFacts''' c p))

getParamsRelatedFacts''' :: Kbgen.Callable -> Bitcode.ParamDeclContent -> [ Kbgen.Fact ]
getParamsRelatedFacts''' callable param = [
        getParamNameFact param,
        getParamResolvedTypeFact param,
        getParamiOfCallableFact param callable
    ]

getParamNameFact :: Bitcode.ParamDeclContent -> Kbgen.Fact
getParamNameFact (Bitcode.ParamDeclContent (Bitcode.ParamVariable _ _ name)) = let
    p = Kbgen.Param (Token.getParamNameLocation name)
    in Kbgen.ParamNameCtor (Kbgen.ParamName p name)

getParamResolvedTypeFact :: Bitcode.ParamDeclContent -> Kbgen.Fact
getParamResolvedTypeFact (Bitcode.ParamDeclContent (Bitcode.ParamVariable t _ name)) = let
    p = Kbgen.Param (Token.getParamNameLocation name)
    in Kbgen.ParamResolvedTypeCtor (Kbgen.ParamResolvedType p (Kbgen.ResolvedType t))

getParamiOfCallableFact :: Bitcode.ParamDeclContent -> Kbgen.Callable -> Kbgen.Fact
getParamiOfCallableFact (Bitcode.ParamDeclContent (Bitcode.ParamVariable _ i name)) c = let
    p = Kbgen.Param (Token.getParamNameLocation name)
    in Kbgen.ParamiOfCallableCtor (Kbgen.ParamiOfCallable p (Kbgen.ParamIndex i) c)

getDataflowFacts :: Cfg -> Set Kbgen.Fact
getDataflowFacts = getDataflowFacts' . instructions

getDataflowFacts' :: Set Bitcode.Instruction -> Set Kbgen.Fact
getDataflowFacts' = Foldable.foldMap' getDataflowFacts''

getDataflowFacts'' :: Bitcode.Instruction -> Set Kbgen.Fact
getDataflowFacts'' = Set.map Kbgen.DataflowEdgeCtor . getDataflowFacts'''

getDataflowFacts''' :: Bitcode.Instruction -> Set Kbgen.DataflowEdge
getDataflowFacts''' (Bitcode.Instruction _ i) = getDataflowFacts'''' i

getDataflowFacts'''' :: Bitcode.InstructionContent -> Set Kbgen.DataflowEdge
getDataflowFacts'''' (Bitcode.Call c) = getDataflowFactsFromCall c
getDataflowFacts'''' (Bitcode.Binop b) = getDataflowFactsFromBinop b
getDataflowFacts'''' (Bitcode.Assign a) = getDataflowFactsFromAssign a
getDataflowFacts'''' (Bitcode.FieldRead r) = getDataflowFactsFromFieldRead r
getDataflowFacts'''' (Bitcode.FieldWrite w) = getDataflowFactsFromFieldWrite w
getDataflowFacts'''' (Bitcode.SubscriptRead r) = getDataflowFactsFromSubscriptRead r
getDataflowFacts'''' (Bitcode.SubscriptWrite w) = getDataflowFactsFromSubscriptWrite w
getDataflowFacts'''' _ = Set.empty

getDataflowFactsFromCall :: Bitcode.CallContent -> Set Kbgen.DataflowEdge
getDataflowFactsFromCall call = List.foldl' Set.union Set.empty (getDataflowFactsFromCall' call)

getDataflowFactsFromCall' :: Bitcode.CallContent -> [ Set Kbgen.DataflowEdge ]
getDataflowFactsFromCall' (Bitcode.CallContent output callee args _) = [
        getDataflowFactsFromCalleeToOutput callee output,
        getDataflowFactsFromEveryArgToOutput output args
    ]

getDataflowFactsFromCalleeToOutput :: Bitcode.Variable -> Bitcode.Variable -> Set Kbgen.DataflowEdge
getDataflowFactsFromCalleeToOutput callee output = let
    u = Kbgen.From (Bitcode.locationVariable callee)
    v = Kbgen.To (Bitcode.locationVariable output)
    in Set.singleton (Kbgen.DataflowEdge u v)

getDataflowFactsFromEveryArgToOutput :: Bitcode.Variable -> [ Bitcode.Value ] -> Set Kbgen.DataflowEdge
getDataflowFactsFromEveryArgToOutput output = Set.fromList . (getDataflowFactsFromEveryArgToOutput' output)

getDataflowFactsFromEveryArgToOutput' :: Bitcode.Variable -> [ Bitcode.Value ] -> [ Kbgen.DataflowEdge ]
getDataflowFactsFromEveryArgToOutput' output = List.map (getDataflowFactsFromEveryArgToOutput'' output)

getDataflowFactsFromEveryArgToOutput'' :: Bitcode.Variable -> Bitcode.Value -> Kbgen.DataflowEdge
getDataflowFactsFromEveryArgToOutput'' output arg = let
    u = Kbgen.From (Bitcode.locationValue arg)
    v = Kbgen.To (Bitcode.locationVariable output)
    in Kbgen.DataflowEdge u v

getDataflowFactsFromBinop :: Bitcode.BinopContent -> Set Kbgen.DataflowEdge
getDataflowFactsFromBinop b = let
    o = Bitcode.binopOutput b
    lhs = Bitcode.binopLhs b
    rhs = Bitcode.binopRhs b
    in Set.fromList [
        Kbgen.DataflowEdge (Kbgen.From (Bitcode.locationValue lhs)) (Kbgen.To (Bitcode.locationVariable o)),
        Kbgen.DataflowEdge (Kbgen.From (Bitcode.locationValue rhs)) (Kbgen.To (Bitcode.locationVariable o))
    ]

getDataflowFactsFromAssign :: Bitcode.AssignContent -> Set Kbgen.DataflowEdge
getDataflowFactsFromAssign (Bitcode.AssignContent output input) = let
    u = Kbgen.From (Bitcode.locationValue input)
    v = Kbgen.To (Bitcode.locationVariable output)
    in Set.singleton (Kbgen.DataflowEdge u v)

getDataflowFactsFromFieldRead :: Bitcode.FieldReadContent -> Set Kbgen.DataflowEdge
getDataflowFactsFromFieldRead (Bitcode.FieldReadContent output input _) = let
    u = Kbgen.From (Bitcode.locationVariable input)
    v = Kbgen.To (Bitcode.locationVariable output)
    in Set.singleton (Kbgen.DataflowEdge u v)

getDataflowFactsFromFieldWrite :: Bitcode.FieldWriteContent -> Set Kbgen.DataflowEdge
getDataflowFactsFromFieldWrite (Bitcode.FieldWriteContent output _ input) = let
    u = Kbgen.From (Bitcode.locationValue input)
    v = Kbgen.To (Bitcode.locationVariable output)
    in Set.singleton (Kbgen.DataflowEdge u v)

getDataflowFactsFromSubscriptRead :: Bitcode.SubscriptReadContent -> Set Kbgen.DataflowEdge
getDataflowFactsFromSubscriptRead (Bitcode.SubscriptReadContent output input _) = let
    u = Kbgen.From (Bitcode.locationVariable input)
    v = Kbgen.To (Bitcode.locationVariable output)
    in Set.singleton (Kbgen.DataflowEdge u v)

getDataflowFactsFromSubscriptWrite :: Bitcode.SubscriptWriteContent -> Set Kbgen.DataflowEdge
getDataflowFactsFromSubscriptWrite (Bitcode.SubscriptWriteContent output _ input) = let
    u = Kbgen.From (Bitcode.locationValue input)
    v = Kbgen.To (Bitcode.locationVariable output)
    in Set.singleton (Kbgen.DataflowEdge u v)

getCallsRelatedFacts :: Cfg -> Set Kbgen.Fact
getCallsRelatedFacts = getCallsRelatedFacts' . justCalls . instructions

getCallsRelatedFacts' :: Set Bitcode.CallContent -> Set Kbgen.Fact
getCallsRelatedFacts' = Foldable.foldMap' getCallsRelatedFacts''

getCallsRelatedFacts'' :: Bitcode.CallContent -> Set Kbgen.Fact
getCallsRelatedFacts'' call = List.foldl' Set.union Set.empty (getCallsRelatedFacts''' call)

getCallsRelatedFacts''' :: Bitcode.CallContent -> [ Set Kbgen.Fact ]
getCallsRelatedFacts''' call = [
        getArgiForCallFacts call,
        getKeywordArgsForCallFacts call,
        getResolvedCallFacts call
    ]

getResolvedCallFacts''' :: Kbgen.Call -> Kbgen.MethodName -> Token.ParamName -> Set Kbgen.Fact
getResolvedCallFacts''' call method p = Set.singleton (Kbgen.CallMethodOfUntypedNamedParamCtor (Kbgen.CallMethodOfUntypedNamedParam call method p))

getResolvedCallFacts'' :: Kbgen.Call -> Kbgen.MethodName -> Kbgen.Class -> Set Kbgen.Fact
getResolvedCallFacts'' call method c = Set.singleton (Kbgen.CallMethodOfClassCtor (Kbgen.CallMethodOfClass call method c))

getResolvedCallFacts'''' :: Kbgen.Call -> Kbgen.FuncName -> Kbgen.FuncDefinedInDir -> Set Kbgen.Fact
getResolvedCallFacts'''' call func d = Set.singleton (Kbgen.Call1stPartyFuncDefinedInDirCtor (Kbgen.Call1stPartyFuncDefinedInDir call func d))

getResolvedCallFacts5 :: Kbgen.Call -> Kbgen.FuncName -> Kbgen.FuncDefinedInFile -> Set Kbgen.Fact
getResolvedCallFacts5 call func f = Set.singleton (Kbgen.Call1stPartyFuncDefinedInFileCtor (Kbgen.Call1stPartyFuncDefinedInFile call func f))

getResolvedCallFacts' :: Fqn.Fqn -> Kbgen.Call -> Set Kbgen.Fact
getResolvedCallFacts' (Fqn.CallMethodOfClass _ m c) call = getResolvedCallFacts'' call (Kbgen.MethodName m) (Kbgen.Class (Token.getClassNameLocation c))
getResolvedCallFacts' (Fqn.CallMethodOfUntypedNamedParam _ m p) call = getResolvedCallFacts''' call (Kbgen.MethodName m) p
getResolvedCallFacts' (Fqn.CallFuncFromImportedDir _ f d) call = getResolvedCallFacts'''' call (Kbgen.FuncName f) (Kbgen.FuncDefinedInDir d)
getResolvedCallFacts' (Fqn.CallFuncFromImportedFile _ func f) call = getResolvedCallFacts5 call (Kbgen.FuncName func) (Kbgen.FuncDefinedInFile f)
getResolvedCallFacts' fqn call = Set.singleton (Kbgen.CallResolvedCtor (Kbgen.CallResolved call (Kbgen.Resolved fqn)))

getResolvedCallFacts :: Bitcode.CallContent -> Set Kbgen.Fact
getResolvedCallFacts c = getResolvedCallFacts' (Bitcode.variableFqn (Bitcode.callee c)) (Kbgen.Call (Bitcode.callLocation c))

getKeywordArgsForCallFacts :: Bitcode.CallContent -> Set Kbgen.Fact
getKeywordArgsForCallFacts callContent = let
    call = Kbgen.Call (Bitcode.locationVariable (Bitcode.callee callContent))
    keywordArgs = keepOnlyKeywrodArgs (Bitcode.args callContent)
    in getKeywordArgsForCallFacts' call keywordArgs

getKeywordArgsForCallFacts' :: Kbgen.Call -> [ Bitcode.KeywordArgVariable ] -> Set Kbgen.Fact
getKeywordArgsForCallFacts' call = Set.fromList . getKeywordArgsForCallFacts'' call

getKeywordArgsForCallFacts'' :: Kbgen.Call -> [ Bitcode.KeywordArgVariable ] -> [ Kbgen.Fact ]
getKeywordArgsForCallFacts'' call = List.map (getKeywordArgsForCallFacts''' call)

getKeywordArgsForCallFacts''' :: Kbgen.Call -> Bitcode.KeywordArgVariable -> Kbgen.Fact
getKeywordArgsForCallFacts''' call kwarg = let
    keyword = Kbgen.Keyword (Bitcode.keywordArgName kwarg)
    arg = Kbgen.Arg (Bitcode.locationValue (Bitcode.keywordArgValue kwarg))
    in Kbgen.KeywordArgForCallCtor (Kbgen.KeywordArgForCall keyword arg call)

keepOnlyKeywrodArgs :: [ Bitcode.Value ] -> [ Bitcode.KeywordArgVariable ]
keepOnlyKeywrodArgs = mapMaybe keepOnlyKeywrodArgs'

keepOnlyKeywrodArgs' :: Bitcode.Value -> Maybe Bitcode.KeywordArgVariable
keepOnlyKeywrodArgs' (Bitcode.KeywordArgCtor keywordArg) = Just keywordArg
keepOnlyKeywrodArgs' _ = Nothing

getArgiForCallFacts :: Bitcode.CallContent -> Set Kbgen.Fact
getArgiForCallFacts callContent = let
    call = Kbgen.Call (Bitcode.callLocation callContent)
    args = List.map (Kbgen.Arg . Bitcode.locationValue) (Bitcode.args callContent)
    argis = zip args (List.map Kbgen.ArgIndex [0..])
    in getArgiForCallFacts' call argis

getArgiForCallFacts' :: Kbgen.Call -> [(Kbgen.Arg, Kbgen.ArgIndex)] -> Set Kbgen.Fact
getArgiForCallFacts' call = Set.fromList . List.map (getArgiForCallFacts'' call)

getArgiForCallFacts'' :: Kbgen.Call -> (Kbgen.Arg, Kbgen.ArgIndex) -> Kbgen.Fact
getArgiForCallFacts'' c (a, i) = Kbgen.ArgiForCallCtor (Kbgen.ArgiForCall a i c)

instructions :: Cfg -> Set Bitcode.Instruction
instructions = Set.map Cfg.theInstructionInside . Cfg.actualNodes . Cfg.nodes

justParams :: Set Bitcode.Instruction -> Set Bitcode.ParamDeclContent
justParams = Foldable.foldMap' justParams'

justParams' :: Bitcode.Instruction -> Set Bitcode.ParamDeclContent
justParams' (Bitcode.Instruction _ (Bitcode.ParamDecl p)) = Set.singleton p
justParams' _ = Set.empty

justCalls :: Set Bitcode.Instruction -> Set Bitcode.CallContent
justCalls = Foldable.foldMap' justCalls'

justCalls' :: Bitcode.Instruction -> Set Bitcode.CallContent
justCalls' (Bitcode.Instruction _ (Bitcode.Call c)) = Set.singleton c
justCalls' _ = Set.empty

getClassRelatedFacts :: Callable.MethodContent -> Set Kbgen.Fact
getClassRelatedFacts m = List.foldl' Set.union Set.empty (getClassRelatedFacts' m)

getClassRelatedFacts' :: Callable.MethodContent -> [ Set Kbgen.Fact ]
getClassRelatedFacts' m = [
        classDef m,
        methodOfClass m,
        classResolvedSupers m
    ]

classDef :: Callable.MethodContent -> Set Kbgen.Fact
classDef m = let
    name = Callable.hostingClassName m
    loc = Token.getClassNameLocation name
    c = Kbgen.Class loc
    f = Kbgen.ClassDefinedInFile (Location.filename loc)
    in Set.singleton (Kbgen.ClassDefCtor (Kbgen.ClassDef c name f))

methodOfClass :: Callable.MethodContent -> Set Kbgen.Fact
methodOfClass m = let
    className = Callable.hostingClassName m
    c = Kbgen.Class (Token.getClassNameLocation className)
    m' = Kbgen.Method (Callable.methodLocation m)
    in Set.singleton (Kbgen.MethodOfClassCtor (Kbgen.MethodOfClass m' c))

classResolvedSupers :: Callable.MethodContent -> Set Kbgen.Fact
classResolvedSupers m = classResolvedSupers' (Callable.hostingClassName m) (Callable.hostingClassSupers m)

classResolvedSupers' :: Token.ClassName -> [ Callable.HostingClassSuper ] -> Set Kbgen.Fact
classResolvedSupers' c = Set.fromList . classResolvedSupers'' c

classResolvedSupers'' :: Token.ClassName -> [ Callable.HostingClassSuper ] -> [ Kbgen.Fact ]
classResolvedSupers'' c = mapMaybe (classResolvedSuper c)

classResolvedSuper :: Token.ClassName -> Callable.HostingClassSuper -> Maybe Kbgen.Fact
classResolvedSuper c (Callable.HostingClassSuper superName (Just fqnSuper)) = classResolvedSuper' c superName fqnSuper
classResolvedSuper _ _ = Nothing

classResolvedSuper' :: Token.ClassName -> Token.SuperName -> Fqn.Fqn -> Maybe Kbgen.Fact
classResolvedSuper' c s (Fqn.FirstPartyImport (Fqn.FirstPartyImportContent path _)) = Just (classResolved1stPartySuper c s path)
classResolvedSuper' c s fqn@(Fqn.ThirdPartyImport _) = Just (classResolved3rdPartySuper c s fqn)
classResolvedSuper' _ _ _ = Nothing

classResolved1stPartySuper :: Token.ClassName -> Token.SuperName -> FilePath -> Kbgen.Fact
classResolved1stPartySuper (Token.ClassName (Token.Named _ c)) s f = classResolved1stPartySuper' (Kbgen.Class c) s (Kbgen.SuperDefinedInFile f)

classResolved3rdPartySuper :: Token.ClassName -> Token.SuperName -> Fqn.Fqn -> Kbgen.Fact
classResolved3rdPartySuper (Token.ClassName (Token.Named _ c)) s fqn = classResolved3rdPartySuper' (Kbgen.Class c) s (Kbgen.SuperQualifiedName fqn)

classResolved1stPartySuper' :: Kbgen.Class -> Token.SuperName -> Kbgen.SuperDefinedInFile -> Kbgen.Fact
classResolved1stPartySuper' c s f = Kbgen.ClassHas1stPartySuperCtor (Kbgen.ClassHas1stPartySuper c s f)

classResolved3rdPartySuper' :: Kbgen.Class -> Token.SuperName -> Kbgen.SuperQualifiedName -> Kbgen.Fact
classResolved3rdPartySuper' c s fqn = Kbgen.ClassHas3rdPartySuperCtor (Kbgen.ClassHas3rdPartySuper c s fqn)

collectParamInstruction' ::  Bitcode.Instruction -> Set Bitcode.CallContent -> Set Bitcode.CallContent
collectParamInstruction' (Bitcode.Instruction _ (Bitcode.Call c)) = Set.insert c
collectParamInstruction' _ = id

collectParamInstruction :: Set Bitcode.CallContent -> Bitcode.Instruction -> Set Bitcode.CallContent
collectParamInstruction = flip collectParamInstruction'

params :: Set Bitcode.Instruction -> Set Bitcode.CallContent
params = Foldable.foldl' collectParamInstruction Set.empty
