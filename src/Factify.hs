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
        getConstIntegersRelatedFacts (Callable.lambdaBody l)
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
        getConstIntegersRelatedFacts (Callable.funcBody f)
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
        getConstIntegersRelatedFacts (Callable.methodBody m)
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
getDataflowFactsFromBinop (Bitcode.BinopContent o lhs rhs) = Set.fromList [
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
