/// Code from VisualFSharpPowerTools project: https://github.com/fsprojects/VisualFSharpPowerTools/blob/master/src/FSharp.Editing/Common/UntypedAstUtils.fs
module FsAutoComplete.UntypedAstUtils

open FSharp.Compiler.SyntaxTree
open System.Collections.Generic
open FSharp.Compiler
open FSharp.Compiler.Text

type Range with
  member inline x.IsEmpty =
    x.StartColumn = x.EndColumn
    && x.StartLine = x.EndLine

type internal ShortIdent = string
type internal Idents = ShortIdent []

let internal longIdentToArray (longIdent: LongIdent) : Idents =
  longIdent |> Seq.map string |> Seq.toArray

/// An recursive pattern that collect all sequential expressions to avoid StackOverflowException
let rec (|Sequentials|_|) =
  function
  | SynExpr.Sequential (_, _, e, Sequentials es, _) -> Some(e :: es)
  | SynExpr.Sequential (_, _, e1, e2, _) -> Some [ e1; e2 ]
  | _ -> None

let (|ConstructorPats|) =
  function
  | SynArgPats.Pats ps -> ps
  | SynArgPats.NamePatPairs (xs, _) -> List.map snd xs

/// matches if the range contains the position
let (|ContainsPos|_|) pos range =
  if Range.rangeContainsPos range pos then
    Some()
  else
    None

/// Active pattern that matches an ident on a given name by the ident's `idText`
let (|Ident|_|) ofName =
  function
  | SynExpr.Ident ident when ident.idText = ofName -> Some()
  | _ -> None

/// matches if the range contains the position
let (|IdentContainsPos|_|) pos (ident: Ident) = (|ContainsPos|_|) pos ident.idRange

/// A pattern that collects all attributes from a `SynAttributes` into a single flat list
let (|AllAttrs|) (attrs: SynAttributes) =
  attrs
  |> List.collect (fun attrList -> attrList.Attributes)

/// A pattern that collects all patterns from a `SynSimplePats` into a single flat list
let (|AllSimplePats|) (pats: SynSimplePats) =
  let rec loop acc pat =
    match pat with
    | SynSimplePats.SimplePats (pats, _) -> acc @ pats
    | SynSimplePats.Typed (pats, _, _) -> loop acc pats

  loop [] pats

/// Returns all Idents and LongIdents found in an untyped AST.
let internal getLongIdents (input: ParsedInput option) : IDictionary<Pos, Idents> =
  let identsByEndPos = Dictionary<Pos, Idents>()

  let addLongIdent (longIdent: LongIdent) =
    let idents = longIdentToArray longIdent

    for ident in longIdent do
      identsByEndPos.[ident.idRange.End] <- idents

  let addLongIdentWithDots (LongIdentWithDots (longIdent, lids) as value) =
    match longIdentToArray longIdent with
    | [||] -> ()
    | [| _ |] as idents -> identsByEndPos.[value.Range.End] <- idents
    | idents ->
      for dotRange in lids do
        identsByEndPos.[Pos.mkPos dotRange.EndLine (dotRange.EndColumn - 1)] <- idents

      identsByEndPos.[value.Range.End] <- idents

  let addIdent (ident: Ident) =
    identsByEndPos.[ident.idRange.End] <- [| ident.idText |]

  let rec walkImplFileInput (ParsedImplFileInput (_, _, _, _, _, moduleOrNamespaceList, _)) =
    List.iter walkSynModuleOrNamespace moduleOrNamespaceList

  and walkSynModuleOrNamespace (SynModuleOrNamespace (_, _, _, decls, _, AllAttrs attrs, _, _)) =
    List.iter walkAttribute attrs
    List.iter walkSynModuleDecl decls

  and walkAttribute (attr: SynAttribute) =
    addLongIdentWithDots attr.TypeName
    walkExpr attr.ArgExpr

  and walkTyparDecl (SynTyparDecl.TyparDecl (AllAttrs attrs, typar)) =
    List.iter walkAttribute attrs
    walkTypar typar

  and walkTypeConstraint =
    function
    | SynTypeConstraint.WhereTyparIsValueType (t, _)
    | SynTypeConstraint.WhereTyparIsReferenceType (t, _)
    | SynTypeConstraint.WhereTyparIsUnmanaged (t, _)
    | SynTypeConstraint.WhereTyparSupportsNull (t, _)
    | SynTypeConstraint.WhereTyparIsComparable (t, _)
    | SynTypeConstraint.WhereTyparIsEquatable (t, _) -> walkTypar t
    | SynTypeConstraint.WhereTyparDefaultsToType (t, ty, _)
    | SynTypeConstraint.WhereTyparSubtypeOfType (t, ty, _) ->
      walkTypar t
      walkType ty
    | SynTypeConstraint.WhereTyparIsEnum (t, ts, _)
    | SynTypeConstraint.WhereTyparIsDelegate (t, ts, _) ->
      walkTypar t
      List.iter walkType ts
    | SynTypeConstraint.WhereTyparSupportsMember (ts, sign, _) ->
      List.iter walkType ts
      walkMemberSig sign

  and walkPat =
    function
    | SynPat.Tuple (_, pats, _)
    | SynPat.ArrayOrList (_, pats, _)
    | SynPat.Ands (pats, _) -> List.iter walkPat pats
    | SynPat.Named (pat, ident, _, _, _) ->
      walkPat pat
      addIdent ident
    | SynPat.Typed (pat, t, _) ->
      walkPat pat
      walkType t
    | SynPat.Attrib (pat, AllAttrs attrs, _) ->
      walkPat pat
      List.iter walkAttribute attrs
    | SynPat.Or (pat1, pat2, _) -> List.iter walkPat [ pat1; pat2 ]
    | SynPat.LongIdent (ident, _, typars, ConstructorPats pats, _, _) ->
      addLongIdentWithDots ident

      typars
      |> Option.iter
           (fun (SynValTyparDecls (typars, _, constraints)) ->
             List.iter walkTyparDecl typars
             List.iter walkTypeConstraint constraints)

      List.iter walkPat pats
    | SynPat.Paren (pat, _) -> walkPat pat
    | SynPat.IsInst (t, _) -> walkType t
    | SynPat.QuoteExpr (e, _) -> walkExpr e
    | _ -> ()

  and walkTypar (Typar (_, _, _)) = ()

  and walkBinding (SynBinding.Binding (_, _, _, _, AllAttrs attrs, _, _, pat, returnInfo, e, _, _)) =
    List.iter walkAttribute attrs
    walkPat pat
    walkExpr e

    returnInfo
    |> Option.iter (fun (SynBindingReturnInfo (t, _, _)) -> walkType t)

  and walkInterfaceImpl (InterfaceImpl (_, bindings, _)) = List.iter walkBinding bindings

  and walkIndexerArg =
    function
    | SynIndexerArg.One (e, _fromEnd, _range) -> walkExpr e
    | SynIndexerArg.Two (e1, _e1FromEnd, e2, _e2FromEnd, _e1Range, _e2Range) -> List.iter walkExpr [ e1; e2 ]

  and walkType =
    function
    | SynType.Array (_, t, _)
    | SynType.HashConstraint (t, _)
    | SynType.MeasurePower (t, _, _) -> walkType t
    | SynType.Fun (t1, t2, _)
    | SynType.MeasureDivide (t1, t2, _) ->
      walkType t1
      walkType t2
    | SynType.LongIdent ident -> addLongIdentWithDots ident
    | SynType.App (ty, _, types, _, _, _, _) ->
      walkType ty
      List.iter walkType types
    | SynType.LongIdentApp (_, _, _, types, _, _, _) -> List.iter walkType types
    | SynType.Tuple (_, ts, _) -> ts |> List.iter (fun (_, t) -> walkType t)
    | SynType.WithGlobalConstraints (t, typeConstraints, _) ->
      walkType t
      List.iter walkTypeConstraint typeConstraints
    | _ -> ()

  and walkClause (Clause (pat, e1, e2, _, _)) =
    walkPat pat
    walkExpr e2
    e1 |> Option.iter walkExpr

  and walkSimplePats =
    function
    | SynSimplePats.SimplePats (pats, _) -> List.iter walkSimplePat pats
    | SynSimplePats.Typed (pats, ty, _) ->
      walkSimplePats pats
      walkType ty

  and walkExpr =
    function
    | SynExpr.Paren (e, _, _, _)
    | SynExpr.Quote (_, _, e, _, _)
    | SynExpr.Typed (e, _, _)
    | SynExpr.InferredUpcast (e, _)
    | SynExpr.InferredDowncast (e, _)
    | SynExpr.AddressOf (_, e, _, _)
    | SynExpr.DoBang (e, _)
    | SynExpr.YieldOrReturn (_, e, _)
    | SynExpr.ArrayOrListOfSeqExpr (_, e, _)
    | SynExpr.CompExpr (_, _, e, _)
    | SynExpr.Do (e, _)
    | SynExpr.Assert (e, _)
    | SynExpr.Lazy (e, _)
    | SynExpr.YieldOrReturnFrom (_, e, _) -> walkExpr e
    | SynExpr.Lambda (_, _, pats, e, _, _) ->
      walkSimplePats pats
      walkExpr e
    | SynExpr.New (_, t, e, _)
    | SynExpr.TypeTest (e, t, _)
    | SynExpr.Upcast (e, t, _)
    | SynExpr.Downcast (e, t, _) ->
      walkExpr e
      walkType t
    | SynExpr.Tuple (_, es, _, _)
    | Sequentials es
    | SynExpr.ArrayOrList (_, es, _) -> List.iter walkExpr es
    | SynExpr.App (_, _, e1, e2, _)
    | SynExpr.TryFinally (e1, e2, _, _, _)
    | SynExpr.While (_, e1, e2, _) -> List.iter walkExpr [ e1; e2 ]
    | SynExpr.Record (_, _, fields, _) ->
      fields
      |> List.iter
           (fun ((ident, _), e, _) ->
             addLongIdentWithDots ident
             e |> Option.iter walkExpr)
    | SynExpr.Ident ident -> addIdent ident
    | SynExpr.ObjExpr (ty, argOpt, bindings, ifaces, _, _) ->
      argOpt
      |> Option.iter
           (fun (e, ident) ->
             walkExpr e
             ident |> Option.iter addIdent)

      walkType ty
      List.iter walkBinding bindings
      List.iter walkInterfaceImpl ifaces
    | SynExpr.LongIdent (_, ident, _, _) -> addLongIdentWithDots ident
    | SynExpr.For (_, ident, e1, _, e2, e3, _) ->
      addIdent ident
      List.iter walkExpr [ e1; e2; e3 ]
    | SynExpr.ForEach (_, _, _, pat, e1, e2, _) ->
      walkPat pat
      List.iter walkExpr [ e1; e2 ]
    | SynExpr.MatchLambda (_, _, synMatchClauseList, _, _) -> List.iter walkClause synMatchClauseList
    | SynExpr.Match (_, e, synMatchClauseList, _) ->
      walkExpr e
      List.iter walkClause synMatchClauseList
    | SynExpr.TypeApp (e, _, tys, _, _, _, _) ->
      List.iter walkType tys
      walkExpr e
    | SynExpr.LetOrUse (_, _, bindings, e, _) ->
      List.iter walkBinding bindings
      walkExpr e
    | SynExpr.TryWith (e, _, clauses, _, _, _, _) ->
      List.iter walkClause clauses
      walkExpr e
    | SynExpr.IfThenElse (e1, e2, e3, _, _, _, _) ->
      List.iter walkExpr [ e1; e2 ]
      e3 |> Option.iter walkExpr
    | SynExpr.LongIdentSet (ident, e, _)
    | SynExpr.DotGet (e, _, ident, _) ->
      addLongIdentWithDots ident
      walkExpr e
    | SynExpr.DotSet (e1, idents, e2, _) ->
      walkExpr e1
      addLongIdentWithDots idents
      walkExpr e2
    | SynExpr.DotIndexedGet (e, args, _, _) ->
      walkExpr e
      List.iter walkIndexerArg args
    | SynExpr.DotIndexedSet (e1, args, e2, _, _, _) ->
      walkExpr e1
      List.iter walkIndexerArg args
      walkExpr e2
    | SynExpr.NamedIndexedPropertySet (ident, e1, e2, _) ->
      addLongIdentWithDots ident
      List.iter walkExpr [ e1; e2 ]
    | SynExpr.DotNamedIndexedPropertySet (e1, ident, e2, e3, _) ->
      addLongIdentWithDots ident
      List.iter walkExpr [ e1; e2; e3 ]
    | SynExpr.JoinIn (e1, _, e2, _) -> List.iter walkExpr [ e1; e2 ]
    | SynExpr.LetOrUseBang (_, _, _, pat, e1, ands, e2, _) ->
      walkPat pat
      walkExpr e1

      for (_, _, _, pat, body, _) in ands do
        walkPat pat
        walkExpr body

      walkExpr e2
    | SynExpr.TraitCall (ts, sign, e, _) ->
      List.iter walkTypar ts
      walkMemberSig sign
      walkExpr e
    | SynExpr.Const (SynConst.Measure (_, m), _) -> walkMeasure m
    | _ -> ()

  and walkMeasure =
    function
    | SynMeasure.Product (m1, m2, _)
    | SynMeasure.Divide (m1, m2, _) ->
      walkMeasure m1
      walkMeasure m2
    | SynMeasure.Named (longIdent, _) -> addLongIdent longIdent
    | SynMeasure.Seq (ms, _) -> List.iter walkMeasure ms
    | SynMeasure.Power (m, _, _) -> walkMeasure m
    | SynMeasure.Var (ty, _) -> walkTypar ty
    | SynMeasure.One
    | SynMeasure.Anon _ -> ()

  and walkSimplePat =
    function
    | SynSimplePat.Attrib (pat, AllAttrs attrs, _) ->
      walkSimplePat pat
      List.iter walkAttribute attrs
    | SynSimplePat.Typed (pat, t, _) ->
      walkSimplePat pat
      walkType t
    | _ -> ()

  and walkField (SynField.Field (AllAttrs attrs, _, _, t, _, _, _, _)) =
    List.iter walkAttribute attrs
    walkType t

  and walkValSig (SynValSig.ValSpfn (AllAttrs attrs, _, _, t, SynValInfo (argInfos, argInfo), _, _, _, _, _, _)) =
    List.iter walkAttribute attrs
    walkType t

    argInfo :: (argInfos |> List.concat)
    |> List.collect (fun (SynArgInfo (AllAttrs attrs, _, _)) -> attrs)
    |> List.iter walkAttribute

  and walkMemberSig =
    function
    | SynMemberSig.Inherit (t, _)
    | SynMemberSig.Interface (t, _) -> walkType t
    | SynMemberSig.Member (vs, _, _) -> walkValSig vs
    | SynMemberSig.ValField (f, _) -> walkField f
    | SynMemberSig.NestedType (SynTypeDefnSig.TypeDefnSig (info, repr, memberSigs, _), _) ->
      let isTypeExtensionOrAlias =
        match repr with
        | SynTypeDefnSigRepr.Simple (SynTypeDefnSimpleRepr.TypeAbbrev _, _)
        | SynTypeDefnSigRepr.ObjectModel (SynTypeDefnKind.TyconAbbrev, _, _)
        | SynTypeDefnSigRepr.ObjectModel (SynTypeDefnKind.TyconAugmentation, _, _) -> true
        | _ -> false

      walkComponentInfo isTypeExtensionOrAlias info
      walkTypeDefnSigRepr repr
      List.iter walkMemberSig memberSigs

  and walkMember =
    function
    | SynMemberDefn.AbstractSlot (valSig, _, _) -> walkValSig valSig
    | SynMemberDefn.Member (binding, _) -> walkBinding binding
    | SynMemberDefn.ImplicitCtor (_, AllAttrs attrs, AllSimplePats pats, _, _, _) ->
      List.iter walkAttribute attrs
      List.iter walkSimplePat pats
    | SynMemberDefn.ImplicitInherit (t, e, _, _) ->
      walkType t
      walkExpr e
    | SynMemberDefn.LetBindings (bindings, _, _, _) -> List.iter walkBinding bindings
    | SynMemberDefn.Interface (t, members, _) ->
      walkType t
      members |> Option.iter (List.iter walkMember)
    | SynMemberDefn.Inherit (t, _, _) -> walkType t
    | SynMemberDefn.ValField (field, _) -> walkField field
    | SynMemberDefn.NestedType (tdef, _, _) -> walkTypeDefn tdef
    | SynMemberDefn.AutoProperty (AllAttrs attrs, _, _, t, _, _, _, _, e, _, _) ->
      List.iter walkAttribute attrs
      Option.iter walkType t
      walkExpr e
    | _ -> ()

  and walkEnumCase (EnumCase (AllAttrs attrs, _, _, _, _)) = List.iter walkAttribute attrs

  and walkUnionCaseType =
    function
    | SynUnionCaseType.UnionCaseFields fields -> List.iter walkField fields
    | SynUnionCaseType.UnionCaseFullType (t, _) -> walkType t

  and walkUnionCase (SynUnionCase.UnionCase (AllAttrs attrs, _, t, _, _, _)) =
    List.iter walkAttribute attrs
    walkUnionCaseType t

  and walkTypeDefnSimple =
    function
    | SynTypeDefnSimpleRepr.Enum (cases, _) -> List.iter walkEnumCase cases
    | SynTypeDefnSimpleRepr.Union (_, cases, _) -> List.iter walkUnionCase cases
    | SynTypeDefnSimpleRepr.Record (_, fields, _) -> List.iter walkField fields
    | SynTypeDefnSimpleRepr.TypeAbbrev (_, t, _) -> walkType t
    | _ -> ()

  and walkComponentInfo
    isTypeExtensionOrAlias
    (ComponentInfo (AllAttrs attrs, typars, constraints, longIdent, _, _, _, _))
    =
    List.iter walkAttribute attrs
    List.iter walkTyparDecl typars
    List.iter walkTypeConstraint constraints

    if isTypeExtensionOrAlias then
      addLongIdent longIdent

  and walkTypeDefnRepr =
    function
    | SynTypeDefnRepr.ObjectModel (_, defns, _) -> List.iter walkMember defns
    | SynTypeDefnRepr.Simple (defn, _) -> walkTypeDefnSimple defn
    | SynTypeDefnRepr.Exception _ -> ()

  and walkTypeDefnSigRepr =
    function
    | SynTypeDefnSigRepr.ObjectModel (_, defns, _) -> List.iter walkMemberSig defns
    | SynTypeDefnSigRepr.Simple (defn, _) -> walkTypeDefnSimple defn
    | SynTypeDefnSigRepr.Exception _ -> ()

  and walkTypeDefn (TypeDefn (info, repr, members, _)) =
    let isTypeExtensionOrAlias =
      match repr with
      | SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.TyconAugmentation, _, _)
      | SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.TyconAbbrev, _, _)
      | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.TypeAbbrev _, _) -> true
      | _ -> false

    walkComponentInfo isTypeExtensionOrAlias info
    walkTypeDefnRepr repr
    List.iter walkMember members

  and walkSynModuleDecl (decl: SynModuleDecl) =
    match decl with
    | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace fragment
    | SynModuleDecl.NestedModule (info, _, modules, _, _) ->
      walkComponentInfo false info
      List.iter walkSynModuleDecl modules
    | SynModuleDecl.Let (_, bindings, _) -> List.iter walkBinding bindings
    | SynModuleDecl.DoExpr (_, expr, _) -> walkExpr expr
    | SynModuleDecl.Types (types, _) -> List.iter walkTypeDefn types
    | SynModuleDecl.Attributes (AllAttrs attrs, _) -> List.iter walkAttribute attrs
    | _ -> ()

  match input with
  | Some (ParsedInput.ImplFile input) -> walkImplFileInput input
  | _ -> ()
  //debug "%A" idents
  identsByEndPos :> _

/// Checks if given position is part of the typed binding
let internal isTypedBindingAtPosition (input: ParsedInput option) (r: Range) : bool =
  let mutable result = false

  let isInside (ran: Range) = Range.rangeContainsRange ran r

  let rec walkImplFileInput (ParsedImplFileInput (_, _, _, _, _, moduleOrNamespaceList, _)) =
    List.iter walkSynModuleOrNamespace moduleOrNamespaceList

  and walkSynModuleOrNamespace (SynModuleOrNamespace (_, _, _, decls, _, AllAttrs attrs, _, _)) =
    List.iter walkAttribute attrs
    List.iter walkSynModuleDecl decls

  and walkAttribute (attr: SynAttribute) = walkExpr attr.ArgExpr

  and walkTyparDecl (SynTyparDecl.TyparDecl (AllAttrs attrs, typar)) =
    List.iter walkAttribute attrs
    walkTypar typar

  and walkTypeConstraint =
    function
    | SynTypeConstraint.WhereTyparIsValueType (t, _)
    | SynTypeConstraint.WhereTyparIsReferenceType (t, _)
    | SynTypeConstraint.WhereTyparIsUnmanaged (t, _)
    | SynTypeConstraint.WhereTyparSupportsNull (t, _)
    | SynTypeConstraint.WhereTyparIsComparable (t, _)
    | SynTypeConstraint.WhereTyparIsEquatable (t, _) -> walkTypar t
    | SynTypeConstraint.WhereTyparDefaultsToType (t, ty, _)
    | SynTypeConstraint.WhereTyparSubtypeOfType (t, ty, _) ->
      walkTypar t
      walkType ty
    | SynTypeConstraint.WhereTyparIsEnum (t, ts, _)
    | SynTypeConstraint.WhereTyparIsDelegate (t, ts, _) ->
      walkTypar t
      List.iter walkType ts
    | SynTypeConstraint.WhereTyparSupportsMember (ts, sign, _) ->
      List.iter walkType ts
      walkMemberSig sign

  and walkPat =
    function
    | SynPat.Tuple (_, pats, _)
    | SynPat.ArrayOrList (_, pats, _)
    | SynPat.Ands (pats, _) -> List.iter walkPat pats
    | SynPat.Named (pat, ident, _, _, _) -> walkPat pat
    | SynPat.Typed (pat, t, ran) ->
      if isInside ran then result <- true
      walkPat pat
      walkType t
    | SynPat.Attrib (pat, AllAttrs attrs, _) ->
      walkPat pat
      List.iter walkAttribute attrs
    | SynPat.Or (pat1, pat2, _) -> List.iter walkPat [ pat1; pat2 ]
    | SynPat.LongIdent (ident, _, typars, ConstructorPats pats, _, _) ->
      typars
      |> Option.iter
           (fun (SynValTyparDecls (typars, _, constraints)) ->
             List.iter walkTyparDecl typars
             List.iter walkTypeConstraint constraints)

      List.iter walkPat pats
    | SynPat.Paren (pat, _) -> walkPat pat
    | SynPat.IsInst (t, _) -> walkType t
    | SynPat.QuoteExpr (e, _) -> walkExpr e
    | _ -> ()

  and walkTypar (Typar (_, _, _)) = ()

  and walkBinding (SynBinding.Binding (_, _, _, _, AllAttrs attrs, _, _, pat, returnInfo, e, _, _)) =
    List.iter walkAttribute attrs
    walkPat pat
    walkExpr e

    returnInfo
    |> Option.iter (fun (SynBindingReturnInfo (t, _, _)) -> walkType t)

  and walkInterfaceImpl (InterfaceImpl (_, bindings, _)) = List.iter walkBinding bindings

  and walkIndexerArg =
    function
    | SynIndexerArg.One (e, _fromEnd, _range) -> walkExpr e
    | SynIndexerArg.Two (e1, _e1FromEnd, e2, _e2FromEnd, _e1Range, _e2Range) -> List.iter walkExpr [ e1; e2 ]

  and walkType =
    function
    | SynType.Array (_, t, _)
    | SynType.HashConstraint (t, _)
    | SynType.MeasurePower (t, _, _) -> walkType t
    | SynType.Fun (t1, t2, _)
    | SynType.MeasureDivide (t1, t2, _) ->
      walkType t1
      walkType t2
    | SynType.App (ty, _, types, _, _, _, _) ->
      walkType ty
      List.iter walkType types
    | SynType.LongIdentApp (_, _, _, types, _, _, _) -> List.iter walkType types
    | SynType.Tuple (_, ts, _) -> ts |> List.iter (fun (_, t) -> walkType t)
    | SynType.WithGlobalConstraints (t, typeConstraints, _) ->
      walkType t
      List.iter walkTypeConstraint typeConstraints
    | _ -> ()

  and walkClause (Clause (pat, e1, e2, _, _)) =
    walkPat pat
    walkExpr e2
    e1 |> Option.iter walkExpr

  and walkSimplePats =
    function
    | SynSimplePats.SimplePats (pats, _) -> List.iter walkSimplePat pats
    | SynSimplePats.Typed (pats, ty, ran) ->
      if isInside ran then result <- true
      walkSimplePats pats
      walkType ty

  and walkExpr =
    function
    | SynExpr.Typed (e, _, ran) ->
      if isInside ran then result <- true
      walkExpr e
    | SynExpr.Paren (e, _, _, _)
    | SynExpr.Quote (_, _, e, _, _)
    | SynExpr.InferredUpcast (e, _)
    | SynExpr.InferredDowncast (e, _)
    | SynExpr.AddressOf (_, e, _, _)
    | SynExpr.DoBang (e, _)
    | SynExpr.YieldOrReturn (_, e, _)
    | SynExpr.ArrayOrListOfSeqExpr (_, e, _)
    | SynExpr.CompExpr (_, _, e, _)
    | SynExpr.Do (e, _)
    | SynExpr.Assert (e, _)
    | SynExpr.Lazy (e, _)
    | SynExpr.YieldOrReturnFrom (_, e, _) -> walkExpr e
    | SynExpr.Lambda (_, _, pats, e, _, _) ->
      walkSimplePats pats
      walkExpr e
    | SynExpr.New (_, t, e, _)
    | SynExpr.TypeTest (e, t, _)
    | SynExpr.Upcast (e, t, _)
    | SynExpr.Downcast (e, t, _) ->
      walkExpr e
      walkType t
    | SynExpr.Tuple (_, es, _, _)
    | Sequentials es
    | SynExpr.ArrayOrList (_, es, _) -> List.iter walkExpr es
    | SynExpr.App (_, _, e1, e2, _)
    | SynExpr.TryFinally (e1, e2, _, _, _)
    | SynExpr.While (_, e1, e2, _) -> List.iter walkExpr [ e1; e2 ]
    | SynExpr.Record (_, _, fields, _) ->
      fields
      |> List.iter (fun ((ident, _), e, _) -> e |> Option.iter walkExpr)
    | SynExpr.ObjExpr (ty, argOpt, bindings, ifaces, _, _) ->
      argOpt
      |> Option.iter (fun (e, ident) -> walkExpr e)

      walkType ty
      List.iter walkBinding bindings
      List.iter walkInterfaceImpl ifaces
    | SynExpr.For (_, ident, e1, _, e2, e3, _) -> List.iter walkExpr [ e1; e2; e3 ]
    | SynExpr.ForEach (_, _, _, pat, e1, e2, _) ->
      walkPat pat
      List.iter walkExpr [ e1; e2 ]
    | SynExpr.MatchLambda (_, _, synMatchClauseList, _, _) -> List.iter walkClause synMatchClauseList
    | SynExpr.Match (_, e, synMatchClauseList, _) ->
      walkExpr e
      List.iter walkClause synMatchClauseList
    | SynExpr.TypeApp (e, _, tys, _, _, _, _) ->
      List.iter walkType tys
      walkExpr e
    | SynExpr.LetOrUse (_, _, bindings, e, _) ->
      List.iter walkBinding bindings
      walkExpr e
    | SynExpr.TryWith (e, _, clauses, _, _, _, _) ->
      List.iter walkClause clauses
      walkExpr e
    | SynExpr.IfThenElse (e1, e2, e3, _, _, _, _) ->
      List.iter walkExpr [ e1; e2 ]
      e3 |> Option.iter walkExpr
    | SynExpr.LongIdentSet (ident, e, _)
    | SynExpr.DotGet (e, _, ident, _) -> walkExpr e
    | SynExpr.DotSet (e1, idents, e2, _) ->
      walkExpr e1
      walkExpr e2
    | SynExpr.DotIndexedGet (e, args, _, _) ->
      walkExpr e
      List.iter walkIndexerArg args
    | SynExpr.DotIndexedSet (e1, args, e2, _, _, _) ->
      walkExpr e1
      List.iter walkIndexerArg args
      walkExpr e2
    | SynExpr.NamedIndexedPropertySet (ident, e1, e2, _) -> List.iter walkExpr [ e1; e2 ]
    | SynExpr.DotNamedIndexedPropertySet (e1, ident, e2, e3, _) -> List.iter walkExpr [ e1; e2; e3 ]
    | SynExpr.JoinIn (e1, _, e2, _) -> List.iter walkExpr [ e1; e2 ]
    | SynExpr.LetOrUseBang (_, _, _, pat, e1, ands, e2, _) ->
      walkPat pat
      walkExpr e1

      for (_, _, _, pat, body, _) in ands do
        walkPat pat
        walkExpr body

      walkExpr e2
    | SynExpr.TraitCall (ts, sign, e, _) ->
      List.iter walkTypar ts
      walkMemberSig sign
      walkExpr e
    | SynExpr.Const (SynConst.Measure (_, m), _) -> walkMeasure m
    | _ -> ()

  and walkMeasure =
    function
    | SynMeasure.Product (m1, m2, _)
    | SynMeasure.Divide (m1, m2, _) ->
      walkMeasure m1
      walkMeasure m2
    | SynMeasure.Named (longIdent, _) -> ()
    | SynMeasure.Seq (ms, _) -> List.iter walkMeasure ms
    | SynMeasure.Power (m, _, _) -> walkMeasure m
    | SynMeasure.Var (ty, _) -> walkTypar ty
    | SynMeasure.One
    | SynMeasure.Anon _ -> ()

  and walkSimplePat =
    function
    | SynSimplePat.Attrib (pat, AllAttrs attrs, _) ->
      walkSimplePat pat
      List.iter walkAttribute attrs
    | SynSimplePat.Typed (pat, t, ran) ->
      if isInside ran then result <- true
      walkSimplePat pat
      walkType t
    | _ -> ()

  and walkField (SynField.Field (AllAttrs attrs, _, _, t, _, _, _, _)) =
    List.iter walkAttribute attrs
    walkType t

  and walkValSig (SynValSig.ValSpfn (AllAttrs attrs, _, _, t, SynValInfo (argInfos, argInfo), _, _, _, _, _, _)) =
    List.iter walkAttribute attrs
    walkType t

    argInfo :: (argInfos |> List.concat)
    |> List.collect (fun (SynArgInfo (AllAttrs attrs, _, _)) -> attrs)
    |> List.iter walkAttribute

  and walkMemberSig =
    function
    | SynMemberSig.Inherit (t, _)
    | SynMemberSig.Interface (t, _) -> walkType t
    | SynMemberSig.Member (vs, _, _) -> walkValSig vs
    | SynMemberSig.ValField (f, _) -> walkField f
    | SynMemberSig.NestedType (SynTypeDefnSig.TypeDefnSig (info, repr, memberSigs, _), _) ->
      let isTypeExtensionOrAlias =
        match repr with
        | SynTypeDefnSigRepr.Simple (SynTypeDefnSimpleRepr.TypeAbbrev _, _)
        | SynTypeDefnSigRepr.ObjectModel (SynTypeDefnKind.TyconAbbrev, _, _)
        | SynTypeDefnSigRepr.ObjectModel (SynTypeDefnKind.TyconAugmentation, _, _) -> true
        | _ -> false

      walkComponentInfo isTypeExtensionOrAlias info
      walkTypeDefnSigRepr repr
      List.iter walkMemberSig memberSigs

  and walkMember =
    function
    | SynMemberDefn.AbstractSlot (valSig, _, _) -> walkValSig valSig
    | SynMemberDefn.Member (binding, _) -> walkBinding binding
    | SynMemberDefn.ImplicitCtor (_, AllAttrs attrs, AllSimplePats pats, _, _, _) ->
      List.iter walkAttribute attrs
      List.iter walkSimplePat pats
    | SynMemberDefn.ImplicitInherit (t, e, _, _) ->
      walkType t
      walkExpr e
    | SynMemberDefn.LetBindings (bindings, _, _, _) -> List.iter walkBinding bindings
    | SynMemberDefn.Interface (t, members, _) ->
      walkType t
      members |> Option.iter (List.iter walkMember)
    | SynMemberDefn.Inherit (t, _, _) -> walkType t
    | SynMemberDefn.ValField (field, _) -> walkField field
    | SynMemberDefn.NestedType (tdef, _, _) -> walkTypeDefn tdef
    | SynMemberDefn.AutoProperty (AllAttrs attrs, _, _, t, _, _, _, _, e, _, _) ->
      List.iter walkAttribute attrs
      Option.iter walkType t
      walkExpr e
    | _ -> ()

  and walkEnumCase (EnumCase (AllAttrs attrs, _, _, _, _)) = List.iter walkAttribute attrs

  and walkUnionCaseType =
    function
    | SynUnionCaseType.UnionCaseFields fields -> List.iter walkField fields
    | SynUnionCaseType.UnionCaseFullType (t, _) -> walkType t

  and walkUnionCase (SynUnionCase.UnionCase (AllAttrs attrs, _, t, _, _, _)) =
    List.iter walkAttribute attrs
    walkUnionCaseType t

  and walkTypeDefnSimple =
    function
    | SynTypeDefnSimpleRepr.Enum (cases, _) -> List.iter walkEnumCase cases
    | SynTypeDefnSimpleRepr.Union (_, cases, _) -> List.iter walkUnionCase cases
    | SynTypeDefnSimpleRepr.Record (_, fields, _) -> List.iter walkField fields
    | SynTypeDefnSimpleRepr.TypeAbbrev (_, t, _) -> walkType t
    | _ -> ()

  and walkComponentInfo
    isTypeExtensionOrAlias
    (ComponentInfo (AllAttrs attrs, typars, constraints, longIdent, _, _, _, _))
    =
    List.iter walkAttribute attrs
    List.iter walkTyparDecl typars
    List.iter walkTypeConstraint constraints

  and walkTypeDefnRepr =
    function
    | SynTypeDefnRepr.ObjectModel (_, defns, _) -> List.iter walkMember defns
    | SynTypeDefnRepr.Simple (defn, _) -> walkTypeDefnSimple defn
    | SynTypeDefnRepr.Exception _ -> ()

  and walkTypeDefnSigRepr =
    function
    | SynTypeDefnSigRepr.ObjectModel (_, defns, _) -> List.iter walkMemberSig defns
    | SynTypeDefnSigRepr.Simple (defn, _) -> walkTypeDefnSimple defn
    | SynTypeDefnSigRepr.Exception _ -> ()

  and walkTypeDefn (TypeDefn (info, repr, members, _)) =
    let isTypeExtensionOrAlias =
      match repr with
      | SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.TyconAugmentation, _, _)
      | SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.TyconAbbrev, _, _)
      | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.TypeAbbrev _, _) -> true
      | _ -> false

    walkComponentInfo isTypeExtensionOrAlias info
    walkTypeDefnRepr repr
    List.iter walkMember members

  and walkSynModuleDecl (decl: SynModuleDecl) =
    match decl with
    | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace fragment
    | SynModuleDecl.NestedModule (info, _, modules, _, _) ->
      walkComponentInfo false info
      List.iter walkSynModuleDecl modules
    | SynModuleDecl.Let (_, bindings, _) -> List.iter walkBinding bindings
    | SynModuleDecl.DoExpr (_, expr, _) -> walkExpr expr
    | SynModuleDecl.Types (types, _) -> List.iter walkTypeDefn types
    | SynModuleDecl.Attributes (AllAttrs attrs, _) -> List.iter walkAttribute attrs
    | _ -> ()

  match input with
  | Some (ParsedInput.ImplFile input) -> walkImplFileInput input
  | _ -> ()
  //debug "%A" idents
  result

/// Gives all ranges for current position
let internal getRangesAtPosition (input: ParsedInput option) (r: Pos) : Range list =
  let mutable result = []


  let addIfInside (ran: Range) =
    let addToResult r = result <- r :: result

    let isInside (ran: Range) = Range.rangeContainsPos ran r

    if isInside ran then addToResult ran



  let rec walkImplFileInput (ParsedImplFileInput (_, _, _, _, _, moduleOrNamespaceList, _)) =
    List.iter walkSynModuleOrNamespace moduleOrNamespaceList

  and walkSynModuleOrNamespace (SynModuleOrNamespace (_, _, _, decls, _, AllAttrs attrs, _, r)) =
    addIfInside r
    List.iter walkAttribute attrs
    List.iter walkSynModuleDecl decls

  and walkAttribute (attr: SynAttribute) =
    addIfInside attr.Range
    walkExpr attr.ArgExpr

  and walkTyparDecl (SynTyparDecl.TyparDecl (AllAttrs attrs, typar)) =
    List.iter walkAttribute attrs
    walkTypar typar

  and walkTypeConstraint =
    function
    | SynTypeConstraint.WhereTyparIsValueType (t, r)
    | SynTypeConstraint.WhereTyparIsReferenceType (t, r)
    | SynTypeConstraint.WhereTyparIsUnmanaged (t, r)
    | SynTypeConstraint.WhereTyparSupportsNull (t, r)
    | SynTypeConstraint.WhereTyparIsComparable (t, r)
    | SynTypeConstraint.WhereTyparIsEquatable (t, r) ->
      addIfInside r
      walkTypar t
    | SynTypeConstraint.WhereTyparDefaultsToType (t, ty, r)
    | SynTypeConstraint.WhereTyparSubtypeOfType (t, ty, r) ->
      addIfInside r
      walkTypar t
      walkType ty
    | SynTypeConstraint.WhereTyparIsEnum (t, ts, r)
    | SynTypeConstraint.WhereTyparIsDelegate (t, ts, r) ->
      addIfInside r
      walkTypar t
      List.iter walkType ts
    | SynTypeConstraint.WhereTyparSupportsMember (ts, sign, r) ->
      addIfInside r
      List.iter walkType ts
      walkMemberSig sign

  and walkPat =
    function
    | SynPat.Tuple (_, pats, r)
    | SynPat.ArrayOrList (_, pats, r)
    | SynPat.Ands (pats, r) ->
      addIfInside r
      List.iter walkPat pats
    | SynPat.Named (pat, ident, _, _, r) ->
      addIfInside r
      walkPat pat
    | SynPat.Typed (pat, t, r) ->
      addIfInside r
      walkPat pat
      walkType t
    | SynPat.Attrib (pat, AllAttrs attrs, r) ->
      addIfInside r
      walkPat pat
      List.iter walkAttribute attrs
    | SynPat.Or (pat1, pat2, r) ->
      addIfInside r
      List.iter walkPat [ pat1; pat2 ]
    | SynPat.LongIdent (ident, _, typars, ConstructorPats pats, _, r) ->
      addIfInside r

      typars
      |> Option.iter
           (fun (SynValTyparDecls (typars, _, constraints)) ->
             List.iter walkTyparDecl typars
             List.iter walkTypeConstraint constraints)

      List.iter walkPat pats
    | SynPat.Paren (pat, r) ->
      addIfInside r
      walkPat pat
    | SynPat.IsInst (t, r) ->
      addIfInside r
      walkType t
    | SynPat.QuoteExpr (e, r) ->
      addIfInside r
      walkExpr e
    | SynPat.Const (_, r) -> addIfInside r
    | SynPat.Wild (r) -> addIfInside r
    | SynPat.Record (_, r) -> addIfInside r
    | SynPat.Null (r) -> addIfInside r
    | SynPat.OptionalVal (_, r) -> addIfInside r
    | SynPat.DeprecatedCharRange (_, _, r) -> addIfInside r
    | SynPat.InstanceMember (_, _, _, accessibility, r) -> addIfInside r
    | SynPat.FromParseError (_, r) -> addIfInside r

  and walkTypar (Typar (_, _, _)) = ()

  and walkBinding (SynBinding.Binding (_, _, _, _, AllAttrs attrs, _, _, pat, returnInfo, e, r, _)) =
    addIfInside r
    List.iter walkAttribute attrs
    walkPat pat
    walkExpr e

    returnInfo
    |> Option.iter
         (fun (SynBindingReturnInfo (t, r, _)) ->
           addIfInside r
           walkType t)

  and walkInterfaceImpl (InterfaceImpl (_, bindings, r)) =
    addIfInside r
    List.iter walkBinding bindings

  and walkIndexerArg =
    function
    | SynIndexerArg.One (e, _fromEnd, _range) -> walkExpr e
    | SynIndexerArg.Two (e1, _e1FromEnd, e2, _e2FromEnd, _e1Range, _e2Range) -> List.iter walkExpr [ e1; e2 ]

  and walkType =
    function
    | SynType.Array (_, t, r)
    | SynType.HashConstraint (t, r)
    | SynType.MeasurePower (t, _, r) ->
      addIfInside r
      walkType t
    | SynType.Fun (t1, t2, r)
    | SynType.MeasureDivide (t1, t2, r) ->
      addIfInside r
      walkType t1
      walkType t2
    | SynType.App (ty, _, types, _, _, _, r) ->
      addIfInside r
      walkType ty
      List.iter walkType types
    | SynType.LongIdentApp (_, _, _, types, _, _, r) ->
      addIfInside r
      List.iter walkType types
    | SynType.Tuple (_, ts, r) ->
      addIfInside r
      ts |> List.iter (fun (_, t) -> walkType t)
    | SynType.WithGlobalConstraints (t, typeConstraints, r) ->
      addIfInside r
      walkType t
      List.iter walkTypeConstraint typeConstraints
    | SynType.LongIdent (longDotId) -> ()
    | SynType.AnonRecd (isStruct, typeNames, r) -> addIfInside r
    | SynType.Var (genericName, r) -> addIfInside r
    | SynType.Anon (r) -> addIfInside r
    | SynType.StaticConstant (constant, r) -> addIfInside r
    | SynType.StaticConstantExpr (expr, r) -> addIfInside r
    | SynType.StaticConstantNamed (expr, _, r) -> addIfInside r
    | SynType.Paren (innerType, r) ->
      addIfInside r
      walkType innerType


  and walkClause (Clause (pat, e1, e2, r, _)) =
    addIfInside r
    walkPat pat
    walkExpr e2
    e1 |> Option.iter walkExpr

  and walkSimplePats =
    function
    | SynSimplePats.SimplePats (pats, r) ->
      addIfInside r
      List.iter walkSimplePat pats
    | SynSimplePats.Typed (pats, ty, r) ->
      addIfInside r
      walkSimplePats pats
      walkType ty

  and walkInterpolatedStringPart =
    function
    | SynInterpolatedStringPart.FillExpr (expr, ident) ->
      ident
      |> Option.iter (fun ident -> addIfInside ident.idRange)

      walkExpr expr
    | SynInterpolatedStringPart.String (s, r) -> addIfInside r

  and walkExpr =
    function
    | SynExpr.Typed (e, _, r) ->
      addIfInside r
      walkExpr e
    | SynExpr.Paren (e, _, _, r)
    | SynExpr.Quote (_, _, e, _, r)
    | SynExpr.InferredUpcast (e, r)
    | SynExpr.InferredDowncast (e, r)
    | SynExpr.AddressOf (_, e, _, r)
    | SynExpr.DoBang (e, r)
    | SynExpr.YieldOrReturn (_, e, r)
    | SynExpr.ArrayOrListOfSeqExpr (_, e, r)
    | SynExpr.CompExpr (_, _, e, r)
    | SynExpr.Do (e, r)
    | SynExpr.Assert (e, r)
    | SynExpr.Lazy (e, r)
    | SynExpr.YieldOrReturnFrom (_, e, r) ->
      addIfInside r
      walkExpr e
    | SynExpr.SequentialOrImplicitYield (_, e1, e2, ifNotE, r) ->
      addIfInside r
      walkExpr e1
      walkExpr e2
      walkExpr ifNotE
    | SynExpr.Lambda (_, _, pats, e, _, r) ->
      addIfInside r
      walkSimplePats pats
      walkExpr e
    | SynExpr.New (_, t, e, r)
    | SynExpr.TypeTest (e, t, r)
    | SynExpr.Upcast (e, t, r)
    | SynExpr.Downcast (e, t, r) ->
      addIfInside r
      walkExpr e
      walkType t
    | SynExpr.Tuple (_, es, _, _)
    | Sequentials es -> List.iter walkExpr es //TODO??
    | SynExpr.ArrayOrList (_, es, r) ->
      addIfInside r
      List.iter walkExpr es
    | SynExpr.App (_, _, e1, e2, r)
    | SynExpr.TryFinally (e1, e2, r, _, _)
    | SynExpr.While (_, e1, e2, r) ->
      addIfInside r
      List.iter walkExpr [ e1; e2 ]
    | SynExpr.Record (_, _, fields, r) ->
      addIfInside r

      fields
      |> List.iter (fun ((ident, _), e, _) -> e |> Option.iter walkExpr)
    | SynExpr.ObjExpr (ty, argOpt, bindings, ifaces, _, r) ->
      addIfInside r

      argOpt
      |> Option.iter (fun (e, ident) -> walkExpr e)

      walkType ty
      List.iter walkBinding bindings
      List.iter walkInterfaceImpl ifaces
    | SynExpr.For (_, ident, e1, _, e2, e3, r) ->
      addIfInside r
      List.iter walkExpr [ e1; e2; e3 ]
    | SynExpr.ForEach (_, _, _, pat, e1, e2, r) ->
      addIfInside r
      walkPat pat
      List.iter walkExpr [ e1; e2 ]
    | SynExpr.MatchLambda (_, _, synMatchClauseList, _, r) ->
      addIfInside r
      List.iter walkClause synMatchClauseList
    | SynExpr.Match (_, e, synMatchClauseList, r) ->
      addIfInside r
      walkExpr e
      List.iter walkClause synMatchClauseList
    | SynExpr.TypeApp (e, _, tys, _, _, tr, r) ->
      addIfInside tr
      addIfInside r
      List.iter walkType tys
      walkExpr e
    | SynExpr.LetOrUse (_, _, bindings, e, r) ->
      addIfInside r
      List.iter walkBinding bindings
      walkExpr e
    | SynExpr.TryWith (e, _, clauses, r, _, _, _) ->
      addIfInside r
      List.iter walkClause clauses
      walkExpr e
    | SynExpr.IfThenElse (e1, e2, e3, _, _, _, r) ->
      addIfInside r
      List.iter walkExpr [ e1; e2 ]
      e3 |> Option.iter walkExpr
    | SynExpr.LongIdentSet (ident, e, r)
    | SynExpr.DotGet (e, _, ident, r) ->
      addIfInside r
      walkExpr e
    | SynExpr.DotSet (e1, idents, e2, r) ->
      addIfInside r
      walkExpr e1
      walkExpr e2
    | SynExpr.DotIndexedGet (e, args, _, r) ->
      addIfInside r
      walkExpr e
      List.iter walkIndexerArg args
    | SynExpr.DotIndexedSet (e1, args, e2, _, _, r) ->
      addIfInside r
      walkExpr e1
      List.iter walkIndexerArg args
      walkExpr e2
    | SynExpr.NamedIndexedPropertySet (ident, e1, e2, r) ->
      addIfInside r
      List.iter walkExpr [ e1; e2 ]
    | SynExpr.DotNamedIndexedPropertySet (e1, ident, e2, e3, r) ->
      addIfInside r
      List.iter walkExpr [ e1; e2; e3 ]
    | SynExpr.JoinIn (e1, _, e2, r) ->
      addIfInside r
      List.iter walkExpr [ e1; e2 ]
    | SynExpr.LetOrUseBang (_, _, _, pat, e1, ands, e2, r) ->
      addIfInside r
      walkPat pat
      walkExpr e1

      for (_, _, _, pat, body, r) in ands do
        addIfInside r
        walkPat pat
        walkExpr body

      walkExpr e2
    | SynExpr.TraitCall (ts, sign, e, r) ->
      addIfInside r
      List.iter walkTypar ts
      walkMemberSig sign
      walkExpr e
    | SynExpr.Const (SynConst.Measure (_, m), r) ->
      addIfInside r
      walkMeasure m
    | SynExpr.Const (_, r) -> addIfInside r
    | SynExpr.AnonRecd (isStruct, copyInfo, recordFields, r) -> addIfInside r
    | SynExpr.Sequential (seqPoint, isTrueSeq, expr1, expr2, r) -> ()
    | SynExpr.Ident (_) -> ()
    | SynExpr.LongIdent (isOptional, longDotId, altNameRefCell, r) -> addIfInside r
    | SynExpr.Set (_, _, r) -> addIfInside r
    | SynExpr.Null (r) -> addIfInside r
    | SynExpr.ImplicitZero (r) -> addIfInside r
    | SynExpr.MatchBang (matchSeqPoint, expr, clauses, r) -> addIfInside r
    | SynExpr.LibraryOnlyILAssembly (_, _, _, _, r) -> addIfInside r
    | SynExpr.LibraryOnlyStaticOptimization (_, _, _, r) -> addIfInside r
    | SynExpr.LibraryOnlyUnionCaseFieldGet (expr, longId, _, r) -> addIfInside r
    | SynExpr.LibraryOnlyUnionCaseFieldSet (_, longId, _, _, r) -> addIfInside r
    | SynExpr.ArbitraryAfterError (debugStr, r) -> addIfInside r
    | SynExpr.FromParseError (expr, r) -> addIfInside r
    | SynExpr.DiscardAfterMissingQualificationAfterDot (_, r) -> addIfInside r
    | SynExpr.Fixed (expr, r) -> addIfInside r
    | SynExpr.InterpolatedString (parts, r) ->
      addIfInside r

      for part in parts do
        walkInterpolatedStringPart part

  and walkMeasure =
    function
    | SynMeasure.Product (m1, m2, r)
    | SynMeasure.Divide (m1, m2, r) ->
      addIfInside r
      walkMeasure m1
      walkMeasure m2
    | SynMeasure.Named (longIdent, r) -> addIfInside r
    | SynMeasure.Seq (ms, r) ->
      addIfInside r
      List.iter walkMeasure ms
    | SynMeasure.Power (m, _, r) ->
      addIfInside r
      walkMeasure m
    | SynMeasure.Var (ty, r) ->
      addIfInside r
      walkTypar ty
    | SynMeasure.One
    | SynMeasure.Anon _ -> ()

  and walkSimplePat =
    function
    | SynSimplePat.Attrib (pat, AllAttrs attrs, r) ->
      addIfInside r
      walkSimplePat pat
      List.iter walkAttribute attrs
    | SynSimplePat.Typed (pat, t, r) ->
      addIfInside r
      walkSimplePat pat
      walkType t
    | SynSimplePat.Id (ident, altNameRefCell, isCompilerGenerated, isThisVar, isOptArg, r) -> addIfInside r


  and walkField (SynField.Field (AllAttrs attrs, _, _, t, _, _, _, r)) =
    addIfInside r
    List.iter walkAttribute attrs
    walkType t

  and walkValSig (SynValSig.ValSpfn (AllAttrs attrs, _, _, t, SynValInfo (argInfos, argInfo), _, _, _, _, _, r)) =
    addIfInside r
    List.iter walkAttribute attrs
    walkType t

    argInfo :: (argInfos |> List.concat)
    |> List.collect (fun (SynArgInfo (AllAttrs attrs, _, _)) -> attrs)
    |> List.iter walkAttribute

  and walkMemberSig =
    function
    | SynMemberSig.Inherit (t, r)
    | SynMemberSig.Interface (t, r) ->
      addIfInside r
      walkType t
    | SynMemberSig.Member (vs, _, r) ->
      addIfInside r
      walkValSig vs
    | SynMemberSig.ValField (f, r) ->
      addIfInside r
      walkField f
    | SynMemberSig.NestedType (SynTypeDefnSig.TypeDefnSig (info, repr, memberSigs, _), r) ->
      addIfInside r

      let isTypeExtensionOrAlias =
        match repr with
        | SynTypeDefnSigRepr.Simple (SynTypeDefnSimpleRepr.TypeAbbrev _, _)
        | SynTypeDefnSigRepr.ObjectModel (SynTypeDefnKind.TyconAbbrev, _, _)
        | SynTypeDefnSigRepr.ObjectModel (SynTypeDefnKind.TyconAugmentation, _, _) -> true
        | _ -> false

      walkComponentInfo isTypeExtensionOrAlias info
      walkTypeDefnSigRepr repr
      List.iter walkMemberSig memberSigs

  and walkMember =
    function
    | SynMemberDefn.AbstractSlot (valSig, _, r) ->
      addIfInside r
      walkValSig valSig
    | SynMemberDefn.Member (binding, r) ->
      addIfInside r
      walkBinding binding
    | SynMemberDefn.ImplicitCtor (_, AllAttrs attrs, AllSimplePats pats, _, _, r) ->
      addIfInside r
      List.iter walkAttribute attrs
      List.iter walkSimplePat pats
    | SynMemberDefn.ImplicitInherit (t, e, _, r) ->
      addIfInside r
      walkType t
      walkExpr e
    | SynMemberDefn.LetBindings (bindings, _, _, r) ->
      addIfInside r
      List.iter walkBinding bindings
    | SynMemberDefn.Interface (t, members, r) ->
      addIfInside r
      walkType t
      members |> Option.iter (List.iter walkMember)
    | SynMemberDefn.Inherit (t, _, r) ->
      addIfInside r
      walkType t
    | SynMemberDefn.ValField (field, r) ->
      addIfInside r
      walkField field
    | SynMemberDefn.NestedType (tdef, _, r) ->
      addIfInside r
      walkTypeDefn tdef
    | SynMemberDefn.AutoProperty (AllAttrs attrs, _, _, t, _, _, _, _, e, _, r) ->
      addIfInside r
      List.iter walkAttribute attrs
      Option.iter walkType t
      walkExpr e
    | SynMemberDefn.Open (longId, r) -> addIfInside r

  and walkEnumCase (EnumCase (AllAttrs attrs, _, _, _, r)) =
    addIfInside r
    List.iter walkAttribute attrs

  and walkUnionCaseType =
    function
    | SynUnionCaseType.UnionCaseFields fields -> List.iter walkField fields
    | SynUnionCaseType.UnionCaseFullType (t, _) -> walkType t

  and walkUnionCase (SynUnionCase.UnionCase (AllAttrs attrs, _, t, _, _, r)) =
    addIfInside r
    List.iter walkAttribute attrs
    walkUnionCaseType t

  and walkTypeDefnSimple =
    function
    | SynTypeDefnSimpleRepr.Enum (cases, r) ->
      addIfInside r
      List.iter walkEnumCase cases
    | SynTypeDefnSimpleRepr.Union (_, cases, r) ->
      addIfInside r
      List.iter walkUnionCase cases
    | SynTypeDefnSimpleRepr.Record (_, fields, r) ->
      addIfInside r
      List.iter walkField fields
    | SynTypeDefnSimpleRepr.TypeAbbrev (_, t, r) ->
      addIfInside r
      walkType t
    | SynTypeDefnSimpleRepr.General (_, _, _, _, _, _, _, r) -> addIfInside r
    | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly (_, r) -> addIfInside r
    | SynTypeDefnSimpleRepr.None (r) -> addIfInside r
    | SynTypeDefnSimpleRepr.Exception (_) -> ()

  and walkComponentInfo
    isTypeExtensionOrAlias
    (ComponentInfo (AllAttrs attrs, typars, constraints, longIdent, _, _, _, r))
    =
    addIfInside r
    List.iter walkAttribute attrs
    List.iter walkTyparDecl typars
    List.iter walkTypeConstraint constraints

  and walkTypeDefnRepr =
    function
    | SynTypeDefnRepr.ObjectModel (_, defns, r) ->
      addIfInside r
      List.iter walkMember defns
    | SynTypeDefnRepr.Simple (defn, r) ->
      addIfInside r
      walkTypeDefnSimple defn
    | SynTypeDefnRepr.Exception _ -> ()

  and walkTypeDefnSigRepr =
    function
    | SynTypeDefnSigRepr.ObjectModel (_, defns, _) -> List.iter walkMemberSig defns
    | SynTypeDefnSigRepr.Simple (defn, _) -> walkTypeDefnSimple defn
    | SynTypeDefnSigRepr.Exception _ -> ()

  and walkTypeDefn (TypeDefn (info, repr, members, r)) =
    addIfInside r

    let isTypeExtensionOrAlias =
      match repr with
      | SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.TyconAugmentation, _, _)
      | SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.TyconAbbrev, _, _)
      | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.TypeAbbrev _, _) -> true
      | _ -> false

    walkComponentInfo isTypeExtensionOrAlias info
    walkTypeDefnRepr repr
    List.iter walkMember members

  and walkSynModuleDecl (decl: SynModuleDecl) =
    match decl with
    | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace fragment
    | SynModuleDecl.NestedModule (info, _, modules, _, r) ->
      addIfInside r
      walkComponentInfo false info
      List.iter walkSynModuleDecl modules
    | SynModuleDecl.Let (_, bindings, r) ->
      addIfInside r
      List.iter walkBinding bindings
    | SynModuleDecl.DoExpr (_, expr, r) ->
      addIfInside r
      walkExpr expr
    | SynModuleDecl.Types (types, r) ->
      addIfInside r
      List.iter walkTypeDefn types
    | SynModuleDecl.Attributes (AllAttrs attrs, r) ->
      addIfInside r
      List.iter walkAttribute attrs
    | SynModuleDecl.ModuleAbbrev (ident, longId, r) -> addIfInside r
    | SynModuleDecl.Exception (_, r) -> addIfInside r
    | SynModuleDecl.Open (longDotId, r) -> addIfInside r
    | SynModuleDecl.HashDirective (_, r) -> addIfInside r

  match input with
  | Some (ParsedInput.ImplFile input) -> walkImplFileInput input
  | _ -> ()
  //debug "%A" idents
  result


let getLongIdentAt ast pos =
  let idents = getLongIdents (Some ast)

  match idents.TryGetValue pos with
  | true, idents -> Some idents
  | _ -> None

/// Returns ranges of all quotations found in an untyped AST
let getQuotationRanges ast =
  let quotationRanges = ResizeArray()

  let rec visitExpr =
    function
    | SynExpr.LongIdentSet (_, expr, _)
    | SynExpr.Typed (expr, _, _)
    | SynExpr.Paren (expr, _, _, _)
    | SynExpr.New (_, _, expr, _)
    | SynExpr.ArrayOrListOfSeqExpr (_, expr, _)
    | SynExpr.CompExpr (_, _, expr, _)
    | SynExpr.ForEach (_, _, _, _, _, expr, _)
    | SynExpr.YieldOrReturn (_, expr, _)
    | SynExpr.YieldOrReturnFrom (_, expr, _)
    | SynExpr.Do (expr, _)
    | SynExpr.DoBang (expr, _)
    | SynExpr.Downcast (expr, _, _)
    | SynExpr.For (_, _, _, _, _, expr, _)
    | SynExpr.Lazy (expr, _)
    | SynExpr.Assert (expr, _)
    | SynExpr.TypeApp (expr, _, _, _, _, _, _)
    | SynExpr.DotSet (_, _, expr, _)
    | SynExpr.DotIndexedSet (_, _, expr, _, _, _)
    | SynExpr.NamedIndexedPropertySet (_, _, expr, _)
    | SynExpr.DotNamedIndexedPropertySet (_, _, _, expr, _)
    | SynExpr.TypeTest (expr, _, _)
    | SynExpr.Upcast (expr, _, _)
    | SynExpr.InferredUpcast (expr, _)
    | SynExpr.InferredDowncast (expr, _)
    | SynExpr.Lambda (_, _, _, expr, _, _)
    | SynExpr.AddressOf (_, expr, _, _) -> visitExpr expr
    | SynExpr.App (_, _, expr1, expr2, _)
    | SynExpr.TryFinally (expr1, expr2, _, _, _)
    | SynExpr.While (_, expr1, expr2, _) ->
      visitExpr expr1
      visitExpr expr2
    | SynExpr.LetOrUseBang (_, _, _, _, expr1, ands, expr2, _) ->
      visitExpr expr1

      for (_, _, _, _, body, _) in ands do
        visitExpr body

      visitExpr expr2
    | SynExpr.Tuple (_, exprs, _, _)
    | SynExpr.ArrayOrList (_, exprs, _)
    | Sequentials exprs -> List.iter visitExpr exprs
    | SynExpr.TryWith (expr, _, clauses, _, _, _, _)
    | SynExpr.Match (_, expr, clauses, _) ->
      visitExpr expr
      visitMatches clauses
    | SynExpr.IfThenElse (cond, trueBranch, falseBranchOpt, _, _, _, _) ->
      visitExpr cond
      visitExpr trueBranch
      falseBranchOpt |> Option.iter visitExpr
    | SynExpr.LetOrUse (_, _, bindings, body, _) ->
      visitBindindgs bindings
      visitExpr body
    | SynExpr.Quote (_, _isRaw, _quotedExpr, _, range) -> quotationRanges.Add range
    | SynExpr.MatchLambda (_, _, clauses, _, _) -> visitMatches clauses
    | SynExpr.ObjExpr (_, _, bindings, _, _, _) -> visitBindindgs bindings
    | SynExpr.Record (_, _, fields, _) ->
      fields
      |> List.choose (fun (_, expr, _) -> expr)
      |> List.iter visitExpr
    | _ -> ()

  and visitBinding (Binding (_, _, _, _, _, _, _, _, _, body, _, _)) = visitExpr body
  and visitBindindgs = List.iter visitBinding

  and visitPattern =
    function
    | SynPat.QuoteExpr (expr, _) -> visitExpr expr
    | SynPat.Named (pat, _, _, _, _)
    | SynPat.Paren (pat, _)
    | SynPat.Typed (pat, _, _) -> visitPattern pat
    | SynPat.Ands (pats, _)
    | SynPat.Tuple (_, pats, _)
    | SynPat.ArrayOrList (_, pats, _) -> List.iter visitPattern pats
    | SynPat.Or (pat1, pat2, _) ->
      visitPattern pat1
      visitPattern pat2
    | SynPat.LongIdent (_, _, _, ctorArgs, _, _) ->
      match ctorArgs with
      | SynArgPats.Pats pats -> List.iter visitPattern pats
      | SynArgPats.NamePatPairs (xs, _) -> xs |> List.map snd |> List.iter visitPattern
    | SynPat.Record (xs, _) -> xs |> List.map snd |> List.iter visitPattern
    | _ -> ()

  and visitMatch (SynMatchClause.Clause (pat, _, expr, _, _)) =
    visitPattern pat
    visitExpr expr

  and visitMatches = List.iter visitMatch

  let visitMember =
    function
    | SynMemberDefn.LetBindings (bindings, _, _, _) -> visitBindindgs bindings
    | SynMemberDefn.Member (binding, _) -> visitBinding binding
    | SynMemberDefn.AutoProperty (_, _, _, _, _, _, _, _, expr, _, _) -> visitExpr expr
    | _ -> ()

  let visitType ty =
    let (SynTypeDefn.TypeDefn (_, repr, defns, _)) = ty

    match repr with
    | SynTypeDefnRepr.ObjectModel (_, objDefns, _) ->
      for d in objDefns do
        visitMember d
    | _ -> ()

    for d in defns do
      visitMember d

  let rec visitDeclarations decls =
    decls
    |> List.iter
         (function
         | SynModuleDecl.Let (_, bindings, _) -> visitBindindgs bindings
         | SynModuleDecl.DoExpr (_, expr, _) -> visitExpr expr
         | SynModuleDecl.Types (types, _) -> List.iter visitType types
         | SynModuleDecl.NestedModule (_, _, decls, _, _) -> visitDeclarations decls
         | _ -> ())

  let visitModulesAndNamespaces modulesOrNss =
    modulesOrNss
    |> Seq.iter (fun (SynModuleOrNamespace (_, _, _, decls, _, _, _, _)) -> visitDeclarations decls)

  ast
  |> Option.iter
       (function
       | ParsedInput.ImplFile (ParsedImplFileInput (_, _, _, _, _, modules, _)) -> visitModulesAndNamespaces modules
       | _ -> ())

  quotationRanges

/// Returns all string literal ranges
let internal getStringLiterals ast : Range list =
  let result = ResizeArray()

  let visitType ty =
    match ty with
    | SynType.StaticConstant (SynConst.String (_, r), _) -> result.Add r
    | _ -> ()

  let rec visitExpr =
    function
    | SynExpr.ArrayOrListOfSeqExpr (_, expr, _)
    | SynExpr.CompExpr (_, _, expr, _)
    | SynExpr.Lambda (_, _, _, expr, _, _)
    | SynExpr.YieldOrReturn (_, expr, _)
    | SynExpr.YieldOrReturnFrom (_, expr, _)
    | SynExpr.New (_, _, expr, _)
    | SynExpr.Assert (expr, _)
    | SynExpr.Do (expr, _)
    | SynExpr.Typed (expr, _, _)
    | SynExpr.Paren (expr, _, _, _)
    | SynExpr.DoBang (expr, _)
    | SynExpr.Downcast (expr, _, _)
    | SynExpr.For (_, _, _, _, _, expr, _)
    | SynExpr.Lazy (expr, _)
    | SynExpr.TypeTest (expr, _, _)
    | SynExpr.Upcast (expr, _, _)
    | SynExpr.InferredUpcast (expr, _)
    | SynExpr.InferredDowncast (expr, _)
    | SynExpr.LongIdentSet (_, expr, _)
    | SynExpr.DotGet (expr, _, _, _)
    | SynExpr.ForEach (_, _, _, _, _, expr, _) -> visitExpr expr
    | SynExpr.App (_, _, expr1, expr2, _)
    | SynExpr.TryFinally (expr1, expr2, _, _, _)
    | SynExpr.NamedIndexedPropertySet (_, expr1, expr2, _)
    | SynExpr.DotNamedIndexedPropertySet (_, _, expr1, expr2, _)
    | SynExpr.While (_, expr1, expr2, _) ->
      visitExpr expr1
      visitExpr expr2
    | SynExpr.LetOrUseBang (_, _, _, _, expr1, ands, expr2, _) ->
      visitExpr expr1

      for (_, _, _, _, body, _) in ands do
        visitExpr body

      visitExpr expr2
    | Sequentials exprs
    | SynExpr.Tuple (_, exprs, _, _)
    | SynExpr.ArrayOrList (_, exprs, _) -> List.iter visitExpr exprs
    | SynExpr.Match (_, expr, clauses, _)
    | SynExpr.TryWith (expr, _, clauses, _, _, _, _) ->
      visitExpr expr
      visitMatches clauses
    | SynExpr.IfThenElse (cond, trueBranch, falseBranchOpt, _, _, _, _) ->
      visitExpr cond
      visitExpr trueBranch
      falseBranchOpt |> Option.iter visitExpr
    | SynExpr.LetOrUse (_, _, bindings, body, _) ->
      visitBindindgs bindings
      visitExpr body
    | SynExpr.Record (_, _, fields, _) ->
      fields
      |> List.choose (fun (_, expr, _) -> expr)
      |> List.iter visitExpr
    | SynExpr.MatchLambda (_, _, clauses, _, _) -> visitMatches clauses
    | SynExpr.ObjExpr (_, _, bindings, _, _, _) -> visitBindindgs bindings
    | SynExpr.Const (SynConst.String (_, r), _) -> result.Add r
    | SynExpr.TypeApp (_, _, tys, _, _, _, _) -> List.iter visitType tys
    | _ -> ()

  and visitBinding (Binding (_, _, _, _, _, _, _, _, _, body, _, _)) = visitExpr body
  and visitBindindgs = List.iter visitBinding
  and visitMatch (SynMatchClause.Clause (_, _, expr, _, _)) = visitExpr expr
  and visitMatches = List.iter visitMatch

  let visitMember =
    function
    | SynMemberDefn.LetBindings (bindings, _, _, _) -> visitBindindgs bindings
    | SynMemberDefn.Member (binding, _) -> visitBinding binding
    | SynMemberDefn.AutoProperty (_, _, _, _, _, _, _, _, expr, _, _) -> visitExpr expr
    | _ -> ()

  let visitTypeDefn ty =
    let (SynTypeDefn.TypeDefn (_, repr, memberDefns, _)) = ty

    match repr with
    | SynTypeDefnRepr.ObjectModel (_, defns, _) ->
      for d in defns do
        visitMember d
    | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.TypeAbbrev (_, SynType.App (_, _, tys, _, _, _, _), _), _) ->
      List.iter visitType tys
    | _ -> ()

    List.iter visitMember memberDefns

  let rec visitDeclarations decls =
    for declaration in decls do
      match declaration with
      | SynModuleDecl.Let (_, bindings, _) -> visitBindindgs bindings
      | SynModuleDecl.DoExpr (_, expr, _) -> visitExpr expr
      | SynModuleDecl.Types (types, _) ->
        for ty in types do
          visitTypeDefn ty
      | SynModuleDecl.NestedModule (_, _, decls, _, _) -> visitDeclarations decls
      | _ -> ()

  let visitModulesAndNamespaces modulesOrNss =
    Seq.iter (fun (SynModuleOrNamespace (_, _, _, decls, _, _, _, _)) -> visitDeclarations decls) modulesOrNss

  ast
  |> Option.iter
       (function
       | ParsedInput.ImplFile (ParsedImplFileInput (_, _, _, _, _, modules, _)) -> visitModulesAndNamespaces modules
       | _ -> ())

  List.ofSeq result

/// Get path to containing module/namespace of a given position
let getModuleOrNamespacePath (pos: Pos) (ast: ParsedInput) =
  let idents =
    match ast with
    | ParsedInput.ImplFile (ParsedImplFileInput (_, _, _, _, _, modules, _)) ->
      let rec walkModuleOrNamespace idents (decls, moduleRange) =
        decls
        |> List.fold
             (fun acc ->
               function
               | SynModuleDecl.NestedModule (componentInfo, _, nestedModuleDecls, _, nestedModuleRange) ->
                 if Range.rangeContainsPos moduleRange pos then
                   let (ComponentInfo (_, _, _, longIdent, _, _, _, _)) = componentInfo
                   walkModuleOrNamespace (longIdent :: acc) (nestedModuleDecls, nestedModuleRange)
                 else
                   acc
               | _ -> acc)
             idents

      modules
      |> List.fold
           (fun acc (SynModuleOrNamespace (longIdent, _, _, decls, _, _, _, moduleRange)) ->
             if Range.rangeContainsPos moduleRange pos then
               walkModuleOrNamespace (longIdent :: acc) (decls, moduleRange)
               @ acc
             else
               acc)
           []
    | ParsedInput.SigFile (ParsedSigFileInput (_, _, _, _, modules)) ->
      let rec walkModuleOrNamespaceSig idents (decls, moduleRange) =
        decls
        |> List.fold
             (fun acc ->
               function
               | SynModuleSigDecl.NestedModule (componentInfo, _, nestedModuleDecls, nestedModuleRange) ->
                 if Range.rangeContainsPos moduleRange pos then
                   let (ComponentInfo (_, _, _, longIdent, _, _, _, _)) = componentInfo
                   walkModuleOrNamespaceSig (longIdent :: acc) (nestedModuleDecls, nestedModuleRange)
                 else
                   acc
               | _ -> acc)
             idents

      modules
      |> List.fold
           (fun acc (SynModuleOrNamespaceSig (longIdent, _, _, decls, _, _, _, moduleRange)) ->
             if Range.rangeContainsPos moduleRange pos then
               walkModuleOrNamespaceSig (longIdent :: acc) (decls, moduleRange)
               @ acc
             else
               acc)
           []

  idents
  |> List.rev
  |> Seq.concat
  |> Seq.map (fun ident -> ident.idText)
  |> String.concat "."

let getIdentUsagesByName ast name =
  let idents = getLongIdents (Some ast)

  idents
  |> Seq.choose (fun (KeyValue (pos, ident)) -> if ident = name then Some pos else None)
  |> List.ofSeq

module HashDirectiveInfo =
  open System.IO

  type IncludeDirective = ResolvedDirectory of string

  type LoadDirective =
    | ExistingFile of string
    | UnresolvableFile of string * previousIncludes: string array

  [<NoComparison>]
  type Directive =
    | Include of IncludeDirective * range
    | Load of LoadDirective * range

  /// returns an array of LoadScriptResolutionEntries
  /// based on #I and #load directives
  let getIncludeAndLoadDirectives ast =
    // the Load items are resolved using fallback resolution relying on previously parsed #I directives
    // (this behaviour is undocumented in F# but it seems to be how it works).

    // list of #I directives so far (populated while encountering those in order)
    // TODO: replace by List.fold if possible
    let includesSoFar = new System.Collections.Generic.List<_>()
    let pushInclude = includesSoFar.Add

    // those might need to be abstracted away from real filesystem operations
    let fileExists = File.Exists
    let directoryExists = Directory.Exists
    let isPathRooted (path: string) = Path.IsPathRooted path

    let getDirectoryOfFile =
      Path.GetFullPathSafe >> Path.GetDirectoryName

    let getRootedDirectory = Path.GetFullPathSafe

    let makeRootedDirectoryIfNecessary baseDirectory directory =
      if not (isPathRooted directory) then
        getRootedDirectory (baseDirectory </> directory)
      else
        directory

    // separate function to reduce nesting one level
    let parseDirectives modules file =
      [| let baseDirectory = getDirectoryOfFile file

         for (SynModuleOrNamespace (_, _, _, declarations, _, _, _, _)) in modules do
           for decl in declarations do
             match decl with
             | SynModuleDecl.HashDirective (ParsedHashDirective ("I", [ directory ], range), _) ->
               let directory =
                 makeRootedDirectoryIfNecessary (getDirectoryOfFile file) directory

               if directoryExists directory then
                 let includeDirective = ResolvedDirectory(directory)
                 pushInclude includeDirective
                 yield Include(includeDirective, range)

             | SynModuleDecl.HashDirective (ParsedHashDirective ("load", files, range), _) ->
               for f in files do
                 if isPathRooted f && fileExists f then

                   // this is absolute reference to an existing script, easiest case
                   yield Load(ExistingFile f, range)

                 else
                   // I'm not sure if the order is correct, first checking relative to file containing the #load directive
                   // then checking for undocumented resolution using previously parsed #I directives
                   let fileRelativeToCurrentFile = baseDirectory </> f

                   if fileExists fileRelativeToCurrentFile then
                     // this is existing file relative to current file
                     yield Load(ExistingFile fileRelativeToCurrentFile, range)

                   else
                     // match file against first include which seemingly have it found
                     let maybeFile =
                       includesSoFar
                       |> Seq.tryPick
                            (function
                            | (ResolvedDirectory d) ->
                              let filePath = d </> f

                              if fileExists filePath then
                                Some filePath
                              else
                                None)

                     match maybeFile with
                     | None -> () // can't load this file even using any of the #I directives...
                     | Some f -> yield Load(ExistingFile f, range)
             | _ -> () |]

    match ast with
    | ParsedInput.ImplFile (ParsedImplFileInput (fn, _, _, _, _, modules, _)) -> parseDirectives modules fn
    | _ -> [||]

  /// returns the Some (complete file name of a resolved #load directive at position) or None
  let getHashLoadDirectiveResolvedPathAtPosition (pos: Pos) (ast: ParsedInput) : string option =
    getIncludeAndLoadDirectives ast
    |> Array.tryPick
         (function
         | Load (ExistingFile f, range) when
           // check the line is within the range
           // (doesn't work when there are multiple files given to a single #load directive)
           Range.rangeContainsPos range pos
           ->
           Some f
         | _ -> None)

module Printf =
  [<NoComparison>]
  type PrintfFunction = { FormatString: Range; Args: Range [] }

  [<NoComparison>]
  type private AppWithArg = { Range: Range; Arg: Range }

  let internal getAll (input: ParsedInput option) : PrintfFunction [] =
    let result = ResizeArray()
    let appStack : AppWithArg list ref = ref []

    let addAppWithArg appWithArg =
      match !appStack with
      | lastApp :: _ when not (Range.rangeContainsRange lastApp.Range appWithArg.Range) -> appStack := [ appWithArg ]
      | _ -> appStack := appWithArg :: !appStack

    let rec walkImplFileInput (ParsedImplFileInput (_, _, _, _, _, moduleOrNamespaceList, _)) =
      List.iter walkSynModuleOrNamespace moduleOrNamespaceList

    and walkSynModuleOrNamespace (SynModuleOrNamespace (_, _, _, decls, _, _, _, _)) = List.iter walkSynModuleDecl decls

    and walkTypeConstraint =
      function
      | SynTypeConstraint.WhereTyparDefaultsToType (_, ty, _)
      | SynTypeConstraint.WhereTyparSubtypeOfType (_, ty, _) -> walkType ty
      | SynTypeConstraint.WhereTyparIsEnum (_, ts, _)
      | SynTypeConstraint.WhereTyparIsDelegate (_, ts, _) -> List.iter walkType ts
      | SynTypeConstraint.WhereTyparSupportsMember (_, sign, _) -> walkMemberSig sign
      | _ -> ()

    and walkBinding (SynBinding.Binding (_, _, _, _, _, _, _, _, returnInfo, e, _, _)) =
      walkExpr e

      returnInfo
      |> Option.iter (fun (SynBindingReturnInfo (t, _, _)) -> walkType t)

    and walkInterfaceImpl (InterfaceImpl (_, bindings, _)) = List.iter walkBinding bindings

    and walkIndexerArg =
      function
      | SynIndexerArg.One (e, _fromEnd, _range) -> walkExpr e
      | SynIndexerArg.Two (e1, _e1FromEnd, e2, _e2FromEnd, _e1Range, _e2Range) -> List.iter walkExpr [ e1; e2 ]

    and walkType =
      function
      | SynType.Array (_, t, _)
      | SynType.HashConstraint (t, _)
      | SynType.MeasurePower (t, _, _) -> walkType t
      | SynType.Fun (t1, t2, _)
      | SynType.MeasureDivide (t1, t2, _) ->
        walkType t1
        walkType t2
      | SynType.App (ty, _, types, _, _, _, _) ->
        walkType ty
        List.iter walkType types
      | SynType.LongIdentApp (_, _, _, types, _, _, _) -> List.iter walkType types
      | SynType.Tuple (_, ts, _) -> ts |> List.iter (fun (_, t) -> walkType t)
      | SynType.WithGlobalConstraints (t, typeConstraints, _) ->
        walkType t
        List.iter walkTypeConstraint typeConstraints
      | _ -> ()

    and walkClause (Clause (_, e1, e2, _, _)) =
      walkExpr e2
      e1 |> Option.iter walkExpr

    and walkSimplePats =
      function
      | SynSimplePats.SimplePats (pats, _) -> List.iter walkSimplePat pats
      | SynSimplePats.Typed (pats, ty, _) ->
        walkSimplePats pats
        walkType ty

    and walkExpr e =
      match e with
      | SynExpr.App (_, _, SynExpr.Ident _, SynExpr.Const (SynConst.String (_, stringRange), _), r) ->
        match !appStack with
        | (lastApp :: _) as apps when Range.rangeContainsRange lastApp.Range e.Range ->
          let intersectsWithFuncOrString (arg: Range) =
            Range.rangeContainsRange arg stringRange
            || arg = stringRange
            || Range.rangeContainsRange arg r
            || arg = r

          let rec loop acc (apps: AppWithArg list) =
            match acc, apps with
            | _, [] -> acc
            | [], h :: t ->
              if not (intersectsWithFuncOrString h.Arg) then
                loop [ h ] t
              else
                loop [] t
            | prev :: _, curr :: rest ->
              if
                Range.rangeContainsRange curr.Range prev.Range
                && not (intersectsWithFuncOrString curr.Arg)
              then
                loop (curr :: acc) rest
              else
                acc

          let args =
            apps
            |> loop []
            |> List.rev
            |> List.map (fun x -> x.Arg)
            |> List.toArray

          let res =
            { FormatString = stringRange
              Args = args }

          result.Add res
        | _ -> ()

        appStack := []
      | SynExpr.App (_, _, SynExpr.App (_, true, SynExpr.Ident op, e1, _), e2, _) ->
        let rec deconstruct =
          function
          | SynExpr.Paren (exp, _, _, _) -> deconstruct exp
          | SynExpr.Tuple (_, exps, _, _) ->
            exps
            |> List.iter (fun exp -> addAppWithArg { Range = e.Range; Arg = exp.Range })

            ()
          | _ -> ()

        addAppWithArg { Range = e.Range; Arg = e2.Range }

        if op.idText = (SourceCodeServices.PrettyNaming.CompileOpName "||>")
           || op.idText = (SourceCodeServices.PrettyNaming.CompileOpName "|||>") then
          deconstruct e1
          walkExpr e2
        else
          if op.idText = (SourceCodeServices.PrettyNaming.CompileOpName "|>") then
            addAppWithArg { Range = e.Range; Arg = e1.Range }

          walkExpr e2
          walkExpr e1
      | SynExpr.App (_, _, SynExpr.App (_, true, _, e1, _), e2, _) ->
        addAppWithArg { Range = e.Range; Arg = e2.Range }
        addAppWithArg { Range = e.Range; Arg = e1.Range }
        walkExpr e1
        walkExpr e2
      | SynExpr.App (_, _, e1, e2, _) ->
        addAppWithArg { Range = e.Range; Arg = e2.Range }
        walkExpr e1
        walkExpr e2
      | _ ->
        match e with
        | SynExpr.Paren (e, _, _, _)
        | SynExpr.Quote (_, _, e, _, _)
        | SynExpr.Typed (e, _, _)
        | SynExpr.InferredUpcast (e, _)
        | SynExpr.InferredDowncast (e, _)
        | SynExpr.AddressOf (_, e, _, _)
        | SynExpr.DoBang (e, _)
        | SynExpr.YieldOrReturn (_, e, _)
        | SynExpr.ArrayOrListOfSeqExpr (_, e, _)
        | SynExpr.CompExpr (_, _, e, _)
        | SynExpr.Do (e, _)
        | SynExpr.Assert (e, _)
        | SynExpr.Lazy (e, _)
        | SynExpr.YieldOrReturnFrom (_, e, _) -> walkExpr e
        | SynExpr.Lambda (_, _, pats, e, _, _) ->
          walkSimplePats pats
          walkExpr e
        | SynExpr.New (_, t, e, _)
        | SynExpr.TypeTest (e, t, _)
        | SynExpr.Upcast (e, t, _)
        | SynExpr.Downcast (e, t, _) ->
          walkExpr e
          walkType t
        | SynExpr.Tuple (_, es, _, _)
        | Sequentials es
        | SynExpr.ArrayOrList (_, es, _) -> List.iter walkExpr es
        | SynExpr.TryFinally (e1, e2, _, _, _)
        | SynExpr.While (_, e1, e2, _) -> List.iter walkExpr [ e1; e2 ]
        | SynExpr.Record (_, _, fields, _) ->
          fields
          |> List.iter (fun (_, e, _) -> e |> Option.iter walkExpr)
        | SynExpr.ObjExpr (ty, argOpt, bindings, ifaces, _, _) ->
          argOpt |> Option.iter (fun (e, _) -> walkExpr e)
          walkType ty
          List.iter walkBinding bindings
          List.iter walkInterfaceImpl ifaces
        | SynExpr.For (_, _, e1, _, e2, e3, _) -> List.iter walkExpr [ e1; e2; e3 ]
        | SynExpr.ForEach (_, _, _, _, e1, e2, _) -> List.iter walkExpr [ e1; e2 ]
        | SynExpr.MatchLambda (_, _, synMatchClauseList, _, _) -> List.iter walkClause synMatchClauseList
        | SynExpr.Match (_, e, synMatchClauseList, _) ->
          walkExpr e
          List.iter walkClause synMatchClauseList
        | SynExpr.TypeApp (e, _, tys, _, _, _, _) ->
          List.iter walkType tys
          walkExpr e
        | SynExpr.LetOrUse (_, _, bindings, e, _) ->
          List.iter walkBinding bindings
          walkExpr e
        | SynExpr.TryWith (e, _, clauses, _, _, _, _) ->
          List.iter walkClause clauses
          walkExpr e
        | SynExpr.IfThenElse (e1, e2, e3, _, _, _, _) ->
          List.iter walkExpr [ e1; e2 ]
          e3 |> Option.iter walkExpr
        | SynExpr.LongIdentSet (_, e, _)
        | SynExpr.DotGet (e, _, _, _) -> walkExpr e
        | SynExpr.DotSet (e1, _, e2, _) ->
          walkExpr e1
          walkExpr e2
        | SynExpr.DotIndexedGet (e, args, _, _) ->
          walkExpr e
          List.iter walkIndexerArg args
        | SynExpr.DotIndexedSet (e1, args, e2, _, _, _) ->
          walkExpr e1
          List.iter walkIndexerArg args
          walkExpr e2
        | SynExpr.NamedIndexedPropertySet (_, e1, e2, _) -> List.iter walkExpr [ e1; e2 ]
        | SynExpr.DotNamedIndexedPropertySet (e1, _, e2, e3, _) -> List.iter walkExpr [ e1; e2; e3 ]
        | SynExpr.JoinIn (e1, _, e2, _) -> List.iter walkExpr [ e1; e2 ]
        | SynExpr.LetOrUseBang (_, _, _, _, e1, ands, e2, _) ->
          walkExpr e1

          for (_, _, _, _, body, _) in ands do
            walkExpr body

          walkExpr e2
        | SynExpr.TraitCall (_, sign, e, _) ->
          walkMemberSig sign
          walkExpr e
        | SynExpr.Const (SynConst.Measure (_, m), _) -> walkMeasure m
        | _ -> ()

    and walkMeasure =
      function
      | SynMeasure.Product (m1, m2, _)
      | SynMeasure.Divide (m1, m2, _) ->
        walkMeasure m1
        walkMeasure m2
      | SynMeasure.Seq (ms, _) -> List.iter walkMeasure ms
      | SynMeasure.Power (m, _, _) -> walkMeasure m
      | SynMeasure.One
      | SynMeasure.Anon _
      | SynMeasure.Named _
      | SynMeasure.Var _ -> ()

    and walkSimplePat =
      function
      | SynSimplePat.Attrib (pat, _, _) -> walkSimplePat pat
      | SynSimplePat.Typed (_, t, _) -> walkType t
      | _ -> ()

    and walkField (SynField.Field (_, _, _, t, _, _, _, _)) = walkType t

    and walkMemberSig =
      function
      | SynMemberSig.Inherit (t, _)
      | SynMemberSig.Interface (t, _) -> walkType t
      | SynMemberSig.ValField (f, _) -> walkField f
      | SynMemberSig.NestedType (SynTypeDefnSig.TypeDefnSig (_, repr, memberSigs, _), _) ->
        walkTypeDefnSigRepr repr
        List.iter walkMemberSig memberSigs
      | SynMemberSig.Member _ -> ()

    and walkMember =
      function
      | SynMemberDefn.Member (binding, _) -> walkBinding binding
      | SynMemberDefn.ImplicitCtor (_, _, AllSimplePats pats, _, _, _) -> List.iter walkSimplePat pats
      | SynMemberDefn.ImplicitInherit (t, e, _, _) ->
        walkType t
        walkExpr e
      | SynMemberDefn.LetBindings (bindings, _, _, _) -> List.iter walkBinding bindings
      | SynMemberDefn.Interface (t, members, _) ->
        walkType t
        members |> Option.iter (List.iter walkMember)
      | SynMemberDefn.Inherit (t, _, _) -> walkType t
      | SynMemberDefn.ValField (field, _) -> walkField field
      | SynMemberDefn.NestedType (tdef, _, _) -> walkTypeDefn tdef
      | SynMemberDefn.AutoProperty (_, _, _, t, _, _, _, _, e, _, _) ->
        Option.iter walkType t
        walkExpr e
      | _ -> ()

    and walkTypeDefnRepr =
      function
      | SynTypeDefnRepr.ObjectModel (_, defns, _) -> List.iter walkMember defns
      | SynTypeDefnRepr.Simple _ -> ()
      | SynTypeDefnRepr.Exception _ -> ()

    and walkTypeDefnSigRepr =
      function
      | SynTypeDefnSigRepr.ObjectModel (_, defns, _) -> List.iter walkMemberSig defns
      | SynTypeDefnSigRepr.Simple _ -> ()
      | SynTypeDefnSigRepr.Exception _ -> ()

    and walkTypeDefn (TypeDefn (_, repr, members, _)) =
      walkTypeDefnRepr repr
      List.iter walkMember members

    and walkSynModuleDecl (decl: SynModuleDecl) =
      match decl with
      | SynModuleDecl.NamespaceFragment fragment -> walkSynModuleOrNamespace fragment
      | SynModuleDecl.NestedModule (_, _, modules, _, _) -> List.iter walkSynModuleDecl modules
      | SynModuleDecl.Let (_, bindings, _) -> List.iter walkBinding bindings
      | SynModuleDecl.DoExpr (_, expr, _) -> walkExpr expr
      | SynModuleDecl.Types (types, _) -> List.iter walkTypeDefn types
      | _ -> ()

    match input with
    | Some (ParsedInput.ImplFile input) -> walkImplFileInput input
    | _ -> ()
    //debug "%A" idents
    result.ToArray()
