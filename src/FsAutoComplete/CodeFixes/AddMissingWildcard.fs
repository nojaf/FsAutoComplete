module FsAutoComplete.CodeFix.AddMissingWildcard

open FSharp.Compiler.SyntaxTrivia
open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Text.Range

let title = "Add wildcard to pattern"

let (|BarMinusGreaterApplication|_|) (e: SynExpr) =
  match e with
  | SynExpr.App(
      isInfix = false
      funcExpr = SynExpr.App(
        isInfix = true
        funcExpr = SynExpr.LongIdent(
          longDotId = SynLongIdent(id = [ ident ]; trivia = [ Some(IdentTrivia.OriginalNotation "|->") ]))
        argExpr = argExpr)) -> Some(ident.idRange, argExpr)
  | _ -> None

let fix (getParseResultsForFile: GetParseResultsForFile) =
  Run.ifDiagnosticByCode (Set.ofList [ "43"; "750" ]) (fun diagnostic codeActionParams ->
    asyncResult {
      let fcsPos = protocolPosToPos diagnostic.Range.Start
      let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
      let! parseAndCheck, _lineString, sourceText = getParseResultsForFile filePath fcsPos
      let parseTree = parseAndCheck.GetParseResults.ParseTree

      let infixApplication =
        let visitor =
          { new SyntaxVisitorBase<range>() with
              member _.VisitExpr(path, traverseSynExpr, defaultTraverse, synExpr) =
                match synExpr with
                | BarMinusGreaterApplication(mOperator, argExpr) ->
                  if rangeContainsPos mOperator fcsPos then
                    // The diagnostic is reported on the operator |-> itself
                    match argExpr with
                    | SynExpr.Match _ -> Some mOperator
                    | _ -> None
                  else
                    // Verify the parent expression is part of the previous match clause
                    match path with
                    | SyntaxNode.SynExpr rhsExpr :: SyntaxNode.SynMatchClause _ :: _ when
                      rangeContainsPos rhsExpr.Range fcsPos
                      ->
                      Some mOperator
                    | _ -> None
                | _ -> defaultTraverse synExpr }

        SyntaxTraversal.Traverse(fcsPos, parseTree, visitor)

      match infixApplication with
      | None -> return []
      | Some mInfixApp ->
        match sourceText.GetLine(mInfixApp.End) with
        | None -> return []
        | Some line ->
          let symbol = parseAndCheck.TryGetSymbolUse mInfixApp.End line

          match symbol with
          | None -> return []
          | Some symbol ->
            match symbol.Symbol with
            | :? FSharpMemberOrFunctionOrValue -> return []
            | _ ->
              return
                [ { Edits =
                      [| { Range = fcsRangeToLsp mInfixApp
                           NewText = "| _ ->" } |]
                    File = codeActionParams.TextDocument
                    Title = title
                    SourceDiagnostic = None
                    Kind = FixKind.Refactor } ]
    })
