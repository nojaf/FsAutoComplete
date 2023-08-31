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

let title = "Add wildcard to pattern"

let fix
  (getParseResultsForFile: GetParseResultsForFile)
  (codeActionParams: CodeActionParams)
  : Async<Result<Fix list, string>> =
  asyncResult {
    let fcsPos = protocolPosToPos codeActionParams.Range.Start
    let filePath = codeActionParams.TextDocument.GetFilePath() |> Utils.normalizePath
    let! parseAndCheck, _lineString, sourceText = getParseResultsForFile filePath fcsPos
    let parseTree = parseAndCheck.GetParseResults.ParseTree

    let infixApplication =
      let visitor =
        { new SyntaxVisitorBase<range>() with
            member _.VisitExpr(path, traverseSynExpr, defaultTraverse, synExpr) =
              match synExpr with
              | SynExpr.App(
                  isInfix = false
                  funcExpr = SynExpr.App(
                    isInfix = true
                    funcExpr = SynExpr.LongIdent(
                      longDotId = SynLongIdent(id = [ ident ]; trivia = [ Some(IdentTrivia.OriginalNotation "|->") ]))
                    argExpr = SynExpr.Match _)) -> Some ident.idRange
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
  }
