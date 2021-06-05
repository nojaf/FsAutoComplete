module FsAutoComplete.CodeFix.ResolveNamespace

open LanguageServerProtocol.Types
open FsAutoComplete.CodeFix
open FsAutoComplete.CodeFix.Types
open FsToolkit.ErrorHandling
open FSharp.Compiler.SourceCodeServices
open FsAutoComplete.LspHelpers
open FsAutoComplete
open FSharp.Compiler.Text

type LineText = string

/// a codefix the provides suggestions for opening modules or using qualified names when an identifier is found that needs qualification
let fix
  (getParseResultsForFile: GetParseResultsForFile)
  (getNamespaceSuggestions: ParseAndCheckResults
                              -> FcsPos
                              -> LineText
                              -> Async<CoreResponse<string * list<StringLongIdent * StringLongIdent * InsertContext * bool> * list<StringLongIdent * StringLongIdent>>>)
  =

  /// insert a line of text at a given line
  let insertLine line lineStr =
    { Range =
        { Start = { Line = line; Character = 0 }
          End = { Line = line; Character = 0 } }
      NewText = lineStr }

  let adjustInsertionPoint (lines: ISourceText) (ctx: InsertContext) =
    let l = ctx.Pos.Line

    match ctx.ScopeKind with
    | TopModule when l > 1 ->
      let line = lines.GetLineString(l - 2)

      let isImplicitTopLevelModule =
        not (
          line.StartsWith "module"
          && not (line.EndsWith "=")
        )

      if isImplicitTopLevelModule then
        1
      else
        l
    | TopModule -> 1
    | ScopeKind.Namespace when l > 1 ->
      [ 0 .. l - 1 ]
      |> List.mapi (fun i line -> i, lines.GetLineString line)
      |> List.tryPick
           (fun (i, lineStr) ->
             if lineStr.StartsWith "namespace" then
               Some i
             else
               None)
      |> function
        // move to the next line below "namespace" and convert it to F# 1-based line number
        | Some line -> line + 2
        | None -> l
    | ScopeKind.Namespace -> 1
    | _ -> l

  let qualifierFix file diagnostic qual =
    { SourceDiagnostic = Some diagnostic
      Edits =
        [| { Range = diagnostic.Range
             NewText = qual } |]
      File = file
      Title = $"Use %s{qual}"
      Kind = Fix }

  let openFix (text: ISourceText) file diagnostic (word: string) (ns, name: string, ctx, multiple) : Fix =
    let insertPoint = adjustInsertionPoint text ctx
    let docLine = insertPoint - 1

    let actualOpen =
      if name.EndsWith word && name <> word then
        let prefix =
          name
            .Substring(0, name.Length - word.Length)
            .TrimEnd('.')

        $"%s{ns}.%s{prefix}"
      else
        ns

    let lineStr =
      let whitespace = String.replicate ctx.Pos.Column " "
      $"%s{whitespace}open %s{actualOpen}\n"

    let edits =
      [| yield insertLine docLine lineStr
         if text.GetLineString(docLine + 1).Trim() <> "" then
           yield insertLine (docLine + 1) ""
         if
           (ctx.Pos.Column = 0 || ctx.ScopeKind = Namespace)
           && docLine > 0
           && not (text.GetLineString(docLine - 1).StartsWith "open")
         then
           yield insertLine (docLine - 1) "" |]

    { Edits = edits
      File = file
      SourceDiagnostic = Some diagnostic
      Title = $"open %s{actualOpen}"
      Kind = Fix }

  Run.ifDiagnosticByMessage
    "is not defined"
    (fun diagnostic codeActionParameter ->
      asyncResult {
        let pos = protocolPosToPos diagnostic.Range.Start

        let filePath =
          codeActionParameter.TextDocument.GetFilePath()
          |> Utils.normalizePath

        let! tyRes, line, lines = getParseResultsForFile filePath pos

        match! getNamespaceSuggestions tyRes pos line with
        | CoreResponse.InfoRes msg
        | CoreResponse.ErrorRes msg -> return []
        | CoreResponse.Res (word, opens, qualifiers) ->
          let quals =
            qualifiers
            |> List.map (fun (_, qual) -> qualifierFix codeActionParameter.TextDocument diagnostic qual)

          let ops =
            opens
            |> List.map (openFix lines codeActionParameter.TextDocument diagnostic word)

          return [ yield! ops; yield! quals ]
      })
