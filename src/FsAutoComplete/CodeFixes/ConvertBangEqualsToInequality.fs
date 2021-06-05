/// fix to convert uses of != to <> to use the proper thing
module FsAutoComplete.CodeFix.ConvertBangEqualsToInequality

open FsToolkit.ErrorHandling
open FsAutoComplete.CodeFix.Types
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers

let fix (getRangeText: GetRangeText) : CodeFix =
  Run.ifDiagnosticByCode
    (Set.ofList [ "43" ])
    (fun diag codeActionParams ->
      asyncResult {
        let fileName =
          codeActionParams.TextDocument.GetFilePath()
          |> Utils.normalizePath

        let! errorText = getRangeText fileName diag.Range
        do! Result.guard (fun () -> errorText = "!=") "Not an != equality usage"

        return
          [ { Title = "Use <> for inequality check"
              File = codeActionParams.TextDocument
              SourceDiagnostic = Some diag
              Kind = Fix
              Edits = [| { Range = diag.Range; NewText = "<>" } |] } ]

      })
