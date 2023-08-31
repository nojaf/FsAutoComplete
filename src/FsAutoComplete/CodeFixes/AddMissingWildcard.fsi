module FsAutoComplete.CodeFix.AddMissingWildcard

open FsAutoComplete.CodeFix.Types

val title: string
val fix: getParseResultsForFile: GetParseResultsForFile -> CodeFix
