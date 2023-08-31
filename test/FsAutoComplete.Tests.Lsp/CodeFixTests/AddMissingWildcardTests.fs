module private FsAutoComplete.Tests.CodeFixTests.AddMissingWildcardTests

open Expecto
open Helpers
open Utils.ServerTests
open Utils.CursorbasedTests
open FsAutoComplete.CodeFix

let tests state =
  fserverTestList (nameof AddMissingWildcard) state defaultConfigDto None (fun server ->
    [ let selectCodeFix = CodeFix.withTitle AddMissingWildcard.title

      testCaseAsync "last match clause is mistaken"
      <| CodeFix.check
        server
        """
type SomeUnion =
    | First
    | Second
    | Third

let testMatch su =
    match su with
    | First -> "hey"
    |->$0 "hello"
        """
        Diagnostics.acceptAll
        selectCodeFix
        """
type SomeUnion =
    | First
    | Second
    | Third

let testMatch su =
    match su with
    | First -> "hey"
    | _ -> "hello"
        """

      ])
