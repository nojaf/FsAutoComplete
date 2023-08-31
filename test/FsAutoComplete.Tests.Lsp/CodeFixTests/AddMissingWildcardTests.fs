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
        (Diagnostics.expectCode "43")
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

      testCaseAsync "match clauses are combined"
      <| CodeFix.check
        server
        """
open System.Threading.Tasks

type SomeUnion =
    | First
    | Second
    | Third

let testMatchTwo su =
    task {
        let z = Some(1)

        match z with
        |Some(n) ->
            return ""
        |->
            $0let! x = Task.Delay(200)
            return "hello"
    }
        """
        (Diagnostics.expectCode "750")
        selectCodeFix
        """
open System.Threading.Tasks

type SomeUnion =
    | First
    | Second
    | Third

let testMatchTwo su =
    task {
        let z = Some(1)

        match z with
        |Some(n) ->
            return ""
        | _ ->
            let! x = Task.Delay(200)
            return "hello"
    }
        """

      ])
