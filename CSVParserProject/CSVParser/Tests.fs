module Tests

open CSVParser
open ParserLibrary
open Expecto

[<Tests>]
let tests =
  testList "CSV_Tests" [
    testCase "ID" <| fun _ ->
      let ideal = Success (ID 23, {TextInput.lines = [|"23"|];TextInput.position = {line = 0;column = 2;};})
      let actual = run ID_P "213,," 
      Expect.equal actual ideal "Should Succeed with the ID value."


  ]
