module Tests

open CSVParser
open ParserLibrary
open Expecto
open System
open JSONParser

[<Tests>]
let tests =
  testList "CSV_Tests" [
  //==================================
  //Testing ID_P
  //==================================
    testCase "Empty input" <| fun _ ->
      let input = ""
      let ideal = ParseFail "Failed to parse id, No more input, At line 0 column 0 "

      let actual = match (run ID_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"


    testCase "unmatched first char" <| fun _ ->
      let input = "ss"
      let ideal = ParseFail "Failed to parse id, Unexpected 's', At line 0 column 0 "

      let actual = match (run ID_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"
    
    
    testCase "unmatched after first char" <| fun _ ->
      let ideal = ID 3
      let input = "3,asfd"

      let actual = match (run ID_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give ID [value] of the first sequence of occurrence  numbers."

    
    testCase "negatives" <| fun _ ->
      let ideal = ParseFail "Failed to parse id, Unexpected '-', At line 0 column 0 "
      let input = "-353"

      let actual = match (run ID_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give []ParseFail] object with information about Failure"


    testCase "floats" <| fun _ ->
      let ideal = ID 35
      let input = "35.3"

      let actual = match (run ID_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give ID [value] of the first sequence of occurrence  numbers."


    testCase "matched" <| fun _ ->
      let ideal = ID 353
      let input = "353"

      let actual = match (run ID_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give ID [value] of the first sequence of occurrence  numbers."
    
  //==================================
  //Testing Date_P
  //==================================
    testCase "Date_P Empty input" <| fun _ ->
      let input = ""
      let ideal = ParseFail "Failed to parse date, No more input, At line 0 column 0 "

      let actual = match (run Date_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"


    testCase "Date_P unmatched first char" <| fun _ ->
      let input = "asdf"
      let ideal = ParseFail "Failed to parse date, Unexpected 'a', At line 0 column 0 "

      let actual = match (run Date_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"


    testCase "Date_P wrong input" <| fun _ ->
      let input = "asdf"
      let ideal = ParseFail "Failed to parse date, Unexpected 'a', At line 0 column 0 "

      let actual = match (run Date_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"


    testCase "Date_P wrong input 2" <| fun _ ->
      let input = "7/2sfe1/2017 21:01"
      let ideal = ParseFail "Failed to parse date, Unexpected 's', At line 0 column 3 "

      let actual = match (run Date_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"

  
    testCase "Date_P wrong input 3" <| fun _ ->
      let input = "7/-21/2017 21:01"
      let ideal = ParseFail "Failed to parse date, Unexpected '-', At line 0 column 2 "

      let actual = match (run Date_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"


    testCase "Date_P wrong input 4" <| fun _ ->
      let input = "7/21/2.017 21:01"
      let ideal = ParseFail "Failed to parse date, Unexpected '.', At line 0 column 6 "

      let actual = match (run Date_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"


    testCase "Date_P good input followed by garbage" <| fun _ ->
      let input = "7/21/2017 21:01afdaasf"
      let ideal = Date (DateTime.Parse "7/21/2017 21:01")

      let actual = match (run Date_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give Date [value] ."


    testCase "Date_P good input" <| fun _ ->
      let input = "7/21/2017 21:01"
      let ideal = Date (DateTime.Parse "7/21/2017 21:01")

      let actual = match (run Date_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give Date [value] ."

  //==================================
  //Testing HTTPMethod_P
  //==================================
    testCase "HTTPMethod_P Empty input" <| fun _ ->
      let input = ""
      let ideal = ParseFail "Failed to parse HttpMethod, No more input, At line 0 column 0 "

      let actual = match (run HTTPMethod_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"


    testCase "HTTPMethod_P wrong input" <| fun _ ->
      let input = "7sd"
      let ideal = ParseFail "Failed to parse HttpMethod, Unexpected '7', At line 0 column 0 "

      let actual = match (run HTTPMethod_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"


    testCase "HTTPMethod_P wrong input 2" <| fun _ ->
      let input = "GE T"
      let ideal = ParseFail "Failed to parse HttpMethod, Unexpected 'G', At line 0 column 0 "

      let actual = match (run HTTPMethod_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"


    testCase "HTTPMethod_P good input followed with garbage" <| fun _ ->
      let input = "GETwafs"
      let ideal = HTTPMethod GET

      let actual = match (run HTTPMethod_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give HTTPMethod GET"

    
    testCase "HTTPMethod_P good input GET" <| fun _ ->
      let input = "GET"
      let ideal = HTTPMethod GET

      let actual = match (run HTTPMethod_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give HTTPMethod GET"


    testCase "HTTPMethod_P good input POST" <| fun _ ->
      let input = "POST"
      let ideal = HTTPMethod POST

      let actual = match (run HTTPMethod_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give HTTPMethod POST"

  //==================================
  //Testing AbsoluteUri_P
  //==================================
    testCase "AbsoluteUri_P Empty input" <| fun _ ->
      let input = ""
      let ideal = ParseFail "Failed to parse AbsoluteUri, No more input, At line 0 column 0 "

      let actual = match (run AbsoluteUri_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"

    testCase "AbsoluteUri_P wrong input 1" <| fun _ ->
      let input = "23wefsd"
      let ideal = ParseFail "Failed to parse AbsoluteUri, Unexpected '2', At line 0 column 0 "

      let actual = match (run AbsoluteUri_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"


    testCase "AbsoluteUri_P wrong input 2" <| fun _ ->
      let input = "htt2ps://choptsalad20170526.restaurant365.net/Pilot/ServiceStack/PublicCustomerVersion"
      let ideal = ParseFail "Failed to parse AbsoluteUri, Unexpected '2', At line 0 column 3 "

      let actual = match (run AbsoluteUri_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"


    testCase "AbsoluteUri_P good input" <| fun _ ->
      let input = "https://choptsalad20170526.restaurant365.net/Pilot/ServiceStack/PublicCustomerVersion"
      let ideal = AbsoluteUri "https://choptsalad20170526.restaurant365.net/Pilot/ServiceStack/PublicCustomerVersion\n"

      let actual = match (run AbsoluteUri_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [AbsoluteUri] object"
  //==================================
  //Testing PathInfo_P
  //==================================
    testCase "PathInfo_P wrong input - doesn't start with '/'" <| fun _ ->
      let input = "23/wfasdfaewfasf"
      let ideal = ParseFail "Failed to parse PathInfo, Unexpected '2', At line 0 column 0 "

      let actual = match (run PathInfo_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"

    
    testCase "PathInfo_P wrong input - invalid character '/'" <| fun _ ->
      let input = "//wfasdfaewfasf"
      let ideal = ParseFail "Failed to parse PathInfo, Unexpected '/', At line 0 column 1 "

      let actual = match (run PathInfo_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"


    testCase "PathInfo_P wrong input - invalid character '%'" <| fun _ ->
      let input = "/wfas\%dfaewfasf"
      let ideal = PathInfo "/wfas"

      let actual = match (run PathInfo_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [PathInfo] object of the first occurance chars"


    testCase "PathInfo_P good input" <| fun _ ->
      let input = "/wfasdfaewfasf,"
      let ideal = PathInfo "/wfasdfaewfasf"

      let actual = match (run PathInfo_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [PathInfo] object"

  //==================================
  //Testing ReqBody_P
  //==================================
    testCase "ReqBody_P wrong input" <| fun _ ->
      let input = "32fsxc,"
      let ideal = ParseFail "Failed to parse ReqBody, Unexpected '3', At line 0 column 0 "

      let actual = match (run ReqBody_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"

    
    testCase "ReqBody_P wrong input 2" <| fun _ ->
      let input = "{sfs,"
      let ideal = ParseFail "Failed to parse ReqBody, Unexpected '{', At line 0 column 0 "

      let actual = match (run ReqBody_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"

    
    testCase "ReqBody_P wrong input 3" <| fun _ ->
      let input = "{    }"
      let ideal = ReqBody (JEmptyObject "{}")

      let actual = match (run ReqBody_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ReqBody] object empty"

    
    testCase "ReqBody_P good input" <| fun _ ->
      let input = "{string: string}"
      let ideal = ReqBody (JObject (Map [("string", JUnquotedString "string")]))

      let actual = match (run ReqBody_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ReqBody] object"

  //==================================
  //Testing ReqDto_P
  //==================================
    testCase "ReqDto_P wrong input" <| fun _ ->
      let input = "32fsxc,"
      let ideal = ParseFail "Failed to parse ReqDto, Unexpected '3', At line 0 column 0 "

      let actual = match (run ReqDto_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"

    
    testCase "ReqDto_P wrong input 2" <| fun _ ->
      let input = "{sfs,"
      let ideal = ParseFail "Failed to parse ReqDto, Unexpected '{', At line 0 column 0 "

      let actual = match (run ReqDto_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"

    
    testCase "ReqDto_P wrong input 3" <| fun _ ->
      let input = "{    }"
      let ideal = ReqDto (JEmptyObject "{}")

      let actual = match (run ReqDto_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ReqDto] object empty"

    
    testCase "ReqDto_P good input" <| fun _ ->
      let input = "{string: string}"
      let ideal = ReqDto (JObject (Map [("string", JUnquotedString "string")]))

      let actual = match (run ReqDto_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ReqDto] object"

  //==================================
  //Testing UserAuthId_P 
  //==================================
    testCase "UserAuthId_P Empty input" <| fun _ ->
      let input = ","
      let ideal = UserAuthId (JUnquotedString "")

      let actual = match (run UserAuthId_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [UserAuthId] object empty"


    testCase "UserAuthId_P unmatched first char" <| fun _ ->
      let input = "ss"
      let ideal = ParseFail "Failed to parse UserAuthId, Unexpected 's', At line 0 column 0 "

      let actual = match (run UserAuthId_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"
    
    
    testCase "UserAuthId_P unmatched after first char" <| fun _ ->
      let input = "3,asfd"
      let ideal = UserAuthId (JNumber 3.0)
      let actual = match (run UserAuthId_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give UserAuthId [value] of the first sequence of occurrence  numbers."


    testCase "UserAuthId_P matched" <| fun _ ->
      let ideal = UserAuthId (JNumber 353.0)
      let input = "353"

      let actual = match (run UserAuthId_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give UserAuthId [value] of the first sequence of occurrence  numbers."
  

  //==================================
  //Testing SessionId_P
  //==================================
    testCase "SessionId_P input 0" <| fun _ ->
      let input = ""
      let ideal = ParseFail "Failed to parse SessionId, No more input, At line 0 column 0 "
      let actual = match (run SessionId_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"


    testCase "SessionId_P input 1" <| fun _ ->
      let input = "234,asadf"
      let ideal = SessionId (JUnquotedString "234")
      let actual = match (run SessionId_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give SessionId_P [value] of the first sequence of occurrence  numbers."


    testCase "SessionId_P input 2" <| fun _ ->
      let input = "3ecsaf53,"
      let ideal = SessionId (JUnquotedString "3ecsaf53")
      let actual = match (run SessionId_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give SessionId_P [value] of the first sequence of occurrence  numbers."

  //==================================
  //Testing IPAddr_P
  //==================================
    testCase "IPAddr_P input empty" <| fun _ ->
      let input = ""
      let ideal = ParseFail "Failed to parse IPAddress, No more input, At line 0 column 0 "
      let actual = match (run IPAddr_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"


    testCase "IPAddr_P input wrong 1" <| fun _ ->
      let input = "dzv2efs"
      let ideal = ParseFail "Failed to parse IPAddress, Unexpected 'd', At line 0 column 0 "
      let actual = match (run IPAddr_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"

    
    testCase "IPAddr_P input wrong 2" <| fun _ ->
      let input = "192..81.1.5"
      let ideal = ParseFail "Failed to parse IPAddress, Unexpected '.', At line 0 column 4 "
      let actual = match (run IPAddr_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"

     
    testCase "IPAddr_P input wrong 3" <| fun _ ->
      let input = "192.81.1.s5"
      let ideal = ParseFail "Failed to parse IPAddress, Unexpected 's', At line 0 column 9 "
      let actual = match (run IPAddr_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"


    testCase "IPAddr_P good input" <| fun _ ->
      let input = "192.81.1.3"
      let ideal = IPAddr "192.81.1.3"
      let actual = match (run IPAddr_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [IPAddr] object"


    testCase "IPAddr_P good input 2" <| fun _ ->
      let input = "192.81.1.3asdfasdf"
      let ideal = IPAddr "192.81.1.3"
      let actual = match (run IPAddr_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [IPAddr] object"

  //==================================
  //Testing ForwardedFor_P
  //==================================
    testCase "ForwardedFor_P empty input" <| fun _ ->
      let input = ","
      let ideal = ForwardedFor (JUnquotedString "")
      let actual = match (run ForwardedFor_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ForwardedFor_P] object"


    testCase "ForwardedFor_P good input 2" <| fun _ ->
      let input = "sdg34tegd334egrf"
      let ideal = ForwardedFor (JUnquotedString "sdg34tegd334egrf\n")
      let actual = match (run ForwardedFor_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ForwardedFor_P] object"

  //==================================
  //Testing Referer_P
  //==================================
    testCase "Referer_P Empty input" <| fun _ ->
      let input = ","
      let ideal = Referer ""

      let actual = match (run Referer_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [Referer] object empty"

    testCase "Referer_P wrong input 1" <| fun _ ->
      let input = "23wefsd"
      let ideal = ParseFail "Failed to parse Referer, Unexpected '2', At line 0 column 0 "

      let actual = match (run Referer_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"


    testCase "Referer_P wrong input 2" <| fun _ ->
      let input = "htt2ps://choptsalad20170526.restaurant365.net/Pilot/ServiceStack/PublicCustomerVersion"
      let ideal = ParseFail "Failed to parse Referer, Unexpected 'h', At line 0 column 0 "

      let actual = match (run Referer_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"


    testCase "Referer_P good input" <| fun _ ->
      let input = "https://choptsalad20170526.restaurant365.net/Pilot/ServiceStack/PublicCustomerVersion"
      let ideal = Referer "https://choptsalad20170526.restaurant365.net/Pilot/ServiceStack/PublicCustomerVersion\n"

      let actual = match (run Referer_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [Referer] object"

  //==================================
  //Testing Headers_P
  //==================================
    testCase "Headers_P wrong input" <| fun _ ->
      let input = "32fsxc,"
      let ideal = ParseFail "Failed to parse Headers, Unexpected '3', At line 0 column 0 "

      let actual = match (run Headers_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"

    
    testCase "Headers_P wrong input 2" <| fun _ ->
      let input = "{sfs,"
      let ideal = ParseFail "Failed to parse Headers, Unexpected '{', At line 0 column 0 "

      let actual = match (run Headers_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"

    
    testCase "Headers_P wrong input 3" <| fun _ ->
      let input = "{    }"
      let ideal = Headers (JEmptyObject "{}")

      let actual = match (run Headers_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [Headers] object empty"

    
    testCase "Headers_P good input" <| fun _ ->
      let input = "{string: string}"
      let ideal = Headers (JObject (Map [("string", JUnquotedString "string")]))

      let actual = match (run Headers_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [Headers] object"

  //==================================
  //Testing FormData_P
  //==================================
    testCase "FormData_P wrong input" <| fun _ ->
      let input = "32fsxc,"
      let ideal = ParseFail "Failed to parse FormData, Unexpected '3', At line 0 column 0 "

      let actual = match (run FormData_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"

    
    testCase "FormData_P wrong input 2" <| fun _ ->
      let input = "{sfs,"
      let ideal = ParseFail "Failed to parse FormData, Unexpected '{', At line 0 column 0 "

      let actual = match (run FormData_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"

    
    testCase "FormData_P wrong input 3" <| fun _ ->
      let input = "{    }"
      let ideal = FormData (JEmptyObject "{}")

      let actual = match (run FormData_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [FormData] object empty"

    
    testCase "FormData_P good input" <| fun _ ->
      let input = "{string: string}"
      let ideal = FormData (JObject (Map [("string", JUnquotedString "string")]))

      let actual = match (run FormData_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [FormData] object"

  //==================================
  //Testing Items_P
  //==================================
    testCase "Items_P wrong input" <| fun _ ->
      let input = "32fsxc,"
      let ideal = ParseFail "Failed to parse Items, Unexpected '3', At line 0 column 0 "

      let actual = match (run Items_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"

    
    testCase "Items_P wrong input 2" <| fun _ ->
      let input = "{sfs,"
      let ideal = ParseFail "Failed to parse Items, Unexpected '{', At line 0 column 0 "

      let actual = match (run Items_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ParseFail] object with information about Failure"

    
    testCase "Items_P wrong input 3" <| fun _ ->
      let input = "{    }"
      let ideal = Items (JEmptyObject "{}")

      let actual = match (run Items_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [Items] object empty"

    
    testCase "Items_P good input" <| fun _ ->
      let input = "{string: string}"
      let ideal = Items (JObject (Map [("string", JUnquotedString "string")]))

      let actual = match (run Items_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [Items] object"

  //==================================
  //Testing Session_P
  //==================================
    testCase "Session_P empty input" <| fun _ ->
      let input = ","
      let ideal = Session ""
      let actual = match (run Session_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [Session_P] object"


    testCase "Session_P good input 2" <| fun _ ->
      let input = "sdg34tegd334egrf,"
      let ideal = Session "sdg34tegd334egrf"
      let actual = match (run Session_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [Session] object"


  //==================================
  //Testing RsponseDto_P
  //==================================
    testCase "RsponseDto_P empty input" <| fun _ ->
      let input = ","
      let ideal = RsponseDto ""
      let actual = match (run RsponseDto_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [RsponseDto] object"


    testCase "RsponseDto_P good input 2" <| fun _ ->
      let input = "sdg34tegd334egrf,"
      let ideal = RsponseDto "sdg34tegd334egrf"
      let actual = match (run RsponseDto_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [RsponseDto] object"

  //==================================
  //Testing ErrorResponse_P
  //==================================
    testCase "ErrorResponse_P empty input" <| fun _ ->
      let input = ","
      let ideal = ErrorResponse ""
      let actual = match (run ErrorResponse_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ErrorResponse] object"


    testCase "ErrorResponse_P good input 2" <| fun _ ->
      let input = "sdg34tegd334egrf,"
      let ideal = ErrorResponse "sdg34tegd334egrf"
      let actual = match (run ErrorResponse_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ErrorResponse] object"

  //==================================
  //Testing ReqDuration_P
  //==================================
    testCase "ReqDuration_P emty input" <| fun _ ->
      let input = ""
      let ideal = ParseFail "Failed to parse ReqDuration, No more input, At line 0 column 0 "
      let actual = match (run ReqDuration_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ReqDuration] object"

    
    testCase "ReqDuration_P emty input 2" <| fun _ ->
      let input = ","
      let ideal = ParseFail "Failed to parse ReqDuration, Unexpected ',', At line 0 column 0 "
      let actual = match (run ReqDuration_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ReqDuration] object"


    testCase "ReqDuration_P wrong input" <| fun _ ->
      let input = "aefsd"
      let ideal = ParseFail "Failed to parse ReqDuration, Unexpected 'a', At line 0 column 0 "
      let actual = match (run ReqDuration_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ReqDuration] object"


    testCase "ReqDuration_P wrong input seconds" <| fun _ ->
      let input = "PT0.0eaf66026S"
      let ideal = ParseFail "Failed to parse ReqDuration, Unexpected 'e', At line 0 column 5 "
      let actual = match (run ReqDuration_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ReqDuration] object"


    testCase "ReqDuration_P good input seconds" <| fun _ ->
      let input = "PT0.066026S"
      let ideal = ReqDuration "PT0.066026S"
      let actual = match (run ReqDuration_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ReqDuration] object"


    testCase "ReqDuration_P good input minutes" <| fun _ ->
      let input = "PT32M0.066026S"
      let ideal = ReqDuration "PT32.000000M0.066026S"
      let actual = match (run ReqDuration_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ReqDuration] object"


    testCase "ReqDuration_P good input hours" <| fun _ ->
      let input = "PT1H32M0.066026S"
      let ideal = ReqDuration "PT1.000000H32.000000M0.066026S"
      let actual = match (run ReqDuration_P input) with
                   |Success (v,_) -> v
                   |Failure (s1,s2,s3) -> ParseFail (sprintf """Failed to parse %s, %s, At line %d column %d """ s1 s2 s3.line s3.column)

      Expect.equal actual ideal "Should give [ReqDuration] object"

  ]

