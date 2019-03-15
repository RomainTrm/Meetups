module Tests

open CommandParser
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Should parse no argument when nothing`` () =
    let result = parseCommand ""
    test <@ List.isEmpty result @>
 
[<Fact>]
let ``Should parse verbose argument`` () =
    let result = parseCommand "-v"
    test <@ result = [Verbose] @>
    
[<Fact>]
let ``Should parse url argument`` () =
    let result = parseCommand "-u https://www.gooogle.com"
    test <@ result = [Url "https://www.gooogle.com"] @>
    
[<Fact>]
let ``Should parse port argument`` () =
    let result = parseCommand "-p 8080"
    test <@ result = [Port 8080] @>
    
[<Fact>]
let ``Should parse all arguments at once`` () =
    let result = parseCommand "-u https://www.gooogle.com -p 8080 -v"
    test <@ result = [Url "https://www.gooogle.com"; Port 8080; Verbose] @>