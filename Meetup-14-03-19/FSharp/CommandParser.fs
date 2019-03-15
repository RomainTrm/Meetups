module CommandParser

type Argument = 
| Verbose
| Url of string
| Port of int

type Parser = string list -> Argument list

let private apply parsersRunner parsers =
    List.collect (fun parserRunner -> parsers |> List.collect parserRunner) parsersRunner
let private (<*>) = apply

let private flagParser token argument commandInputs =
    match commandInputs |> List.contains token with
    | true -> [argument]
    | false -> []
        
let rec private extractArgumentWithValue token argument = function
    | head::value::_ 
        when head = token
        -> [argument value]
    | _::tail -> extractArgumentWithValue token argument tail
    | [] -> []

let private urlParser = extractArgumentWithValue "-u" Url
let private portParser = extractArgumentWithValue "-p" (int >> Port)
let private verboseParser = flagParser "-v" Verbose

let parsers = [urlParser; portParser; verboseParser]

let private runParsers parsers commandInputs =
    let runParser (parser: Parser) = parser commandInputs
    [runParser] <*> parsers

let parseCommand (commandInput:string) : Argument list =
    commandInput.Split " " 
    |> Array.toList
    |> runParsers parsers