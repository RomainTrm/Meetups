module CommandParser

type Argument = 
| Verbose
| Url of string
| Port of int

type Parser = string list -> Argument list

let private apply parsersRunner parsers =
    List.collect (fun parserRunner -> parsers |> List.collect parserRunner) parsersRunner
let private (<*>) = apply

let private verboseParser : Parser =
    fun commandInputs -> 
        let isVerbose = commandInputs |> List.contains "-v"
        if isVerbose 
        then [Verbose]
        else []

let rec private extractArgumentWithValue argument parse = 
    function
    | head::value::_ 
        when head = argument
        -> [parse value]
    | _::tail -> extractArgumentWithValue argument parse tail
    | [] -> []

let private urlParser : Parser = 
    fun commandInputs -> 
        let extractUrl = extractArgumentWithValue "-u" Url
        extractUrl commandInputs

let private portParser : Parser = 
    fun commandInputs -> 
        let parsePort port = Port (int port)
        let extractPort = extractArgumentWithValue "-p" parsePort
        extractPort commandInputs

let private runParsers parsers commandInputs =
    let runParser (parser: Parser) = parser commandInputs
    [runParser] <*> parsers

let parseCommand (commandInput:string) : Argument list =
    commandInput.Split " " 
    |> Array.toList
    |> runParsers [urlParser; portParser; verboseParser]