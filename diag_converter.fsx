
open System.IO
open System.Text.RegularExpressions

type File =
    {
        Name: string
        Frequency: int
        FrequencyUnit: string
        TimeUnit: string
        UnlimitedDimension: string
        Commented: bool
    }

type Diagnostic =
    {
        Name: string
        Variable: string
        Files: string list
        Reduction: string
        Region: string
        Kind: int
        Module: string
        Commented: bool

    }
    static member (+) (left: Diagnostic, right: Diagnostic) =
        if (left.Name, left.Variable, left.Reduction, left.Region, left.Kind, left.Module) = (right.Name, right.Variable, right.Reduction, right.Region, right.Kind, right.Module) then
            {
                Name = left.Name;
                Variable = left.Variable;
                Files = (left.Files @ right.Files);
                Reduction = left.Reduction;
                Region = left.Region;
                Kind = left.Kind;
                Module = left.Module;
                Commented = false
            }
        else failwith "These diagnostics cannot be composed."

/// Union of all the relevant token types
type Token =
    | NewLine
    | Comma
    | Comment
    | WhiteSpace
    | Str of string
    | Int of int
    | Other of string

/// Tokenizer helper pattern for regexes
let (|Match|_|) pattern input =
    let m = Regex.Match (input, pattern)
    if m.Success then Some m.Value else None

// Hack alert
let unquote (s: string) =
    s.[1..(s.Length-2)]

let quote (s: string) =
    String.concat "" ["\""; s; "\""]

/// Convert string to a token based on a regex pattern
let toToken = function
    | Match @"^\n|^\r"                 s -> s, Token.NewLine
    | Match @"^,"                      s -> s, Token.Comma
    | Match @"^#"                      s -> s, Token.Comment
    | Match @"^\s+"                    s -> s, Token.WhiteSpace
    | Match  "^[a-zA-Z_\.][a-zA-Z0-9_\.]*" s -> s, Token.Str s
    | Match  "^\"[^\"]+\""             s -> s, Token.Str (unquote s)
    | Match  "^\[[^\]]+\]"             s -> s, Token.Str s
    | Match @"^-?\d+"                  s -> s, Token.Int (int s)
    | Match @"."                       s -> s, Token.Other (string s)
    | _ -> failwith "Invalid Token"

/// Convert string into a list of parsable tokens
let tokenize (s: string) =
    // Convert substrings to tokens, starting at position 'index' in the string.
    let rec tokenize' index (s: string) =
        if index = s.Length then []
        else
            let text, token = toToken (s.Substring index)
            token :: tokenize' (index + text.Length) s
    tokenize' 0 s
    // strip out the whitespace tokens; note that since new line characters are distinct, they will remain.
    |> List.choose (function (Token.WhiteSpace | Token.Comma) -> None | t -> Some t)


/// Megahack to convert tokens to ints. try to fix for proper Token.Int -> int conversion
let tokenToInt (tokens: Token list) =
    tokens |> List.map (string >> (fun x -> x.Substring 4) >> int)

/// Megahack to convert tokens to strings. try to fix for proper Token.Str -> string conversion
let tokenToStr (tokens: Token list) =
    tokens |> List.map (
        function
        | Token.NewLine    -> "\n"
        | Token.Comma      -> ","
        | Token.Comment    -> "#"
        | Token.WhiteSpace -> " "
        | Token.Str      s -> string s // might need more specific matching
        | Token.Int      i -> string i
        | Token.Other    o -> string o
)

/// Converts diagnostic entries into objects, including commented-out entries
let (|DiagLine|_|) = function
    | Token.Str modjuwel :: Token.Str name :: Token.Str variable :: Token.Str file :: Token.Str all :: Token.Str reduction :: Token.Str region :: Token.Int kind :: Token.NewLine :: tail ->
        Some ({ Name = name; Variable = variable; Files = [file]; Reduction = reduction; Region = region; Kind = kind; Module = modjuwel; Commented = false}, tail)
    | Token.Comment :: Token.Str modjuwel :: Token.Str name :: Token.Str variable :: Token.Str file :: Token.Str all :: Token.Str reduction :: Token.Str region :: Token.Int kind :: Token.NewLine :: tail ->
        Some ({ Name = name; Variable = variable; Files = [file]; Reduction = reduction; Region = region; Kind = kind; Module = modjuwel; Commented = true}, tail)
    | _ -> None

/// Converts diagnostic entries into objects, including commented-out entries
let (|FileLine|_|) = function
    | Token.Str name :: Token.Int freq :: Token.Str freqUnit :: Token.Int something :: Token.Str timeUnit :: Token.Str unlimitedDimension :: tail ->
        Some ({ Name = name; Frequency = freq; FrequencyUnit = freqUnit; TimeUnit = timeUnit; UnlimitedDimension = unlimitedDimension; Commented = false}, tail)
    | Token.Comment :: Token.Str name :: Token.Int freq :: Token.Str freqUnit :: Token.Int something :: Token.Str timeUnit :: Token.Str unlimitedDimension :: tail ->
        Some ({ Name = name; Frequency = freq; FrequencyUnit = freqUnit; TimeUnit = timeUnit; UnlimitedDimension = unlimitedDimension; Commented = true}, tail)
    | _ -> None

let lex (tokens: Token list) =
    let rec lex' (tokens: Token list) acc =
        let files, diags = acc
        match tokens with
        | DiagLine (d,[])   -> (files, diags @ [d])
        | DiagLine (d,tail) -> (files, diags @ [d]) |> lex' tail
        | FileLine (f,[])   -> (files @ [f], diags)
        | FileLine (f,tail) -> (files @ [f], diags) |> lex' tail
        | head :: tail -> (files, diags) |> lex' tail
        | [] -> (files, diags)

    let files, diags = lex' tokens ([],[])

    let inDifferentFile diag1 diag2 =
        (diag1.Name, diag1.Variable, diag1.Reduction, diag1.Region, diag1.Kind, diag1.Module) =
         (diag2.Name, diag2.Variable, diag2.Reduction, diag2.Region, diag2.Kind, diag2.Module) &&
        diag1.Files <> diag2.Files

    let rec reduceDiags (diags: Diagnostic list) =
        match diags with
        | [] -> []
        | head :: tail ->
            List.filter (inDifferentFile head) tail
            |> fun x -> if not x.IsEmpty then (head :: x) |> List.reduce (+) else head
            |> fun x -> x :: List.filter ((inDifferentFile head) >> not) tail

    reduceDiags diags



let table = File.ReadAllText "diags_in.txt"

let tokens = table |> tokenize
let b = tokens |> lex;;
