///Contains functions for parsing dates from strings
[<RequireQualifiedAccess>]
module Parser

open System
open Model

///Contains regular expressions used by this module
[<RequireQualifiedAccess>]
module private Patterns = 

    let [<Literal>] year = "^([1-9]{1}[0-9]*) (BCE|CE)$"
    let [<Literal>] month = "^([a-z]{3}) ([1-9]{1}[0-9]*) (BCE|CE)$"
    let [<Literal>] day = "^([1-9]{1}[0-9]{0,1}) ([a-z]{3}) ([1-9]{1}[0-9]*) (BCE|CE)$"
    let [<Literal>] before = "^< (.+)$"
    let [<Literal>] after = "^> (.+)$"
    let [<Literal>] between = "^(.+) - (.+)$"

///True if an input may represent a fuzzy date
let mayBeFuzzyDate input = 
    [ Patterns.year; Patterns.month; Patterns.day; ]
    |> List.exists (Regex.isMatch input)

///Attempts to parse a fuzzy date from a string
let readFuzzyDate = 

    let getEra input = 
        if (String.equalci input "BCE") then
            BCE
        else
            CE

    let tryGetMonth =

        let shortNames = 
            [ 1 .. 12 ]
            |> List.map (fun month -> 
                    let date = DateTime (1, month, 1)
                    in (month, date.ToString ("MMM"))
                )

        fun shortName ->
            shortNames
            |> List.tryFind (snd >> (String.equalci shortName))
            |> Option.map fst            

    fun input -> 
        match input with
        | Regex.Pattern Patterns.year groups -> 
            FuzzyDate.createYear 
            <| (getEra (Regex.getGroup 2 groups))
            <| (Int32.Parse (Regex.getGroup 1 groups))             

        | Regex.Pattern Patterns.month groups -> 
            match (tryGetMonth (Regex.getGroup 1 groups)) with
            | Some month -> 
                FuzzyDate.createMonth   
                <| (getEra (Regex.getGroup 3 groups))
                <| (Int32.Parse (Regex.getGroup 2 groups))
                <| month                
            | _ -> Error "The input contained an invalid month."

        | Regex.Pattern Patterns.day groups ->
            match (tryGetMonth (Regex.getGroup 2 groups)) with
            | Some month ->
                FuzzyDate.createDay
                <| (getEra (Regex.getGroup 4 groups))
                <| (Int32.Parse (Regex.getGroup 3 groups))
                <| month
                <| (Int32.Parse (Regex.getGroup 1 groups))
                
            | _ -> Error "The input contained an invalid month."

        | _ -> Error "The input does not appear to contain a date."

///True if an input may represent an event date
let mayBeEventDate = function
    | Regex.Pattern Patterns.before groups | Regex.Pattern Patterns.after groups -> mayBeFuzzyDate (Regex.getGroup 1 groups) 
    | Regex.Pattern Patterns.between groups -> (mayBeFuzzyDate (Regex.getGroup 1 groups)) && (mayBeFuzzyDate (Regex.getGroup 2 groups))
    | input -> mayBeFuzzyDate input

///Attempts to parse an event date from a string
let readEventDate = function
    | Regex.Pattern Patterns.before groups ->         
        groups
        |> Regex.getGroup 1 
        |> readFuzzyDate
        |> Outcome.map EventDate.createBefore

    | Regex.Pattern Patterns.after groups ->
        groups
        |> Regex.getGroup 1
        |> readFuzzyDate
        |> Outcome.map EventDate.createAfter

    | Regex.Pattern Patterns.between groups ->
        
        let first = readFuzzyDate (Regex.getGroup 1 groups)
        let last = readFuzzyDate (Regex.getGroup 2 groups)

        match (first, last) with
        | (Error message, _) -> Error message
        | (_, Error message) -> Error message
        | (Success firstDate, Success lastDate) -> EventDate.createBetween firstDate lastDate

    | input ->
        readFuzzyDate input
        |> Outcome.map EventDate.createSpecific        
