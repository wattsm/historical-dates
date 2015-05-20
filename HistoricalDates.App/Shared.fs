///Contains useful shared functions
[<AutoOpen>]
module Shared

open System

///Contains functions for working with String
[<RequireQualifiedAccess>]
module String = 

    ///True if two strings are equal, ignoring case
    let equalci x y = String.Equals (x, y, StringComparison.OrdinalIgnoreCase)

///Contains functions for working with Option<T>
[<RequireQualifiedAccess>]
module Option = 

    ///"Unwrap" an option using a default value for None
    let unwrap default' = function
        | Some value -> value
        | _ -> default'

///Contains functions for working with regular expressions
[<RequireQualifiedAccess>]
module Regex = 

    open System.Text.RegularExpressions
    open System.Linq

    ///Active pattern used to match a regular expression and access the match groups if successful
    let (|Pattern|_|) pattern input = 

        let result = Regex.Match (input, pattern, RegexOptions.IgnoreCase)

        if (result.Success) then
            let groups = 
                result.Groups.Cast<Group> ()
                |> Seq.mapi (fun index group -> index, group.Value)
            in (Some groups)
        else
            None

    ///True if an input matches a regular expression
    let isMatch input pattern = Regex.IsMatch (input, pattern, RegexOptions.IgnoreCase)
            
    ///Gets a group by index (where 0 is the entire input string and 1 is the first bracketed clause)
    let getGroup index = Seq.find (fst >> ((=) index)) >> snd    

    ///Performs a regular expression search/replace
    let replace (input : string) (search : string) (replacement : string) = 
        Regex.Replace (input, search, replacement)

///A union used to desctibe success/failure outcomes
type Outcome<'T> = 
    | Success of 'T
    | Error of String

///Contains functions for working with the Outcome<T> type
module Outcome = 
    
    ///Apply a function to a success outcome
    let map f = function
        | Success value -> Success (f value)
        | Error message -> Error message