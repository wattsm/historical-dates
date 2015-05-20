///Contains the domain model for historical dates
module Model

open System

///Discriminated union for the two eras
type Era = 
    | BCE
    | CE

///Contains functions for calculating leap years
[<AutoOpen>]
module LeapYears = 

    ///This module uses the proleptic Gregorian calendar (http://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar) with "historical" leap years 
    ///(which is to say none prior to 46 BCE, erroneous triennial values between 46 BCE and 4 CE, Julian (1 every 4 years) between 4 CE and 1582 CE and then Gregorian values thereafter.

    ///Contains the starting years for various leap year calculations
    [<RequireQualifiedAccess>]
    module StartYears = 
        let [<Literal>] Triennial = -45
        let [<Literal>] Julian = 4
        let [<Literal>] Gregorian = 1582

    ///Contains helper functions for leap year calculations
    [<AutoOpen>]
    module private Helpers = 

        ///Gets the astronomical year, where 1BCE is 0
        ///http://en.wikipedia.org/wiki/Astronomical_year_numbering
        let getAstronomicalYear year = function
            | CE -> year
            | BCE -> (year - 1) * -1

        ///Active pattern used to decide on the leap year calculation to be used
        let (|None|Triennial|Julian|Gregorian|) astronomicalYear = 
            if (astronomicalYear >= StartYears.Gregorian) then

                //Our current calendar which uses the rule that a year is a leap year if it is divisible by and 4 and, if also 
                //divisible by 100, 400.
                Gregorian

            else if (astronomicalYear >= StartYears.Julian) then

                //The Gregorian calendar is introduced in 1582 CE, prior to that use the Julian calculation.
                //http://en.wikipedia.org/wiki/Gregorian_calendar
                Julian

            else if (astronomicalYear >= StartYears.Triennial) then

                //Mistakes were made, let's move on. Leap years between 46 BCE (astronomical year -45) and 8 BCE (-7)
                //were erroneously added every 3 years. A gap was then introduced to realign the calendar, with the standard
                //Julian calculation resuming in 4 CE.
                //http://www.wwu.edu/skywise/leapyear.html
                //http://en.wikipedia.org/wiki/Julian_calendar#Leap_year_error
                Triennial

            else

                //The concept of leap years was added in 46 BCE (astronomical year -45) so any year prior to that
                //will not have had a leap year.
                None

        ///True if a year is an erroneous triennial leap year in the period between 46 BCE and 4 CE
        let isTriennialLeapYear = 

            //Triennial leap year calculations vary, using the latest work (Bennett) 
            //http://en.wikipedia.org/wiki/Julian_calendar#Leap_year_error

            let triennialLeapYears = [ -43 .. 3 .. -7 ] //Every 3 years between 44 BCE and 8 BCE

            fun astronomicalYear ->
                List.exists ((=) astronomicalYear) triennialLeapYears

        ///True if a year is a calculated leap year according to the Julian method - e.g. divisible by 4 
        let isJulianLeapYear astronomicalYear = ((astronomicalYear % 4) = 0)

        ///True if a year is a calculated leap year according to the Gregorian method - e.g. divisible by 4, and 400 if also divisible by 100
        let isGregorianLeapYear astronomicalYear = 
            match ((astronomicalYear % 4), (astronomicalYear % 100)) with
            | (0, 0) -> (astronomicalYear % 400) = 0
            | (0, _) -> true                
            | _ -> false    

    ///True if a year is a leap year
    let isLeapYear era year = 

        //Calculate the astronomical date, where 1BC is "year zero". This allows us to work out leap years for all dates. 
        let astronomicalYear = getAstronomicalYear year era

        match astronomicalYear with
        | None -> false
        | Triennial -> isTriennialLeapYear astronomicalYear
        | Julian -> isJulianLeapYear astronomicalYear
        | Gregorian -> isGregorianLeapYear astronomicalYear        

    ///Active pattern used to decide if a year is a leap year
    let (|LeapYear|StandardYear|) (era, year) = 
        if (isLeapYear era year) then
            LeapYear
        else
            StandardYear

///Contains functions for working with months
[<RequireQualifiedAccess>]
module Month = 

    ///Get the number of days in a month
    let getDayCount = 

        let daysInMonth = [ 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31; ]

        fun era year month -> 
            match (era, year) with
            | LeapYear when (month = 2) -> 29
            | _ -> List.nth daysInMonth (month - 1)

///Contains functions for working with years
[<RequireQualifiedAccess>]
module Year = 

    ///Constants to remove magic numbers. Also, y'know, easy to update in case the number of days in a year changes...
    let [<Literal>] private daysInNormalYear = 365
    let [<Literal>] private daysInLeapYear = 366
        
    ///Get the number of days in a year
    let getDayCount era year = 
        match (era, year) with
        | LeapYear -> daysInLeapYear
        | _ -> daysInNormalYear

    ///Get day number within a year of a given date
    let getDayNumber era year month day = 
        [ 1 .. month ]
        |> Seq.sumBy (fun current ->
                if (current = month) then
                    day
                else
                    Month.getDayCount era year current
            )

    ///Get the number of days remaining in a year for a given date
    let getDaysRemaining era year month day = 

        let currentDay = getDayNumber era year month day
        let daysInYear = getDayCount era year

        (daysInYear - currentDay)

///Contains functions and types for working with "fuzzy" historical dates
[<RequireQualifiedAccess>]
module FuzzyDate = 

    ///Describes a basic fuzzy date
    type Data = {
        Day : Int32 option;
        Month : Int32 option;
        Year : Int32;
        Era : Era;
    }
    with
        static member Empty = 
            {
                Day = None;
                Month = None;
                Year = 1;
                Era = Era.CE;
            }

    ///Instance union type
    type Instance = FuzzyDate of Data

    ///Apply some function to a fuzzy date
    let private _apply f (FuzzyDate data) = f data

    ///Validate a given date's components
    let private _validate era year month day = 

        let month' = Option.unwrap 1 month
        let day' = Option.unwrap 1 day

        if (year < 1) then (Some "The year cannot be less than 1.")
        else if (month' < 1) || (month' > 12) then (Some "The month must be between 1 and 12.")
        else if (day' < 1) || (day' > (Month.getDayCount era year month')) then (Some "The day is not valid for the given month and year.")
        else None

    ///Validate a date's components and then create a success result if valid
    let private _create era year month day = 
        match (_validate era year month day) with
        | Some message -> Error message
        | _ -> 
            let date = 
                FuzzyDate (
                    { Data.Empty 
                        with 
                            Day = day; 
                            Month = month; 
                            Year = year; 
                            Era = era; 
                    }
                )
            in (Success date)

    ///Create a day, or fully specified date - e.g. 31 Jan 42BCE
    let createDay era year month day = _create era year (Some month) (Some day)

    ///Create a month - e.g. Jan 42BCE
    let createMonth era year month = _create era year (Some month) None

    ///Create a year - e.g. 42BCE
    let createYear era year = _create era year None None
        
    //Get a sort value, which is the number of days from 1st of Jan 1BCE, with that day being 0
    let getSortValue = 
        _apply (fun data ->

            let month = Option.unwrap 1 data.Month
            let day = Option.unwrap 1 data.Day

            match (data.Year, data.Era) with
            | (1, BCE) -> (Year.getDayNumber BCE data.Year month day) - 1 //1st Jan 1BCE is day 0
            | _ ->

                let dayCount = 
                    match data.Era with
                    | BCE -> 
                        //Remember that BCE dates are backwards - e.g. 31st Dec 3BCE moves to 1st Jan 2BCE. 
                        [ 2 .. data.Year ]
                        |> List.sumBy (fun year ->
                                if(year = data.Year) then
                                    (Year.getDaysRemaining BCE year month day) + 1 //Add one to nudge calculation from 31 Dec 2BCE to 1 Jan 1BCE
                                else
                                    Year.getDayCount BCE year
                            )
                    | _ ->
                        //The sum of all days to date in the CE era plus the number of days in 1 BCE
                        let dayCountCe = 
                            [ 1 .. data.Year ]
                            |> List.sumBy (fun year ->
                                    if (year = data.Year) then
                                        (Year.getDayNumber CE year month day) - 1 //Subtract one to switch to zero-based day number
                                    else
                                        Year.getDayCount CE year
                                )
                        in (dayCountCe + (Year.getDayCount BCE 1))

                let multiplier =
                    match data.Era with
                    | BCE -> -1
                    | _ -> 1

                dayCount * multiplier
        )
    
    ///Get a string representation of this date
    let toString  = 
        _apply (fun data -> 
            
            let day = Option.unwrap 1 data.Day
            let month = Option.unwrap 1 data.Month
            let date = DateTime (data.Year, month, day)

            let mask = 
                match (data.Day, data.Month) with
                | (Some _, _) -> "dd MMM yyy"
                | (_, Some _) -> "MMM yyy"
                | _ -> "yyy"

            let era = 
                match data.Era with
                | BCE -> "BCE"
                | _ -> "CE"
            
            let value = sprintf "%s %s" (date.ToString mask) era

            //NOTE DateTime's ToString does not seem to support full year formats with less than 3 digits - e.g 32 becomes 032 - so we manually
            //deal with this case using a regex. Eww.

            Regex.replace value "0([0-9]{2})" "$1"
        )

///Contains functions and types for historical event dates
[<RequireQualifiedAccess>]
module EventDate =

    ///Union describing various types of event date
    type Data = 
        | Specific of FuzzyDate.Instance
        | Before of FuzzyDate.Instance
        | After of FuzzyDate.Instance
        | Between of (FuzzyDate.Instance * FuzzyDate.Instance)

    ///Instance union type
    type Instance = EventDate of Data

    ///Apply some function to an event date
    let private _apply f (EventDate data) = f data

    ///Creates a single specific date - e.g. 18 Apr 1472CE
    let createSpecific date = EventDate (Specific date)

    ///Creates a "some time before" date - e.g. < 13BCE
    let createBefore date = EventDate (Before date)

    ///Creates a "some time after" date - e.g. > 13BCE
    let createAfter date = EventDate (After date)

    ///Creates a date range - e.g. 13BCE - 14 Jun 34CE
    let createBetween first last = 
    
        let firstValue = FuzzyDate.getSortValue first
        let secondValue = FuzzyDate.getSortValue last

        if (firstValue > secondValue) then
            Error "The second date cannot come before the first."
        else        
            Success (EventDate (Between (first, last)))

    ///Gets the value to be used as a comparison when sorting event dates
    let getSortValue = _apply (fun data ->
        let date, adjustment = 
            match data with
            | Specific date -> (date, 0.0M)                                 //Absolute date
            | Before date -> (date, -0.1M)                                  //Just before events on date
            | Between (date, _) | After date -> (date, 0.1M)                //Just after events on date
        in (decimal (FuzzyDate.getSortValue date)) + adjustment
    )

    ///Gets a string representation of this event date
    let toString = 
        _apply (function
            | Specific date -> FuzzyDate.toString date
            | After date -> sprintf "> %s" (FuzzyDate.toString date)
            | Before date -> sprintf "< %s" (FuzzyDate.toString date)
            | Between (first, last) -> sprintf "%s - %s" (FuzzyDate.toString first) (FuzzyDate.toString last)
        )