module ``Model facts``

open System
open Xunit
open Xunit.Extensions
open FsCheck
open FsCheck.Xunit
open Model

module ``LeapYears facts`` = 

    module ``isLeapYear facts`` =

        [<AutoOpen>]
        module Generators = 

            type PreGenerator = 
                static member TestCase () = 
                    Gen.choose (1, 1800)
                    |> Arb.fromGen
                    |> Arb.filter (fun x -> x > 46) //Leap years do not exist prior to 46 BC

            type GregorianGenerator = 
                static member TestCase () =
                    Gen.choose (1, 1800)
                    |> Arb.fromGen
                    |> Arb.filter (fun x -> (x >= 1582 && x <= 9999)) //Gregorian leap years are every 4 years ish

            type JulianGenerator = 
                static member TestCase () =
                    Gen.choose (1, 1800)
                    |> Arb.fromGen
                    |> Arb.filter (fun x -> (x >= 4 && x < 1582)) //Julian leap years are every 4 years

            type FixedGenerator = 
                static member TestCase() = 
                    Arb.fromGen (gen {

                        let! era = 
                            Gen.choose (1, 2)
                            |> Gen.map (fun x -> 
                                    if (x = 1) then
                                        BCE
                                    else
                                        CE
                                )

                        let! year = 
                            match era with
                            | CE -> Gen.choose (1, 4)
                            | _ -> Gen.choose (1, 46)

                        return (year, era)                            
                    })

        [<Property(Arbitrary = [| typeof<GregorianGenerator> |])>]
        let ``Leap years calculated correctly for Gregorian CE dates`` (year : Int32) = 
            (LeapYears.isLeapYear CE year) = (DateTime.IsLeapYear year)

        [<Property(Arbitrary = [| typeof<JulianGenerator> |])>]
        let ``Leap years calculated correctly for Julian CE dates`` (year : Int32) = 
            (LeapYears.isLeapYear CE year) = ((year % 4) = 0)

        [<Fact>]
        let ``Leap years only have to be divisible by 4 for Julian CE dates`` () = 
            Assert.True (LeapYears.isLeapYear CE 1100)

        [<Property(Arbitrary = [| typeof<PreGenerator> |])>]
        let ``Leap years calculated correctly for pre-Julian dates`` (year : Int32) = 
            not (LeapYears.isLeapYear BCE year)

        [<Property(Arbitrary = [| typeof<FixedGenerator> |])>]
        let ``Leap years calculated correctly for fixed Julian leap years`` (year : Int32, era : Era) = 
            let expected = 
                [ (44, BCE); (41, BCE); (38, BCE); (35, BCE); (32, BCE); (29, BCE); (26, BCE); (23, BCE); (20, BCE); (17, BCE); (14, BCE); (11, BCE); (8, BCE); (4, CE); ]
                |> List.exists ((=) (year, era))
            in (LeapYears.isLeapYear era year) = expected

module ``FuzzyDate facts`` =

    module ``getSortValue facts`` = 

        [<Theory>]
        [<InlineData ("1 Jan 1 BCE", 0)>]
        [<InlineData ("2 Jan 1 BCE", 1)>]
        [<InlineData ("1 Jan 1 CE", 365)>]
        [<InlineData ("31 Dec 2 BCE", -1)>]        
        [<InlineData ("1 Jan 2 BCE", -365)>]
        let ``Sort value is the number of days since 01/01/0001 BCE`` (input : String, expectedValue : Int32) =
            match (Parser.readFuzzyDate input) with
            | Success date ->

                let actualValue = FuzzyDate.getSortValue date
                
                Assert.Equal (expectedValue, actualValue)    

            | Error message -> invalidOp message

        [<Theory>]
        [<InlineData ("1 BCE")>]
        [<InlineData ("Jan 1 BCE")>]
        let ``Sort value uses default values for partial dates where appropriate`` (input : String) = 
            match (Parser.readFuzzyDate input) with
            | Success date ->

                let sortValue = FuzzyDate.getSortValue date

                Assert.Equal (0, sortValue)

            | Error message -> invalidOp message
            
                
        [<Theory>]
        [<InlineData ("1 Jan 5 CE", 1826)>] //4 CE is a leap year, so expected value is (5 x 365) + 1
        [<InlineData ("1 Jan 9 BCE", -2921)>] //8 BCE is a leap year, so expected value is (8 * 365) + 1
        let ``Sort value correctly includes leap days`` (input : String, expectedValue : Int32) = 
            match (Parser.readFuzzyDate input) with
            | Success date ->

                let sortValue = FuzzyDate.getSortValue date

                Assert.Equal (expectedValue, sortValue)

            | Error message -> invalidOp message

module ``EventDate facts`` =

    module ``getSortValue facts`` = 

        let private assertSortValue input expectedValue = 
            match (Parser.readEventDate input) with
            | Success date ->

                let actualValue = EventDate.getSortValue date
                
                Assert.Equal (expectedValue, actualValue)    

            | Error message -> invalidOp message

        [<Theory>]
        [<InlineData ("1 Jan 1 BCE", 0.0)>]
        [<InlineData ("2 Jan 1 BCE", 1.0)>]
        [<InlineData ("1 Jan 1 CE", 365.0)>]
        [<InlineData ("31 Dec 2 BCE", -1.0)>]        
        [<InlineData ("1 Jan 2 BCE", -365.0)>]
        let ``Sort value for specific dates is the number of days since 01/01/0001 BCE`` (input : String, expectedValue : double) =    
            assertSortValue input (decimal expectedValue)

        [<Theory>]
        [<InlineData ("< 1 Jan 1 BCE", -0.1)>]
        [<InlineData ("< 2 Jan 1 BCE", 0.9)>]
        [<InlineData ("< 1 Jan 1 CE", 364.9)>]
        [<InlineData ("< 31 Dec 2 BCE", -1.1)>]        
        [<InlineData ("< 1 Jan 2 BCE", -365.1)>]
        let ``Sort value for before dates is the number of days since 01/01/0001 BCE minus 0.1`` (input : String, expectedValue : double) =
            assertSortValue input (decimal expectedValue)

        [<Theory>]
        [<InlineData ("> 1 Jan 1 BCE", 0.1)>]
        [<InlineData ("> 2 Jan 1 BCE", 1.1)>]
        [<InlineData ("> 1 Jan 1 CE", 365.1)>]
        [<InlineData ("> 31 Dec 2 BCE", -0.9)>]        
        [<InlineData ("> 1 Jan 2 BCE", -364.9)>]
        let ``Sort value of after dates is the number of days since 01/01/0001 BCE plus 0.1`` (input : String, expectedValue : double) =
            assertSortValue input (decimal expectedValue)

        [<Theory>]
        [<InlineData ("1 Jan 1 BCE - 1 Jan 1000 CE", 0.1)>]
        [<InlineData ("2 Jan 1 BCE - 1 Jan 1000 CE", 1.1)>]
        [<InlineData ("1 Jan 1 CE - 1 Jan 1000 CE", 365.1)>]
        [<InlineData ("31 Dec 2 BCE - 1 Jan 1000 CE", -0.9)>]        
        [<InlineData ("1 Jan 2 BCE - 1 Jan 1000 CE", -364.9)>]
        let ``Sort value of between dates is the number of days since 01/01/0001 BCE using the start date plus 0.1`` (input : String, expectedValue : double) =
            assertSortValue input (decimal expectedValue)