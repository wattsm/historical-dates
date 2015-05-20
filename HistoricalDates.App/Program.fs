module Program

open System
open System.Diagnostics
open Model

type Event = {
    Date : EventDate.Instance;
    Title : String;
    Url : String option;
}
with

    override this.ToString () = 
        sprintf "%-20s %s" (EventDate.toString this.Date) this.Title

    static member Create title dateString url = 
        match (Parser.readEventDate dateString) with
        | Error message -> invalidOp message
        | Success date ->        
            {
                Date = date;
                Title = title;
                Url = url;
            }

[<RequireQualifiedAccess>]
module IndexedEvent = 

    let create index event = (index, event)

    let getRandomSortValue _ = Guid.NewGuid ()

    //Primary sort by calculated sort value, secondary sort by position in original list (e.g. if two items have identical 
    //sort values then assume they were in the correct chronological order in the original ist).
    let getChronologicalSortValue (index, event) = 
        let sortValue = EventDate.getSortValue event.Date
        in (sortValue, index) 

    let print (_, event) = printfn "%O" event

[<RequireQualifiedAccess>]
module Json = 

    open System.IO
    open System.Reflection
    open Newtonsoft.Json.Linq

    [<AutoOpen>]
    module private Helpers = 

        let getDataArray () =

            let assembly = Assembly.GetExecutingAssembly ()
            use stream = assembly.GetManifestResourceStream ("RomanBattles.json")
            use reader = new StreamReader (stream)

            JArray.Parse (reader.ReadToEnd ())

        let getArrayObjects (jarray : JArray) = 
            jarray.Children ()
            |> Seq.map (fun token -> token :?> JObject)

        let tryGetPropertyValue (jobject : JObject) path = 
            match (jobject.SelectToken path) with
            | :? JValue as jvalue -> Some (string jvalue.Value)
            | _ -> None

        let getPropertyValue jobject = 
            tryGetPropertyValue jobject
            >> Option.get

    let getEvents = 
        getDataArray
        >> getArrayObjects
        >> Seq.map (fun jobject -> 

                let date = getPropertyValue jobject "$['date']"
                let title = getPropertyValue jobject "$['title']"
                let url = tryGetPropertyValue jobject "$['url']"
        
                Event.Create title date url
            )

[<RequireQualifiedAccess>]
module Html = 

    open System.IO
    open System.Text
    open System.Xml

    [<AutoOpen>]
    module private Helpers = 

        let writeEvent (xml : XmlWriter) event =             
            xml.WriteStartElement "tr"

            xml.WriteStartElement "td"
            xml.WriteAttributeString ("style", "text-align: right;")
            xml.WriteString (EventDate.toString event.Date)
            xml.WriteEndElement ()

            xml.WriteStartElement "td"
            xml.WriteAttributeString ("style", "padding-left: 10px;")
            
            match event.Url with
            | Some url ->

                xml.WriteStartElement "a"
                xml.WriteAttributeString ("target", "_blank")
                xml.WriteAttributeString ("href", url)
                xml.WriteString event.Title
                xml.WriteEndElement ()

            | _ -> xml.WriteString event.Title

            xml.WriteEndElement ()

        let writeEvents (xml : XmlWriter) events = 

            xml.WriteStartElement "table"

            xml.WriteStartElement "thead"
            xml.WriteStartElement "tr"

            xml.WriteStartElement ("th")
            xml.WriteAttributeString ("style", "width: 150px; text-align: right;")
            xml.WriteString "Date"
            xml.WriteEndElement ()

            xml.WriteStartElement ("th")
            xml.WriteAttributeString ("style", "text-align: left; padding-left: 10px;")
            xml.WriteString "Description"
            xml.WriteEndElement ()

            xml.WriteStartElement "tbody"

            Seq.iter (writeEvent xml) events

            xml.WriteEndElement ()

            xml.WriteEndElement ()

        let getHtml events = 

            let xml = StringBuilder ()

            let settings = XmlWriterSettings ()
            settings.OmitXmlDeclaration <- true
            settings.Indent <- true

            use writer = XmlWriter.Create (xml, settings)

            writer.WriteStartElement "html"

            writer.WriteStartElement "head"
            writer.WriteElementString ("title", "Roman Battles")
            writer.WriteEndElement ()

            writer.WriteStartElement "body"

            writer.WriteElementString ("h1", "Roman Battles")

            writeEvents writer events

            writer.WriteEndElement ()

            writer.WriteEndElement ()

            writer.Flush ()

            xml.ToString ()

    let generate events = 

        let path = Path.Combine (AppDomain.CurrentDomain.BaseDirectory, "Battles.html")

        if (File.Exists path) then
            File.Delete path

        let html = getHtml events

        File.WriteAllText (path, html)

        path

[<EntryPoint>]
let main _ = 

    let fileName = 
        Json.getEvents ()
        |> Seq.mapi IndexedEvent.create
        |> Seq.sortBy IndexedEvent.getRandomSortValue
        |> Seq.sortBy IndexedEvent.getChronologicalSortValue
        |> Seq.map snd
        |> Html.generate

    Process.Start (fileName)
    |> ignore

    0

