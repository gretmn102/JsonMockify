module Program
open Newtonsoft.Json.Linq
open Argu

module Json =
    let des x =
        Newtonsoft.Json.Linq.JToken.Parse x

    let desf path =
        System.IO.File.ReadAllText path |> des

    let ser (x: JToken) =
        // Newtonsoft.Json.JsonConvert.SerializeObject(x, Newtonsoft.Json.Formatting.Indented)
        Newtonsoft.Json.JsonConvert.SerializeObject(x)

    let serf path jtoken =
        let res = ser jtoken
        System.IO.File.WriteAllText(path, res)

let mockify (root: JToken) =
    let rec loop (x: JToken) =
        match x.Type with
        | JTokenType.Array ->
            x :?> JArray
            |> Seq.iter loop

        | JTokenType.Object ->
            let x = x :?> JObject
            x.Properties()
            |> Seq.iter (fun x ->
                match x.Value.Type with
                | JTokenType.String ->
                    x.Value <- "lorem ipsum"
                | _ ->
                    loop x.Value
            )

        | JTokenType.String ->
            ()
        | JTokenType.Null
        | JTokenType.Boolean
        | JTokenType.Integer
        | JTokenType.Float
        | JTokenType.Date ->
            ()
        | _ ->
            ()

    loop root

type CliArguments =
    | [<ExactlyOnce>] Output of string
    | [<MainCommand; ExactlyOnce; Last>] Src_Path of string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Src_Path(_) -> "specify source path."
            | Output(_) -> "specify path of output."

[<EntryPoint>]
let main args =
    let argParser = ArgumentParser<CliArguments>(programName = "JsonMockify.exe")
    let res =
        try
            Ok (argParser.ParseCommandLine(args))
        with e ->
            Error e.Message

    match res with
    | Error errMsg ->
        printfn "%s" errMsg
        1
    | Ok res ->
        let srcPath = res.GetResult Src_Path
        let dstPath = res.GetResult Output
        let rawJson = Json.desf srcPath
        mockify rawJson
        Json.serf dstPath rawJson

        printfn "Done!"

        0
