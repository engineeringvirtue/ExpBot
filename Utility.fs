namespace ExpBot
open DSharpPlus
open Chessie.ErrorHandling
open System
open System.Threading.Tasks
open DSharpPlus.Entities
open DSharpPlus.EventArgs
open System.Text.RegularExpressions

module Utility =
    let GetArrayIndex array (index:int) =
        try
            Some (Array.item index array)
        with
            | :? System.IndexOutOfRangeException -> None

    let GetGuild (client:DiscordClient) =
        client.Guilds |> Seq.toList |> List.head |> (fun x -> x.Value)

    let FindRoleByName (client:DiscordClient) name =
        let guild = client |> GetGuild
        guild.Roles |> Seq.tryFind (fun x -> x.Name=name)

    let LogRoles (client:DiscordClient) (x:GuildCreateEventArgs) = async {
        let guild = x.Guild
        guild.Roles |> Seq.toList |> List.iter (fun x -> printfn "Role named %s has ID %i" x.Name x.Id)
    }

    let GetGuildUser (usr:DiscordUser) =
        match usr with
            | :? DiscordMember as x ->
                Some x
            | _ -> None

    let (|RegexMatch|_|) pattern input =
       let m = Regex.Match(input,pattern)
       if (m.Success) then Some (m.Groups |> Seq.toList |> List.map (fun x -> x.Value)) else None

    let (|PassOrWarn|_|) = function
        | Pass y | Warn (y,_) -> Some y
        | _ -> None

    open Microsoft.FSharp.Reflection

    let asMap (recd:'T) =
      [ for p in FSharpType.GetRecordFields(typeof<'T>) ->
          p.Name, p.GetValue(recd) ]
      |> Map.ofSeq

    //http://fssnip.net/bj
    let levenshtein word1 word2 =
        let preprocess = fun (str : string) -> str.ToLower().ToCharArray()
        let chars1, chars2 = preprocess word1, preprocess word2
        let m, n = chars1.Length, chars2.Length
        let table : int[,] = Array2D.zeroCreate (m + 1) (n + 1)
        for i in 0..m do
            for j in 0..n do
                match i, j with
                | i, 0 -> table.[i, j] <- i
                | 0, j -> table.[i, j] <- j
                | _, _ ->
                    let delete = table.[i-1, j] + 1
                    let insert = table.[i, j-1] + 1
                    //cost of substitution is 2
                    let substitute = 
                        if chars1.[i - 1] = chars2.[j - 1] 
                            then table.[i-1, j-1] //same character
                            else table.[i-1, j-1] + 2
                    table.[i, j] <- List.min [delete; insert; substitute]
        let dist = table.[m, n]
        1.0 - (float dist/(Math.Max (m,n) |> float))

    // let ResultBindIgnore y x =
    //     match x with
    //         | Fail err -> Bad err
    //         | PassOrWarn okval -> (y okval)

    let ResultBindAsyncIgnore y x = async {
        match x with
            | Fail err -> ()
            | PassOrWarn okval -> do! y okval
    }

    let OptionToRes err = function
        | Some x -> ok x
        | None -> fail err

    let UnixTime secsoffset =
        let diff = DateTime.Now - DateTime(1970,01,01)
        diff.TotalSeconds-secsoffset |> int64

    let BoolToInt bool =
        match bool with
            | true -> 1.0
            | false -> 0.0

    let ToPercent float =
        (int (float*100.0))

    let Log (x:string) = Console.WriteLine (x)

    open FParsec
    let LiftFParsecRes = function
        | Success (res,_,_) -> ok res
        | Failure (err,_,_) -> Trial.fail err

module JSONConverter =
    open Newtonsoft.Json

    let toJson value =
        JsonConvert.SerializeObject(value)
    let ofJson json =
        JsonConvert.DeserializeObject<'T>(json)