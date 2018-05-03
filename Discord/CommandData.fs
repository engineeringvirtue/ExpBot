namespace ExpBot
open DSharpPlus
open DSharpPlus.Entities
open Chessie.ErrorHandling

open Data

open Utility
open DSharpPlus.EventArgs

module CommandData =
    type CmdInfo = {Message:DiscordMessage;Client:DiscordClient}
    and Cmd = (CmdInfo -> string -> Async<Result<unit,unit>>)
    and Cmds = Cmd list

    let MatchPrefix (prefix:char) (str:string) =
        match str.StartsWith (prefix) with
            | true -> ok str.[1..]
            | false -> fail ()

    let MatchCommand (command:string) (str:string) =
        match str.StartsWith (command) with
            | true -> ok str.[command.Length..]
            | false -> fail ()

    let ArgumentMatch (str:string) =
        ok ([],str)


    let MatchStringArg (seperator:string) (nextseperator:string) (argdata,str:string) =
        let matcher = seperator+"(.+?)"+nextseperator+"|"+seperator+"(.+)"
        let seperatormatcher = (seperator+"(.+)")
        match str with
            | RegexMatch seperatormatcher [x] when nextseperator="" ->
                ok (x::argdata, "")
            | RegexMatch matcher [x;_] when x<>"" ->
                ok (x::argdata,str.[x.Length+seperator.Length..])
            | RegexMatch matcher [_;x] when x<>"" ->
                ok (x::argdata,str.[x.Length+seperator.Length..])
            | _ -> fail ()


    let MatchOptional (matchfunc:string->string->string list*string->Result<string list*string,unit>) (seperator:string) (nextseperator:string) (x:string list*string) =
        let res = matchfunc seperator nextseperator x

        match res with
            | Fail _ -> ok x
            | PassOrWarn y -> ok y


    let CmdsToEvent (cmds:Cmd list) (cmdinfo:CmdInfo) =
        let {CmdInfo.Message=msg} = cmdinfo
        let itercmds (cmd:Cmd) =
            cmd cmdinfo msg.Content |> Async.Ignore |> Async.Start

            ()

        List.iter itercmds cmds


    let CmdResult (x:Result<_,_>) =
        ok ""