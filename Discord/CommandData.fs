namespace ExpBot
open DSharpPlus
open DSharpPlus.Entities
open Chessie.ErrorHandling

open Data

open Utility
open DSharpPlus.EventArgs

open FParsec

module CommandData =
    type CmdInfo = {Message:DiscordMessage;Client:DiscordClient}
    and Cmd = (CmdInfo -> string -> Async<unit>)
    and Cmds = Cmd list

    let ArgumentMatch (str:string) =
        ok ([],str)

    let CmdsToEvent (cmds:Cmds) (cmdinfo:CmdInfo) =
        let {CmdInfo.Message=msg} = cmdinfo
        let itercmds (cmd:Cmd) =
            cmd cmdinfo msg.Content |> Async.Ignore |> Async.Start

            ()

        List.iter itercmds cmds
    let CmdBase prefix (str:string) (cmdstr:string) =
        cmdstr.StartsWith (prefix+str) |>
            function
                | true -> cmdstr.Substring (prefix.Length+str.Length) |> ok
                | false -> Trial.fail ()


    let CmdResult (x:Result<_,_>) =
        ok ""