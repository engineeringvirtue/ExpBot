namespace ExpBot
open DSharpPlus
open DSharpPlus.Entities
open Chessie.ErrorHandling

open Data

open Utility
open DSharpPlus.EventArgs

open CommandData
open ActionsExp
open ActionsRole

open FParsec

module Commands =
    let prefixbase = CmdBase "-"

    let statuscmd connstr ranks cmdinfo = prefixbase "status" >> ResultBindAsyncIgnore (statusfunc connstr ranks cmdinfo)
    let breakdowncmd connstr ranks cmdinfo = prefixbase "breakdown" >> ResultBindAsyncIgnore (breakdownfunc connstr ranks cmdinfo)
    let giverolecmd connstr cmdinfo = prefixbase "giverole" >> ResultBindAsyncIgnore (giverolefunc connstr cmdinfo)

    let evs connstr ranks client msg = CmdsToEvent [statuscmd connstr ranks; breakdowncmd connstr ranks; giverolecmd connstr] {Message=msg; Client=client;}

    let ExpBotMessageCreated (config:BotConfig) ranks (client:DiscordClient) (messageargs:MessageCreateEventArgs) = async {
        let! msg = messageargs.Channel.GetMessageAsync (messageargs.Message.Id) |> Async.AwaitTask

        if messageargs.Author.Id = client.CurrentUser.Id || List.exists (fun x -> messageargs.Author.Id=x) config.Bots then
            return ()

        if List.exists (fun x -> messageargs.Channel.Id=x) config.BotChannels || messageargs.Channel.IsPrivate then
            evs config.ConnString ranks client msg
        else
            match GetGuildUser messageargs.Author with
                | Some user ->
                    if msg.Content <> "" then
                        MessageHandleContainer config ranks msg user |> Async.Start
                | _ -> ()
    }