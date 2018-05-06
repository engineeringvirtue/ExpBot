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

module Commands =
    //COMMANDS
    let CommandBase cmd = (MatchPrefix '-') >> bind (MatchCommand cmd)

    let statuscmd connstr ranks cmdinfo = CommandBase "status" >> ResultBindAsyncIgnore (statusfunc connstr ranks cmdinfo)
    let breakdowncmd connstr ranks cmdinfo = CommandBase "breakdown" >> ResultBindAsyncIgnore (breakdownfunc connstr ranks cmdinfo)

    let evs connstr ranks client msg = CmdsToEvent [statuscmd connstr ranks; breakdowncmd connstr ranks] {Message=msg; Client=client;}

    let ExpBotMessageCreated (config:BotConfig) ranks (client:DiscordClient) (messageargs:MessageCreateEventArgs) = async {
        let! msg = messageargs.Channel.GetMessageAsync (messageargs.Message.Id) |> Async.AwaitTask

        if messageargs.Author.Id = client.CurrentUser.Id || List.exists (fun x -> messageargs.Author.Id=x) config.Bots then
            return ()

        if List.exists (fun x -> messageargs.Channel.Id=x) config.BotChannels then
            evs config.ConnString ranks client msg
        else
            match GetGuildUser messageargs.Author with
                | Some user ->
                    if msg.Content <> "" then
                        MessageHandleContainer config ranks msg user |> Async.Start
                | _ -> ()
    }