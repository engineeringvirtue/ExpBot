namespace ExpBot
open DSharpPlus
open DSharpPlus.Entities
open Chessie.ErrorHandling

open Data

open Utility
open DSharpPlus.EventArgs

module Commands =
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


    //COMMANDS
    let CommandBase cmd = (MatchPrefix '~') >> bind (MatchCommand cmd)

    let statusfunc connstr ranks (info:CmdInfo) str = async {
        use conn = LimeBeanData.InitializeConn connstr
        let author = info.Message.Author
        let! user = LimeBeanMapping.GetOrMakeUser (int64 author.Id) conn

        match user with
            | PassOrWarn {Data.User.Exp=Exp exp; Rank=rank} ->
                let (Rank (_,roleid)) = List.item rank ranks
                let nextrolename, expstring =
                    match List.tryItem (rank+1) ranks with
                        | Some (Rank (Exp nextrankexp,roleid)) ->
                            let role =
                                match roleid with
                                    | Some x ->
                                        let role = info.Message.Channel.Guild.GetRole (x)
                                        role.Name
                                    | None -> "None"
                            role, string (int exp)+"/"+string (int nextrankexp)+"."
                        | None -> "None", string (int exp)+"/infinity. Keep going! You'll get there!"

                let rolename,rolecolor =
                    match roleid with
                        | Some roleid ->
                            let role = info.Message.Channel.Guild.GetRole (roleid)
                            role.Name, role.Color
                        | None ->
                            "None", DiscordColor.DarkButNotBlack

                let embed = DiscordEmbedBuilder ()
                let embed = embed.AddField ("Exp", expstring)
                let embed = embed.AddField ("Role", rolename, true)
                let embed = embed.AddField ("Next Role", nextrolename, true)
                let embed = embed.WithColor (rolecolor)
                let embed = embed.WithTitle "Status"
                let embed = embed.WithAuthor (author.Username, author.AvatarUrl, author.AvatarUrl)
                let embed = embed.Build ()
                do! info.Message.RespondAsync (embed=embed) |> Async.AwaitTask |> Async.Ignore

                return ok ()
            | Fail err -> return Bad err
    }

    open LimeBeanData
    let breakdownfunc connstr ranks (info:CmdInfo) str = async {
        use conn = LimeBeanData.InitializeConn connstr
        let author = info.Message.Author
        let! msgs = conn |> LimeBeanMapping.GetLastMessagesMadeByUser (int64 author.Id)

        match msgs with
            | [] ->
                do! info.Message.RespondAsync ("You have not sent any messages yet!") |> Async.AwaitTask |> Async.Ignore
            | msgs ->
                let consistent = List.averageBy (Get "consistent" >> BoolToInt) msgs
                let spam = List.averageBy (Get "spam" >> BoolToInt) msgs
                let same = List.averageBy (Get "sameperson" >> BoolToInt) msgs

                let topercentstring float =
                    let p = float |> ToPercent
                    string p+"%"

                let embed = DiscordEmbedBuilder ()
                let embed = embed.AddField ("Consistent", consistent |> topercentstring, true)
                let embed = embed.AddField ("Spam", spam |> topercentstring, true)
                let embed = embed.AddField ("Continuations of your messages", same |> topercentstring, true)
                let embed = embed.WithColor DiscordColor.Blurple
                let embed = embed.WithTitle "Breakdown"
                let embed = embed.WithAuthor (author.Username, author.AvatarUrl, author.AvatarUrl)
                let embed = embed.Build ()
                do! info.Message.RespondAsync (embed=embed) |> Async.AwaitTask |> Async.Ignore

        return ok ()
    }

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
                    Analyze.MessageHandleContainer config ranks msg user |> Async.Start
                | _ -> ()
    }