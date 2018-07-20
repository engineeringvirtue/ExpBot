namespace ExpBot

open DSharpPlus.Entities
open Chessie.ErrorHandling

open Utility
open Data
open Analyze

open CommandData
open LimeBeanData
open LimeBeanMapping

module ActionsExp =
    let MessageHandleContainer config ranks (message:DiscordMessage) (user:DiscordMember) = async {
        let! res = {UserId=int64 user.Id; Contents=message.Content; Exp=Exp 0.0} |> HandleMessage config ranks message

        let doEffect (user:DiscordMember) ef = async {
            match ef with
                | ChangeRole newroleid ->
                    let role = user.Guild.GetRole (newroleid)
                    do! user.GrantRoleAsync (role,"EXP!") |> Async.AwaitTask
                | Announcement str ->
                    let chan = message.Channel.Guild.GetChannel (config.AnnouncementChannel)
                    do! chan.SendMessageAsync str |> Async.AwaitTask |> Async.Ignore
        }

        match res with
            | PassOrWarn res when List.isEmpty res |> not ->
                do! List.map (doEffect user) res |> List.toSeq |> Async.Parallel |> Async.Ignore
                ()
            | _ -> ()
    }

    let target info =
            match info.Message.MentionedUsers |> Seq.tryHead with
                | Some x -> x
                | None -> info.Message.Author

    let statusfunc connstr ranks (info:CmdInfo) str = async {
        use conn = InitializeConn connstr |> ConfigureUser
        let {Client=client; Message=msg} = info
        let target = target info

        let! user = GetOrMakeUser (target.Id |> int64) conn

        let {Data.User.Exp=Exp exp; Rank=rank} = user
        let (Rank (_,roleid)) = List.item rank ranks

        let guild = GetGuild client

        let nextrolename, expstring =
            match List.tryItem (rank+1) ranks with
                | Some (Rank (Exp nextrankexp,roleid)) ->
                    let role =
                        match roleid with
                            | Some x ->
                                let role = guild.GetRole (x)
                                role.Name
                            | None -> "None"
                    role, string (int exp)+"/"+string (int nextrankexp)+"."
                | None -> "None", string (int exp)+"/infinity. Keep going! You'll get there!"

        let rolename,rolecolor =
            match roleid with
                | Some roleid ->
                    let role = guild.GetRole (roleid)
                    role.Name, role.Color
                | None ->
                    "None", DiscordColor.DarkButNotBlack

        let embed = DiscordEmbedBuilder ()
        let embed = embed.AddField ("Exp", expstring)
        let embed = embed.AddField ("Role", rolename, true)
        let embed = embed.AddField ("Next Role", nextrolename, true)
        let embed = embed.WithColor (rolecolor)
        let embed = embed.WithTitle "Status"
        let embed = embed.WithAuthor (target.Username, target.AvatarUrl, target.AvatarUrl)
        let embed = embed.Build ()
        do! msg.RespondAsync (embed=embed) |> Async.AwaitTask |> Async.Ignore
    }

    let breakdownfunc connstr ranks (info:CmdInfo) str = async {
        use conn = InitializeConn connstr |> ConfigureUser
        let {Message=msg; Client=client;} = info
        let target = target info
        let! msgs = conn |> GetLastMessagesMadeByUser (int64 target.Id)

        match msgs with
            | [] ->
                do! msg.RespondAsync ("You have not sent any messages yet!") |> Async.AwaitTask |> Async.Ignore
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
                let embed = embed.WithAuthor (target.Username, target.AvatarUrl, target.AvatarUrl)
                let embed = embed.Build ()
                do! msg.RespondAsync (embed=embed) |> Async.AwaitTask |> Async.Ignore
    }