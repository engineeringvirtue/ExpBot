namespace ExpBot

open DSharpPlus
open DSharpPlus.Entities

open Utility
open LimeBeanData
open LimeBeanMapping

open ActionsUtility

open CommandData
open FParsec
open Chessie.ErrorHandling

module ActionsRole =
    let parserole (guild:DiscordGuild) (msg:DiscordMessage) rolefilter str =
        let parsedrole = run (spaces >>. manySatisfy (isLetter)) str |> LiftFParsecRes
        let mentionedrole =
            if msg.Channel.IsPrivate then None else
                msg.MentionedRoles |> Seq.tryHead

        let roles = guild.Roles |> Seq.toList |> List.filter (fun x -> List.contains x rolefilter)
        let handlerole (role:DiscordRole option) = role |> Option.map (fun x -> x.Id) |> OptionToRes "Role is not valid!"
        match parsedrole, mentionedrole with
            | _, Some x ->
                roles |> List.tryFind (fun y -> x.Id = y.Id) |> handlerole
            | PassOrWarn x, _ ->
                roles |> List.tryFind (fun y -> x.ToLower() = y.Name.ToLower()) |> handlerole
            | _ -> fail "Could not parse role!"

    let addrolefunc connstr (info:CmdInfo) str = async {
        let {Message=msg;Client=client;} = info
        use conn = InitializeConn connstr
        let! roles = conn |> ListRoles

        match parserole (GetGuild client) msg str with
            | Some x ->
                do! conn |> AddRole x
                ok ()
            | None -> fail ""
    }

    let listrolesfunc connstr (info:CmdInfo) str = async {
        let {Message=msg;Client=client;} = info
        use conn = InitializeConn connstr
        let! roles = conn |> ListRoles
        let guild = GetGuild client

        let embed =
            (roles |> List.fold (fun (embed:DiscordEmbedBuilder) x ->
                let rolename = guild.GetRole(x).Name
                embed.AddField (rolename, "", false)
            ) (DiscordEmbedBuilder()))
             .WithTitle("Roles")
             .WithThumbnailUrl(guild.IconUrl)
             .Build()

        return! msg.RespondAsync(embed=embed) |> Async.AwaitTask
    }

    let giverolefunc connstr (info:CmdInfo) str = async {
        let {Message=msg;Client=client;} = info
        use conn = InitializeConn connstr
        let! roles = conn |> ListRoles

        let parsedrole = run (spaces >>. manySatisfy (isLetter)) str |> LiftFParsecRes
        let mentionedrole =
            if msg.Channel.IsPrivate then None else
                msg.MentionedRoles |> Seq.tryHead

        let guild = GetGuild client

        let giverole role = async {
            let! user = guild.GetMemberAsync (msg.Author.Id) |> Async.AwaitTask
            return! user.GrantRoleAsync (role, "welp he wanted it") |> Async.AwaitTask
        }

        let! res = async {
            match parsedrole,mentionedrole with
                | PassOrWarn (x), _ ->
                    match roles |> List.tryPick (fun id ->
                            let role = guild.GetRole (id)
                            if role.Name.ToLower () = x.ToLower () then Some role else None) with

                        | Some x ->
                            do! giverole x
                            return ok ()
                        | None -> return fail "Could not find role!"

                | Fail (x),_ ->
                    return Bad x
                | _, Some x when roles |> List.exists (fun y -> y=x.Id) ->
                    do! giverole x
                    return ok ()
                | _,None ->
                    return fail "No roles found in command!"
                | _,_ ->
                    return fail "Could not find role!"
        }
        do! res |> HandleRes msg

        ()
    }