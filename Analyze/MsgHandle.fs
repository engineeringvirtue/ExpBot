namespace ExpBot
open Data
open Filters
open DapperMapping

open DSharpPlus
    open DSharpPlus.Entities

module Analyze =
    let ComputeFilterIntensities filters baseexp =
            List.fold (fun state intensity ->
                        match intensity with
                            | FilterIntensity (intensity, op) ->
                                op state intensity
                            | NoEffect -> state
                            | NegateAll -> 0.0) baseexp filters

    type DiscordEffect =
        | ChangeRole of uint64
        | Announcement of string

    let HandleMessage {ConnString=connstr;} ranks (dmsgdata:DiscordMessage) newmsg = async {
        use conn = DapperData.InitializeConn connstr

        let! user = GetOrMakeUser newmsg.UserId conn

        let! repeatintensity = newmsg |> CheckForRepetition conn
        let spamintensity = newmsg |> CheckForSpam

        let! replyintensity = newmsg |> CheckForReply conn
        let lengthintensity = newmsg |> CheckForLength
        let! consistencyintensity = newmsg |> CheckForConsistency conn

        let consistent = match consistencyintensity with | NegateAll -> true | _ -> false
        let spam =
            match spamintensity, repeatintensity, lengthintensity with
                | FilterIntensity (spam,_), _, _ when spam > 0.0 -> true
                | _, FilterIntensity (rep,_), _ when rep > 40.0 -> true
                | _, _, FilterIntensity (len,_) when len < 0.0 -> true
                | _ -> false
        let same = match replyintensity with | FilterIntensity (x,_) when x > 0.0 -> true | _ -> false

        Utility.Log ("Repeat: "+string repeatintensity+" Spam: "+string spamintensity+" Reply: "+string replyintensity+" Length: "+string lengthintensity+" Consistency: "+string consistencyintensity)
        let unprocexp = float 50 |> ComputeFilterIntensities [repeatintensity; spamintensity; replyintensity; lengthintensity; consistencyintensity]
        let exp = if unprocexp < float 0 then float 0 else unprocexp

        do! conn |> DapperMapping.MakeMessage {Message={newmsg with Exp=Exp exp}; MessageBreakdown={Spam=spam; Consistent=consistent; SamePerson=same}} |> Async.Ignore

        match user with
            | Ok x ->
                let {Rank=rank;Exp=Exp curexp} = x
                let newexp = curexp+exp
                let nextranki = rank+1
                let newuser, msg =
                    match List.tryItem (nextranki) ranks with
                        | Some (Rank (Exp nextrankexp,roleid)) when newexp >= nextrankexp ->
                            let remainder = newexp-nextrankexp
                            let msg = match roleid with
                                        | Some roleid ->
                                            let change = [ChangeRole roleid]
                                            if nextranki % 10 = 0 then
                                                let role = dmsgdata.Channel.Guild.GetRole (roleid)
                                                (dmsgdata.Author.Mention+" has reached "+role.Name+"!" |> Announcement)::change
                                            else change
                                        | None -> []
                            {x with Rank=rank+1; Exp=Exp remainder}, msg
                        | _ -> {x with Exp=Exp newexp}, []
                do! conn |> UpdateUser newuser |> Async.Ignore
                return Ok msg
            | Error _ -> return Error "idk wut happen i think a doggo died"
    }

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
            | Ok res when List.isEmpty res |> not ->
                do! List.map (doEffect user) res |> List.toSeq |> Async.Parallel |> Async.Ignore
                ()
            | _ -> ()
    }