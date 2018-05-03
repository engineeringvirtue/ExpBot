namespace ExpBot

open DSharpPlus
open DSharpPlus.Entities
open DSharpPlus.EventArgs

open LimeBeanData
open LimeBeanMapping

open Data

module ActionsUtility =
    let RestoreRoles {ConnString=str} (ranks:Rank list) (client:DiscordClient) (x:GuildCreateEventArgs) = async {
        let guild = x.Guild

        let ranknums = ranks |> List.map (fun (Rank x) -> x)
                        |> List.mapi (fun i (x,y) -> (i,y))
                        |> List.choose (fun (i,y) -> match y with | Some x -> (i,x) |> Some | None -> None)
                        |> List.rev //so if the user has multiple roles, the one at the top gets the most attention

        let chooser (mem:DiscordMember) =
            let memroles = mem.Roles |> Seq.toList
            let rank =
                if not memroles.IsEmpty then
                    ranknums |> List.tryFind (fun (_,x) ->
                        match memroles |> List.tryFind (fun y -> x=y.Id) with
                            | Some _ -> true
                            | None -> false)
                else None

            async {
                match rank with
                    | Some (i,_) ->
                            use conn = InitializeConn str |> ConfigureUser
                            let! u = conn |> GetOrMakeUser (int64 mem.Id)
                            do! conn |> UpdateUser {u with Rank=i}
                    | _ -> ()
            }

        let! members = guild.GetAllMembersAsync () |> Async.AwaitTask
        do! members |> Seq.toList
                |> List.map chooser
                |> Async.Parallel |> Async.Ignore
    }