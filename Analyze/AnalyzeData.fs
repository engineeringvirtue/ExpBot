namespace ExpBot

module Data =
    type Exp = Exp of float

    type Rank = Rank of (Exp*uint64 option)
    type User = {Exp:Exp; Rank:int; UserId:int64;}

    type Message = {Exp:Exp; Contents:string; UserId:int64;}
    type MessageBreakdown = {Spam:bool; Consistent:bool; SamePerson:bool;}
    type TempMessage = {Message:Message; MessageBreakdown:MessageBreakdown;}

    type FilterIntensity =
        | FilterIntensity of (float*(float -> float -> float))
        | NegateAll
        | NoEffect

    type Breakdown = {Spam:int; Consistent:int; Rank:Rank; SamePerson:int}
    type Status = {Exp:Exp; Breakdown:Breakdown}

    type BotConfig = {ConnString:string; Token:string; BotChannels:uint64 list; Bots:uint64 list; AnnouncementChannel: uint64; RankIds:uint64 list;}

    let MakeRankExp ranks id =
        match ranks with
            | [] -> Rank (Exp 0.0, id)
            | (Rank (Exp x,_))::_ ->
                let exp = (float x)*1.5+2000.0 |> round |> Exp
                Rank (exp, id)

    let MakeRanksExp ids =
        let newlist = None::(List.map Some ids)
        List.fold (fun state acc -> (MakeRankExp state acc)::state) [] newlist |> List.rev