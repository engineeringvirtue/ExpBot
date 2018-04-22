namespace ExpBot

open Data
open DapperMapping
open Utility

module Filters =
    let ExtractExpFloat = (fun {DatabaseTypes.DBTempMessage.Exp=Exp exp} -> (float exp))

    let FilterIntensityCon y x = FilterIntensity (x,y)
    let FilterIntensitySub = FilterIntensityCon (-)
    let FilterIntensityAdd = FilterIntensityCon (+)
    let FilterIntensityMult = FilterIntensityCon (*)

    let CheckForRepetition conn {Exp=exp; Contents=contents; UserId=uid;} = async {
        let! msgid = conn |> GetLastMessageId
        let! msg = conn |> GetMessage (int64 msgid)

        match msg with
            | None -> return NoEffect
            | Some {Contents=contents2;} ->
                let percent = levenshtein contents contents2
                return (int percent)*70 |> float |> FilterIntensitySub
    }

    let CheckForSpam {Exp=exp; Contents=contents; UserId=uid;} =
        match contents with
            | RegexMatch "([A-Z]{2,})" _ -> 15 |> float |> FilterIntensitySub
            | _ -> NoEffect

    let CheckForReply conn {Exp=exp; Contents=contents; UserId=uid;} = async {
        let! msgid = conn |> GetLastMessageId
        let! msg = conn |> GetMessage (int64 msgid)

        let twohours = UnixTime (3600.0*2.0)

        match msg with
            | Some {UserId=uid2;Time=time} when uid=uid2 && time > twohours ->
                return FilterIntensitySub 35.0
            | Some {Time=time} when time > twohours -> return FilterIntensityAdd 20.0
            | _ -> return NoEffect
    }

    let CheckForLength {Exp=exp; Contents=contents; UserId=uid;} =
        let len = contents.Length-10

        if len > 10 then
            FilterIntensityAdd 10.0
        else FilterIntensityAdd (float len)

    let CheckForConsistency conn {Exp=exp; Contents=contents; UserId=uid;} = async {
        let! msgs = conn |> GetLastMessagesMadeByUser uid
        let lasthour = Utility.UnixTime 10800.0 |> int64
        let lasthourmsgs = List.filter (fun {DatabaseTypes.Time=time} -> time>lasthour) msgs

        let lasthoursum = match msgs with
                            | [] -> 0 |> float | _ -> List.sumBy ExtractExpFloat msgs

        if lasthoursum > 1200.0 then
            return NegateAll
        else
            let average = match msgs with
                            | [] -> 0 |> float | _ -> List.averageBy ExtractExpFloat msgs
            System.Console.WriteLine average
            return FilterIntensityMult ((average/80.0)+1.0)
    }