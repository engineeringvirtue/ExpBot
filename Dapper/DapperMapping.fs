namespace ExpBot
open Npgsql
open Dapper

open DapperData
open Data
open System

module DatabaseTypes =
    type DBTempMessage = {Exp:Exp;Contents:string;UserId:int64;Spam:bool;Consistent:bool;SamePerson:bool;Time:int64;Id:int64}

open DatabaseTypes

module DapperMapping =
    let GetOrMakeUser id conn = async {
        let defaultuser = {UserId=id; Exp=Exp 0.0; Rank=0 }
        let! res = conn |> ParametrizedQuery<User> "SELECT * FROM users WHERE \"UserId\"=@UserId" defaultuser
        match res |> Seq.tryHead with
            | Some x -> return Ok x
            | None ->
                do! conn |> Execute "INSERT INTO users (\"UserId\",\"Exp\",\"Rank\") VALUES (@UserId,@Exp,@Rank)" defaultuser |> Async.Ignore
                return Ok defaultuser
    }

    let PurgeOldMessages oldsecs conn = async {
        let diff = DateTime.Now - DateTime(1970,01,01)
        let secs = diff.TotalSeconds-oldsecs
        return! conn |> Execute "DELETE FROM messages WHERE \"Time\" < @Today" (Map ["Today",ToObj secs])
    }

    let MessageToMap {Message={Exp=exp; Contents=contents; UserId=uid;}; MessageBreakdown={Spam=spam;Consistent=consistent;SamePerson=same}} =
         Map ["Exp",ToObj exp;"Contents",ToObj contents;"UserId",ToObj uid;"Spam",ToObj spam;"Consistent",ToObj consistent;"SamePerson",ToObj same]

    let MakeMessage msg conn =
        let diff = DateTime.Now - DateTime(1970,01,01)
        let map = MessageToMap msg |> Map.add "Time" (ToObj diff.TotalSeconds)
        conn |> Execute "INSERT INTO messages (\"Exp\",\"Contents\",\"UserId\",\"Spam\",\"Consistent\",\"SamePerson\",\"Time\") VALUES (@Exp,@Contents,@UserId,@Spam,@Consistent,@SamePerson,@Time)" map

    let GetLastMessagesMadeByUser userid conn = async {
        let! seq = conn |> MapParametrizedQuery<DBTempMessage> "SELECT * FROM messages WHERE \"UserId\"=@UserId" (Map ["UserId",ToObj userid])
        return seq |> Seq.toList
    }

    let GetLastMessageId conn = async {
        let! ids = conn |> Query "SELECT \"Id\" FROM messages order by \"Id\" desc"
        match Seq.tryHead ids with
            | Some x -> return x
            | None -> return -1
    }

    let GetMessage (id:int64) conn = async {
        let! seq = conn |> MapParametrizedQuery<DBTempMessage> "SELECT * FROM messages WHERE \"Id\"=@Id" (Map ["Id",ToObj id])
        return seq |> Seq.tryHead
    }

    let UpdateUser newuser conn = async {
        return! conn |> Execute "UPDATE users SET \"Exp\"=@Exp, \"Rank\"=@Rank WHERE \"UserId\"=@UserId" newuser
    }

    let RemoveUser id conn = async {
        return! conn |> Execute "DELETE FROM users WHERE \"UserId\"=@UserId" (Map ["UserId",id])
    }