namespace ExpBot

open Npgsql
open LimeBean

open LimeBeanData
open Data

open System
open Chessie.ErrorHandling

module LimeBeanMapping =
    let ConfigureUser conn =
        conn |> CustomKey "user" "userid"

    let UserToBean {UserId=id; Exp=Exp exp; Rank=rank} api =
        Dispense "user" api |> Put "userid" id |> Put "exp" exp |> Put "rank" rank
    let BeanToUser b =
        let getb x = Get x b
        {UserId=getb "userid"; Exp=getb "exp" |> Exp; Rank=getb "rank"}

    let MakeUser u api = async {
        let b = api |> UserToBean u
        api |> Store b
    }

    let GetOrMakeUser id conn = async {
        let defaultuser = {UserId=id; Exp=Exp 0.0; Rank=0 }
        let res = conn |> Find "user" "WHERE UserId={0}" [|id|]
        match res |> Array.tryHead with
            | Some x -> return x |> BeanToUser
            | None ->
                conn |> MakeUser defaultuser |> ignore
                return defaultuser
    }

    let PurgeOldMessages oldsecs conn = async {
        let diff = DateTime.Now - DateTime(1970,01,01)
        let secs = diff.TotalSeconds-oldsecs

        return conn |> Exec "DELETE FROM Message WHERE Time < {0}" [|ToObj secs|]
    }

    let MessageToBean {Message={Exp=Exp exp; Contents=contents; UserId=uid;}; MessageBreakdown={Spam=spam;Consistent=consistent;SamePerson=same}} time api =
        api |> Dispense "message" |> Put "exp" exp |> Put "contents" contents |> Put "userid" uid
            |> Put "spam" spam |> Put "consistent" consistent |> Put "sameperson" same |> Put "time" time

    let MakeMessage msg conn = async {
        let diff = DateTime.Now - DateTime(1970,01,01)
        let b = conn |> MessageToBean msg diff.TotalSeconds
        conn |> Store b
    }

    let GetLastMessagesMadeByUser userid conn = async {
        let arr = conn |> Find "message" "WHERE UserId={0}" [|userid|]
        return arr |> Array.toList
    }

    let GetLastMessageId conn = async {
        let ids = conn |> Find "message" "order by Id desc" [||]
        match Array.tryHead ids with
            | Some x -> return x |> Get "id"
            | None -> return -1
    }

    let GetMessage (id:int64) conn = async {
        let seq = conn |> Find "message" "WHERE Id={0}" [|id|]
        return seq |> Seq.tryHead
    }

    let UpdateUser newuser conn = async {
        return conn |> Store (conn |> UserToBean newuser)
    }

    let RemoveUser id conn = async {
        return conn |> Exec "DELETE FROM user WHERE UserId={0}" [|id|]
    }

    // let AddRole id conn = async {
    //     return 
    // }