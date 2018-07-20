namespace ExpBot

open Npgsql
open LimeBean

open LimeBeanData
open Data

open RoleData

open System
open Chessie.ErrorHandling

module LimeBeanMapping =
(*

   _   _ ____  _____ ____
  | | | / ___|| ____|  _ \
  | | | \___ \|  _| | |_) |
  | |_| |___) | |___|  _ <
   \___/|____/|_____|_| \_\


*)

    let ConfigureUser conn =
        conn |> CustomKey "user" "userid"

    let UserToBean {UserId=id; Exp=Exp exp; Rank=rank} api =
        Dispense "user" api |> Put "userid" id |> Put "exp" exp |> Put "rank" rank
    let BeanToUser b =
        let getb x = Get x b
        {UserId=getb "userid"; Exp=getb "exp" |> Exp; Rank=getb "rank"}

    let MakeUser u api = async {
        let b = api |> UserToBean u
        do! api |> Store b
    }

    let GetOrMakeUser id conn = async {
        let defaultuser = {UserId=id; Exp=Exp 0.0; Rank=0 }
        let! res = conn |> Find "user" "WHERE UserId={0}" [|id|]
        match res |> Array.tryHead with
            | Some x -> return x |> BeanToUser
            | None ->
                conn |> MakeUser defaultuser |> ignore
                return defaultuser
    }

(*

   __  __ _____ ____ ____    _    ____ _____ ____
  |  \/  | ____/ ___/ ___|  / \  / ___| ____/ ___|
  | |\/| |  _| \___ \___ \ / _ \| |  _|  _| \___ \
  | |  | | |___ ___) |__) / ___ \ |_| | |___ ___) |
  |_|  |_|_____|____/____/_/   \_\____|_____|____/


*)

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
        return! conn |> Store b
    }

    let GetLastMessagesMadeByUser (userid:int64) conn = async {
        let! arr = conn |> Find "message" "WHERE UserId={0}" [|userid|]
        return arr |> Array.toList
    }

    let GetLastMessageId conn = async {
        let! ids = conn |> Find "message" "order by Id desc" [||]
        match Array.tryHead ids with
            | Some x -> return x |> Get "id"
            | None -> return -1
    }

    let GetMessage (id:int64) conn = async {
        let! seq = conn |> Find "message" "WHERE Id={0}" [|id|]
        return seq |> Seq.tryHead
    }

    let UpdateUser newuser conn = async {
        return! conn |> Store (conn |> UserToBean newuser)
    }

    let RemoveUser (id:int64) conn = async {
        let! user = conn |> Load "user" id |> Async.ofAsyncResult
        user |> bind (fun x -> ok (conn |> Trash x)) |> ignore
    }

(*

____   ___  _     _____ ____
|  _ \ / _ \| |   | ____/ ___|
| |_) | | | | |   |  _| \___ \
|  _ <| |_| | |___| |___ ___) |
|_| \_\\___/|_____|_____|____/


*)

    let RoleToBean (role:GiveRole) conn =
        conn |> Dispense "role" |> Put "id" role

    let BeanToRole bean =
        bean |> Get "id" |> GiveRole

    let AddRole role conn = async {
        let bean = conn |> RoleToBean role
        return! conn |> Store bean
    }

    let RemoveRole id conn = asyncTrial {
        let! role = conn |> Load "role" id
        do! conn |> Trash role
        return ok ()
    }

    let ListRoles conn = async {
        let! roles = conn |> Find "role" "" [||]
        return roles |> Array.map BeanToRole |> Array.toList
    }