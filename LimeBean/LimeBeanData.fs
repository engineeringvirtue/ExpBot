namespace ExpBot

open Npgsql

open Chessie.ErrorHandling
open LimeBean

module LimeBeanData =
    let InitializeConn connstring =
        let api = new BeanApi(connstring,typeof<NpgsqlConnection>)
        api

    let CloseConn (conn:NpgsqlConnection) =
        conn.Close ()

    let ToObj x =
        x :> obj

    let Dispense x (api:BeanApi) =
        api.Dispense (x)

    let Store x (api:BeanApi) =
        api.Store (x) |> ignore |> Async.singleton

    let Key keytype (keyval:string) (api:BeanApi) =
        api.Key (keytype,keyval)
        api

    let CustomKey keytype (keyval:string) (api:BeanApi) =
        api.Key (keytype,keyval,false)
        api

    let Trash x (api:BeanApi) =
        api.Trash (x) |> ignore |> Async.singleton

    let Count btype sql param (api:BeanApi) =
        api.Count (btype,sql,param) |> Async.singleton

    let Load btype (id:obj) (api:BeanApi) =
        let bean = api.Load (btype,id)
        (if isNull bean then fail "Row not found" else ok bean) |> Async.singleton |> AR

    let Find btype exp param (api:BeanApi) =
        api.Find (btype,exp,param) |> Async.singleton

    let Get<'T> (name:string) (bean:Bean) =
        bean.Get<'T>(name)

    let (|Get|) name bean =
        bean |> Get name

    let Put key value (bean:Bean) =
        bean.Put (key,value)

    let Exec query param (api:BeanApi) =
        api.Exec (query,param) |> ignore |> Async.singleton