namespace ExpBot
open Npgsql
open Dapper
open System.Dynamic
open System.Collections.Generic
open System

type OptionHandler<'T>() =
    inherit SqlMapper.TypeHandler<option<'T>>()

    override __.SetValue(param, value) =
        let valueOrNull =
            match value with
            | Some x -> box x
            | None -> null

        param.Value <- valueOrNull

    override __.Parse value =
        if isNull value || value = box DBNull.Value
        then None
        else Some (value :?> 'T)

type BoxHandler<'T>() =
    inherit SqlMapper.TypeHandler<'T>()
    override __.SetValue(param, value) =
        param.Value <- JSONConverter.toJson value

    override __.Parse value =
        let str = value :?> string
        str |> JSONConverter.ofJson

module DapperData =
    open Data

    let AddTypeHandlers () =
        SqlMapper.AddTypeHandler(BoxHandler<Exp>())

    let Execute (query:string) (param:obj) (connection:NpgsqlConnection) =
        AddTypeHandlers ()
        connection.ExecuteAsync(query, param) |> Async.AwaitTask

    let Query<'Result> (query:string) (connection:NpgsqlConnection) =
        AddTypeHandlers ()
        connection.QueryAsync<'Result>(query) |> Async.AwaitTask

    let ParametrizedQuery<'Result> (query:string) (param:obj) (connection:NpgsqlConnection) =
        AddTypeHandlers ()
        connection.QueryAsync<'Result>(query, param) |> Async.AwaitTask

    let MapParametrizedQuery<'Result> (query:string) (param : Map<string,_>) (connection:NpgsqlConnection) = async {
        let expando = ExpandoObject()
        let expandoDictionary = expando :> IDictionary<string,obj>
        for paramValue in param do
            expandoDictionary.Add(paramValue.Key, paramValue.Value)

        return! connection |> ParametrizedQuery<'Result> query expando
    }

    let MapExecute (query:string) (param : Map<string,_>) (connection:NpgsqlConnection) = async {
        let expando = ExpandoObject()
        let expandoDictionary = expando :> IDictionary<string,obj>
        for paramValue in param do
            expandoDictionary.Add(paramValue.Key, paramValue.Value :> obj)

        return! connection |> Execute query expando
    }

    let InitializeConn connstring =
        let conn = new NpgsqlConnection (connstring)
        conn.Open ()
        conn

    let CloseConn (conn:NpgsqlConnection) =
        conn.Close ()

    let ToObj x =
        x :> obj