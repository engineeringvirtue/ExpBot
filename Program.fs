// Learn more about F# at http://fsharp.org

namespace ExpBot

open System
open ExpBot.Data
open ExpBot
open System.IO
open System.Threading

module Program =

    [<EntryPoint>]
    let main argv =
        let config =
            if File.Exists "./Config.json" then
                JSONConverter.ofJson (File.ReadAllText "./Config.json")
            else
                let config = {ConnString="Server=127.0.0.1;Port=5432;Database=expbot;User Id=postgres;Password=postgres;";
                            Token="NDAzODIzNzg4ODI0NzIzNDU2.DUM5vg.J26MIj4ePAo6G-XHbU5aF7xTv7k";
                            BotChannels=[uint64 404124693709324288L]; Bots=[];
                            RankIds=[uint64 404228175053455370L; uint64 404228189146054668L; uint64 404228213360033812L; uint64 403828776028340224L; uint64 403828283902263296L; uint64 403828533316812801L; uint64 403828559019507714L; uint64 403828799298600961L; uint64 403828577436696586L; uint64 404174048290013184L; uint64 404172984887017483L; uint64 404174120176320532L; uint64 404174231526703104L; uint64 404227957645639691L];
                            AnnouncementChannel=uint64 404168591567749121L}
                File.WriteAllText ("./Config.json",JSONConverter.toJson config)
                config

        let init connstring = async {
            use conn = LimeBeanData.InitializeConn connstring
            let! sqltoexec = File.ReadAllTextAsync ("./Dependencies/SQL/init.sql") |> Async.AwaitTask
            return conn |> LimeBeanData.Exec sqltoexec [||]
        }

        let rec timer connstring = async {
            use conn = LimeBeanData.InitializeConn connstring
            do! conn |> LimeBeanMapping.PurgeOldMessages 86400.0 |> Async.Ignore
            do! Async.Sleep (3600*1000)
            return! timer connstring
            conn.Dispose ()
        }

        let {ConnString=connstring} = config

        printfn "Initialize brand new database? (y/n)"
        if Console.ReadLine() = "y" then
            init connstring |> Async.Ignore |> Async.RunSynchronously

        timer connstring |> Async.Start

        Initialize.StartCodeKey config |> Async.RunSynchronously
        0 // return an integer exit code
