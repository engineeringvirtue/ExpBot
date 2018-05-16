namespace ExpBot
open DSharpPlus
open System.Threading.Tasks
open System
open DSharpPlus.EventArgs

open ActionsUtility

module Initialize =
    let MakeNewClient token =
        let config = DiscordConfiguration ()
        config.set_Token token
        config.set_TokenType TokenType.Bot
        new DiscordClient (config)

    let Ready (client:DiscordClient) args = async {
        Console.WriteLine ("Bot started!")
    }

    let AddMessageCreated x (bot:DiscordClient) =
        bot.add_MessageCreated (fun args -> x args |> Async.Start; Task.CompletedTask); bot

    let AddMemberLeave x (bot:DiscordClient) =
        bot.add_GuildMemberRemoved (fun args -> x args |> Async.Start; Task.CompletedTask); bot

    let AddReady x (bot:DiscordClient) =
        bot.add_Ready (fun args -> x bot args |> Async.Start; Task.CompletedTask); bot

    let AddGuildAvaliable x (bot:DiscordClient) =
        bot.add_GuildAvailable (fun args -> x bot args |> Async.Start; Task.CompletedTask); bot

    let Connect (bot:DiscordClient) =
        bot.ConnectAsync () |> Async.AwaitTask |> Async.RunSynchronously
        bot

    let LoopForever (client:DiscordClient) =
        async {
            let mutable input = ""
            while true do
                input <- System.Console.ReadLine ()
                ()
        }

    let Log (msg:DebugLogMessageEventArgs) =
        Console.WriteLine (msg.Message)
        Task.CompletedTask
    let LogMessage (msg:MessageCreateEventArgs) =
        Console.WriteLine (msg.Author.Username.ToString()+" said "+msg.Message.Content)

    let AddLog (client:DiscordClient) =
        client.DebugLogger.LogMessageReceived.Add (fun msg -> Log msg |> ignore; ())
        client.add_ClientErrored (fun msg -> Console.WriteLine msg; Task.CompletedTask)
        client

    let StartCodeKey (config:ExpBot.Data.BotConfig) = async {
        let ranks = Data.MakeRanksExp config.RankIds
        do! MakeNewClient config.Token |> AddGuildAvaliable Utility.LogRoles |> AddGuildAvaliable (RestoreRoles config ranks)
            |> AddReady Ready |> AddLog
            |> fun x -> AddMessageCreated (Commands.ExpBotMessageCreated config ranks x) x
            |> AddMemberLeave
                (fun x ->
                    use conn = LimeBeanData.InitializeConn config.ConnString
                    conn |> LimeBeanMapping.ConfigureUser |> LimeBeanMapping.RemoveUser (int64 x.Member.Id) |> Async.Ignore)
            |> Connect |> LoopForever
    }