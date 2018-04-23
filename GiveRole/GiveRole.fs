namespace ExpBot

open DSharpPlus
open DSharpPlus.Entities

open RoleData

module GiveRole =
    let giverole rolename (user:DiscordMember) =
        let roles = user.Guild.Roles
        