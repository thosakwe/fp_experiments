// Learn more about F# at http://fsharp.org

open System
open System.IO

type Account = {Name: string; Balance: float}

let serializeAccount (a: Account) =
    String.Format ("{0}={1}", a.Name, a.Balance)

let parseAccount (line: string) =
    let split = line.Split "="
    {Name = split.[0]; Balance = Double.Parse(split.[1])}

let parseAccounts (text: string) =
    let lines = text.Split "\n"
    Array.map parseAccount (text.Split "\n")

let loadAccounts =
    if File.Exists "accounts.txt" then
        parseAccounts (File.ReadAllText "accounts.txt")
    else
        Array.empty<Account>

[<EntryPoint>]
let main argv =
    let accounts = loadAccounts
    printf "Enter your name: "

    let name = Console.ReadLine ()
    let hasName (a: Account) = a.Name.Equals(name)

    let account =
        if Array.exists hasName accounts then
            Array.find hasName accounts
        else
            let a = {Name = name; Balance = 0.0}
            let result = (File.AppendText "accounts.txt", serializeAccount a)
            a

    Console.WriteLine (String.Format ("Hi, {0}! You have {1} dollars.", account.Name, account.Balance))
    0 // return an integer exit code
