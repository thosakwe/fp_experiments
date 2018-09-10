open List
open Printf
open Str
open String

type account = {name: string; balance: float}

exception AccountError of string

let emptyAccount =
    {name = ""; balance=0.0}

let accountIsValid a =
    a.name != "" && a.balance > 0.0

let accountToString account =
    account.name ^ "=" ^ (string_of_float account.balance)

(* Parse an account of the format <name>=<balance> *)
let parseAccount line =
    print_string ("Hm: " ^ line ^"!\n") ;

    if (String.trim line) == "" then
        emptyAccount
    else
        let split = String.split_on_char '=' line in

        if (List.length split) < 2 then
            emptyAccount
        else
            let name = List.nth split 0 in
            let balanceString = List.nth split 1 in
            let balance = float_of_string balanceString in
            {name = name; balance = balance}

(* Parse a list of accounts *)
let parseAccounts lines =
    List.map parseAccount lines

let rec read_line chan s =
    try
        let current = s ^ (input_line chan) in
        read_line chan current
    with End_of_file ->
        s

let read_file filename =
    try
        let chan = open_in filename in
        read_line chan ""
    with Sys_error(_) ->
        ""

let prompt msg =
    print_string (msg ^ " ");
    flush stdout;
    input_line stdin

let chooseAccount accounts originalText =
    if (List.length accounts) == 0 then
        let accountName = prompt "Enter name for new account:" in
        let newAccount = {name = accountName; balance = 0.0} in
        let newContents = originalText ^ "\n" ^ (accountToString newAccount) in
        let chan = open_out "accounts.txt" in
        fprintf chan "%s\n" newContents;
        close_out chan;
        newAccount
    else
        let accountName = prompt "Choose an account:" in
        let whereMatchesName = List.filter (fun a -> a.name == accountName) accounts in

        if (List.length whereMatchesName) == 0 then
            raise (AccountError "No matching account...")
        else
            List.nth whereMatchesName 0

let linesOf str =
    Str.split (Str.regexp "\n") str

let () =
    let contents = read_file "accounts.txt" in
    let allAccounts = parseAccounts (linesOf contents) in
    let validAccounts = filter accountIsValid allAccounts in
    try
        let account = chooseAccount validAccounts contents in
        print_string ((accountToString account) ^ "\n")
    with AccountError (msg) ->
        print_string (msg ^ "\n")