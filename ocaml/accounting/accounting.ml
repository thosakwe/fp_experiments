open List
open String

type account = {name: string; balance: float}

let emptyAccount () =
    {name = ""; balance=0.0}

let accountIsValid a =
    a.name != "" && a.balance > 0.0

let accountToString account =
    "Hey"

(* Parse an account of the format <name>=<balance> *)
let parseAccount (line) =
    if (String.trim line) = "" then
        emptyAccount ()
    else
        let split = String.split_on_char '=' line in

        if (List.length split) < 2 then
            emptyAccount ()
        else
            let name = List.nth split 0 in
            let balanceString = List.nth split 1 in
            let balance = float_of_string balanceString in
            {name = name; balance = balance}

(* Parse a list of accounts *)
let parseAccounts (lines) =
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

let readAccountFile =
    let contents = read_file "accounts.txt" in
    let allAccounts = parseAccounts (String.split_on_char '\n' contents) in
    let validAccounts = filter accountIsValid allAccounts in
    List.iter (fun a -> print_string (accountToString a)) validAccounts