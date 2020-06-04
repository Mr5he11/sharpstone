(*
 * Sharpstone: a tiny card game simulator
 *
 * Written as project exam template for Computer Science, Laboratorio di Programmazione
 * Freely adapted from Heartstone (TM) by Blizzard Entertainment, Inc.
 *
 * (C) 2016 Alvise Spanò @ DAIS, Università Ca' Foscari, Venezia
 *)

module LabProg2016.Sharpstone

#if INTERACTIVE
#r "System.Runtime.Serialization.dll"
#endif

open System
open System.IO
open System.Runtime.Serialization
open System.Text

// globals
//

let rand = new Random (23)    // remove seed argument for making randomness indeterministic

/// Generate a random integer within the interval (a, b) inclusively.
let rnd_int a b = rand.Next (a, b + 1) 


// type definitions
//

/// Defines the card type.
[< DataContract; StructuralEquality; NoComparison >]
type card = {
    [< field: DataMember(Name = "id") >] id : string
    [< field: DataMember(Name = "name") >] name : string
    [< field: DataMember(Name = "cost") >] cost : int
    [< field: DataMember(Name = "type") >] typee : string
    [< field: DataMember(Name = "attack") >] attack : int
    [< field: DataMember(Name = "health") >] mutable health : int
}
with
    override c.ToString () = sprintf "%s [Id:%s  Atk:%d  HP:%d]" c.name c.id c.attack c.health 

/// Deck type alias.
type deck = card list

/// Defined the player type.
[< StructuralEquality; NoComparison >]
type player = {
    name : string
    mutable life : int
    mutable deck : deck
}
with
    override p.ToString () = sprintf "%s [Life:%d  Deck:%d]" p.name p.life p.deck.Length


// JSON stuff
//

/// Convert a JSON string into a typed value.
let unjson<'t> (input : string) : 't =  
    use ms = new MemoryStream(ASCIIEncoding.Default.GetBytes(input)) 
    let obj = (new Json.DataContractJsonSerializer(typeof<'t>)).ReadObject(ms) 
    obj :?> 't

/// Parse a JSON deck given the filename.
let parse_deck (filename : string) = 
    use fstr = File.OpenRead filename
    use rd = new StreamReader (fstr)
    printfn "Parsing JSON file \"%s\"..." fstr.Name
    rd.ReadToEnd () |> unjson<card[]> |> Array.toList


// printers
//

/// Prints the turn number header. Call this function at the beginning of each turn.
let print_turn_begin (turn : int) = printfn "\nTurn %d:" turn

/// Prints the status of the 2 players. Call this function at the end of each turn.
let print_turn_end (p1 : player, p2 : player) = printfn "\t%O\n\t%O" p1 p2

/// Prints the information of 2 cards fighting. Call this function at each turn when both players have a card.
let print_turn_2cards (c1 : card, c2 : card) = printfn "%O VS %O" c1 c2

/// Prints the information of 1 card fighting against a player with no cards. Call this function at each turn when only 1 players have a card.
let print_turn_1card (p : player, c : card) = printfn "* %O VS player %O" c p

/// Prints the information of 2 players when both have no cards. Call this function at each turn no cards have been drawn.
let print_turn_no_cards (p1 : player, p2 : player) = printfn "* Both %O and %O have no cards" p1 p2

/// Prints the information of a dead cards. Call this function when a card dies.
let print_card_death (c : card) = printfn "+ %O died (%d overkill)" { c with health = 0 } -c.health

/// Pretty print a player.
let pretty_player p = p.ToString ()


// combat mechanics
//
(*--- MECCANICHE DI COMBATTIMENTO - inizio parte implementata --- *)

//filtriamo il deck dalle carte non minion
let rec filter_deck (D : deck) : deck =
    match D with 
        [] -> []
        | x::xs -> 
            if (x.typee <> "MINION") then filter_deck xs
            else x::(filter_deck xs)

//carta nulla 
let carta_nulla : card = {id = "0000000"; name = "NULL"; cost = 0; typee = "MINION"; attack = 0; health = -1}

//funzione per rimuovere la carta dal mazzo
let rimuovi_carta (P : player) (id : string) : deck =
    let D : deck = P.deck in
        let rec aux (D : deck) (id : string) =
            match D with 
                [] -> []
                | x::xs -> if (x.id = id) then 
                                                print_card_death x
                                                aux xs "0000000"
                           else x::(aux xs id)
        in aux D id

//funzione draw_card che dato un giocatore e il costo in mana mi restituisce una lista delle carte possibilmente giocabili
let get_card_list (mana : int) (P : player) : card list =
        let rec aux (D : deck) (punti : float) (scelta : card list) =
            match D with
                [] -> scelta
                | x::xs -> 
                    let punteggio : float = float (x.attack) / float (x.health) in
                    if (x.cost <= mana && punteggio >= punti) then 
                        if (punteggio = punti) then aux xs punteggio (x::scelta)
                        else aux xs punteggio [x] 
                    else aux xs punti scelta
        in aux P.deck 0.0 [] 

//funzione che seleziona in maniera casuale una delle carte giocabili
let rec draw_card (mana : int) (P : player) : card =
    let c = get_card_list mana P in
    let n : int = rand.Next (0 , c.Length) in
    let rec aux (lst : card list) (r : int) =
        match lst with
            [] -> carta_nulla
            | x::xs -> 
                if (r = 0) then x
                else aux xs (r - 1)
    in aux c n   

//funzione pricipale da implementare
let fight deck1 deck2 =
   //inizializziamo deck_p1 e deck_p2
   let mutable deck_p1 : int = 0
   let mutable deck_p2 : int = 0
   //ed i futuri deck dei due giocatori
   let mutable deck_player_1 : deck = []
   let mutable deck_player_2 : deck = []

   //chiediamo al giocatore 1 con che deck vuole giocare, con controllo dell'input
   while ((deck_p1 < 1)||(deck_p1 > 3)) do
       Console.Write("\nDammi il numero di deck con cui vuole giocare il giocatore 1 (1, 2 o 3) : ")
       deck_p1 <- int(Console.ReadLine())
   while ((deck_p2 < 1)||(deck_p2 > 3)) do
       Console.Write("Dammi il numero di deck con cui vuole giocare il giocatore 2 (1, 2 o 3) : ")
       deck_p2 <- int(Console.ReadLine())
   //creiamo i due deck

   //DECK DEL PLAYER 1
   if (deck_p1 = 1) then deck_player_1 <- filter_deck (parse_deck "deck1.txt")
   else if (deck_p1 = 2) then deck_player_1 <- filter_deck (parse_deck "deck2.txt")
   else deck_player_1 <- filter_deck (parse_deck "deck3.txt")

   //DECK DEL PLAYER 2
   if (deck_p2 = 1) then deck_player_2 <- filter_deck (parse_deck "deck1.txt")
   else if (deck_p2 = 2) then deck_player_2 <- filter_deck (parse_deck "deck2.txt")
   else deck_player_2 <- filter_deck (parse_deck "deck3.txt")

   // costruiamo i due player filtrando il mazzo in modo che ci siano solo Minion
   let p1 = { name = "P1"; life = 30; deck = deck_player_1 }
   let p2 = { name = "P2"; life = 30; deck = deck_player_2 } 
   let mutable turn = 1  //contatore di turno
   let mutable quit = false //per uscire eventualmente dal ciclo
   printfn "\nINITIAL CONDITIONS : \nplayer1 = %s\nplayer2 = %s \n" (pretty_player p1) (pretty_player p2)
   while not quit && p1.life > 0 && p2.life > 0 do
        print_turn_begin turn      // stampiamo il turno
        let mana = if turn > 10 then 10 else turn
        // ora estraiamo le carte con draw_card, che prende la prima carta che può essere giocata
        let c1 = draw_card mana p1  
        let c2 = draw_card mana p2
        // ed in base all'estrazione facciamo qualcosa
        match c1, c2 with
        // se entrambi i giocatori hanno carte giocabili si gioca
        | a,b when (a.id <> "0000000" && b.id <> "0000000") -> 
                                                            //stampo condizioni di gioco
                                                            print_turn_2cards (c1 , c2) 
                                                            //modifico salute carte interessate
                                                            c1.health <- c1.health - c2.attack
                                                            c2.health <- c2.health - c1.attack
                                                            //se la salute è negativa, allora elimino la carta dal deck e sottraggo il valore assoluto alla vita del giocatore
                                                            if (c1.health <= 0) then 
                                                                                    p1.deck <- rimuovi_carta p1 c1.id
                                                                                    p1.life <- (p1.life + c1.health)                                                                                    
                                                            if (c2.health <= 0) then 
                                                                                    p2.deck <- rimuovi_carta p2 c2.id
                                                                                    p2.life <- (p2.life + c2.health)
                                                            //nel caso le due carte giocate avessero attacco 0, termino il ciclo, altrimenti loop infinito
                                                            if (c1.attack = 0 && c2.attack = 0) then quit <- true                                                                                    
        // se uno dei due giocatori non ha carte da estrarre facciamo uno scontro con esito
        | a,b when (a.id = "0000000" && b.id <> "0000000") -> 
                                                            print_turn_1card (p2 , c2)
                                                            p1.life <- p1.life - c2.attack
        | a,b when (a.id <> "0000000" && b.id = "0000000") -> 
                                                            print_turn_1card (p1 , c1)
                                                            p2.life <- p2.life - c1.attack
        // se nessuno dei due giocatori ha più carte assegnamo 'quit <- true' per uscire immediatamente dal ciclo
        | a,b when (a.id = "0000000" && b.id = "0000000") -> 
                                                            print_turn_no_cards (p1 , p2)
                                                            quit <- true
        // se nessuna delle opzioni precedenti si è verificata, c'è qualcosa che non va...
        | _ -> failwith "Errore"
        
        // stampiamo lo stato dei player
        print_turn_end (p1 , p2)
        // prima di ripetere il ciclo incrementiamo il turno con un assegnamento
        turn <- turn + 1

   // a partita conclusa stampiamo l'esito
   if p1.life = p2.life then printfn "\nTie"
   elif p1.life > p2.life then printfn "\nP1 wins"
   else printfn "\nP2 wins"
   // e ritorniamo lo stato dei due player e l'ultimo turno giocato
   p1, p2, turn - 1

(*--- FINE COMBATTIMENTO - fine parte implementata ---*)



// main code
//

[< EntryPoint >]
let main argv =
    let code =
        try
            if argv.Length <> 2 then
                printfn "Usage: Sharpstone <DECK1> <DECK2>"
                0
            else
                let p filename = parse_deck filename    // function for parsing a JSON file defining a deck as a list of cards
                let d1 = p argv.[0]                     // parse the first argument of the executable (DECK1)
                let d2 = p argv.[1]                     // parse the second argument of the executable (DECK2)
                let p1, p2, turn as r = fight d1 d2
                // print final result
                printfn "\nResult:\n\t%d Turns\n\t%O\n\t%O\n\tHash: %X" turn p1 p2 (r.GetHashCode ())
                0

        with e -> printfn "Uncaught exception: %O" e; 1

    #if DEBUG
    printfn "\n\nPress any key to exit..."
    Console.ReadKey () |> ignore
    #endif
    code