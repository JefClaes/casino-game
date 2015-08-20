namespace Casino.Game

open System
open Casino.Game.Domain

module Program =

    let systemColor box =
        match box with
        | Yellow -> ConsoleColor.Yellow
        | Green -> ConsoleColor.Green
        | Red -> ConsoleColor.Red
        | Blue -> ConsoleColor.Blue
        | White -> ConsoleColor.White
        | Purple -> ConsoleColor.Magenta

    let drawBox box hit =             
        Console.ForegroundColor <- systemColor <| box             
            
        printf "[%s] " <| match hit with | true -> "X" | false -> " "

        Console.ResetColor()                    

    let youLost () = printfn "Unlucky, you lost."
    let youWon factor = printfn "You won your stake x %A" factor

    let render spinResult =
        printfn ""

        boxes 
        |> Seq.iter (fun b -> 
            let hit = (b = spinResult.Box)
            drawBox b hit)              
            
        [1 .. 2] 
        |> Seq.iter (fun _ -> printfn " ")

        match spinResult.Value with
        | Win factor -> 
            [1 .. 3] 
            |> Seq.iter(fun _ -> youWon factor)
        | Lose -> youLost()

    let rec start (shouldContinue : unit -> bool) =
        moreRealisticSpin() |> render

        if shouldContinue() then 
            start shouldContinue

    let drawEmptyBox box =         
        Console.ForegroundColor <- systemColor box
            
        printf "[ ] "

        Console.ResetColor()

    let pct x y = float x / float y * float 100        

    let distribution times (spin:unit->SpinResult) =    
        [ 1 .. times ] 
        |> Seq.map (fun _ -> spin()) 
        |> Seq.countBy (fun result -> result.Box)
        |> Seq.map (fun (k, v) -> (k, pct v times))        
        
    let printDistribution desc distribution =
        printfn ""
        printfn "Distribution: %s" desc
        printfn ""

        for element in distribution do
            let box, percentage = element
            Console.ForegroundColor <- systemColor box               
            printfn "%A = %A%%" box percentage
            Console.ResetColor()    
            
    let printHouseEdge desc houseEdge =
        printfn ""
        printfn "HouseEdge: %s" desc
        printfn ""
        printfn "%A" houseEdge

    let forceHouseEdge times (spin:unit->SpinResult) =      
        let stake = 1M
        let bets = decimal times * stake
        let wins =         
            [ 1 .. times ]
            |> Seq.map (fun _ -> spin())
            |> Seq.choose (fun x -> 
                match x.Value with 
                | Win factor -> Some factor
                | _ -> None)
            |> Seq.sumBy (fun f -> stake * f)
        
        bets / wins

    let houseEdge oddsOfWinning winnings oddsOfLosing stake =            
        let x : decimal = oddsOfWinning * winnings - oddsOfLosing * stake
        Math.Round(x, 2)

    [<EntryPoint>]
    let main argv = 
        printfn ""  

        let times = 100000
      
        pureSpin 
        |> forceHouseEdge times 
        |> printHouseEdge "Pure spin"

        moreRealisticSpin 
        |> forceHouseEdge times 
        |> printHouseEdge "More realistic spin"
        
        pureSpin 
        |> distribution times 
        |> printDistribution "Pure spin"
        
        moreRealisticSpin 
        |> distribution times 
        |> printDistribution "More Realistic spin"
      
//        start (fun () ->
//            Console.WriteLine()
//            Console.WriteLine("Go again?")            
//            Console.ReadLine() = "yes!")                                 
//
//        printfn ""
//
//        houseEdge ( 1M / 6M ) 4M ( 5M / 6M ) 1M  |> printfn "House edge = %A"        

        Console.ReadLine() |> ignore

        0
