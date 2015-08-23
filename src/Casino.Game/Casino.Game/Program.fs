namespace Casino.Game

open System
open Casino.Game.Domain
open Casino.Game.Math

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

    let drawEmptyBox box =         
        Console.ForegroundColor <- systemColor box
            
        printf "[ ] "

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

    let printPayout desc payout =
        printfn ""
        printfn "Payout: %s" desc
        printfn ""
        printfn "%A" payout 

    let run times f =
        [ 1 .. times ] 
        |> Seq.map(fun _ -> f())    
    
    let p f x =
        f x
        x

    [<EntryPoint>]
    let main argv = 
        printfn ""  

        let times = 100000
        let pureSpinResults = run times pureSpin
        let moreRealisticSpinResults = run 100000 moreRealisticSpin
      
        pureSpinResults 
        |> p (fun x -> x |> houseEdge |> printHouseEdge "Pure spin")
        |> p (fun x -> x |> payout |> printPayout "Pure spin")
        |> distribution |> printDistribution "Pure spin"

        moreRealisticSpinResults 
        |> p (fun x -> x |> houseEdge |> printHouseEdge "More realistic spin")
        |> p (fun x -> x |> payout |> printPayout "More realistic spin")
        |> distribution |> printDistribution "More realistic spin"                                    
      
        printfn ""

        theoreticalHouseEdge ( 1M / 6M ) 5M ( 5M / 6M ) 1M  
        |> printHouseEdge "Pure spin (theoretical)"        

        theoreticalHouseEdge ( 1M / 6M ) 4M ( 5M / 6M ) 1M  
        |> printHouseEdge "More realistic spin (theoretical)"   

        start (fun () ->
            Console.WriteLine()
            Console.WriteLine("Go again?")            
            Console.ReadLine() = "yes!")                                       

        Console.ReadLine() |> ignore

        0
