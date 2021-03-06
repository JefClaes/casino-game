﻿namespace Casino.Game

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

    let drawEmptyBoxes () = 
        boxes |> Seq.iter (fun box -> drawEmptyBox box)

    let youLost () = printfn "Unlucky, you lost."
    let youWon multiplier = printfn "You won your stake x %A" multiplier 

    let sleep () = System.Threading.Thread.Sleep(100)
    let resetLine () = Console.Write("\r") 

    let drawSpinningLine boxes =        
        [0 .. ( boxes |> Seq.length ) - 1]        
        |> Seq.iter (fun i -> 
            sleep()
            resetLine()       
            boxes |> Seq.iteri (fun ii b -> drawBox b (ii = i)))     
            
    let drawSpinningLines times boxes =
        Console.WriteLine()        

        [ 0 .. times - 1 ] |> Seq.iter (fun i -> drawSpinningLine boxes)       

    let drawFinalSpinningLine hitBox boxes =
        let linelength = boxes |> Seq.length
        let hitBoxIndex = boxes |> Seq.toArray |> Array.findIndex(fun x -> x = hitBox)                                              

        [0 .. linelength - 1]        
        |> Seq.iter (fun i -> 
            sleep() 
            resetLine()            

            boxes 
            |> Seq.iteri (fun ii b -> 
                match b = hitBox && ii < i with
                | true -> drawBox b true
                | false -> 
                    match ii <= hitBoxIndex && ii = i with
                    | true -> drawBox b true
                    | false -> drawBox b false                
            ))   

    let render spinResult =
        printfn ""        
            
        boxes |> drawSpinningLines 1
        boxes |> drawFinalSpinningLine spinResult.Box

        [1 .. 2] 
        |> Seq.iter (fun _ -> printfn " ")

        match spinResult.Value with
        | Win multiplier -> 
            [1 .. 3] 
            |> Seq.iter(fun _ -> youWon multiplier)
        | Lose -> youLost()

    let rec start (shouldContinue : unit -> bool) =
        pureSpin() |> render

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
            
    let printHouseEdge desc (houseEdge:decimal) =       
        printfn ""
        printfn "HouseEdge: %s" desc
        printfn ""
        
        if houseEdge <= 0M then
            Console.ForegroundColor <- ConsoleColor.Red
        else
            Console.ForegroundColor <- ConsoleColor.Green

        printfn "%A" <| Math.Round(houseEdge, 4)

        Console.ResetColor()

    let printPayout desc (payout:decimal) =
        printfn ""
        printfn "Payout: %s" desc
        printfn ""

        if payout <= 1M then
            Console.ForegroundColor <- ConsoleColor.Red
        else
            Console.ForegroundColor <- ConsoleColor.Green

        printfn "%A" <| Math.Round(payout, 4)

        Console.ResetColor()

    let run times f =
        [ 1 .. times ] 
        |> Seq.map(fun _ -> f())    
    
    let p f x =
        f x
        x

    [<EntryPoint>]
    let main argv = 
        drawEmptyBoxes()

        printfn ""  

        start (fun () ->
            Console.WriteLine()
            Console.WriteLine("Go again?")            
            Console.ReadLine() = "yes!")    

        let times = 3000000

        let spinStats spin desc =
            let results = run times spin

            results 
            |> p (fun x -> x |> houseEdge |> printHouseEdge desc)
            |> p (fun x -> x |> payout |> printPayout desc)
            |> distribution |> printDistribution desc

        spinStats pureSpin "Pure spin"
        spinStats pureSpinWithHouseEdge "Pure spin with house edge"
        spinStats moreRealisticSpin "More realistic spin"                       
      
        printfn ""

        theoreticalHouseEdge ( 1M / 6M ) 5M ( 5M / 6M ) 1M  
        |> printHouseEdge "Pure spin (theoretical)"        

        theoreticalHouseEdge ( 1M / 6M ) 4M ( 5M / 6M ) 1M  
        |> printHouseEdge "More realistic spin (theoretical)"                                                     

        Console.ReadLine() |> ignore

        0
