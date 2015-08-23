namespace Casino.Game

open Casino.Game.Domain

module Math =

    let pct x y = float x / float y * float 100        

    let distribution results =    
        results
        |> Seq.countBy (fun result -> result.Box)
        |> Seq.map (fun (box, count) -> (box, pct count <| Seq.length results))       
        
    let houseEdge results =      
        let stake = 1M
        let bets = decimal (results |> Seq.length) * stake
        let wins =         
            results
            |> Seq.choose (fun x -> 
                match x.Value with 
                | Win factor -> Some factor
                | _ -> None)
            |> Seq.sumBy (fun f -> stake * f)
        
        if (wins = 0M) then
            100M
        else
            System.Math.Round(bets / wins, 2)

    let payout results =      
        let stake = 1M
        let bets = decimal (results |> Seq.length) * stake
        let wins =         
            results
            |> Seq.choose (fun x -> 
                match x.Value with 
                | Win factor -> Some factor
                | _ -> None)
            |> Seq.sumBy (fun f -> stake * f)
        
        if (bets = 0M) then
            100M
        else
            System.Math.Round(wins / bets, 2)

    let theoreticalHouseEdge oddsOfWinning winnings oddsOfLosing stake =            
        let x : decimal = oddsOfWinning * winnings - oddsOfLosing * stake
        System.Math.Round(x, 2)

