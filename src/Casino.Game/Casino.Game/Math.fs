namespace Casino.Game

open Casino.Game.Domain

module Math =

    let pct x y = float x / float y * float 100        

    let distribution results =    
        results
        |> Seq.countBy (fun result -> result.Box)
        |> Seq.map (fun (box, count) -> 
            (box, pct count <| Seq.length results))       
        
    let totalBets results =
        decimal (results |> Seq.length) * 1M

    let totalWins results =
        results
        |> Seq.choose (fun x -> 
            match x.Value with 
            | Win factor -> Some factor
            | _ -> None)
        |> Seq.sumBy (fun f -> 1M * f)  
        
    let payout results =      
        let bets = totalBets results
        let wins = totalWins results     
        
        match bets with
        | 0M -> 1M
        | _ -> wins / bets                
        
    let houseEdge results =      
        1M - payout results            

    let theoreticalHouseEdge oddsOfWinning winnings oddsOfLosing stake =            
        let x : decimal = oddsOfWinning * winnings - oddsOfLosing * stake
        System.Math.Round(x, 2)

