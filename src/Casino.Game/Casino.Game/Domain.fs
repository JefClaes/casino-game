namespace Casino.Game

module Domain =

    type Box = 
        | Yellow 
        | Green 
        | Red 
        | Blue 
        | White 
        | Purple      

     type RandomNumber = 
        | Num1 
        | Num2 
        | Num3 
        | Num4 
        | Num5 
        | Num6 

    type BoxValue = 
        | Win of WinFactor 
        | Lose  
    and WinFactor = decimal

    type SpinResult = { 
        Box : Box 
        Value : BoxValue
    }         

    type RNG = unit -> RandomNumber
    type MapToBox = RandomNumber -> Box
    type MapToValue = Box -> BoxValue  
    type Spin = RNG -> MapToBox -> MapToValue -> SpinResult

    let boxes = [ Yellow; Green; Red; Blue; White; Purple ]   

    let rng () =         
        let rnd = new System.Random()
        let n = rnd.Next(1, 7)        

        match n with
        | 1 -> Num1
        | 2 -> Num2
        | 3 -> Num3
        | 4 -> Num4
        | 5 -> Num5
        | 6 -> Num6
        | _ -> failwith "Random number could not be mapped"

    let mapToBox rnd =   
        match rnd with
        | Num1 -> Yellow 
        | Num2 -> Green
        | Num3 -> Red
        | Num4 -> Blue  
        | Num5 -> White
        | Num6 -> Purple  

    let mapToValue box =
        match box with
        | Red -> Win 6M
        | _ -> Lose

    let mapToValueWithHouseEdge box =
        match box with
        | Red -> Win 5M
        | _ -> Lose

    let mapToBoxWithNearMisses rnd =
        match rnd with
        | Num1 | Num2 -> Green 
        | Num3 -> Red
        | Num4 | Num5 | Num6 -> Blue    

    let spin : Spin = fun rng mapToBox mapToValue ->                              
        let box = rng() |> mapToBox
        let value = box |> mapToValue                       
        
        { Box = box; Value = value }
    
    let pureSpin () = spin rng mapToBox mapToValue 
    let spinWithNearMisses () = spin rng mapToBoxWithNearMisses mapToValueWithHouseEdge 

