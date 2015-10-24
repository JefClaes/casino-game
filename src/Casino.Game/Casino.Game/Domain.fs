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
        | Win of Multiplier 
        | Loss
    and Multiplier = decimal  

    type SpinResult = { 
        Box : Box 
        Value : BoxValue
    }         

    type Spin = unit -> SpinResult
    type RNG = unit -> RandomNumber
    type MapToBox = RandomNumber -> Box
    type MapToValue = Box -> BoxValue          

    let boxes = [ Yellow; Green; Red; Blue; White; Purple ]   

    let outOfRange n = invalidArg "n" <| sprintf "value was %i" n

    let rnd = new System.Random()

    let rng () =         
        let n = rnd.Next(1, 7)        

        match n with
        | 1 -> Num1
        | 2 -> Num2
        | 3 -> Num3
        | 4 -> Num4
        | 5 -> Num5
        | 6 -> Num6
        | _ -> outOfRange n

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
        | _ -> Loss

    let mapToValueWithHouseEdge box =
        match box with
        | Red -> Win 5M
        | _ -> Loss

    let mapToBoxWithNearMisses rnd =
        match rnd with
        | Num1 | Num2 -> Green 
        | Num3 -> Red
        | Num4 | Num5 | Num6 -> Blue    

    // (unit -> 'a) -> ('a -> Box) -> (Box -> BoxValue) -> SpinResult
    let spin rng mapToBox mapToValue =                           
        let box = rng() |> mapToBox
        let value = box |> mapToValue                       
        
        { Box = box; Value = value }
    
    let pureSpinWithHouseEdge : Spin = 
        fun() -> spin rng mapToBox mapToValueWithHouseEdge

    let pureSpin : Spin = 
        fun() -> spin rng mapToBox mapToValue 

    let moreRealisticSpin : Spin = 
        fun() -> 
            spin 
                rng 
                mapToBoxWithNearMisses 
                mapToValueWithHouseEdge 

