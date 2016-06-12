module Fitness    
    open Domain

    let private siteAfterAction action situation =
        match action with
        | North -> situation.north
        | South -> situation.south
        | West -> situation.west
        | East -> situation.east
        | _ -> situation.current

    let private contentAfterAction action situation = 
        situation |> siteAfterAction action |> contentOf

    let private positionAfterAction action situation = 
        match contentAfterAction action situation with
        | Wall -> currentPosition situation
        | _ -> fst (siteAfterAction action situation)
        
    let fitness content action = 
        match content, action with
        | Empty, Pick -> -1
        | Treasure , Pick -> 10
        | Wall, _ -> -5
        | _ , _ -> 0

    let outcome action situation =
        {
            fitness = fitness (contentAfterAction action situation) action
            nextPosition = positionAfterAction action situation
        }