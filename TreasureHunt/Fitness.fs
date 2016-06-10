module Fitness    
    type Content = Empty | Treasure | Wall
    type Action = Pick | StayPut | North | South | East | West
    type Position = Position of int * int
    type ActionOutcome = {fitness:int; nextPosition:Position}
    type Site = Position * Content 
    type Situation = {current:Site; north:Site; south:Site; west:Site; east:Site}

    let private siteContent site = snd site

    let private sitePosition site = fst site

    let private currentPosition situation = sitePosition situation.current

    let private siteAfterAction action situation =
        match action with
        | North -> situation.north
        | South -> situation.south
        | West -> situation.west
        | East -> situation.east
        | _ -> situation.current

    let private positionAfterAction action situation = 
        fst (siteAfterAction action situation)

    let fitness content action = 
        match content, action with
        | Wall, _ -> -5
        | _ , StayPut -> 0
        | _, North | _, South | _, East | _, West -> 0
        | Empty, _ -> -1
        | Treasure , Pick -> 10

    let outcomeAfterAction action situation =
        let contentAfterAction = situation |> siteAfterAction action |> siteContent
        
        {
            fitness = fitness contentAfterAction action
            nextPosition = 
                match contentAfterAction with
                | Wall -> currentPosition situation
                | _ -> positionAfterAction action situation
        }