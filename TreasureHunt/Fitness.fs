module Fitness    
    type Position = Position of int * int
    type Content = Empty | Treasure | Wall
    type Site = Position * Content 
    type Situation = {current:Site; north:Site; south:Site; west:Site; east:Site}
    type ActionOutcome = {fitness:int; nextPosition:Position}
    type Action = Pick | StayPut | North | South | East | West

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

    let private contentAfterAction action situation = 
        situation |> siteAfterAction action |> siteContent

    let private positionAfterAction action situation = 
        match contentAfterAction action situation with
        | Wall -> currentPosition situation
        | _ -> fst (siteAfterAction action situation)
        
    let fitness content action = 
        match content, action with
        | Empty, Pick -> -1
        | Treasure , Pick -> 10
        | Wall, _ -> -5
        | _ , StayPut -> 0
        | _, North | _, South | _, East | _, West -> 0

    let outcome action situation =
        {
            fitness = fitness (contentAfterAction action situation) action
            nextPosition = positionAfterAction action situation
        }