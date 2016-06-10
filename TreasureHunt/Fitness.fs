module Fitness    
    type Content = Empty | Treasure | Wall
    type Action = Pick | StayPut | North | South | East | West
    type Position = Position of int * int
    type SiteAction = {fitness:int; nextPosition:Position}
    type Site = Position * Content 
    type Situation = {current:Site; north:Site; south:Site; west:Site; east:Site}

    let calculateFitness site action = 
        match site, action with
        | Wall, _ -> -5
        | _ , StayPut -> 0
        | _, North | _, South | _, East | _, West -> 0
        | Empty, _ -> -1
        | Treasure , Pick -> 10
    
    let nextPosition action currentPosition =
        match action, currentPosition with
        | North, Position(x,y) -> Position(x, y - 1)
        | South, Position(x,y) -> Position(x, y + 1)
        | West, Position(x,y) -> Position(x - 1, y)
        | East, Position(x,y) -> Position(x + 1, y)
        | _ -> currentPosition

    let siteForAction action situation =
        match action with
        | North -> situation.north
        | South -> situation.south
        | West -> situation.west
        | East -> situation.east
        | _ -> situation.current

    let contentForSite site =
        snd site

    let nextPositionForSituation action situation =
        let siteContentAfterAction = situation |> siteForAction action |> contentForSite
        match siteContentAfterAction with
        | Wall -> fst situation.current
        | _ -> nextPosition action (fst situation.current)

    let performAction site action currentPosition =
            {
                fitness = calculateFitness site action;
                nextPosition = nextPosition action currentPosition
            }