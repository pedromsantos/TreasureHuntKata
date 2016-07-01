module Domain
    type Position = Position of int * int
    type Content = Empty | Treasure | Wall
    type Site = Position * Content 
    type Situation = {current:Site; north:Site; south:Site; west:Site; east:Site}
    type ActionOutcome = {fitness:int; nextPosition:Position}
    type Action = Pick | StayPut | North | South | East | West

    type Actions = Action list
    type StrategyRow = {Current:Content; North:Content; South:Content; West:Content; East:Content}
    type StrategyRows = StrategyRow seq
    type Strategy = StrategyRows * Actions

    type CreateStrategyRows = unit -> StrategyRows

    let contentOf site = snd site
    let positionOf site = fst site
    let currentPosition situation = positionOf situation.current

    let move action position =
        match action, position with
        | North, Position(x,y) -> Position(x, y - 1)
        | South, Position(x,y) -> Position(x, y + 1)
        | West, Position(x,y) -> Position(x - 1, y)
        | East, Position(x,y) -> Position(x + 1, y)
        | _ -> position

    let createStrategyRows:CreateStrategyRows = fun () -> 
        let contents = Infrastructure.AllCases<Content> |> Seq.map (fun c -> snd c)
        
        seq { 
            for current in contents do
                for north in contents do
                    for south in contents do
                        for east in contents do
                            for west in contents do
                                yield {Current=current; North=north; South=south; West=west; East=east}
            } |> Seq.cache
