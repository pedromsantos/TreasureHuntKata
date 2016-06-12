module Domain
    type Position = Position of int * int
    type Content = Empty | Treasure | Wall
    type Site = Position * Content 
    type Situation = {current:Site; north:Site; south:Site; west:Site; east:Site}
    type ActionOutcome = {fitness:int; nextPosition:Position}
    type Action = Pick | StayPut | North | South | East | West

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