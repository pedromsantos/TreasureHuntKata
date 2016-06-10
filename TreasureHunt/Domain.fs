module Domain
    type Position = Position of int * int
    type Content = Empty | Treasure | Wall
    type Site = Position * Content 
    type Situation = {current:Site; north:Site; south:Site; west:Site; east:Site}
    type ActionOutcome = {fitness:int; nextPosition:Position}
    type Action = Pick | StayPut | North | South | East | West