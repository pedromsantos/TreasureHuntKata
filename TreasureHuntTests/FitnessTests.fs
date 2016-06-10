module FitnessTests
    open NUnit.Framework
    open Swensen.Unquote
  
    type Site = Empty | Treasure | Wall
    type Action = Pick | StayPut | North | South | East | West
    type Position = Position of int * int
    type SiteAction = {fitness:int; nextPosition:Position}

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

    let performAction site action currentPosition =
            {
                fitness = calculateFitness site action;
                nextPosition = nextPosition action currentPosition
            }

    [<Test>]
    let ``Should score -1 if current position is empty and action is pick``() =
        test <@ calculateFitness Empty Pick = -1 @>

    [<Test>]
    let ``Should score 10 if current position has treasure and action is pick``() =
        test <@ calculateFitness Treasure Pick = 10 @>

    [<Test>]
    let ``Should score -5 if current position is wall``() =
        test <@ calculateFitness Wall Pick = -5 @>

    [<Test>]
    let ``Should score 0 if the action is stay put on Empty or Trweasure site`` () = 
        test <@ calculateFitness Empty StayPut = 0 @>
        test <@ calculateFitness Treasure StayPut = 0 @>

    [<Test>]
    let ``Should score 0 if the action is move`` () = 
        test <@ calculateFitness Empty North = 0 @>
        test <@ calculateFitness Empty South = 0 @>
        test <@ calculateFitness Empty East  = 0 @>
        test <@ calculateFitness Empty West  = 0 @>

    [<Test>]
    let ``Perform StayPut action does not change current position`` () =
        test <@ nextPosition StayPut (Position(0,0)) = Position(0,0) @>
        test <@ nextPosition North (Position(0,1)) = Position(0,0) @>
        test <@ nextPosition North (Position(0,2)) = Position(0,1) @>
        test <@ nextPosition North (Position(1,2)) = Position(1,1) @>
        test <@ nextPosition South (Position(1,2)) = Position(1,3) @>
        test <@ nextPosition West (Position(1,1)) = Position(0,1) @>
        test <@ nextPosition East (Position(1,1)) = Position(2,1) @>