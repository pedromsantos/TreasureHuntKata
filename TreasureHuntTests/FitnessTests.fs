module FitnessTests
    open NUnit.Framework
    open Swensen.Unquote
    open Fitness

    let newSituation 
        position currentContent northContent southContent westContent eastContent = 
        {
            current = (position, currentContent)
            north = (position |> nextPosition North, northContent)
            south =(position |> nextPosition South, southContent)
            west = (position |> nextPosition West,  westContent)
            east = (position |> nextPosition East,  eastContent)
        }

    [<Test>]
    let ``Should score -1 if current position is empty and action is pick``() =
        test <@ fitness Empty Pick = -1 @>

    [<Test>]
    let ``Should score 10 if current position has treasure and action is pick``() =
        test <@ fitness Treasure Pick = 10 @>

    [<Test>]
    let ``Should score -5 if current position is wall``() =
        test <@ fitness Wall Pick = -5 @>

    [<Test>]
    let ``Should score 0 if the action is stay put on empty or treasure site`` () = 
        test <@ fitness Empty StayPut = 0 @>
        test <@ fitness Treasure StayPut = 0 @>

    [<Test>]
    let ``Should score 0 if the action is move`` () = 
        test <@ fitness Empty North = 0 @>
        test <@ fitness Empty South = 0 @>
        test <@ fitness Empty East  = 0 @>
        test <@ fitness Empty West  = 0 @>

    [<Test>]
    let ``Perform stay put action does not change current position`` () =
        test <@ nextPosition StayPut (Position(0,0)) = Position(0,0) @>
        
    [<Test>]
    let ``Perform move action changes current position`` () =
        test <@ nextPosition North (Position(0,1)) = Position(0,0) @>
        test <@ nextPosition North (Position(0,2)) = Position(0,1) @>
        test <@ nextPosition North (Position(1,2)) = Position(1,1) @>
        test <@ nextPosition South (Position(1,2)) = Position(1,3) @>
        test <@ nextPosition West (Position(1,1)) = Position(0,1) @>
        test <@ nextPosition East (Position(1,1)) = Position(2,1) @>

    [<Test>]
    let ``Perform move action bounces back against a wall`` () =
        let currentSituation = newSituation (Position(1,1)) Empty Wall Empty Empty Empty
        let samePosition = positionAfterAction StayPut currentSituation

        test <@ (nextPositionForSituation North currentSituation).nextPosition = samePosition @>

    [<Test>]
    let ``Perform move action ends in wall fitness = -5`` () =
        let currentSituation = newSituation (Position(1,1)) Empty Wall Empty Empty Empty
        let samePosition = positionAfterAction StayPut currentSituation

        test <@ (nextPositionForSituation North currentSituation).fitness = -5 @>

    [<Test>]
    let ``Perform move action return new position from situation`` () =
        let currentSituation = newSituation (Position(1,1)) Empty Empty Empty Empty Empty
        let northPosition = positionAfterAction North currentSituation 
        
        test <@ (nextPositionForSituation North currentSituation).nextPosition = northPosition @>