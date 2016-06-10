module FitnessTests
    open NUnit.Framework
    open Swensen.Unquote
    open Fitness

    let nextPosition action currentPosition =
        match action, currentPosition with
        | North, Position(x,y) -> Position(x, y - 1)
        | South, Position(x,y) -> Position(x, y + 1)
        | West, Position(x,y) -> Position(x - 1, y)
        | East, Position(x,y) -> Position(x + 1, y)
        | _ -> currentPosition

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
    let ``Move bounces back against a wall`` () =
        let currentSituation = newSituation (Position(1,1)) Empty Wall Empty Empty Empty
        let samePosition = fst currentSituation.current

        test <@ (outcomeAfterAction North currentSituation).nextPosition = samePosition @>

    [<Test>]
    let ``Move ends in wall fitness -5`` () =
        let currentSituation = newSituation (Position(1,1)) Empty Wall Empty Empty Empty
        let samePosition = fst currentSituation.current

        test <@ (outcomeAfterAction North currentSituation).fitness = -5 @>

    [<Test>]
    let ``Move return new position from situation`` () =
        let situation = newSituation (Position(1,1)) Empty Empty Empty Empty Empty
        let currentPosition = fst situation.current 
        let northPosition = fst situation.north 
        let southPosition = fst situation.south 
        let westPosition = fst situation.west
        let eastPosition = fst situation.east 
        
        test <@ (outcomeAfterAction StayPut situation).nextPosition = currentPosition @>
        test <@ (outcomeAfterAction Pick situation).nextPosition = currentPosition @>
        test <@ (outcomeAfterAction North situation).nextPosition = northPosition @>
        test <@ (outcomeAfterAction South situation).nextPosition = southPosition @>
        test <@ (outcomeAfterAction West situation).nextPosition = westPosition @>
        test <@ (outcomeAfterAction East situation).nextPosition = eastPosition @>