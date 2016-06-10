module FitnessTests
    open NUnit.Framework
    open Swensen.Unquote
    open Domain
    open Fitness

    let newSituation 
        position currentContent northContent southContent westContent eastContent = 
        {
            current = (position, currentContent)
            north = (position |> move North, northContent)
            south =(position |> move South, southContent)
            west = (position |> move West,  westContent)
            east = (position |> move East,  eastContent)
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
    let ``Move returns new position from situation`` () =
        let situation = newSituation (Position(1,1)) Empty Empty Empty Empty Empty
        let currentPosition = fst situation.current 
        let northPosition = fst situation.north 
        let southPosition = fst situation.south 
        let westPosition = fst situation.west
        let eastPosition = fst situation.east 
        
        test <@ (outcome StayPut situation).nextPosition = currentPosition @>
        test <@ (outcome Pick situation).nextPosition = currentPosition @>
        test <@ (outcome North situation).nextPosition = northPosition @>
        test <@ (outcome South situation).nextPosition = southPosition @>
        test <@ (outcome West situation).nextPosition = westPosition @>
        test <@ (outcome East situation).nextPosition = eastPosition @>

    [<Test>]
    let ``Move bounces back against a wall`` () =
        let situation = newSituation (Position(1,1)) Empty Wall Empty Empty Empty
        let samePosition = fst situation.current

        test <@ (outcome North situation).nextPosition = samePosition @>

    [<Test>]
    let ``Move hits wall fitness -5`` () =
        let situation = newSituation (Position(1,1)) Empty Wall Empty Empty Empty
        let samePosition = fst situation.current

        test <@ (outcome North situation).fitness = -5 @>