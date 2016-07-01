module DomainTests
    open NUnit.Framework
    open Swensen.Unquote
    open Domain

    [<Test>]
    let ``Perform move action changes current position`` () =
        test <@ move North (Position(0,1)) = Position(0,0) @>
        test <@ move North (Position(0,2)) = Position(0,1) @>
        test <@ move North (Position(1,2)) = Position(1,1) @>
        test <@ move South (Position(1,2)) = Position(1,3) @>
        test <@ move West (Position(1,1)) = Position(0,1) @>
        test <@ move East (Position(1,1)) = Position(2,1) @>

    [<Test>]
    let ``Creates all startegy rows`` () =
        test <@ createStrategyRows() |> Seq.length = 243 @>

        let firstRun = createStrategyRows() |> Seq.toList
        let secondRun = createStrategyRows() |> Seq.toList

        test <@  firstRun = secondRun @>