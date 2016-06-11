module GridTests
    open NUnit.Framework
    open Swensen.Unquote
    open Domain
    open Grid

    let sitesForRow row grid = 
        grid
        |> Seq.filter (fun (p,_) -> 
            match p with 
            | Position(r,_) when r = row -> true
            | _ -> false)
    
    

    [<Test>]
    let ``Should create grid 10x10``() =
        test <@ create |> Seq.length = 100  @>

    [<Test>]
    let ``Grid should contain walls on first row``() =
        test <@ create 
                |> sitesForRow 0 
                |> Seq.filter (fun (_,c) -> c = Wall ) 
                |> Seq.length = 10 @>

    [<Test>]
    let ``Grid should contain walls on last row``() =
        test <@ create 
                |> sitesForRow 9 
                |> Seq.filter (fun (_,c) -> c = Wall ) 
                |> Seq.length = 10 @>
    
    let sitesForColumn column grid = 
        grid
        |> Seq.filter (fun (p,_) -> 
            match p with 
            | Position(_,c) when c = column -> true
            | _ -> false)
    
    [<Test>]
    let ``Grid should contain walls on first column``() =
        test <@ create 
                |> sitesForColumn 0 
                |> Seq.filter (fun (_,c) -> c = Wall ) 
                |> Seq.length = 10 @>

    [<Test>]
    let ``Grid should contain walls on last column``() =
        test <@ create 
                |> sitesForColumn 9 
                |> Seq.filter (fun (_,c) -> c = Wall ) 
                |> Seq.length = 10 @>

    [<Test>]
    let ``Grid should contain some treasures``() =
        let grid = create
        test <@ grid
                |> Seq.filter (fun (_,c) -> c = Treasure)
                |> Seq.length > 0 @>

    [<Test>]
    let ``Should create situation from grid position``() =
        let situation = create |> situation (Position(1,1))
        
        test <@ fst situation.current = Position(1,1) @>
        test <@ fst situation.north = Position(1,0) @>
        test <@ fst situation.south = Position(1,2) @>
        test <@ fst situation.west = Position(0,1) @>
        test <@ fst situation.east = Position(2,1) @>

        test <@ snd situation.north = Wall @>
        test <@ snd situation.west = Wall @>