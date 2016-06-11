module Grid
    open Domain
    
    type Grid = Site seq

    let private size = 10
    let private treasureRandomTop = size * 10
    let private treasureThresholdChance = 60
    let private rand = System.Random()

    let private TreasureOrEmpty = 
        let number = rand.Next(1, treasureRandomTop)
        match number with
        | n when n < treasureThresholdChance -> Treasure
        | _ -> Empty

    let private isWall line = 
        line = 0 || line = size - 1

    let private contentForPosition row column = 
        let content =
            if isWall row || isWall column 
            then Wall
            else TreasureOrEmpty
        Position(row, column), content

    let create = 
        seq { 
            for row in 0 .. size - 1 do
                for column in 0 .. size - 1 do
                    yield contentForPosition row column
        }

    let findSiteBy position grid =
        grid |> Seq.find (fun site -> positionOf site = position)

    let situation position grid =
        {
            current = grid |> findSiteBy position; 
            north   = grid |> findSiteBy (move North position); 
            south   = grid |> findSiteBy (move South position);
            west    = grid |> findSiteBy (move West  position);
            east    = grid |> findSiteBy (move East  position);
        }