module Grid
    open Domain
    
    type Grid = Site seq

    let private size = 10
    let private maxTreasures = size * size
    let private treasureThresholdChance = (int)((float)maxTreasures * 0.7)
    let private rand = System.Random()

    let private nextRand min max =
        rand.Next(min, max)

    let private treasureOrEmpty =
        let number = nextRand 1 maxTreasures
        match number with
        | n when n < treasureThresholdChance -> Treasure
        | _ -> Empty

    let private isWall line = 
        line = 0 || line = size - 1

    let private contentAt row column = 
        let content =
            if row |> isWall || column |> isWall 
            then Wall
            else treasureOrEmpty
        Position(row, column), content

    let create = 
        seq { 
            for row in 0 .. size - 1 do
                for column in 0 .. size - 1 do
                    yield contentAt row column
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