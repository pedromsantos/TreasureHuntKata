module Grid
    open Domain

    let private size = 10
    let private treasureRandomTop = size * 10
    let private treasureThresholdChance = 50
    let private rand = System.Random()

    let randomizeContent = 
        let number = rand.Next(1, treasureRandomTop)
        match number with
        | n when n >= treasureThresholdChance -> Treasure
        | _ -> Empty

    let private contentForPosition row column = 
        if row = 0 || row = size - 1 || column = 0 || column = size - 1 then
            Position(row, column), Wall
        else
            Position(row, column), randomizeContent

    let create = 
        seq { for row in 0 .. size - 1 do
                for column in 0 .. size - 1 do
                    yield
                        contentForPosition row column
            }

    let situation position grid =
        {
            current = grid |> Seq.find (fun site -> sitePosition site = position); 
            north = grid |> Seq.find (fun site -> sitePosition site = move North position); 
            south = grid |> Seq.find (fun site -> sitePosition site = move South position);
            west = grid |> Seq.find (fun site -> sitePosition site = move West position);
            east = grid |> Seq.find (fun site -> sitePosition site = move East position);
        }