module draft exposing (..)case letters of
        letter1 :: moreLetters ->
            [ text letter1 |> centered |> fixedwidth |> filled black
            , recursiveList model moreLetters
                |> scale model.myscale
                |> rotate model.myrotate
                |> move ( model.move_x, model.move_y )
            ]
                |> group

        [] ->
            group []


recursiveList m (m.txt |> String.toList |> List.map String.fromChar)


shapeFun m =
    let
        recurList listOfLetters =
            case listOfLetters of
                x :: rest ->
                    [ text x
                        |> centered
                        |> fixedwidth
                        |> filled black
                        |> {m | currentLetter = x}
                    , recurList rest
                        |> scale m.myscale
                        |> rotate m.myrotate
                        |> move ( m.move_x, m.move_y )
                    ]
                        |> group

                _ ->
                    group []
    in
    recurList (m.txt |> String.toList |> List.map String.fromChar)



case letters of
        letter1 :: moreLetters ->
            [ text letter1 |> centered |> fixedwidth |> filled black
            , recursiveList model moreLetters
                |> scale model.myscale
                |> rotate model.myrotate
                |> move ( model.move_x, model.move_y )
            ]
                |> group

        [] ->
            group []

tstste = 
    let 
        func = 
            case letters of
                letter1 :: moreLetters ->
                    [ text letter1 |> centered |> fixedwidth |> filled black
                    , recursiveList model moreLetters
                        |> scale model.myscale
                        |> rotate model.myrotate
                        |> move ( model.move_x, model.move_y )
                    ]
                        |> group

                [] ->
                    group []
    in
        
    