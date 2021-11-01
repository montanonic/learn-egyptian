module Utils exposing (..)

{-| Slides a "window" over a list, returning the chunks that fall within the purview of the window.
The first parameter sets the size of the window and how many elements it slides over each step. The
operation is truncating if the step would slide the window past the end, so

    slidingWindow { size = 2, step = 2 } [ 1, 2, 3, 4, 5 ] == [ [ 1, 2 ], [ 3, 4 ] ]

because given the step size, the last window would be [5, null].

-}


slidingWindow : { size : Int, step : Int } -> List a -> List (List a)
slidingWindow { size, step } list =
    list
        |> List.foldl
            (\elem ( window, result ) ->
                let
                    currentWindow =
                        window ++ [ elem ]
                in
                if List.length currentWindow /= size then
                    -- wait until we have a large enough window
                    ( currentWindow, result )

                else
                    -- make sure not to lose the current element when resizing the window
                    ( List.drop step currentWindow, currentWindow :: result )
            )
            ( [], [] )
        |> Tuple.second
        |> List.reverse
