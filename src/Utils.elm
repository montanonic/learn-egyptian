module Utils exposing (..)

import List.Zipper as Zipper exposing (Zipper)



-- update helpers


pure : model -> ( model, Cmd msg )
pure model =
    ( model, Cmd.none )


impure : model -> (model -> Cmd msg) -> ( model, Cmd msg )
impure model effect =
    ( model, effect model )



-- zipper helpers


deconstructZipper : Zipper a -> { before : List a, current : a, after : List a }
deconstructZipper z =
    { before = Zipper.before z, current = Zipper.current z, after = Zipper.after z }
