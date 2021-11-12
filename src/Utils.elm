module Utils exposing (..)

import List.Zipper as Zipper exposing (Zipper)


deconstructZipper : Zipper a -> { before : List a, current : a, after : List a }
deconstructZipper z =
    { before = Zipper.before z, current = Zipper.current z, after = Zipper.after z }
