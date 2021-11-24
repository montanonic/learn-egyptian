module Flashcards exposing (..)

import Time exposing (Posix)


{-| Data-agnostic flashcard type for managing basic flashcard concerns.
-}
type alias Flashcard a =
    { data : a
    , history : List (FlashcardHistoryEntry Int)
    }


type alias FlashcardHistoryEntry a =
    { timestamp : a -- posix
    , remembered : Bool
    }


getHistory : Flashcard a -> List (FlashcardHistoryEntry Posix)
getHistory flashcard =
    flashcard.history
        |> List.map
            (\{ timestamp, remembered } ->
                { timestamp = Time.millisToPosix timestamp, remembered = remembered }
            )
