module Flashcard exposing (..)

import List.Extra as ListE
import Time exposing (Posix)


{-| Flashcard type for managing basic flashcard concerns.
-}
type alias Flashcard a =
    { data : a
    , history : List (FlashcardHistoryEntry Int)
    }


type alias FlashcardHistoryEntry a =
    { timestamp : a -- posix
    , remembered : Bool
    }


makeflashcards : List a -> List (Flashcard a)
makeflashcards =
    List.map (\x -> { data = x, history = [] })


addHistoryEntry : Posix -> Bool -> Flashcard a -> Flashcard a
addHistoryEntry timestamp remembered flashcard =
    { flashcard
        | history =
            { timestamp = Time.posixToMillis timestamp, remembered = remembered }
                :: flashcard.history
    }


{-| The last entry in the list of flashcards gives the time of last review (in ms), which we can use to
calculate which cards to surface first.
-}
getTimeOfLastReview : List (Flashcard a) -> Int
getTimeOfLastReview cards =
    ListE.last cards
        |> Maybe.andThen getMostRecentHistory
        |> Maybe.map .timestamp
        |> Maybe.withDefault 0


{-| in absolute milliseconds.
-}
getMostRecentHistory : Flashcard a -> Maybe (FlashcardHistoryEntry Int)
getMostRecentHistory card =
    List.head card.history


{-| seconds to milliseconds
-}
sToMs : Int -> Int
sToMs seconds =
    1000 * seconds


{-| By default the next card up is just the next in the list, cuz clicking next moves the card to
the end of the queue. But we're going to copy Anki a bit and try out this simple review strategy:

2 correct recalls in a row are needed to move on, the first will be in vocab context with no
containing sentence, and until you recall it it will keep showing up as vocab. Once a single correct
recall has been made, the word transitions into sentence mode to prime you for the reading by
noticing the word in context. if you fail the word there it will remain in sentence mode, but you'll
need to get it correct twice in a row to move on.

the job of determining the render mode is left to another function on the review history. this
function's goal is to instead surface any cards you've gotten wrong the next time that minute is up,
because we want to keep the number of new cards you're seeing down so that you're not cycling
through 20 cards you're learning before seeing the first again.

so in other words, based upon the "time of last review", this function might jump past the next card to grab the first card in the list that has a most recent failure which has been past a minute due.

currently there's no other jump-ahead, as the focus is on lesson-integrated flashcards where ideally we're not doing too many at once, but otherwise for longer reviews we definitely would want to jump ahead the 1 correct review items to be due at 10mins just like Anki, so that, again, you don't have too long pass before revisiting the stuff that you've already seen.

-}
getNextCard : List (Flashcard a) -> Maybe (Flashcard a)
getNextCard cards =
    let
        lastReview =
            getTimeOfLastReview cards
                |> Debug.log "last review"

        jumpAheadCandidate =
            cards
                |> ListE.find
                    (\card ->
                        case getMostRecentHistory card of
                            Just history ->
                                let
                                    _ =
                                        Debug.log "history" history
                                in
                                -- it was marked forgotten and it has been more than a minute since
                                not history.remembered
                                    && ((lastReview - history.timestamp) > sToMs 60)

                            Nothing ->
                                False
                    )
    in
    case jumpAheadCandidate of
        Just c ->
            Just c

        Nothing ->
            List.head cards


getHistory : Flashcard a -> List (FlashcardHistoryEntry Posix)
getHistory flashcard =
    flashcard.history
        |> List.map
            (\{ timestamp, remembered } ->
                { timestamp = Time.millisToPosix timestamp, remembered = remembered }
            )
