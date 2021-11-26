module WordDisplay exposing (..)

import Regex



{- this module might be better called "word marking/labelling", as it's more about categorizing the
   words within a text.
-}


{-| These are the different categories of meaning that our interactive word view has, each rendered
differently.

TODO: Maybe rename this to WordOrNonWord and create a second `WordDisplayTypes` that introduces
phrases. Cleaner?

-}
type WordDisplayTypes
    = DisplayWord String
    | DisplayNonWord String
    | DisplayWordOfPhrase String -- temporary hack stand-in that just marks words that are part of a phrase differently, without actually showing the phrase connected
    | DisplayPhrase (List ( String, Int )) -- each string in a phrase is considered a word, we keep the original indices because a Phrase is processed by replacing DisplayWords that are part of a phrase with just the DisplayPhrase construct. Hmmmmmmmmm, come to think of it, this totally fucks everything O_O, cuz our code depended on using DisplayWord. I'll sleep on this one.


{-| without altering the spacing of the underlying text, list off words vs. non-words. it
expects a line of text and doesn't support paragraphs simply because line spacing needs
more manual handling in the HTML layout itself. So while I guess we could detect newlines
here, it would muddle this code, and frankly it's easier to handle it outside.

the importance of separating word from non-word as pure data is multitude, and allows us
to do things like apply further processing to the words themselves by adding/removing
tashkyl.

-}
markWordCharsFromNonWordChars : String -> List WordDisplayTypes
markWordCharsFromNonWordChars lineOfText =
    let
        rxString =
            " *():.?؟!,”،=\\-"

        nonWordDetectorRx =
            Maybe.withDefault Regex.never <|
                -- match all punctuation, then everything else
                Regex.fromString ("([" ++ rxString ++ "]*)" ++ "([^" ++ rxString ++ "]*)")
    in
    List.concatMap
        (\match ->
            {- our rx yields two groups, thus two submatches. the first submatch will only
               be non-word chars, the second will always be word chars. of course, either
               might be empty for any given match because Kleene-* yielding 0 results is
               still a successful match. remember the rx tries to match everything it can,
               then stops, but Elm regex is global so after the rx stops matching, it will
               retry again at the point it's currently at. this is why we get multiple
               matches for each text (hence the outer iteration), and then the inner
               matching is for each *instance* of the rx matching, of which two subgroups
               are matched.
            -}
            case match.submatches of
                [ mnonWordChars, mwordChars ] ->
                    (case mnonWordChars of
                        Just nonWordChars ->
                            [ DisplayNonWord nonWordChars ]

                        Nothing ->
                            []
                    )
                        ++ (case mwordChars of
                                Just wordChars ->
                                    [ DisplayWord wordChars ]

                                Nothing ->
                                    []
                           )

                _ ->
                    -- this branch shouldn't ever be reached anyways
                    []
        )
        (Regex.find nonWordDetectorRx lineOfText)


getWord : WordDisplayTypes -> Maybe String
getWord wdt =
    case wdt of
        DisplayWord w ->
            Just w

        DisplayWordOfPhrase w ->
            Just w

        _ ->
            Nothing
