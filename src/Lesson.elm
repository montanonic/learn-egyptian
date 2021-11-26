module Lesson exposing (Lesson, getLessonTexts, getUnidentifiedWordsInLesson, getWops, getWords)

import Dict exposing (Dict)
import List.Extra as ListE
import Maybe.Extra as MaybeE
import WordDisplay
import WordOrPhrase as WOP exposing (WOP)


type alias Lesson =
    { text : String
    , audioFileType : String
    }


getLessonTexts : Dict String Lesson -> List String
getLessonTexts =
    Dict.values >> List.map .text


{-| In order of first appearance, no duplicates.
-}
getWords : String -> List String
getWords lessonText =
    getWordsFromString lessonText
        -- cleanup, but I think getWordsFromString might actually be thorough enough to not need any
        -- of this except for the unique function
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)
        |> ListE.unique


getWops : Dict String WOP -> String -> List WOP
getWops wops lessonText =
    getWords lessonText
        |> List.filterMap (\word -> WOP.get word wops)


getUnidentifiedWordsInLesson : String -> Dict String WOP -> List String
getUnidentifiedWordsInLesson lessonText wops =
    getWords lessonText
        -- keep only those not appearing in our existing dict
        |> List.filter (\word -> WOP.get word wops |> MaybeE.isNothing)


getWordsFromString : String -> List String
getWordsFromString str =
    str |> WordDisplay.markWordCharsFromNonWordChars |> List.filterMap WordDisplay.getWord
