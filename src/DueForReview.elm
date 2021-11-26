module DueForReview exposing (..)

import Dict exposing (Dict)
import Lesson exposing (Lesson)
import Ordering
import WordOrPhrase as WOP exposing (WOP)


{-| TODO: Integrate simple spaced repetition reviews for New and Recognized words. In general
though, everytime you see a word you're learning it's really helpful to see it again the next day.
-}
wopsDueForReview : List WOP -> List WOP
wopsDueForReview wops =
    wops
        |> List.map (\wop -> ( wop, WOP.lastReviewedOn wop |> Maybe.withDefault 0 ))
        |> List.sortBy Tuple.second
        |> List.map Tuple.first


type alias LessonWithReviewDensity =
    { lessonTitle : String, lessonText : String, densityRatio : Float, totalDueInLesson : Int, dueWopsInLesson : List WOP }


{-| Density is given in two numbers: the amount of wops due for review contained within a lesson,
total, along with the ratio of due wops to the wops a lesson has. This ratio however completely
ignores word repetition, and only considers unique words. A different calculation is needed if how
many times a word appears in a lesson is considered a desirable metric.
-}
lessonsByReviewDensity : Dict String WOP -> Dict String Lesson -> List WOP -> List LessonWithReviewDensity
lessonsByReviewDensity allWops lessons dueWops =
    List.map
        (\( title, lesson ) ->
            let
                dueWopsInLesson =
                    Lesson.getWops (WOP.intoDict dueWops) lesson.text

                allWopsInLesson =
                    Lesson.getWops allWops lesson.text

                ( totalDueInLesson, totalWopsInLesson ) =
                    ( List.length dueWopsInLesson, List.length allWopsInLesson )

                ratio =
                    toFloat totalDueInLesson / toFloat totalWopsInLesson
            in
            { lessonTitle = title, lessonText = lesson.text, densityRatio = ratio, totalDueInLesson = totalDueInLesson, dueWopsInLesson = dueWopsInLesson }
        )
        (Dict.toList lessons)
        |> List.sortWith
            -- the algo below does a two-tiered sort where ratio is the first metric but for tied ratio, total due wins out
            (Ordering.byField .densityRatio
                |> Ordering.breakTiesWith (Ordering.byField .totalDueInLesson)
                |> Ordering.reverse
            )
