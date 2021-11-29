module DueForReview exposing (..)

import Dict exposing (Dict)
import Lesson exposing (Lesson)
import Ordering
import Time
import WordOrPhrase as WOP exposing (WOP)



{- every New word that I studied today should be shown in a review again tomorrow. every familiar word should be shown in approx 5 days. -}


{-| TODO: Integrate simple spaced repetition reviews for New and Recognized words. In general
though, everytime you see a word you're learning it's really helpful to see it again the next day.
-}
wopsDueForReview : List WOP -> List WOP
wopsDueForReview wops =
    wops
        |> List.map (\wop -> ( wop, WOP.lastReviewedOn wop |> Maybe.withDefault 0 ))
        -- smaller review time is longer ago
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


{-| Give all wops, the current time, and all lessons, finds less well-known words that haven't been
reviewed yet or not seen for longer, and returns the top 5 lessons containing the highest density of
those words. There are a lot of arbitrary choices made in this function, it is highly subject to change.
-}
getLessonsDueForReview :
    { a | wops : Dict String WOP, tick : Time.Posix, lessons : Dict String Lesson }
    -> List LessonWithReviewDensity
getLessonsDueForReview model =
    let
        oneDay =
            24 * 60 * 60 * 1000

        dueForReviewList =
            {- TODO: The way this is calculated is just temporary. Once we have review data for most
               words, we can move to a more useful calculation. or at least have two: one for SRS-style
               it would be useful to review this again today, and then this more general review of
               words that you haven't seen for the longest time, useful at least for earlier stage
               vocabulary acquisition

               TODO: as I have improved upon this, the functionality should be integrated into the
               wopsDueForReview function eventually.
            -}
            wopsDueForReview (Dict.values model.wops)
                |> List.filter (\{ familiarityLevel } -> familiarityLevel <= 2)
                |> List.filter
                    (\wop ->
                        Maybe.map
                            (\lastReviewed ->
                                if wop.familiarityLevel == 1 then
                                    -- for level 1 keep only those reviewed more than 24 hours ago,
                                    -- otherwise don't need to review again
                                    (Time.posixToMillis model.tick - lastReviewed) > oneDay

                                else
                                    -- for level 2, we can wait at least 4 days before reviewing
                                    -- again.
                                    (Time.posixToMillis model.tick - lastReviewed) > 4 * oneDay
                            )
                            (WOP.lastReviewedOn wop)
                            -- any missing a review date are up for review
                            |> Maybe.withDefault True
                    )
                {- among other things, this limit helps the performance cost of calculating the next step of
                   this with the lesson lookup. turns out that doing a search for every due word in every
                   lesson isn't cheap!

                   the other thing it does is since wops come in order of reviewed longest ago, it
                   prioritizes those wops over other perhaps more recently reviewed ones. is this the algo we want? who knows. it's how it works right now though.
                -}
                |> List.take 100
    in
    lessonsByReviewDensity model.wops model.lessons dueForReviewList
