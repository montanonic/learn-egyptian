module SM2Flashcards exposing (SM2Data, SM2FlashcardData, SM2UserGrade)

{-| Only the SM2UserGrade data is needed to produce our SM2Data. This has the benefit of not storing
redundant data in exchange for CPU time. This benefit is worth it because having the full history of
responses and being able to recreate the full history of SM2Data allows for arbitrary analysis.
However, the following cost is incurred: the learner can no longer customize when they review their card. What if a mistake was made? Well I guess we could just go back and change the user response (always displaying "last response" somewhere in the UI and letting them edit it ("did you enter this by mistake? click to edit your last response")).
-}


type alias SM2FlashcardData =
    { egyptian : String, english : String, englishData : List SM2UserGrade, egyptianData : List SM2UserGrade }


{-| Technically, all that is needed is the algorithm and the user grade and everything else can be calculated, because we know the initial value. It is, in functional programming parlence, a scan operation. Perhaps then that is how we should actually store the data? Just as a list of user grades? Fun.

Thus the SM2Data record is fully derived from successive applications of the algorithm on our list of user data.

-}
type alias SM2Data =
    { correctAnswersInARow : Int, easeFactor : Float, daysUntilNextReview : Int }


{-| Ranges from 0 to 5. Not validated: design UI accordingly. Stores the time of grading
-}
type alias SM2UserGrade =
    Int
