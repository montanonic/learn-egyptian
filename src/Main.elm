port module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Dict.Extra as DictE
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import List.Extra as ListE
import Maybe.Extra as MaybeE
import Set exposing (Set)
import Utils exposing (slidingWindow)



-- MAIN


type alias Flags =
    { sm2FlashcardData : List SM2FlashcardData, lessons : List ( String, String ) }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


port storeFlashcardEntry : SM2FlashcardData -> Cmd msg



-- port storeNewLesson : ( String, String ) -> Cmd msg


port storeLessonData : List ( String, String ) -> Cmd msg


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MODEL


type alias Model =
    { draft : String
    , mainWords : List String -- all arabic
    , sm2FlashcardData : List SM2FlashcardData
    , flashcardEgyptianInput : String
    , flashcardEnglishInput : String

    -- lessons
    , newLessonText : String
    , newLessonTitle : String
    , lessons : Dict String String -- title -> text
    , selectedLesson : Maybe String
    }


init : Flags -> ( Model, Cmd Msg )
init { sm2FlashcardData, lessons } =
    ( { draft = ""
      , mainWords = []
      , sm2FlashcardData = sm2FlashcardData
      , flashcardEgyptianInput = ""
      , flashcardEnglishInput = ""
      , newLessonText = ""
      , newLessonTitle = ""
      , lessons = Dict.fromList lessons
      , selectedLesson = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = DraftChanged String
    | MainWordEntered
    | MainWordUp Int
    | MainWordDown Int
    | MainWordDelete Int
    | StoreSM2FlashcardEntry { egyptian : String, english : String }
    | FlashcardEgyptianInput String
    | FlashcardEnglishInput String
    | ChangeNewLessonText String
    | ChangeNewLessonTitle String
    | CreateNewLesson
    | UpdateLesson ( String, String )
    | SelectLesson String
    | DeselectLesson -- useful in this development design at least, not a good long-term design



-- Use the `sendMessage` port when someone presses ENTER or clicks
-- the "Send" button. Check out index.html to see the corresponding
-- JS where this is piped into a WebSocket.
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DraftChanged draft ->
            ( { model | draft = process draft }
            , Cmd.none
            )

        MainWordEntered ->
            pure { model | mainWords = model.mainWords ++ String.split " " model.draft, draft = "" }

        MainWordDelete i ->
            pure { model | mainWords = ListE.removeAt i model.mainWords }

        MainWordUp i ->
            -- [a, b, c, d, e]
            -- [a, c, b, d, e]
            pure { model | mainWords = ListE.swapAt (i - 1) i model.mainWords }

        MainWordDown i ->
            pure { model | mainWords = ListE.swapAt i (i + 1) model.mainWords }

        StoreSM2FlashcardEntry { egyptian, english } ->
            let
                entry =
                    { egyptian = egyptian, english = english, englishData = [], egyptianData = [] }
            in
            ( { model | flashcardEgyptianInput = "", flashcardEnglishInput = "", sm2FlashcardData = entry :: model.sm2FlashcardData }, storeFlashcardEntry entry )

        FlashcardEgyptianInput str ->
            pure { model | flashcardEgyptianInput = str }

        FlashcardEnglishInput str ->
            pure { model | flashcardEnglishInput = str }

        ChangeNewLessonText text ->
            pure { model | newLessonText = text }

        ChangeNewLessonTitle title ->
            pure { model | newLessonTitle = title }

        CreateNewLesson ->
            impure
                { model | newLessonText = "", newLessonTitle = "", lessons = Dict.insert model.newLessonTitle model.newLessonText model.lessons }
                (.lessons >> Dict.toList >> storeLessonData)

        UpdateLesson ( existingTitle, newTitle ) ->
            let
                processTitleChange : Model -> Model
                processTitleChange m =
                    if existingTitle /= newTitle then
                        { m
                            | lessons = Dict.remove existingTitle m.lessons
                            , selectedLesson = Just newTitle
                        }

                    else
                        m
            in
            impure
                ({ model | lessons = Dict.insert newTitle model.newLessonText model.lessons } |> processTitleChange)
                (.lessons >> Dict.toList >> storeLessonData)

        SelectLesson title ->
            pure
                { model
                    | selectedLesson = Just title
                    , newLessonText =
                        Dict.get title model.lessons
                            |> Maybe.withDefault ""
                    , newLessonTitle = title
                }

        DeselectLesson ->
            pure { model | selectedLesson = Nothing, newLessonText = "", newLessonTitle = "" }


pure : Model -> ( Model, Cmd Msg )
pure model =
    ( model, Cmd.none )


impure : Model -> (Model -> Cmd Msg) -> ( Model, Cmd Msg )
impure model effect =
    ( model, effect model )


{-| The process for in-place transliterating latin characters into corresponding arabic characters.

This needs to be bugfixed because the handling of the last character leads to issues like when a two-character code ends in a character that is also a single character code it outputs the two-character char followed by the single character char (yikes).

-}
process : String -> String
process str =
    {- expand the string into overlapping pairs of two, in addition to the final element by itself,
       this is how we can find a contiguous pair for more complex transliterations. we'll collapse
       the pairs back down at the end.

       due to arabic being RTL, we
    -}
    let
        unprocessedPairs =
            -- Debug.log "unprocessedPairs"
            slidingWindow { step = 1, size = 2 } (String.toList str)
                ++ (case ListE.last (String.toList str) of
                        Just lastChar ->
                            [ [ lastChar ] ]

                        -- this branch will only occur with an empty input box
                        Nothing ->
                            []
                   )

        {- Our `unprocessedPairs` consists of sublists of size 2, with a size 1 list at the end, so
           `List.head` will *always* match, and our `withDefault` uses a space char just to make the
           compiler happy.
        -}
        firstOfPair pair =
            List.head pair |> Maybe.withDefault ' '

        pairs : List (List Char)
        pairs =
            unprocessedPairs
                |> List.map
                    (\pair ->
                        {- find the pairs that begin with a non-arabic char -}
                        if not (Set.member (firstOfPair pair) supportedArabicChars) then
                            {- once found, it will either be a standalone char, or a pair of
                               non-arabic chars. we check for two-char matches first, because
                               otherwise a one-char match could 100% of the time prevent a two-char
                               match that starts with that char. if no two-char matches, then we
                               move on to checking one-char matching.

                               when we hit a match, we replace the pair with a singleton list of the
                               transliterated character. our last step takes all the first elements
                               of sublists and reconstructs the original, so no second element is
                               needed
                            -}
                            case Dict.get (String.fromList pair) twoLatinCharToArabicDict of
                                Just aMatch ->
                                    [ aMatch ]

                                Nothing ->
                                    {- if the pair didn't match, maybe the single character will -}
                                    case Dict.get (firstOfPair pair) oneLatinCharToArabicDict of
                                        Just aMatch ->
                                            [ aMatch ]

                                        Nothing ->
                                            {- if this first of pair is a standalone character that
                                               could complete a two-character rule, we keep it so
                                               that the user can complete the rule, otherwise we
                                               remove it
                                            -}
                                            if
                                                Dict.keys twoLatinCharToArabicDict
                                                    |> List.any
                                                        (\key ->
                                                            String.toList key
                                                                |> List.head
                                                                |> MaybeE.unwrap False
                                                                    (\ch -> ch == firstOfPair pair)
                                                        )
                                            then
                                                pair

                                            else
                                                []

                        else
                            {- if it starts with an arabic char we just preserve it -}
                            pair
                    )
    in
    {- now we collapse our overlapping windows back into text, where only the first char in each
       window needs to be selected, along with the last char in the last window: examine

        [[a, b], [b, c], [fixed], [], [e, f], [f]].

       fixed represents one or two chars that get transformed into updated text, [] represents
       removed text, and you can see that by selecting the first elements we restore the original
       list.
    -}
    List.map (\pair -> List.head pair) pairs
        -- collapse Maybe's, removing Nothing
        |> List.filterMap identity
        |> String.fromList


{-| Finds the first single or pair of latin chars.
-}
findFirstNonArabicChars : String -> Maybe ( Char, Maybe Char )
findFirstNonArabicChars str =
    let
        list =
            String.filter (\char -> not (Set.member char supportedArabicChars)) str
                |> String.toList
                |> List.take 2
    in
    ListE.getAt 0 list |> Maybe.map (\first -> ( first, ListE.getAt 1 list ))


{-| More specifically, keeps only the `supportedArabicChars`.
-}
trimNonArabicChars : String -> String
trimNonArabicChars =
    String.filter (\char -> Set.member char supportedArabicChars)


{-| our pool of arabic characters is derived from our dictionaries, ensuring that only the arabic
characters we actually support are used, and all others are removed.
-}
supportedArabicChars : Set Char
supportedArabicChars =
    (Dict.values oneLatinCharToArabicDict ++ Dict.values twoLatinCharToArabicDict)
        |> Set.fromList


arabicCharToLatinTransliterate : Dict Char String
arabicCharToLatinTransliterate =
    simpleMerge
        (DictE.invert oneLatinCharToArabicDict |> Dict.map (\_ v -> String.fromChar v))
        (DictE.invert twoLatinCharToArabicDict)


{-| Assumes dicts have no overlaps; prefers entries from dict1 if so.
-}
simpleMerge : Dict comparable a -> Dict comparable a -> Dict comparable a
simpleMerge d1 d2 =
    Dict.merge (\k v -> Dict.insert k v) (\k v1 _ -> Dict.insert k v1) (\k v -> Dict.insert k v) d1 d2 Dict.empty


{-| Dictionary of arabic letters that correspond to a single latin (English) character for
translitteration purposes.

The \\ character is a temporary hack to properly type the period character RTL style. There is no
arabic period char, so you need to use the unicode for arabic script preceding it, but we're using
the Char datatype, and that combo results in two codes. The solution is to not use the Char type for
our outputs.

-}
oneLatinCharToArabicDict : Dict Char Char
oneLatinCharToArabicDict =
    Dict.fromList [ ( ' ', ' ' ), ( '\\', '\u{061C}' ), ( '.', '.' ), ( ',', '،' ), ( '?', '؟' ), ( 'a', 'َ' ), ( 'A', 'ع' ), ( 'b', 'ب' ), ( 'd', 'د' ), ( 'D', 'ض' ), ( 'e', 'ِ' ), ( 'f', 'ف' ), ( 'g', 'ج' ), ( 'G', 'غ' ), ( 'h', 'ه' ), ( 'H', 'ح' ), ( 'i', 'ِ' ), ( 'k', 'ك' ), ( 'K', 'خ' ), ( 'l', 'ل' ), ( 'm', 'م' ), ( 'n', 'ن' ), ( 'o', 'ُ' ), ( 'q', 'ق' ), ( 'r', 'ر' ), ( 's', 'س' ), ( 'S', 'ص' ), ( 't', 'ت' ), ( 'u', 'ُ' ), ( 'w', 'و' ), ( 'W', 'ّ' ), ( 'y', 'ي' ), ( 'Y', 'ى' ), ( 'z', 'ز' ), ( 'Z', 'ظ' ) ]


{-| Dictionary of arabic letters that correspond to a combination of two latin (English) characters
for translitteration purposes. To make conversion unambiguous, all of these rules do not begin with
any of the characters listed in `oneLatinCharToArabicDict`.
-}
twoLatinCharToArabicDict : Dict String Char
twoLatinCharToArabicDict =
    Dict.fromList [ ( "; ", '؛' ), ( ";-", 'ا' ), ( ";a", 'أ' ), ( ";i", 'إ' ), ( ";e", 'إ' ), ( ";u", 'أ' ), ( ";~", 'آ' ), ( "c-", 'ء' ), ( "'m", 'ة' ), ( "'d", 'ذ' ), ( "'t", 'ث' ), ( "'s", 'ش' ) ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Learn Egyptian" ]
        , newAndEditLessonView model
        , lessonsView model
        , case model.selectedLesson of
            Nothing ->
                span [] []

            Just title ->
                selectedLessonView model title
        ]


newLessonView : Model -> Html Msg
newLessonView model =
    let
        creationDisabled =
            (model.newLessonText == "")
                || (model.newLessonTitle == "")
                || (Dict.get model.newLessonTitle model.lessons /= Nothing)
    in
    div [ class "new-lesson-view" ]
        [ label [] [ text "Make a new lesson" ]
        , input [ placeholder "title", value model.newLessonTitle, onInput ChangeNewLessonTitle ] []
        , textarea [ rows 15, cols 60, onInput ChangeNewLessonText, value model.newLessonText ] []
        , button [ onClick CreateNewLesson, disabled creationDisabled ] [ text "Create Lesson" ]
        , h3 [] [ text "Preview:" ]
        , div []
            (model.newLessonText
                |> String.split "\n"
                |> List.map (\line -> p [] [ text line ])
            )
        ]


{-| Covers creating lessons and editing them.
-}
newAndEditLessonView : Model -> Html Msg
newAndEditLessonView model =
    let
        buttonDisabled =
            case model.selectedLesson of
                Nothing ->
                    (model.newLessonText == "")
                        || (model.newLessonTitle == "")
                        -- don't allow creation if it conflicts with an existing lesson
                        || (Dict.get model.newLessonTitle model.lessons /= Nothing)

                {- disable the button if all fields stayed the same, or if a new title conflicts -}
                Just title ->
                    (Dict.get title model.lessons == Just model.newLessonText)
                        && (title == model.newLessonTitle || Dict.get model.newLessonTitle model.lessons /= Nothing)
    in
    div [ class "new-lesson-view" ]
        [ case model.selectedLesson of
            Nothing ->
                label [] [ text "Make a new lesson" ]

            Just _ ->
                div []
                    [ button [ onClick DeselectLesson ] [ text "Deselect Lesson" ]
                    , label [] [ text <| "Edit lesson" ]
                    ]
        , input [ placeholder "title", value model.newLessonTitle, onInput ChangeNewLessonTitle ] []
        , textarea [ rows 15, cols 60, onInput ChangeNewLessonText, value model.newLessonText ] []
        , case model.selectedLesson of
            Nothing ->
                button [ onClick CreateNewLesson, disabled buttonDisabled ] [ text "Create Lesson" ]

            Just title ->
                button [ onClick <| UpdateLesson ( title, model.newLessonTitle ), disabled buttonDisabled ] [ text "Update Lesson" ]
        ]


lessonsView : Model -> Html Msg
lessonsView model =
    div [ class "lessons-view" ]
        [ h3 [] [ text "select a lesson" ]
        , div [ style "display" "flex" ]
            (model.lessons
                |> Dict.keys
                |> List.map (\title -> button [ onClick <| SelectLesson title ] [ text title ])
            )
        ]


selectedLessonView : Model -> String -> Html Msg
selectedLessonView model title =
    let
        lessonText =
            Dict.get title model.lessons |> Maybe.withDefault ""
    in
    div [ class "selected-lesson-view" ]
        [ h2 [] [ text <| "Title: " ++ title ]
        , displayWords model lessonText
        ]


{-| This is how we embellish our words with all of the nice app functionality.
-}
displayWords : Model -> String -> Html Msg
displayWords _ lessonText =
    div []
        (lessonText
            |> String.split "\n"
            |> List.map (\line -> p [] [ text line ])
        )


oldView : Model -> Html Msg
oldView model =
    div []
        [ h2 [] [ text "Enter Vocab (Eg, Eng)" ]
        , input [ onInput FlashcardEgyptianInput, value model.flashcardEgyptianInput ] []
        , input [ onInput FlashcardEnglishInput, value model.flashcardEnglishInput ] []
        , button
            [ onClick <|
                StoreSM2FlashcardEntry
                    { egyptian = model.flashcardEgyptianInput, english = model.flashcardEnglishInput }
            ]
            [ text "add to SM2 vocab" ]
        , ul []
            (List.map
                (\flash -> li [ style "display" "flex" ] [ div [ style "margin-right" "8px" ] [ text flash.egyptian ], div [] [ text flash.english ] ])
                model.sm2FlashcardData
            )
        , h2 [] [ text "Type Egyptian" ]
        , input
            [ type_ "text"
            , placeholder "Draft"
            , onInput DraftChanged
            , value model.draft
            , style "font-size" "16px"
            , attribute "dir" "rtl"
            , on "keydown" (ifIsEnter MainWordEntered)
            ]
            []
        , h3 [] [ text "Full Sentence:" ]
        , p [ style "font-size" "26px", attribute "dir" "rtl" ] [ text <| String.join " " <| model.mainWords ]
        , ul []
            (List.indexedMap
                (\i word ->
                    let
                        btn a b =
                            button (style "height" "18px" :: a) b
                    in
                    div [ style "display" "flex" ]
                        [ li [ style "font-size" "20px", style "margin-right" "8px" ] [ text word ]
                        , btn [ onClick <| MainWordUp i ] [ text "Up" ]
                        , btn [ onClick <| MainWordDown i ] [ text "Down" ]
                        , btn [ onClick <| MainWordDelete i ] [ text "Delete" ]
                        ]
                )
                model.mainWords
            )
        ]



-- DETECT ENTER


ifIsEnter : msg -> D.Decoder msg
ifIsEnter msg =
    D.field "key" D.string
        |> D.andThen
            (\key ->
                if key == "Enter" then
                    D.succeed msg

                else
                    D.fail "some other key"
            )
