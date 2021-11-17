port module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyPress, onMouseMove)
import Debug exposing (toString)
import Dict exposing (Dict)
import Dict.Extra as DictE
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as ListE
import List.Zipper as Zipper exposing (Zipper)
import Maybe.Extra as MaybeE
import Random
import Random.List as RandomList
import Regex
import SM2Flashcards exposing (SM2FlashcardData)
import Set exposing (Set)
import Utils
import WordOrPhrase as WOP exposing (WOP, update)



-- MAIN


type alias Flags =
    { sm2FlashcardData : List SM2FlashcardData
    , lessons : List ( String, Lesson )
    , lessonTranslations : List ( String, String )
    , wops : List ( String, WOP )
    , newWopFlashcards : Maybe { before : List WOP, current : WOP, after : List WOP }
    }


type alias Lesson =
    { text : String
    , audioFileType : String
    }


{-| Default `audioFileType` is "wav".
-}
makeLesson : String -> Lesson
makeLesson text =
    { text = text, audioFileType = "wav" }


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


port storeLessonTranslations : List ( String, String ) -> Cmd msg


port storeLessons : List ( String, Lesson ) -> Cmd msg


port storeWops : List ( String, WOP ) -> Cmd msg


port storeNewWopFlashcards : Maybe { before : List WOP, current : WOP, after : List WOP } -> Cmd msg


port saveLocalStorageToClipboard : () -> Cmd msg


port mouseMoveImageLesson : Decode.Value -> Cmd msg


{-| Focus on the html element with the given id.
-}
port autofocusId : String -> Cmd msg


port receiveKalimniEvents : (KalimniEvent -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { drawInLesson } =
    {- TODO: improve deselection of words ; currently it's too broad and even clicking on the word edit interface deselects it. -}
    -- if not (String.isEmpty model.selectedWop) then
    --     Browser.Events.onMouseDown (D.succeed DeselectWord)
    -- else
    Sub.batch
        [ onKeyPress keyDecoder
        , if drawInLesson then
            receiveKalimniEvents GotKalimniEvent

          else
            Sub.none
        ]



{- so to construct absolutely position rectangles, we need to know where, proportionally on the
   image, the rectangle should start drawing, and where it should end drawing. In short: we need the
   start and end corners. because rectangles are always drawn in the browser from the top-left, if
   the datums we receive are the bottom left corner and the top right corner, we'll need to compute
   the origin of the top-left corner from that. the top-left, by definition, is a value with min Y
   (top of a browser is Y 0, so we go the opposite to math graphs), and min X.

   I'll make it easy for myself for now and only give it in top-left to bottom-right order.

   both clicks must have come from events of same width and height otherwise the proportions will be
   messed up.
-}


type alias KalimniEvent =
    { relativeX : Float
    , relativeY : Float
    , width : Int
    , height : Int
    , naturalWidth : Int
    , naturalHeight : Int
    }


type alias KalimniLessonPage =
    { naturalWidth : Int
    , naturalHeight : Int
    , currentScaling : Float
    , textBoxes : List LessonTextBox
    }


{-| ALl position data is stored as relative proportions, so that if the image is scaled, the text
can naturally be scaled along with it.
-}
type alias LessonTextBox =
    { topLeft : ( Float, Float )
    , bottomRight : ( Float, Float )
    , text : String
    }


{-| Converts from relative coords back to actual pixel values.
-}
textBoxGetTopLeftCoord : KalimniLessonPage -> LessonTextBox -> ( String, String )
textBoxGetTopLeftCoord { currentScaling, naturalWidth, naturalHeight } { topLeft } =
    let
        ( tlX, tlY ) =
            topLeft
    in
    ( tlY * currentScaling * toFloat naturalWidth |> round |> intToPx
    , tlX * currentScaling * toFloat naturalHeight |> round |> intToPx
    )


lessonTextBoxWidthAndHeight : KalimniLessonPage -> LessonTextBox -> List (Html.Attribute msg)
lessonTextBoxWidthAndHeight { currentScaling, naturalWidth, naturalHeight } { topLeft, bottomRight } =
    let
        ( ( tlX, tlY ), ( brX, brY ) ) =
            ( topLeft, bottomRight )
    in
    [ style "width" <| intToPx <| round <| (brX - tlX) * toFloat naturalWidth * currentScaling
    , style "height" <| intToPx <| round <| (brY - tlY) * toFloat naturalHeight * currentScaling
    ]


intToPx : Int -> String
intToPx n =
    toString n ++ "px"



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
    , lessons : Dict String Lesson -- title -> text
    , lessonTranslations : Dict String String -- title -> translation
    , selectedLesson : String
    , selectedWop : String -- just the key (AKA the stringified word or phrase), for lookup in the wops Dict
    , selectedWopTagsBuffer : String
    , wops : Dict String WOP
    , newWopDefinition : String
    , mouseDownWord : ( Int, Int, String ) -- line index (which line), word index (which word within line)
    , hoveredWord : String

    -- new flashcard impl
    , onFlashcardPage : Bool
    , newWopFlashcards : Maybe (Zipper WOP)

    -- kalimni stuff
    , lastKalimniEvent : Maybe KalimniEvent
    , selectedKalimniLessonPage : Maybe KalimniLessonPage
    , drawInLesson : Bool
    , drawRectangleKalimniBuffer : Maybe KalimniEvent
    , startingRectangleCoordinate : Maybe ( Float, Float )
    }


init : Flags -> ( Model, Cmd Msg )
init { sm2FlashcardData, lessons, wops, lessonTranslations, newWopFlashcards } =
    ( { draft = ""
      , mainWords = []
      , sm2FlashcardData = sm2FlashcardData
      , flashcardEgyptianInput = ""
      , flashcardEnglishInput = ""
      , newLessonText = ""
      , newLessonTitle = ""
      , lessons = Dict.fromList lessons
      , lessonTranslations = Dict.fromList lessonTranslations
      , selectedLesson = ""
      , selectedWop = ""
      , selectedWopTagsBuffer = ""
      , wops = Dict.fromList wops
      , newWopDefinition = ""
      , mouseDownWord = ( 0, 0, "" )
      , hoveredWord = ""
      , onFlashcardPage = False
      , newWopFlashcards = Maybe.map (\{ before, current, after } -> Zipper.from before current after) newWopFlashcards

      -- kalimni stuff
      , lastKalimniEvent = Nothing
      , selectedKalimniLessonPage =
            Just
                { naturalWidth = 1000
                , naturalHeight = 400
                , currentScaling = 1.0
                , textBoxes = []
                }
      , drawInLesson = False
      , drawRectangleKalimniBuffer = Nothing
      , startingRectangleCoordinate = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SaveDataModelToClipboard
      -- Flashcard stuff, currently not used *at all*, and needs to be repurposed.
    | DraftChanged String
    | MainWordEntered
    | MainWordUp Int
    | MainWordDown Int
    | MainWordDelete Int
    | StoreSM2FlashcardEntry { egyptian : String, english : String }
    | FlashcardEgyptianInput String
    | FlashcardEnglishInput String
      -- Lesson stuff
    | ChangeNewLessonText String
    | ChangeNewLessonTitle String
    | CreateNewLesson
    | UpdateLesson ( String, String )
    | ChangeLessonAudioFileType String
    | SelectLesson String
    | DeselectLesson -- useful in this development design at least, not a good long-term design
    | BackendAudioUpdated (Result Http.Error String)
    | SelectWord String
    | EditSelectedWOPDefinition Int String
    | EditSelectedWOPNotes String
    | SetSelectedWOPFamiliarityLevel Int
    | EditSelectedWOPRomanization String
    | EditSelectedWOPTagsBuffer String
    | SetSelectedWOPTags String
    | DeselectWOP
    | SaveSelectedNewWOP
    | EditSelectedNewWOPDefinition String
    | AddTranslationToSelectedLesson
    | EditTranslationOfSelectedLesson String
    | MouseDownOnWord Int Int String -- used to start phrase creation
    | OpenPhraseCreationUI Int Int String
      -- new flashcard stuff
    | NavigateToFlashcardPage -- open up the flashcard view
    | MakeNewWopFlashcardSet (Maybe (List WOP)) (Random.Generator (List WOP))
    | NextFlashcard
      -- kalimni arabi stuff
    | GotKalimniEvent KalimniEvent
    | ToggleDrawInLesson
    | LessonImageClick
      -- misc (currently uncategorized)
    | KeyPress String
    | WordHoverStart String
    | WordHoverLeave



-- Use the `sendMessage` port when someone presses ENTER or clicks
-- the "Send" button. Check out index.html to see the corresponding
-- JS where this is piped into a WebSocket.
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotKalimniEvent kalimniEvent ->
            pure { model | lastKalimniEvent = Just kalimniEvent }

        ToggleDrawInLesson ->
            pure
                (if model.drawInLesson then
                    { model | drawInLesson = not model.drawInLesson }

                 else
                    -- start drawing, with empty rectangle coord buffer
                    { model
                        | drawInLesson = not model.drawInLesson
                        , startingRectangleCoordinate = Nothing
                    }
                )

        LessonImageClick ->
            if model.drawInLesson then
                case model.startingRectangleCoordinate of
                    Just topLeft ->
                        model.lastKalimniEvent
                            |> Maybe.map
                                (\{ relativeX, relativeY } ->
                                    { model
                                        | selectedKalimniLessonPage =
                                            Maybe.map
                                                (\lessonPage ->
                                                    { lessonPage | textBoxes = { topLeft = topLeft, bottomRight = ( relativeX, relativeY ), text = "" } :: lessonPage.textBoxes }
                                                )
                                                model.selectedKalimniLessonPage
                                                |> Debug.log "blah"
                                        , startingRectangleCoordinate = Nothing
                                    }
                                )
                            |> Maybe.withDefault model
                            |> pure

                    Nothing ->
                        pure
                            { model
                                | startingRectangleCoordinate =
                                    Maybe.andThen
                                        (\ke ->
                                            Just ( ke.relativeX, ke.relativeY )
                                        )
                                        model.lastKalimniEvent
                            }

            else
                pure model

        KeyPress key ->
            {- handle pressing #'s 1 through 4 to change familiarity level -}
            if (not <| String.isEmpty model.selectedWop) && model.selectedWop == model.hoveredWord then
                String.toInt key
                    |> Maybe.andThen
                        (\n ->
                            if n >= 1 && n <= 4 then
                                Just <| update (SetSelectedWOPFamiliarityLevel n) model

                            else
                                Nothing
                        )
                    |> MaybeE.unwrap (pure model) identity

            else
                pure model

        WordHoverStart word ->
            pure { model | hoveredWord = word }

        WordHoverLeave ->
            pure { model | hoveredWord = "" }

        SaveDataModelToClipboard ->
            ( model, saveLocalStorageToClipboard () )

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
                { model
                    | newLessonText = ""
                    , newLessonTitle = ""
                    , lessons =
                        Dict.update model.newLessonTitle
                            (Maybe.map (\lesson -> { lesson | text = model.newLessonText }))
                            model.lessons
                }
                (.lessons >> Dict.toList >> storeLessons)

        {- BUG: When lesson title changes, the ordering of model updates leads to the active lesson
           being deselected. This is more of a problem with our conflation of viewing a lesson and editing a lesson, the sturucture isn't very well-conceived.
        -}
        UpdateLesson ( existingTitle, newTitle ) ->
            let
                processTitleChange : Model -> Model
                processTitleChange m =
                    if existingTitle /= newTitle then
                        { m
                            | lessons = Dict.remove existingTitle m.lessons
                            , selectedLesson = newTitle
                            , lessonTranslations = Dict.remove existingTitle m.lessonTranslations
                        }

                    else
                        m

                existingTranslation =
                    Dict.get existingTitle model.lessonTranslations

                newModel =
                    { model
                        | lessons =
                            Dict.update newTitle
                                (Maybe.map (\lesson -> { lesson | text = model.newLessonText }))
                                model.lessons
                        , lessonTranslations =
                            existingTranslation
                                |> Maybe.map (\et -> Dict.insert newTitle et model.lessonTranslations)
                                |> Maybe.withDefault model.lessonTranslations
                    }
                        |> processTitleChange

                {- audios are tied to lessons only by title, so when the title changes we need to change the file too -}
                updateAudioName =
                    if existingTitle /= newTitle then
                        Http.post
                            { url = "http://localhost:3000/updateAudioName"
                            , body =
                                Http.jsonBody <|
                                    Encode.object
                                        [ ( "old", Encode.string existingTitle )
                                        , ( "new", Encode.string newTitle )
                                        , ( "type"
                                          , Encode.string
                                                (newModel.lessons
                                                    |> Dict.get newTitle
                                                    |> Maybe.map .audioFileType
                                                    |> Maybe.withDefault "wav"
                                                )
                                          )
                                        ]
                            , expect = Http.expectString BackendAudioUpdated
                            }

                    else
                        Cmd.none
            in
            ( newModel
            , Cmd.batch
                [ storeLessons (newModel.lessons |> Dict.toList)
                , storeLessonTranslations (newModel.lessonTranslations |> Dict.toList)
                , updateAudioName
                ]
            )

        ChangeLessonAudioFileType newFileType ->
            impure
                { model
                    | lessons =
                        Dict.update model.selectedLesson
                            (Maybe.map (\lesson -> { lesson | audioFileType = newFileType }))
                            model.lessons
                }
                (.lessons >> Dict.toList >> storeLessons)

        BackendAudioUpdated response ->
            -- let
            --     _ =
            --         Debug.log "BackendAudioUpdated" response
            -- in
            pure model

        SelectLesson title ->
            pure
                { model
                    | selectedLesson = title
                    , newLessonText =
                        Dict.get title model.lessons
                            |> Maybe.map .text
                            |> Maybe.withDefault ""
                    , newLessonTitle = title
                    , selectedWop = ""
                }

        DeselectLesson ->
            pure { model | selectedLesson = "", newLessonText = "", newLessonTitle = "", selectedWop = "" }

        SelectWord word ->
            impure { model | selectedWop = word, selectedWopTagsBuffer = "" }
                (\{ selectedWop } ->
                    -- autofocus the definition field if the word is not known
                    case WOP.get selectedWop model.wops of
                        Just _ ->
                            Cmd.none

                        Nothing ->
                            autofocusId "newWopDefinition"
                )

        DeselectWOP ->
            pure { model | selectedWop = "" }

        EditSelectedWOPDefinition defNumber definition ->
            impure
                { model
                    | wops =
                        WOP.update model.selectedWop
                            (Maybe.map (WOP.setDefinition defNumber definition))
                            model.wops
                }
                (.wops >> Dict.toList >> storeWops)

        {- TODO: This really should have debouncing on storing the word because it's an expensive op. -}
        EditSelectedWOPNotes notes ->
            impure
                { model
                    | wops =
                        WOP.update model.selectedWop
                            (Maybe.map (WOP.setNotes notes))
                            model.wops
                }
                (.wops >> Dict.toList >> storeWops)

        SetSelectedWOPFamiliarityLevel familiarityLevel ->
            impure
                { model
                    | wops =
                        WOP.update model.selectedWop
                            (Maybe.andThen (WOP.setFamiliarityLevel familiarityLevel))
                            model.wops
                }
                (.wops >> Dict.toList >> storeWops)

        EditSelectedWOPRomanization romanization ->
            impure
                { model
                    | wops =
                        WOP.update model.selectedWop
                            (Maybe.map (\wop -> { wop | romanization = romanization }))
                            model.wops
                }
                (.wops >> Dict.toList >> storeWops)

        EditSelectedWOPTagsBuffer tagString ->
            pure { model | selectedWopTagsBuffer = tagString }

        SetSelectedWOPTags tagString ->
            impure
                { model
                    | wops =
                        WOP.update model.selectedWop
                            (Maybe.map (WOP.setTags tagString))
                            model.wops
                    , selectedWopTagsBuffer = ""
                }
                (.wops >> Dict.toList >> storeWops)

        EditSelectedNewWOPDefinition definition ->
            pure { model | newWopDefinition = definition }

        SaveSelectedNewWOP ->
            let
                wopKeyList =
                    if WOP.keyIsPhrase model.selectedWop then
                        String.split " " model.selectedWop

                    else
                        [ model.selectedWop ]
            in
            impure
                { model
                    | wops =
                        WOP.insert model.selectedWop
                            (WOP.makeWOP wopKeyList model.newWopDefinition)
                            model.wops
                    , newWopDefinition = ""
                }
                (.wops >> Dict.toList >> storeWops)

        AddTranslationToSelectedLesson ->
            pure { model | lessonTranslations = Dict.insert model.selectedLesson "" model.lessonTranslations }

        EditTranslationOfSelectedLesson newTranslation ->
            impure { model | lessonTranslations = Dict.insert model.selectedLesson newTranslation model.lessonTranslations } (.lessonTranslations >> Dict.toList >> storeLessonTranslations)

        MouseDownOnWord lineIndex wordIndex word ->
            pure { model | mouseDownWord = ( lineIndex, wordIndex, word ) }

        OpenPhraseCreationUI lineIndexEnd wordIndexEnd _ ->
            let
                ( lineIndexStart, wordIndexStart, _ ) =
                    model.mouseDownWord
            in
            if (wordIndexEnd /= wordIndexStart) && (lineIndexEnd == lineIndexStart) then
                {- in this branch we've guaranteed the start and end words are different (well, technically the word might be repeated but they're separate instances). also the line of text is the same (it seems like a bad idea to create a phrase across sentence boundaries...). thus we construct a phrase from the words in between. -}
                let
                    selectedLessonText =
                        Dict.get model.selectedLesson model.lessons |> Maybe.map .text |> Maybe.withDefault ""

                    line =
                        ListE.getAt lineIndexEnd (splitIntoCleanLines selectedLessonText)
                            |> Maybe.withDefault ""

                    {- so we can get all the words from the start word to the end word, but we also want to ensure there's nothing in-between (except spaces, which we'll filter out (inelegantly) here) -}
                    wordsAndNonWordsInPhraseSegment =
                        let
                            {- this ensures that we can select in the reverse direction just as well -}
                            smaller =
                                Basics.min wordIndexStart wordIndexEnd

                            larger =
                                Basics.max wordIndexStart wordIndexEnd
                        in
                        markWordCharsFromNonWordChars line
                            |> List.drop smaller
                            |> List.take (larger - smaller + 1)
                            -- we need to filter AFTER the list manipulations because spaces alter the original indices of our words... things could be better, but this is the way it is with the code as it is.
                            |> List.filter (\wordOrNonWord -> wordOrNonWord /= DisplayNonWord " ")

                    {- in this final step, we want to ensure there are no non-words present (remember, spaces, which are fine, have already been removed) -}
                    canConstructPhrase =
                        List.all
                            (\wordOrNonWord ->
                                case wordOrNonWord of
                                    DisplayNonWord _ ->
                                        False

                                    -- even a single non-word here should prevent us from constructing the phrase
                                    _ ->
                                        True
                            )
                            wordsAndNonWordsInPhraseSegment
                in
                if canConstructPhrase then
                    pure
                        { model
                            | selectedWop =
                                wordsAndNonWordsInPhraseSegment
                                    |> List.map
                                        (\word ->
                                            case word of
                                                DisplayWord w ->
                                                    w

                                                _ ->
                                                    ""
                                        )
                                    |> String.join " "
                        }

                else
                    pure model

            else
                pure model

        NavigateToFlashcardPage ->
            pure { model | onFlashcardPage = True }

        MakeNewWopFlashcardSet maybeRandomizedWops randomizedWopsGenerator ->
            case maybeRandomizedWops of
                Just wops ->
                    {- for now let's limit flashcard pools to 100 -}
                    impure { model | newWopFlashcards = Zipper.fromList (List.take 100 wops) }
                        (.newWopFlashcards >> Maybe.map Utils.deconstructZipper >> storeNewWopFlashcards)

                Nothing ->
                    ( model
                    , Random.generate
                        (\wops ->
                            MakeNewWopFlashcardSet
                                (Just wops)
                                randomizedWopsGenerator
                        )
                        randomizedWopsGenerator
                    )

        NextFlashcard ->
            impure { model | newWopFlashcards = Maybe.andThen Zipper.next model.newWopFlashcards }
                (.newWopFlashcards >> Maybe.map Utils.deconstructZipper >> storeNewWopFlashcards)


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
            ListE.groupsOfWithStep 2 1 (String.toList str)
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



{- lesson and word audio -}
{- what if mousing over the period, question-mark, and exclamation-mark punctuation highlighted it
   in green with cursor:pointer, and clicking it played the audio of the sentence? or maybe pressing
   'p' on the start word and releasing it on the end word (or the inverse) will play audio between
   those sections. perhaps double-clicking a word could play audio from it up to the end of the
   sentence?

    so lots of options for an audio UI.

    now what about associating words with audio so that when clicked on they play it? I'll want a
    long bar UI of the track audio, and then maybe a smaller UI for the 5 seconds before and after
    the current point in the overall audio. this zoomed in view is where you can scrub left and
    right to get to the exact word boundary.

    though that will fundamentally be less reliable than sentence boundaries. okay, so let's figure
    out this one. so I start the audio, and bam, the first sentence is said. now maybe I'm holding
    the mouse to play audio, and releasing it stops it. then I have a nice UI bar for scrubbing back
    and forth in time to get the end just right. a 1-second loop where the goal is to have the end
    of it be the end of the finished word? maybe it's 1 second but the audio is half speed.

    so I mark the boundary, by clicking, and there's scrub buttons for jumping left or right by
    100ms or 10ms. but those could apply to either the overarching audio or to the sentence boundary
    marker.
-}
-- VIEW


view : Model -> Html Msg
view model =
    if model.onFlashcardPage then
        flashcardView model

    else
        div []
            [ h2 [] [ text "Learn Egyptian" ]

            -- , Maybe.map (\lesson -> imageLessonPage lesson model) model.selectedKalimniLessonPage
            --     |> Maybe.withDefault (span [] [])
            , button [ onClick SaveDataModelToClipboard ]
                [ text "Save Data Model to Clipboard (4MB limit)" ]
            , newAndEditLessonView model
            , button [ onClick NavigateToFlashcardPage ] [ text "Go to Flashcards Page" ]
            , lessonsView model
            , if String.isEmpty model.selectedLesson then
                span [] []

              else
                selectedLessonView model model.selectedLesson
            ]


imageLessonPage : KalimniLessonPage -> Model -> Html Msg
imageLessonPage lessonPage model =
    let
        displayLessonBoxes =
            lessonPage.textBoxes
                |> List.map
                    (\textBox ->
                        let
                            ( top, left ) =
                                textBoxGetTopLeftCoord lessonPage textBox
                        in
                        div
                            ([ class "lesson-text-box"
                             , style "top" top
                             , style "left" left
                             ]
                                ++ lessonTextBoxWidthAndHeight lessonPage textBox
                            )
                            [ text "new box" ]
                    )
    in
    div [ class "image-lesson-page" ]
        [ button [ onClick ToggleDrawInLesson ]
            [ if model.drawInLesson then
                text "Stop Drawing"

              else
                text "Draw in Lesson"
            ]
        , div [ class "image-lesson-view" ]
            ([ img
                [ onClick LessonImageClick
                , id "img"
                , src "https://www.splitbrain.org/_media/blog/2010-06/ocr/verdana-black-300.png"
                ]
                []
             ]
                ++ displayLessonBoxes
            )
        ]


flashcardView : Model -> Html Msg
flashcardView model =
    div [ class "flashcard-page" ]
        [ case model.newWopFlashcards of
            Just newWopFlashcards ->
                let
                    wop =
                        Zipper.current newWopFlashcards

                    currentFlashcardNumber =
                        Zipper.before newWopFlashcards |> List.length |> (+) 1 |> String.fromInt

                    totalSize =
                        Zipper.toList newWopFlashcards |> List.length |> String.fromInt
                in
                div []
                    [ p [ class "word new" ] [ text (String.join ", " wop.wordOrPhrase) ]
                    , p []
                        [ text (List.head wop.definitions |> Maybe.withDefault "no definition") ]
                    , button
                        [ onClick NextFlashcard ]
                        [ text "Next Card" ]
                    , p [] [ text ("on flashcard " ++ currentFlashcardNumber ++ "/" ++ totalSize) ]
                    , getExampleSentenceForWop wop (Dict.values model.lessons |> List.map .text) model.wops
                    ]

            Nothing ->
                button
                    [ onClick
                        (MakeNewWopFlashcardSet Nothing
                            (RandomList.shuffle <| WOP.listWopsOfLevel 1 model.wops)
                        )
                    ]
                    [ text "Start a new set of new wops" ]
        ]


{-| So this is the fun part of a flashcard: looking up an example sentence for it in the lessons.

This is a simple algorithm that will return on the first match. If lessons are provided in a
randomized order, the sentence used will differ. In fact, if given a random order list, we can just
cycle the list at the point of every match and really try to ensure that there's lesson variety in
the flashcards.

That's not a big deal to me right now so I'll omit that.

Returning a String would be nice for composing, but it's actually a bit easier to just render the
html here for now, which is what I'll do.

A more useful search strategy could involve generating a very large dict iteratively. The Dict would
map wops to a list of lessons that contain them, perhaps also (nicely) alongside the indices at
which the WOP can be found. The Dict.update function would be crucial here, and we'd just do that in
a long reduce over lessons. The end result being a dict where we just give it the WOP, and we can
immediately see every single lesson it belongs to, and every position it takes within that lesson.
Pretty cool! But I'll stick with this v1.0 implementation for now cuz it was faster to conceptualize
and get running.

-}
getExampleSentenceForWop : WOP -> List String -> Dict String WOP -> Html Msg
getExampleSentenceForWop wop lessons wops =
    if WOP.isPhrase wop then
        p [] [ text "phrases currently unsupported" ]

    else
        let
            word =
                String.join " " wop.wordOrPhrase

            lessonContainingWord =
                ListE.find
                    (\lesson ->
                        markWordCharsFromNonWordChars lesson
                            |> List.any
                                (\displayWord ->
                                    case displayWord of
                                        DisplayWord lessonWord ->
                                            WOP.tashkylEquivalent word lessonWord

                                        _ ->
                                            False
                                )
                    )
                    lessons

            fullSentenceContainingWord =
                Maybe.map
                    (\lesson ->
                        let
                            ( beforeMatch, withMatch ) =
                                markWordCharsFromNonWordChars lesson
                                    |> ListE.splitWhen
                                        (\displayWord ->
                                            case displayWord of
                                                DisplayWord lessonWord ->
                                                    not (WOP.tashkylEquivalent word lessonWord)

                                                _ ->
                                                    True
                                        )
                                    {- we know there's a match since the lesson was fetched from the
                                       criteria of containing the target word
                                    -}
                                    |> Maybe.withDefault ( [], [] )

                            ( match, afterMatch ) =
                                ListE.uncons withMatch |> Maybe.withDefault ( DisplayWord "", [] )

                            terminalPunctuationRx =
                                ".?؟!" |> Regex.fromString |> Maybe.withDefault Regex.never

                            isSentenceTerminal displayWord =
                                case displayWord of
                                    DisplayNonWord nonWord ->
                                        -- keep going as long as we don't hit terminal punctuation
                                        Regex.contains terminalPunctuationRx nonWord

                                    _ ->
                                        True

                            sentenceBeforeMatch =
                                ListE.takeWhileRight (not << isSentenceTerminal) beforeMatch

                            sentenceAfterMatch =
                                ListE.findIndex isSentenceTerminal afterMatch
                                    |> Maybe.withDefault 0
                                    |> (\endIndex -> List.take (endIndex + 1) afterMatch)
                        in
                        List.concat [ sentenceBeforeMatch, [ match ], sentenceAfterMatch ]
                    )
                    lessonContainingWord

            wordFamiliarityClass =
                class <| String.toLower <| WOP.displayFamiliarityLevel (WOP.getFamiliarityLevel word wops)
        in
        case fullSentenceContainingWord of
            Nothing ->
                div [] [ text word ]

            Just exampleSentence ->
                div [ class "example-sentence" ]
                    (List.map
                        (\displayWordInSentence ->
                            case displayWordInSentence of
                                DisplayWord wordInSentence ->
                                    if WOP.tashkylEquivalent wordInSentence word then
                                        span [ class "target-word-in-sentence", wordFamiliarityClass ] [ text word ]

                                    else
                                        span [ wordFamiliarityClass ] [ text wordInSentence ]

                                DisplayNonWord nonWordInSentence ->
                                    span [ class "non-word" ] [ text nonWordInSentence ]

                                _ ->
                                    div [] [ text "INVALID" ]
                        )
                        exampleSentence
                    )


{-| Covers creating lessons and editing them.
-}
newAndEditLessonView : Model -> Html Msg
newAndEditLessonView model =
    let
        buttonDisabled =
            if String.isEmpty model.selectedLesson then
                -- in this case we're creating a new lesson
                (model.newLessonText == "")
                    || (model.newLessonTitle == "")
                    -- don't allow creation if it conflicts with an existing lesson
                    || (Dict.get model.newLessonTitle model.lessons /= Nothing)
                {- disable the button if all fields stayed the same, or if a new title conflicts -}

            else
                -- otherwise we're updating an existing lesson if we click this button
                ((Dict.get model.selectedLesson model.lessons |> Maybe.map .text) == Just model.newLessonText)
                    && (model.selectedLesson == model.newLessonTitle || Dict.get model.newLessonTitle model.lessons /= Nothing)

        lessonFileType =
            Dict.get model.selectedLesson model.lessons |> Maybe.map .audioFileType |> Maybe.withDefault ""

        audioFileTypeRadioButtons =
            if String.isEmpty model.selectedLesson then
                {- don't show audio selection until lesson is created (reason being that I'd need to
                   make another buffer field)
                -}
                div [] []

            else
                div []
                    [ div [] [ text "select file type for associated audio:" ]
                    , Html.form [ style "display" "flex", style "width" "120px", style "margin-left" "auto" ]
                        [ input
                            [ type_ "radio"
                            , name "fileTypeWav"
                            , onInput ChangeLessonAudioFileType
                            , checked (lessonFileType == "wav")
                            , value "wav"
                            ]
                            []
                        , label [ for "fileTypeWav" ] [ text "wav" ]
                        , input
                            [ type_ "radio"
                            , name "fileTypeMp3"
                            , onInput ChangeLessonAudioFileType
                            , checked (lessonFileType == "mp3")
                            , value "mp3"
                            ]
                            []
                        , label [ for "fileTypeMp3" ] [ text "mp3" ]
                        ]
                    ]
    in
    div [ class "new-lesson-view" ]
        [ if String.isEmpty model.selectedLesson then
            label [] [ text "Make a new lesson" ]

          else
            div []
                [ label [ style "display" "block" ] [ text <| "Edit lesson" ]
                , button [ onClick DeselectLesson ] [ text "Deselect Lesson" ]
                ]
        , input [ placeholder "title", value model.newLessonTitle, onInput ChangeNewLessonTitle ] []
        , audioFileTypeRadioButtons
        , div [ class "textarea-container" ] [ textarea [ rows 15, cols 60, onInput ChangeNewLessonText, value model.newLessonText ] [] ]
        , if String.isEmpty model.selectedLesson then
            button [ onClick CreateNewLesson, disabled buttonDisabled ] [ text "Create Lesson" ]

          else
            button [ onClick <| UpdateLesson ( model.selectedLesson, model.newLessonTitle ), disabled buttonDisabled ] [ text "Update Lesson" ]
        ]


lessonsView : Model -> Html Msg
lessonsView model =
    let
        wordsOfLevel n =
            (WOP.listWopsOfLevel n model.wops |> List.length |> String.fromInt) ++ " " ++ WOP.displayFamiliarityLevel n
    in
    div [ class "lessons-view" ]
        [ h3 [] [ text "select a lesson" ]
        , h5 []
            [ text <|
                "currently learning "
                    ++ String.fromInt (Dict.size model.wops)
                    ++ " words!"
                    ++ " including: "
                    ++ String.join ", " (List.map wordsOfLevel [ 1, 2, 3, 4 ])
            ]
        , div [ class "lesson-selector" ]
            (model.lessons
                |> Dict.keys
                |> List.map (\title -> button [ onClick <| SelectLesson title ] [ text title ])
            )
        ]


selectedLessonView : Model -> String -> Html Msg
selectedLessonView model title =
    let
        mlesson =
            Dict.get title model.lessons

        lessonText =
            mlesson |> Maybe.map .text |> Maybe.withDefault ""

        lessonAudioType =
            mlesson |> Maybe.map .audioFileType |> Maybe.withDefault "wav"
    in
    div [ class "selected-lesson-view" ]
        [ h2 [] [ text <| "Title: " ++ title ]
        , audio [ controls True, src <| "http://localhost:3000/audio/" ++ title ++ "." ++ lessonAudioType ] []
        , div [ class "lesson-words-and-lookup" ] [ div [ class "selected-word-edit-and-lesson-translation" ] [ selectedWordEdit model, lessonTranslationBox model ], displayWords model lessonText ]
        ]


lessonTranslationBox : Model -> Html Msg
lessonTranslationBox model =
    div [ class "lesson-translation-box" ]
        [ case Dict.get model.selectedLesson model.lessonTranslations of
            Nothing ->
                button [ onClick AddTranslationToSelectedLesson ] [ text "Add Translation" ]

            Just translation ->
                textarea [ rows 15, cols 20, value translation, onInput EditTranslationOfSelectedLesson ] []
        ]


selectedWordEdit : Model -> Html Msg
selectedWordEdit model =
    div
        [ class "selected-word-edit" ]
        (if String.isEmpty model.selectedWop then
            [ p [] [ text "Select a word to view options" ] ]

         else
            case WOP.get model.selectedWop model.wops of
                Just wop ->
                    p [ class "primary-definition" ] [ text <| model.selectedWop ++ " = ", text <| String.join " || " wop.definitions ]
                        :: List.indexedMap
                            (\i def ->
                                input
                                    [ placeholder ("Definition #" ++ String.fromInt (i + 1))
                                    , value def
                                    , onInput <| EditSelectedWOPDefinition i
                                    ]
                                    []
                            )
                            (wop.definitions ++ [ "" ])
                        ++ [ input [ placeholder "romanization", value wop.romanization, onInput EditSelectedWOPRomanization, class "romanization" ] []

                           -- NOTE: I'm not finding tags to be useful yet, so I'm just gonna to remove the UI for them for now.
                           --    , div [ class "selected-word-tags-edit" ]
                           --         [ div []
                           --             (if not (String.isEmpty model.selectedWopTagsBuffer) then
                           --                 {- the matching tags we show are ONLY for the last term in the list -}
                           --                 [ text "Matching tags: ", text <| String.join ", " <| WOP.fuzzyMatchTag (model.selectedWopTagsBuffer |> WOP.stringToTags |> ListE.last |> Maybe.withDefault "") model.wops ]
                           --              else
                           --                 [ text "Tags: ", text <| String.join ", " wop.tags ]
                           --             )
                           --         , input [ onInput EditSelectedWOPTagsBuffer, value model.selectedWopTagsBuffer, onFocus (EditSelectedWOPTagsBuffer <| String.join ", " wop.tags) ] []
                           --         , button [ onClick (SetSelectedWOPTags model.selectedWopTagsBuffer) ] [ text "Save Tags" ]
                           --         ]
                           , textarea
                                [ cols 20, rows 10, onInput EditSelectedWOPNotes, value wop.notes ]
                                []
                           , div [ class "familiarity-level-selector" ]
                                (List.map
                                    (\n ->
                                        button
                                            [ classList [ ( "selected", wop.familiarityLevel == n ) ]
                                            , class <| String.toLower <| WOP.displayFamiliarityLevel n
                                            , onClick <| SetSelectedWOPFamiliarityLevel n
                                            , title <| WOP.displayFamiliarityLevel n
                                            ]
                                            [ text <| String.fromInt n ]
                                    )
                                    [ 1, 2, 3, 4 ]
                                )
                           ]

                Nothing ->
                    [ p [ class "primary-definition" ] [ text <| model.selectedWop ]
                    , input [ id "newWopDefinition", placeholder "add a definition", onInput EditSelectedNewWOPDefinition, value model.newWopDefinition ] []
                    , button [ onClick SaveSelectedNewWOP, disabled (String.isEmpty model.newWopDefinition) ] [ text "Save New Word" ]
                    ]
        )


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


{-| turns the blob of text into a list of lines of data with no leading whitespace
and only uniform spacing (excepting tabs, I don't think that is handled)
-}
splitIntoCleanLines : String -> List String
splitIntoCleanLines text =
    let
        {- Condenses multiple spaces in a row into a single space. -}
        uniformSpacing line =
            String.split " " line
                -- multiple spaces in a row will stay after the split as empty string entries, where the number of entries is (#of-spaces - 1). so this step ensures empty strings are removed so we have cleaner data to work with.
                |> List.filter (not << String.isEmpty)
                |> String.join " "
    in
    String.split "\n" text
        |> List.map String.trim
        |> List.map uniformSpacing


{-| without altering the spacing of the underlying text, list off words vs. non-words. it
expects a line of text and doesn't support paragraphs simply because line spacing needs
more manual handling in the HTML layout itself. So while I guess we could detect newlines
here, it would muddle this code, and frankly it's easier to handle it outside.

output is a list of ("word", theWordString) | ("non-word", theNonWordString)

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


{-| This step should follow `markWordCharsFromNonWordChars`. It will mark words that belong to a
phrase as such. In the display code, we can still render those words as normal, but in addition
place them in a span that applies phrase styling to the whole unit.;

This almost works correctly for UI purposes but there's one significant snafu: it completely fucks with normal indexing. Now of course we can just store the original indexes in the phrase itself, and, well, I guess that will work for now.

According to my debug log this doesn't even seem to be working right now.

-}
markPhrases : List ( String, Int ) -> List WordDisplayTypes -> List WordDisplayTypes
markPhrases allPhrasesWithLengths list =
    let
        phraseSet =
            Set.fromList (List.map Tuple.first allPhrasesWithLengths)

        wordsWithIndices : List ( String, Int )
        wordsWithIndices =
            list
                |> List.indexedMap (\i x -> ( i, x ))
                |> List.filterMap
                    (\( i, wordOrNonWord ) ->
                        case wordOrNonWord of
                            DisplayWord word ->
                                Just ( word, i )

                            _ ->
                                Nothing
                    )

        sortedLengths =
            allPhrasesWithLengths
                |> List.map Tuple.second
                |> List.sort

        {- sliding windows of length n let us find possible matches to phrases -}
        wordGroupsOfLength n =
            ListE.groupsOfWithStep n 1 wordsWithIndices

        {- returns a list of indexed-phrases (needed for UI rendering, I could prob figure out
           something cleaner) along with their start and end indices
        -}
        matchingPhrases : List ( List ( String, Int ), ( Int, Int ) )
        matchingPhrases =
            sortedLengths
                |> List.concatMap
                    (\phraseSize ->
                        wordGroupsOfLength phraseSize
                            |> List.filterMap
                                (\possiblePhraseMatch ->
                                    let
                                        phrase =
                                            possiblePhraseMatch |> List.map Tuple.first |> String.join " "
                                    in
                                    if WOP.member phrase phraseSet then
                                        Just
                                            ( possiblePhraseMatch
                                            , possiblePhraseMatch
                                                |> List.map Tuple.second
                                                |> (\l -> ( List.head l |> Maybe.withDefault 0, ListE.last l |> Maybe.withDefault 0 ))
                                            )

                                    else
                                        Nothing
                                )
                    )
                |> List.sortBy (\( _, ( startI, _ ) ) -> startI)

        {- now we have a list containing our phrase and the start and end indices of it, we can use
           this data to alter the original data set by replacing all the values in the range of
           those indices with the phrase data structure
        -}
        -- _ =
        --     Debug.log "FOLLLLLDRRRR" (List.foldr (\x acc -> x :: acc) [] (List.map Tuple.first wordsWithIndices))
        {- HACK: short term solution is mark phrase words differently: -}
        wordToPhraseWord wdt =
            case wdt of
                DisplayWord word ->
                    DisplayWordOfPhrase word

                _ ->
                    wdt
    in
    List.foldr
        (\( i, wordOrNonWord ) ( ls, mnextMatchingPhrase, restPhrases ) ->
            case mnextMatchingPhrase of
                Just ( phrase, ( startI, endI ) ) ->
                    if i >= startI && i < endI then
                        -- HACK: short term solution is mark these words differently:
                        ( wordToPhraseWord wordOrNonWord :: ls, mnextMatchingPhrase, restPhrases )
                        -- -- once we're in range, start matching the phrase (removing words until we replace them all with a phrase)
                        -- ( ls, mnextMatchingPhrase, restPhrases )

                    else if i == endI then
                        -- HACK: short term solution is mark these words differently:
                        ( wordToPhraseWord wordOrNonWord :: ls, List.head restPhrases, List.tail restPhrases |> Maybe.withDefault [] )
                        --     ( DisplayPhrase phrase :: ls, List.head restPhrases, List.tail restPhrases |> Maybe.withDefault [] )

                    else
                        ( wordOrNonWord :: ls, mnextMatchingPhrase, restPhrases )

                Nothing ->
                    -- all done matching
                    ( wordOrNonWord :: ls, Nothing, [] )
        )
        ( [], List.head matchingPhrases, List.tail matchingPhrases |> Maybe.withDefault [] )
        (List.indexedMap (\i x -> ( i, x )) list)
        |> (\( x, _, _ ) -> x)


{-| This is how we embellish our words with all of the nice app functionality.
-}
displayWords : Model -> String -> Html Msg
displayWords model lessonText =
    let
        {- li = lineIndex, wi = wordIndex, partOfPhrase = HACK short term solution -}
        displayWord word li wi partOfPhrase =
            span
                [ classList
                    [ ( "word", True )
                    , ( "selected", WOP.tashkylEquivalent model.selectedWop word )
                    , ( "in-dict", WOP.get word model.wops |> MaybeE.isJust )
                    , ( "part-of-phrase", partOfPhrase )
                    ]
                , class <|
                    String.toLower <|
                        WOP.displayFamiliarityLevel (WOP.getFamiliarityLevel word model.wops)
                , onClick
                    (if model.selectedWop == word then
                        DeselectWOP

                     else
                        SelectWord word
                    )
                , onMouseDown (MouseDownOnWord li wi word)
                , onMouseUp (OpenPhraseCreationUI li wi word)
                , onMouseOver <| WordHoverStart word
                , onMouseLeave WordHoverLeave
                ]
                [ text word ]
    in
    div [ class "displayed-words" ]
        (splitIntoCleanLines lessonText
            |> List.indexedMap
                (\li line ->
                    p []
                        (markWordCharsFromNonWordChars line
                            |> markPhrases (WOP.allPhrases model.wops)
                            -- |> Debug.log "markedPhrases data!"
                            -- |> always (markWordCharsFromNonWordChars line)
                            |> List.indexedMap
                                (\wi chunk ->
                                    case chunk of
                                        DisplayWord word ->
                                            displayWord word li wi False

                                        DisplayNonWord nonWord ->
                                            span [ class "non-word" ] [ text nonWord ]

                                        DisplayWordOfPhrase word ->
                                            displayWord word li wi True

                                        DisplayPhrase _ ->
                                            div [] []
                                )
                        )
                )
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


{-| Detect Enter.
-}
ifIsEnter : msg -> Decode.Decoder msg
ifIsEnter msg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Enter" then
                    Decode.succeed msg

                else
                    Decode.fail "some other key"
            )


keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.map KeyPress
