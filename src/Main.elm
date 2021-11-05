port module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Dict.Extra as DictE
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E
import List.Extra as ListE
import Maybe.Extra as MaybeE
import Regex
import SM2Flashcards exposing (SM2FlashcardData)
import Set exposing (Set)
import Utils exposing (slidingWindow)
import WordOrPhrase as WOP exposing (WOP)



-- MAIN


type alias Flags =
    { sm2FlashcardData : List SM2FlashcardData
    , lessons : List ( String, String )
    , lessonTranslations : List ( String, String )
    , wops : List ( String, WOP )
    }


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


port storeLessonData : List ( String, String ) -> Cmd msg


port storeWops : List ( String, WOP ) -> Cmd msg


port saveLocalStorageToClipboard : () -> Cmd msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    {- TODO: improve deselection of words ; currently it's too broad and even clicking on the word edit interface deselects it. -}
    -- if not (String.isEmpty model.selectedWop) then
    --     Browser.Events.onMouseDown (D.succeed DeselectWord)
    -- else
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
    , lessonTranslations : Dict String String -- title -> translation
    , selectedLesson : String
    , selectedWop : String -- just the key (AKA the stringified word or phrase), for lookup in the wops Dict
    , wops : Dict String WOP
    , newWopDefinition : String
    , mouseDownWord : ( Int, Int, String ) -- line index (which line), word index (which word within line)
    }


init : Flags -> ( Model, Cmd Msg )
init { sm2FlashcardData, lessons, wops, lessonTranslations } =
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
      , wops = Dict.fromList wops
      , newWopDefinition = ""
      , mouseDownWord = ( 0, 0, "" )
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
    | SelectLesson String
    | DeselectLesson -- useful in this development design at least, not a good long-term design
    | BackendAudioUpdated (Result Http.Error String)
    | SelectWord String
    | EditSelectedWOPDefinition Int String
    | EditSelectedWOPNotes String
    | DeselectWOP
    | SaveSelectedNewWOP
    | EditSelectedNewWOPDefinition String
    | AddTranslationToSelectedLesson
    | EditTranslationOfSelectedLesson String
    | MouseDownOnWord Int Int String -- used to start phrase creation
    | OpenPhraseCreationUI Int Int String



-- Use the `sendMessage` port when someone presses ENTER or clicks
-- the "Send" button. Check out index.html to see the corresponding
-- JS where this is piped into a WebSocket.
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                { model | newLessonText = "", newLessonTitle = "", lessons = Dict.insert model.newLessonTitle model.newLessonText model.lessons }
                (.lessons >> Dict.toList >> storeLessonData)

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
                        }

                    else
                        m

                newModel =
                    { model | lessons = Dict.insert newTitle model.newLessonText model.lessons }
                        |> processTitleChange

                {- audios are tied to lessons only by title, so when the title changes we need to change the file too -}
                updateAudioName =
                    if existingTitle /= newTitle then
                        Http.post
                            { url = "http://localhost:3000/updateAudioName"
                            , body =
                                Http.jsonBody <|
                                    E.object
                                        [ ( "old", E.string existingTitle )
                                        , ( "new", E.string newTitle )
                                        ]
                            , expect = Http.expectString BackendAudioUpdated
                            }

                    else
                        Cmd.none
            in
            ( newModel, Cmd.batch [ newModel.lessons |> Dict.toList |> storeLessonData, updateAudioName ] )

        BackendAudioUpdated response ->
            let
                _ =
                    Debug.log "BackendAudioUpdated" response
            in
            pure model

        SelectLesson title ->
            pure
                { model
                    | selectedLesson = title
                    , newLessonText =
                        Dict.get title model.lessons
                            |> Maybe.withDefault ""
                    , newLessonTitle = title
                }

        DeselectLesson ->
            pure { model | selectedLesson = "", newLessonText = "", newLessonTitle = "" }

        SelectWord word ->
            pure { model | selectedWop = word }

        DeselectWOP ->
            pure { model | selectedWop = "" }

        EditSelectedWOPDefinition defNumber definition ->
            impure
                { model
                    | wops =
                        Dict.update model.selectedWop
                            (Maybe.map (WOP.setDefinition defNumber definition))
                            model.wops
                }
                (.wops >> Dict.toList >> storeWops)

        {- TODO: This really should have debouncing on storing the word because it's an expensive op. -}
        EditSelectedWOPNotes notes ->
            impure
                { model
                    | wops =
                        Dict.update model.selectedWop
                            (Maybe.map (WOP.setNotes notes))
                            model.wops
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

                _ =
                    Debug.log "model.selectedWop and WOP.makeWOP" ( model.selectedWop, WOP.makeWOP wopKeyList model.newWopDefinition )
            in
            impure
                { model
                    | wops =
                        Dict.insert model.selectedWop
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
                        Dict.get model.selectedLesson model.lessons |> Maybe.withDefault ""

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
        , button [ onClick SaveDataModelToClipboard ] [ text "Save Data Model to Clipboard (4MB limit)" ]
        , newAndEditLessonView model
        , lessonsView model
        , if String.isEmpty model.selectedLesson then
            span [] []

          else
            selectedLessonView model model.selectedLesson
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
            if String.isEmpty model.selectedLesson then
                (model.newLessonText == "")
                    || (model.newLessonTitle == "")
                    -- don't allow creation if it conflicts with an existing lesson
                    || (Dict.get model.newLessonTitle model.lessons /= Nothing)
                {- disable the button if all fields stayed the same, or if a new title conflicts -}

            else
                (Dict.get model.selectedLesson model.lessons == Just model.newLessonText)
                    && (model.selectedLesson == model.newLessonTitle || Dict.get model.newLessonTitle model.lessons /= Nothing)
    in
    div [ class "new-lesson-view" ]
        [ if String.isEmpty model.selectedLesson then
            label [] [ text "Make a new lesson" ]

          else
            div []
                [ button [ onClick DeselectLesson ] [ text "Deselect Lesson" ]
                , label [] [ text <| "Edit lesson" ]
                ]
        , input [ placeholder "title", value model.newLessonTitle, onInput ChangeNewLessonTitle ] []
        , textarea [ rows 15, cols 60, onInput ChangeNewLessonText, value model.newLessonText ] []
        , if String.isEmpty model.selectedLesson then
            button [ onClick CreateNewLesson, disabled buttonDisabled ] [ text "Create Lesson" ]

          else
            button [ onClick <| UpdateLesson ( model.selectedLesson, model.newLessonTitle ), disabled buttonDisabled ] [ text "Update Lesson" ]
        ]


lessonsView : Model -> Html Msg
lessonsView model =
    div [ class "lessons-view" ]
        [ h3 [] [ text "select a lesson" ]
        , h5 [] [ text <| "currently learning " ++ String.fromInt (Dict.size model.wops) ++ " words!" ]
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
        , audio [ controls False, src <| "http://localhost:3000/audio/" ++ title ++ ".wav" ] []
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
            case Dict.get model.selectedWop model.wops of
                Just word ->
                    p [ class "primary-definition" ] [ text <| model.selectedWop ++ " = ", text <| Maybe.withDefault "" <| List.head word.definitions ]
                        :: List.indexedMap
                            (\i def ->
                                input
                                    [ placeholder ("Definition #" ++ String.fromInt i)
                                    , value def
                                    , onInput <| EditSelectedWOPDefinition i
                                    ]
                                    []
                            )
                            word.definitions
                        ++ [ textarea
                                [ cols 20, rows 10, onInput EditSelectedWOPNotes, value word.notes ]
                                []
                           ]

                Nothing ->
                    [ p [ class "primary-definition" ] [ text <| model.selectedWop ]
                    , input [ placeholder "add a definition", onInput EditSelectedNewWOPDefinition, value model.newWopDefinition ] []
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
            " *():.?؟,=\\-"

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
                                    if Set.member phrase phraseSet then
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

        -- |> Debug.log "matchingPhrases"
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
                    , ( "selected", model.selectedWop == word )
                    , ( "known", Dict.member word model.wops )
                    , ( "part-of-phrase", partOfPhrase )
                    ]
                , onClick
                    (if model.selectedWop == word then
                        DeselectWOP

                     else
                        SelectWord word
                    )
                , onMouseDown (MouseDownOnWord li wi word)
                , onMouseUp (OpenPhraseCreationUI li wi word)
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
                            |> Debug.log "markedPhrases data!"
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
