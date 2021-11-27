module EnterNewWops exposing (Model, Msg(..), init, update, view, viewPage)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra
import WordOrPhrase as WOP exposing (WOP)



{- An experiment in modular component design with low-friction. This component is intended to just
   be a place to dump new WOP definitions ahead of time into the system.

   eventually though I think I'll turn this into a full-blown wops management page with search, editing, adding, reviewing, etc.
-}
-- MODEL


type alias ParentModel a =
    { a | wops : Dict String WOP }


type alias Model =
    { newWop : WOP
    , createdWops : List WOP
    , viewingEnterNewWops : Bool
    }


init : Model
init =
    { newWop = initWOP
    , createdWops = []
    , viewingEnterNewWops = False
    }


initWOP =
    WOP.makeWOP [] ""



-- UPDATE


type Msg
    = EditWOPKey String
    | EditWOPDefinition String
    | SaveNewWOP
    | GoBack


update : Msg -> Model -> Model
update msg model =
    case msg of
        EditWOPKey key ->
            { model | newWop = setWopKey model.newWop key }

        EditWOPDefinition def ->
            { model | newWop = WOP.setDefinition 0 def model.newWop }

        SaveNewWOP ->
            if wopNotEmpty model.newWop then
                { model | newWop = initWOP, createdWops = cleanUpWopKey model.newWop :: model.createdWops }

            else
                model

        GoBack ->
            -- parent will handle rest
            model



-- VIEW


view : Model -> ParentModel a -> Html Msg
view model parent =
    div [ class "enter-new-wops" ]
        [ div [ class "input-container" ]
            [ input [ class "egyptian", placeholder "egyptian", onInput EditWOPKey, value <| WOP.key model.newWop ] []
            , input [ class "english", placeholder "definition", onInput EditWOPDefinition, value <| String.join "" model.newWop.definitions ] []
            ]
        , if wopAlreadyExists model parent then
            p [] [ text "this wop already exists!" ]

          else
            p [ style "visibility" "hidden" ] [ text "nothing" ]
        , button
            [ class "save-new-wop"
            , onClick SaveNewWOP
            , disabled
                ((String.isEmpty <| WOP.key model.newWop)
                    || wopAlreadyExists model parent
                    || (String.isEmpty <| String.join "" model.newWop.definitions)
                )
            ]
            [ text "Save New WOP" ]
        , if not <| List.isEmpty <| WOP.searchWop (WOP.key model.newWop) parent.wops then
            p [] [ text "related wops: " ]

          else
            p [] []
        , div []
            (List.map
                (\wop ->
                    div [] [ text <| WOP.key wop ++ " : " ++ String.join ", " wop.definitions ]
                )
                (WOP.searchWop (WOP.key model.newWop) parent.wops)
            )
        , p [] [ text "new wops" ]
        , button [ onClick GoBack, disabled (List.isEmpty model.createdWops) ]
            [ text "commit new wops and go back" ]
        , ol []
            (List.map
                (\wop ->
                    p [] [ text (WOP.key wop), text ": ", text (String.join "" wop.definitions) ]
                )
                model.createdWops
            )
        ]



-- HELPERS


wopAlreadyExists : Model -> ParentModel a -> Bool
wopAlreadyExists { createdWops, newWop } parent =
    List.any Maybe.Extra.isJust
        [ WOP.get (WOP.key newWop) (WOP.intoDict createdWops)
        , WOP.get (WOP.key newWop) parent.wops
        ]


wopNotEmpty : WOP -> Bool
wopNotEmpty wop =
    List.length wop.wordOrPhrase >= 1


{-| Leaves fragments of "" in the list, which must be cleaned up by `cleanUpWopKey`.
-}
setWopKey : WOP -> String -> WOP
setWopKey wop key =
    { wop | wordOrPhrase = key |> String.split " " |> List.map String.trim }


{-| This must be ran before saving the wop, as this ensures a properly formed key.
-}
cleanUpWopKey : WOP -> WOP
cleanUpWopKey wop =
    { wop | wordOrPhrase = wop.wordOrPhrase |> List.filter (not << String.isEmpty) }


viewPage : Model -> Model
viewPage model =
    { model | viewingEnterNewWops = True }
