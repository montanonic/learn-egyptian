module EADictionary exposing (..)

import Dict exposing (Dict)



{-
   Holds all our word definitions.
-}


{-| An egyptian word.
-}
type alias EAWord =
    { word : String
    , -- English words that should look up to it.
      lookups : List String
    , definitions : List String
    }


{-| A one-to-one lookup from English word to Egyptian word.
-}
simpleLookup : String -> Maybe String
simpleLookup englishWord =
    Dict.get englishWord simpleDict


{-| A one-to-one lookup dictionary from English word to Egyptian word.
-}
simpleDict : Dict String String
simpleDict =
    Dict.fromList
        [ -- empty
          ( "", "" )
        , ( "", "" )
        , ( "", "" )
        , ( "computers", "كـُمبيوتـَرا َت" )
        , ( "computer", "كـُمبيوتـَر" )
        , ( "food", "أَكل" )
        , ( "a lot", "كـِتير" )
        , ( "much", "كـِتير" )
        ]


type alias SubjectWord =
    { word : String
    , gender : Maybe GGender -- أَنا must have its gender inferred from context, likewise إِحناَ
    , person : GPerson
    , number : GNumber
    }


type alias VerbWord =
    { word : String
    , gender : GGender
    , person : GPerson
    , number : GNumber
    , tense : GTense
    }


type alias ObjectWord =
    { word : String
    , gender : GGender
    , number : GNumber
    }



{-
   For all of the following structures, the G prefix stands for "grammatical"
-}


type GGender
    = Masc
    | Fem


type
    GPerson
    -- note for comments: singular on left, plural on right
    = First -- I/we
    | Second -- you/y'all
    | Third -- (him/her)/them


type GNumber
    = Singular
      -- | Dual -- only matters with objects and only in a minor way (worth omitting for now?)
    | Plural


{-| The grammatical tense is needed solely for verbs, nothing else changes from it.
-}
type GTense
    = Past
    | Present
    | PresentContinuous
    | Future
    | Subjunctive -- used with modal forms such as "can," "may," "might," "must," "should," and "would"


{-| We must know the gender of the speaker when using أَنا because it cannot be hardcoded.
-}
subjectWords : Dict String SubjectWord
subjectWords =
    Dict.fromList
        [ -- empty
          ( "أَنا", { word = "أَنا", gender = Nothing, person = First, number = Singular } )
        , ( "إِحنا", { word = "إِحنا", gender = Nothing, person = First, number = Plural } )
        , ( "إِنتَ", { word = "إِنتَ", gender = Just Masc, person = Second, number = Singular } )
        , ( "إِنتي", { word = "إِنتي", gender = Just Fem, person = Second, number = Singular } )
        , ( "إِنتي", { word = "إِنتي", gender = Just Fem, person = Second, number = Singular } )
        ]
