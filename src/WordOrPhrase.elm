module WordOrPhrase exposing (..)

import Dict exposing (Dict)
import Dict.Extra as DictE
import List.Extra as ListE
import Set exposing (Set)



{- What is the real difference between a word an a phrase in terms of how it's used in a language?

   They are in many ways one-in-the-same, and I can see why LingQ would just call these lingqs.

   I might (cheekily) call them WOPs, wordOrPhrases. wordsAndPhrases is more correct since it is a
   union of words or phrases, yielding both things :). Naming shiz aside, currently the main obvious
   difference is that a phrase must reference a list of words. And we might wish to only lookup
   phrases, separate from words. So let's design a custom data structure that has a convenient
   interface for yielding both.

   Do we *need* to know if something is a phrase? Or is that something we can just ask for when we
   need. I'm guessing the latter. For example, we have selectedWord, which can change to selectedWOP
   without any real loss. And then in the places where we *want* to use the phrase information, we
   just can.

   One thing we need to handle still is de-tashkylization. Perhaps one way to do this is have an
   option fullTashkyl field, where I can manually verify what the full proper version is, and then
   *any* words will match *unless* they have tashkyl markings that contradict any of the official
   versions. But how do we do a dictionary lookup in this case of partial tashkyl? We can of course
   just look up the word without tashkyl. This gets really tricky. I'm not sure I'm ready to solve
   this yet.
-}


{-| A word or a phrase, behind a uniform data structure.
-}
type alias WOP =
    { wordOrPhrase : List String -- (must not be empty) a singleton is a word, otherwise is a phrase
    , definitions : List String
    , notes : String
    , tags : List String
    , familiarityLevel : Int -- currently 1 through 4
    }


displayFamiliarityLevel : Int -> String
displayFamiliarityLevel familiarityLevel =
    case familiarityLevel of
        1 ->
            "New"

        2 ->
            "Recognized"

        3 ->
            "Familiar"

        4 ->
            "Learned"

        _ ->
            ""


{-| If you give this an empty list everything will break :). I could install a Nonempty list package
for this, but I think it's slightly overkill for now.
-}
makeWOP : List String -> String -> WOP
makeWOP wordOrPhrase definition =
    { wordOrPhrase = wordOrPhrase, definitions = [ definition ], notes = "", tags = [], familiarityLevel = 1 }


setDefinition : Int -> String -> WOP -> WOP
setDefinition defNumber definition wop =
    { wop | definitions = ListE.setAt defNumber definition wop.definitions }


setNotes : String -> WOP -> WOP
setNotes notes wop =
    { wop | notes = notes }


setFamiliarityLevel : Int -> WOP -> Maybe WOP
setFamiliarityLevel level wop =
    if level >= 1 && level <= 4 then
        Just { wop | familiarityLevel = level }

    else
        Nothing


{-| For a word the key is just the word, and for a phrase like "لو سمحت", the key will be that whole
string with the spaces in between.
-}
key : WOP -> String
key { wordOrPhrase } =
    wordOrPhraseToKey wordOrPhrase


wordOrPhraseToKey : List String -> String
wordOrPhraseToKey wordOrPhrase =
    String.join " " wordOrPhrase


{-| Does the given WOP key correspond to a phrase?
-}
keyIsPhrase : String -> Bool
keyIsPhrase key_ =
    List.length (String.split " " key_) > 1


isWord : WOP -> Bool
isWord { wordOrPhrase } =
    List.length wordOrPhrase == 1


isPhrase : WOP -> Bool
isPhrase { wordOrPhrase } =
    List.length wordOrPhrase > 1


{-| Returns the keys for all of the currently stored phrases, along with their length information
since this is helpful for looking up phrases of the correct length.
-}
allPhrases : Dict String WOP -> List ( String, Int )
allPhrases dict =
    Dict.keys dict
        |> List.filterMap
            (\k ->
                let
                    length =
                        List.length <| String.split " " k
                in
                if length == 1 then
                    Nothing

                else
                    Just ( k, length )
            )



-- TASHKYL
{- phase 1 for our greater support of Egyptian is to have our word lookup dictionary use non-tashkyl
   lookups. when there's a hit, then if the source word has tashkyl we check for a match. if a match
   can't be found we simply mark it as an unknown word as usual, and if more than one match is
   found, we give the learner a little menu to switch between them easily. the currently selected
   one in the menu should stay selected when that word is encountered elsewhere, as a sensible
   default. of course the best solution is to store what the selected instance of the word is at
   every independent location, but that solution can be evolved into over time.

   but IMO, my phase 1 just needs to remove the tashkyl. I'm not likely to hit a meaningful
   edge-case yet. enta/enty is like the only one, but it really doesn't matter cuz it's just like
   obvious.

   shaddah is also commonly used, so we're going to let that stay in the word and the lookup as
   normal for now.
-}


{-| Specifically just fathah, kasrah, and dammah. Not shaddah (or sukoon).
-}
tashkylSet : Set Char
tashkylSet =
    Set.fromList [ 'َ', 'ِ', 'ُ' ]


splitOffTashkyl : String -> ( String, List Char )
splitOffTashkyl string =
    let
        chars =
            String.toList string

        justTashkyl =
            chars |> List.filter (\ch -> Set.member ch tashkylSet)

        withoutTashkyl =
            chars |> ListE.filterNot (\ch -> Set.member ch tashkylSet) |> String.fromList
    in
    ( withoutTashkyl, justTashkyl )


{-| Removes fathah, kasrah, and dammah from the word.
-}
removeFKD : String -> String
removeFKD =
    splitOffTashkyl >> Tuple.first


{-| Two words are tashkyl equivalent if their non-tashkyl forms are equal, and if either has no
tashkyl, or if they both have tashkyl and are either exact matches or at least non-conflicting
tashkyl.
-}
tashkylEquivalent : String -> String -> Bool
tashkylEquivalent wop1 wop2 =
    let
        ( wt1, jt1 ) =
            splitOffTashkyl wop1

        ( wt2, jt2 ) =
            splitOffTashkyl wop2
    in
    wt1 == wt2 && (List.isEmpty jt1 || List.isEmpty jt2 || ListE.isSubsequenceOf jt1 jt2 || ListE.isSubsequenceOf jt2 jt1)


{-| Automatically removes fathah, kasrah, dammah, from the word so that it can be properly looked up
as a key.
-}
get : String -> Dict String WOP -> Maybe WOP
get wopKey =
    Dict.get (removeFKD wopKey)


{-| Automatically removes fathah, kasrah, dammah, from the word so that it can be properly looked up
as a key.
-}
update : String -> (Maybe WOP -> Maybe WOP) -> Dict String WOP -> Dict String WOP
update wopKey =
    Dict.update (removeFKD wopKey)


{-| Automatically removes fathah, kasrah, dammah, from the word so that it can be properly looked up
as a key.
-}
insert : String -> WOP -> Dict String WOP -> Dict String WOP
insert wopKey =
    Dict.insert (removeFKD wopKey)


migrateDictionary : Dict String WOP -> Dict String WOP
migrateDictionary =
    DictE.mapKeys (\k -> removeFKD k)
