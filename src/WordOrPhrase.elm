module WordOrPhrase exposing (..)

import Dict exposing (Dict)
import List.Extra as ListE
import Set exposing (Set)
import String.Extra as StringE
import Time exposing (Posix)



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
    , romanization : String -- optional guide for pronunciation

    {- TODO: reviewHistory should contain the context of where the item was reviewed. was it a
       lesson, a flashcard? etc... it's also easier to think about multiple reviews in terms of
       number of times rather than discrete entries, so if a word is clicked on 12 times in a
       lesson, that lesson's review history could have that number stored, rather than having 12
       events.

       thinking about this more, I want to see ALL contexts in which a word was looked up, but
       within a single lesson I think I'd like to just track the click timestamps of words within it
       for a day rather than counting each click as a singular event. so the lesson event context is
       built up by day, and then there will be vocab flash review contexts as well.
    -}
    , reviewHistory : List { timestamp : Int, lessonId : Maybe String } -- Posix stored as a milliseconds Int for port-friendliness; helps to track what words are due for looking at again. Existing entries didn't have an associated lesson so they're nulled out for now. Once we have more review data I can just delete those reviews but for now I don't want to lose that progress. Because clicking on a wop is done for many reasons, not just reviewing, a lot of clicks can happen in a short period of time in a lesson. Any number of clicks will be considered a review, and of course marking words as reviewed will tick-up the click counter for any words not clicked on during a lesson. In other words, it's like a "lesson session", cuz 100clicks in 1 lesson means WAY less than 2 clicks in that lesson, 3 clicks two hours later, 1 click five hours later. Right? So later on I'll want to condense all our timestamps into lesson sessions. A session might be as simple as: from the time of first click of a word within that lesson, any other clicked word will be considered a part of that lesson session if occuring within 30minutes to an hour, yes? What happens when you step away though, hmm. Well, I think since our minimal SRS interval is one day, maybe we'll just keep it simple by condensing reviews down into one days worth per separate lesson. Yeah, that's the right model. The last thing is to just track bedtime properly.
    }


type ReviewLocation
    = SentenceContext
    | FlashcardContext


type alias ReviewContext =
    { location : ReviewLocation
    , timestamp : Int
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
    { wordOrPhrase = wordOrPhrase, definitions = [ definition ], romanization = "", notes = "", tags = [], familiarityLevel = 1, reviewHistory = [] }


intoDict : List WOP -> Dict String WOP
intoDict wops =
    wops |> List.map (\wop -> ( key wop, wop )) |> Dict.fromList


setDefinition : Int -> String -> WOP -> WOP
setDefinition defNumber definition wop =
    if defNumber == List.length wop.definitions then
        -- adding a new definition
        { wop | definitions = wop.definitions ++ [ definition ] }

    else if defNumber /= 0 && defNumber == (List.length wop.definitions - 1) && definition == "" then
        {- wop must always have 1 definition, but aside from the first one, if the last one is set
           to empty, we delete it.
        -}
        { wop | definitions = ListE.removeAt defNumber wop.definitions }

    else
        -- editing an existing definition (will not alter anything if index is out of range)
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


{-| NOTE: By the way this is designed and used, we're probably better off just storing the LAST
review time, since our metric isn't very elaborate. It's not worth changing really, because the
current methodology isn't harmful beyond being more complex, but we also don't really use previous
review history. It's far more useful to have things like which lessons a wop was reviewed in,
because _that_ can be used to offer more review variety. That's what we should ultimately move
towards. Another great one is: when doing flashcard reviews, track that a word was reviewed in that
context, and also which sentence it was reviewed under. Again, useful information for variety sake.

FOLLOWUP NOTE: Storing the review history also tied to which lesson a wop was reviewed in would be useful for showing the word in unique contexts for future reviews and in general ensuring we get sentence variety for a single wop. I think storing such data is useful, so I'm going to do so.

-}
addReviewTime : Posix -> String -> WOP -> WOP
addReviewTime time lessonId wop =
    { wop | reviewHistory = { timestamp = Time.posixToMillis time, lessonId = Just lessonId } :: wop.reviewHistory }


{-| absolute ms timestamp
-}
lastReviewedOn : WOP -> Maybe Int
lastReviewedOn wop =
    wop.reviewHistory |> List.map .timestamp |> List.minimum


setTags : String -> WOP -> WOP
setTags tagString wop =
    { wop | tags = stringToTags tagString }


stringToTags : String -> List String
stringToTags tagString =
    if String.isEmpty tagString then
        []

    else
        String.split "," tagString
            |> List.map StringE.clean
            |> ListE.filterNot String.isEmpty


getTagList : Dict String WOP -> List String
getTagList d =
    Dict.values d
        |> List.concatMap .tags
        |> ListE.unique
        |> List.sort


fuzzyMatchTag : String -> Dict String WOP -> List String
fuzzyMatchTag tag d =
    List.filter (\existingTag -> ListE.isSubsequenceOf (String.toList tag) (String.toList existingTag))
        (getTagList d)


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


{-| Specifically just fathah, kasrah, dammah, and sukoon. Not shaddah (or sukoon).
-}
tashkylSet : Set Char
tashkylSet =
    Set.fromList [ 'َ', 'ِ', 'ُ', 'ْ' ]


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


{-| Automatically removes fathah, kasrah, dammah, sukoon, from the WOP so that it can be properly looked up
as a key.
-}
get : String -> Dict String WOP -> Maybe WOP
get wopKey =
    Dict.get (removeFKD wopKey)


{-| Automatically removes fathah, kasrah, dammah, sukoon, from the WOP so that it can be properly looked up
as a key.
-}
update : String -> (Maybe WOP -> Maybe WOP) -> Dict String WOP -> Dict String WOP
update wopKey =
    Dict.update (removeFKD wopKey)


{-| Automatically removes fathah, kasrah, dammah, sukoon, from the WOP so that it can be properly looked up
as a key.
-}
insert : String -> WOP -> Dict String WOP -> Dict String WOP
insert wopKey =
    Dict.insert (removeFKD wopKey)


{-| Automatically removes fathah, kasrah, dammah, sukoon, from the WOP so that it can be properly looked up
as a key.
-}
member : String -> Set String -> Bool
member wopKey =
    Set.member (removeFKD wopKey)


listWopsOfLevel : Int -> Dict String WOP -> List WOP
listWopsOfLevel familiarityLevel =
    Dict.values >> List.filter (\wop -> wop.familiarityLevel == familiarityLevel)


getFamiliarityLevel : String -> Dict String WOP -> Int
getFamiliarityLevel wopKey wops =
    get wopKey wops
        |> Maybe.map (\wop -> wop.familiarityLevel)
        -- this default shouldn't hit, so I pick an intentionally bad value
        |> Maybe.withDefault 0
