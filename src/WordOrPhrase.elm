module WordOrPhrase exposing (..)

import List.Extra as ListE



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
    }


{-| If you give this an empty list everything will break :). I could install a Nonempty list package
for this, but I think it's slightly overkill for now.
-}
makeWOP : List String -> String -> WOP
makeWOP wordOrPhrase definition =
    { wordOrPhrase = wordOrPhrase, definitions = [ definition ], notes = "", tags = [] }


setDefinition : Int -> String -> WOP -> WOP
setDefinition defNumber definition wop =
    { wop | definitions = ListE.setAt defNumber definition wop.definitions }


setNotes : String -> WOP -> WOP
setNotes notes wop =
    { wop | notes = notes }


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
