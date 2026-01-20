module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Random


-- TYPES
type Status = Correct | Present | Absent

-- CONSTANTS
wORD_LIST : List String
wORD_LIST =
    [ "CAT", "DOG", "SUN", "BOX", "ICE", "SKY", "FLY", "RUN", "TEA", "CUP"
    , "FIRE", "WIND", "COLD", "SHIP", "FISH", "BIRD", "TREE", "MOON", "STAR", "LION"
    , "PLANE", "SHIRT", "BREAD", "MOUSE", "TRAIN", "GRAPE", "CLOUD", "BRICK", "LIGHT", "WATER"
    , "BOTTLE", "GARDEN", "SPRING", "WINTER", "SUMMER", "BRIDGE", "FLOWER", "SCHOOL", "ORANGE", "BANANA"
    , "PIZZA", "JUNGLE", "GUITAR", "COFFEE", "PLAYER", "ROCKET", "WINDOW", "HAMMER", "FOREST", "CHURCH"
    ]

keyboardRows : List (List String)
keyboardRows =
    [ [ "Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P" ]
    , [ "A", "S", "D", "F", "G", "H", "J", "K", "L" ]
    , [ "ENTER", "Z", "X", "C", "V", "B", "N", "M", "⌫" ]
    ]


-- MAIN
main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, generateRandomWord )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


-- MODEL
type alias Model =
    { draft : List String
    , guesses : List String
    , targetWord : String
    , wordSize : Int
    , maxTries : Int
    , triesRemaining : Int
    , message : String
    }

init : Model
init =
    { draft = []
    , guesses = []
    , targetWord = ""
    , wordSize = 0
    , maxTries = 0
    , triesRemaining = 0
    , message = "Loading..."
    }


-- UPDATE
type Msg 
    = Submit 
    | NewTargetWord String 
    | Reset 
    | KeyPressed String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        hasWon = List.member model.targetWord model.guesses
        isGameOver = (model.triesRemaining <= 0 && model.maxTries > 0) || hasWon
    in
    case msg of
        NewTargetWord word ->
            let
                size = String.length word
                tries = size + 1
            in
            ( { model 
                | targetWord = word
                , wordSize = size
                , maxTries = tries
                , triesRemaining = tries
                , draft = List.repeat size ""
                , message = ""
                , guesses = []
              }
            , Cmd.none 
            )

        Reset ->
            ( model, generateRandomWord )

        KeyPressed key ->
            if isGameOver then
                ( model, Cmd.none )
            else if key == "ENTER" then
                update Submit model
            else if key == "⌫" then
                let
                    lastIdx = 
                        model.draft 
                            |> List.indexedMap (\i c -> (i, c)) 
                            |> List.filter (\(_, c) -> c /= "") 
                            |> List.reverse 
                            |> List.head 
                            |> Maybe.map Tuple.first
                    
                    newDraft = 
                        case lastIdx of
                            Just idx ->
                                List.indexedMap (\i c -> if i == idx then "" else c) model.draft
                            Nothing ->
                                model.draft
                in
                ( { model | draft = newDraft, message = "" }, Cmd.none )
            else if String.length key == 1 && Char.isAlpha (String.toList key |> List.head |> Maybe.withDefault ' ') then
                let
                    firstEmpty = 
                        model.draft 
                            |> List.indexedMap (\i c -> (i, c)) 
                            |> List.filter (\(_, c) -> c == "") 
                            |> List.head 
                            |> Maybe.map Tuple.first
                    
                    newDraft = 
                        case firstEmpty of
                            Just idx ->
                                List.indexedMap (\i c -> if i == idx then key else c) model.draft
                            Nothing ->
                                model.draft
                in
                ( { model | draft = newDraft, message = "" }, Cmd.none )
            else
                ( model, Cmd.none )

        Submit ->
            let
                guess = String.concat model.draft
                isDraftComplete = List.all (\c -> c /= "") model.draft
            in
            if isGameOver then
                ( model, Cmd.none )
            else if not isDraftComplete then
                ( { model | message = "Word too short!" }, Cmd.none )
            else
                let
                    newGuesses = model.guesses ++ [ guess ]
                    newHasWon = guess == model.targetWord
                    newTries = model.triesRemaining - 1
                    finalMsg = 
                        if newHasWon then "You Win! 🎉" 
                        else if newTries <= 0 then "Game Over! The word was " ++ model.targetWord 
                        else ""
                in
                ( { model 
                    | guesses = newGuesses
                    , draft = List.repeat model.wordSize ""
                    , triesRemaining = newTries
                    , message = finalMsg 
                  }
                , Cmd.none 
                )


-- LOGIC
getLetterStatuses : String -> String -> List (String, Status)
getLetterStatuses target guess =
    let
        targetList = String.split "" target
        guessList = String.split "" guess
        isCorrect i char = (List.drop i targetList |> List.head) == Just char
    in
    List.indexedMap (\i char -> 
        if isCorrect i char then (char, Correct) 
        else if List.member char targetList then (char, Present) 
        else (char, Absent)
    ) guessList

getKeyStatus : String -> List String -> String -> Maybe Status
getKeyStatus target guesses key =
    let
        allOccurrences = 
            guesses 
                |> List.map (getLetterStatuses target)
                |> List.foldl (++) []
                |> List.filter (\(char, _) -> char == key)
                |> List.map Tuple.second
    in
    if List.member Correct allOccurrences then Just Correct
    else if List.member Present allOccurrences then Just Present
    else if List.member Absent allOccurrences then Just Absent
    else Nothing


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown (Decode.map KeyPressed keyDecoder)

keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.map (\key ->
            if key == "Enter" then "ENTER"
            else if key == "Backspace" then "⌫"
            else String.toUpper key
        )


-- VIEW
view : Model -> Html Msg
view model =
    let
        hasWon = List.member model.targetWord model.guesses
        isGameOver = (model.triesRemaining <= 0 && model.maxTries > 0) || hasWon
    in
    div [ style "padding" "20px", style "font-family" "sans-serif", style "text-align" "center", style "max-width" "500px", style "margin" "0 auto" ]
        [ div [ style "color" "#cc0000", style "font-size" "10px", style "margin-bottom" "5px" ] [ text ("DEBUG: " ++ model.targetWord) ]
        , div [ style "font-size" "32px", style "font-weight" "bold" ] [ text "ELM-WORDLE" ]
        , div [ style "margin-bottom" "10px", style "color" "#666" ] 
            [ text (String.fromInt model.triesRemaining ++ " / " ++ String.fromInt model.maxTries ++ " tries left") ]
        
        , div [ style "margin-bottom" "20px" ] (List.map (viewFinishedRow model.targetWord) model.guesses)
        
        , if not isGameOver then
            div [ style "display" "flex", style "justify-content" "center", style "gap" "5px", style "margin-bottom" "20px" ]
                (List.map viewStaticBox model.draft)
          else
            div [ style "font-size" "24px", style "margin" "20px", style "font-weight" "bold", style "color" (if hasWon then "#6aaa64" else "#d32f2f") ] 
                [ text model.message ]
        
        , viewKeyboard model isGameOver
        
        , if isGameOver then
            button [ onClick Reset, style "margin-top" "20px", style "padding" "10px 30px", style "cursor" "pointer", style "background" "#333", style "color" "white", style "border" "none", style "border-radius" "4px" ] [ text "PLAY AGAIN" ]
          else
            div [ style "color" "#d32f2f", style "margin-top" "10px", style "height" "20px" ] [ text model.message ]
        ]

viewStaticBox : String -> Html Msg
viewStaticBox val =
    div [ style "width" "45px", style "height" "45px", style "border" "2px solid #333", style "display" "flex", style "justify-content" "center", style "align-items" "center", style "font-size" "20px", style "font-weight" "bold", style "text-transform" "uppercase" ] [ text val ]

viewKeyboard : Model -> Bool -> Html Msg
viewKeyboard model isDisabled =
    div [ style "display" "flex", style "flex-direction" "column", style "gap" "8px", style "margin-top" "20px" ]
        (List.map (\keys -> div [ style "display" "flex", style "justify-content" "center", style "gap" "6px" ] (List.map (viewKey model isDisabled) keys)) keyboardRows)

viewKey : Model -> Bool -> String -> Html Msg
viewKey model isDisabled key =
    let
        status = getKeyStatus model.targetWord model.guesses key
        isSpecial = String.length key > 1 || key == "⌫"
        bgColor =
            case status of
                Just Correct -> "#6aaa64"
                Just Present -> "#c9b458"
                Just Absent -> "#3a3a3c"
                Nothing -> "#d3d6da"
        txtColor = if status == Nothing then "black" else "white"
    in
    button [ onClick (KeyPressed key), disabled isDisabled, style "padding" (if isSpecial then "15px 10px" else "15px 0"), style "min-width" (if isSpecial then "60px" else "40px"), style "background-color" bgColor, style "color" txtColor, style "border" "none", style "border-radius" "4px", style "font-weight" "bold", style "cursor" "pointer" ] [ text key ]

viewFinishedRow : String -> String -> Html Msg
viewFinishedRow target guess =
    div [ style "display" "flex", style "justify-content" "center", style "gap" "5px", style "margin-bottom" "5px" ]
        (getLetterStatuses target guess |> List.map viewLetterTile)

viewLetterTile : (String, Status) -> Html Msg
viewLetterTile (letter, status) =
    let
        bgColor =
            case status of
                Correct -> "#6aaa64"
                Present -> "#c9b458"
                Absent -> "#787c7e"
    in
    div [ style "width" "45px", style "height" "45px", style "background-color" bgColor, style "color" "white", style "display" "flex", style "justify-content" "center", style "align-items" "center", style "font-weight" "bold", style "border" "1px solid #fff", style "text-transform" "uppercase" ] [ text letter ]

generateRandomWord : Cmd Msg
generateRandomWord =
    let
        gen = case wORD_LIST of
                x :: xs -> Random.uniform x xs
                [] -> Random.constant "ELM"
    in
    Random.generate NewTargetWord gen
