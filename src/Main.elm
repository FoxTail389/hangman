module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set exposing (Set)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode exposing (null)
import Array exposing (set)
import Json.Decode exposing (string)



---- MODEL ----


type Model =
    Loading
    | Running GameState
    | Error


type alias GameState =
    { phrase : String
    , guesses : Set String
    }


init : ( Model, Cmd Msg)
init =
    ( Loading
    , fetchWord
    )




---- UPDATE ----


type Msg
    = Guess String
    | Restart 
    | NewPhrase (Result Http.Error String)



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Guess char ->
            case model of
               Running gameState ->
                    ( Running { gameState | guesses = Set.insert char gameState.guesses}, Cmd.none)

               _->
                    (model, Cmd.none)
        Restart ->
            ( Loading, fetchWord)
        NewPhrase result ->
            case result of
                Ok phrase ->
                    ( Running {phrase = phrase, guesses = Set.empty}, Cmd.none )

                Err _ ->
                    ( Error, Cmd.none )

fetchWord : Cmd Msg
fetchWord =
    Http.get {
        url = "https://victorious-frost-pigment.glitch.me/word"
       , expect = Http.expectJson NewPhrase wordDecoder
    }
wordDecoder : Decoder String
wordDecoder =
    Decode.field "word" Decode.string




  


---- VIEW ----


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            div [] [text "Loading"]
        Running gameState ->
            viewgameState gameState
        Error ->
            div [] [text "Error"]
viewgameState : GameState -> Html Msg
viewgameState gameState =
    let
        phraseHtml=
            gameState.phrase
            |>String.split ""
            |> List.map (\char ->
                if char == " " then
                    " "
                else if Set.member char gameState.guesses then
                    char
                else 
                    "_" 
                )
            |> List.map (\char ->
            span [] [text char]
                )
            |> div []
        phraseSet =
            gameState.phrase
            |> String.split ""
            |> Set.fromList
        failuresHtml = 
            gameState.guesses
            |>Set.toList
            |>List.filter (\char -> not <| Set.member char phraseSet)
            |>List.map (\char -> span [] [text char])
            |> div []
        buttonsHtml =
            "abcdefghijklmnopqrstuvwxyz" 
            |> String.split ""
            |> List.map (\char ->
                button [onClick <| Guess char] [text char]
                )
            |> div []
    in
    div []
        [ phraseHtml
        , buttonsHtml
        , failuresHtml
        , button [ onClick Restart ] [text "Restart"]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
