module Main exposing (main)

import Browser
import Html exposing (Html, div, img, li, text, ul)
import Html.Attributes exposing (src)
import Http
import Json.Decode as Decode
import List



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Movie =
    { title : String
    , year : String
    , imdbID : String
    , poster : String
    }


type alias Movies =
    List Movie


type Model
    = Failure
    | Loading
    | Success Movies


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , getStarWarsMovies
    )



-- UPDATE


type Msg
    = GotMovies (Result Http.Error Movies)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotMovies result ->
            case result of
                Ok movies ->
                    ( Success movies, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html msg
view model =
    case model of
        Failure ->
            text "I was unable to load movies..."

        Loading ->
            text "Loading..."

        Success movies ->
            ul []
                (List.map
                    (\movie ->
                        li []
                            [ img [ src movie.poster ] []
                            , div []
                                [ div [] [ text movie.title ]
                                , div [] [ text movie.year ]
                                ]
                            ]
                    )
                    movies
                )



-- HTTP


getStarWarsMovies : Cmd Msg
getStarWarsMovies =
    Http.get
        { url = "https://www.omdbapi.com/?s=star%20wars&apikey=fbb467ef"
        , expect = Http.expectJson GotMovies decodeMovies
        }


decodeMovies : Decode.Decoder Movies
decodeMovies =
    Decode.at [ "Search" ] (Decode.list decodeMovie)


decodeMovie : Decode.Decoder Movie
decodeMovie =
    Decode.map4 Movie
        (Decode.field "Title" Decode.string)
        (Decode.field "Year" Decode.string)
        (Decode.field "imdbID" Decode.string)
        (Decode.field "Poster" Decode.string)
