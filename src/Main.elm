module Main exposing (Msg(..), main, update, view)

import Browser exposing (Document)
import Browser.Events as BE
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Http
import Joke exposing (Body(..), Category(..), Joke)
import Json.Decode as JD exposing (Decoder)



-- MODEL


type alias Model =
    { title : String
    , mJoke : Maybe Joke
    , category : Category
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "Elm Jokes" Nothing Any
      -- , Cmd.none
    , Cmd.batch [ get Any ]
    )



-- MSG


type Msg
    = Got (Result Http.Error Joke)
    | KeyPress String
    | Set Category



--UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ category } as model) =
    case msg of
        Got (Ok joke) ->
            ( { model | mJoke = Just joke }, Cmd.none )

        Got (Err err) ->
            ( model, Cmd.none )

        KeyPress key ->
            ( model
            , if key == " " then
                get category

              else
                Cmd.none
            )

        Set newCategory ->
            ( { model | category = newCategory }, Cmd.none )



-- FETCH


get : Category -> Cmd Msg
get category =
    Http.get
        { url = "https://sv443.net/jokeapi/category/" ++ Joke.categoryToString category
        , expect = Http.expectJson Got Joke.decoder
        }



-- VIEW


view : Model -> Document Msg
view { title, mJoke, category } =
    { title = title
    , body =
        [ H.header []
            [ viewRadioFor category Any
            , viewRadioFor category Dark
            , viewRadioFor category Programming
            , viewRadioFor category Miscellaneous
            ]
        , H.main_ []
            [ case mJoke of
                Just joke ->
                    viewJoke joke

                Nothing ->
                    H.text ""
            ]
        , H.footer []
            [ H.text "Press [Space] for new joke"
            , H.span []
                [ H.text "Jokes provided by "
                , H.a [ A.target "_blank", A.href "https://sv443.net/jokeapi" ] [ H.text "Sv443 " ]
                ]
            ]
        ]
    }


viewRadioFor : Category -> Category -> Html Msg
viewRadioFor current cat =
    let
        id =
            Joke.categoryToString cat

        selected =
            current == cat
    in
    H.div []
        [ H.input
            [ A.type_ "radio"
            , A.name "category"
            , A.id id
            , A.checked selected
            , E.onCheck (\_ -> Set cat)
            ]
            []
        , H.label [ A.for id ] [ H.text <| id ]
        ]


viewJoke : Joke -> Html msg
viewJoke { category, body } =
    H.section []
        [ H.div [ A.class "category" ] [ H.text <| Joke.categoryToString category ]
        , viewJokeBody body
        ]


viewJokeBody : Body -> Html msg
viewJokeBody body =
    case body of
        Single joke ->
            H.article [ A.class "single" ] [ H.text joke ]

        TwoParts setup delivery ->
            H.article [ A.class "twopart " ]
                [ H.div [] [ H.text setup ]
                , H.div [] [ H.text delivery ]
                ]



-- MAIN


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> BE.onKeyPress keyDecoder
        }


keyDecoder : Decoder Msg
keyDecoder =
    JD.map KeyPress (JD.field "key" JD.string)
