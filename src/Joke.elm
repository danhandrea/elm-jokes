module Joke exposing (Body(..), Category(..), Joke, categoryToString, decoder)

import Json.Decode as JD exposing (Decoder)


type alias Joke =
    { category : Category
    , body : Body
    , id : Int
    }


type Body
    = Single String
    | TwoParts String String


type Category
    = Programming
    | Miscellaneous
    | Dark
    | Any


categoryToString : Category -> String
categoryToString category =
    case category of
        Programming ->
            "programming"

        Miscellaneous ->
            "miscellaneous"

        Dark ->
            "dark"

        Any ->
            "any"


categoryDecoder : Decoder Category
categoryDecoder =
    JD.string
        |> JD.andThen
            (\str ->
                case String.toLower str of
                    "programming" ->
                        JD.succeed Programming

                    "miscellaneous" ->
                        JD.succeed Miscellaneous

                    "dark" ->
                        JD.succeed Dark

                    "any" ->
                        JD.succeed Any

                    somethingElse ->
                        JD.fail <| "Unknown category: " ++ somethingElse
            )


bodyDecoder : Decoder Body
bodyDecoder =
    JD.field "type" JD.string
        |> JD.andThen
            (\str ->
                case String.toLower str of
                    "single" ->
                        JD.field "joke" JD.string
                            |> JD.andThen
                                (\joke ->
                                    Single joke
                                        |> JD.succeed
                                )

                    "twopart" ->
                        JD.field "setup" JD.string
                            |> JD.andThen
                                (\setup ->
                                    JD.field "delivery" JD.string
                                        |> JD.andThen
                                            (\delivery ->
                                                TwoParts setup delivery
                                                    |> JD.succeed
                                            )
                                )

                    other ->
                        JD.fail <| "Unknown type " ++ other
            )


decoder : Decoder Joke
decoder =
    JD.map3 Joke
        (JD.field "category" categoryDecoder)
        bodyDecoder
        (JD.field "id" JD.int)
