module Main exposing
    ( Model
    , Msg(..)
    , RamdaFunction
    , Sig(..)
    , SigClass(..)
    , SigType(..)
    , TypeVar(..)
    , init
    , main
    , parseClass
    , parseSig
    , parseSigType
    , update
    , view
    )

import Browser
import Char
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Json.Decode as JD
import Parser exposing (..)



---- MODEL ----


type alias Model =
    { --    ramda : Result JD.Error (List RamdaFunction)
    }


type SigType
    = Generic
    | GenericVar TypeVar
    | Boolean
    | Number
    | ListT SigType
    | Function (List SigType)
    | Object SigType SigType


type TypeVar
    = TypeVar String


type SigClass
    = SigClass String TypeVar


type Sig
    = SigWithClass (List SigClass) (List SigType)
    | SigList (List SigType)


extractSigTypes : Sig -> List SigType
extractSigTypes sig =
    case sig of
        SigList xs ->
            xs

        SigWithClass _ xs ->
            xs


type alias RamdaFunction =
    { description : String
    , name : String
    , sig : Sig
    , category : String
    }


parseSigType : Parser.Parser SigType
parseSigType =
    oneOf
        [ succeed Boolean
            |. keyword "Boolean"
        , succeed Number
            |. keyword "Number"
        , succeed Generic
            |. keyword "*"
        , map GenericVar typeVar
        , succeed ListT
            |. symbol "["
            |. spaces
            |= lazy (\_ -> parseSigType)
            |. symbol "]"
        , succeed Function
            |. symbol "("
            |. spaces
            |= lazy (\_ -> map extractSigTypes parseSig)
            |. symbol ")"
        , succeed Object
            |. symbol "{"
            |. spaces
            |= lazy (\_ -> parseSigType)
            |. symbol ":"
            |. spaces
            |= lazy (\_ -> parseSigType)
            |. symbol "}"
        ]


word : Parser String
word =
    succeed ()
        |. spaces
        |. chompIf Char.isUpper
        |. chompWhile (\c -> Char.isAlpha c)
        |> getChompedString


typeVar : Parser TypeVar
typeVar =
    succeed ()
        |. chompIf Char.isLower
        |> getChompedString
        |> map TypeVar


parseClass : Parser.Parser SigClass
parseClass =
    succeed SigClass
        |= word
        |. spaces
        |= typeVar


parseSig : Parser.Parser Sig
parseSig =
    let
        parseArguments =
            sequence
                { start = ""
                , separator = "->"
                , end = ""
                , spaces = spaces
                , item = parseSigType
                , trailing = Optional
                }
    in
    map SigList parseArguments



--    oneOf
--        [
--        map SigWithClass
--          (
--            |= parseClasses
--            |. spaces
--            |. symbol "=>"
--            |. spaces
--            |= parseArguments
--          )
--         map SigList parseArguments
--        ]
--parserTypeFunction : Parser.Parser Type
--parserTypeFunction =
--    Parser.succeed TypeFunction
--
--
--signatureParser :
--
--decodeSignature : String -> Sig
--decodeSignature string =
--    Parser.run signatureParser string
--    |> Result.withDefault []
----    string
----        |> String.replace ": Generic" ""
----        |> Debug.log "cleared"
----        |> String.split " -> "
----        |> List.map (String.trim)
----        |> List.map
----            (\x ->
----                case x of
----                    "Boolean" ->
----                        Boolean
----
----                    "*" ->
----                        Generic
----
----                    "Number" ->
----                        Number
----
----                    _ ->
----                        Debug.log x Generic
----            )
--decodeRamda : JD.Decoder (List RamdaFunction)
--decodeRamda =
--    JD.list
--        (JD.map4 RamdaFunction
--            (JD.field "description" JD.string)
--            (JD.field "name" JD.string)
--            (JD.map decodeSignature (JD.field "sig" JD.string))
--            (JD.field "category" JD.string)
--        )


init : JD.Value -> ( Model, Cmd Msg )
init value =
    ( { --     ramda = JD.decodeValue decodeRamda value
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ]



---- PROGRAM ----


main : Program JD.Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
