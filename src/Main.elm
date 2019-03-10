module Main exposing
    ( Model
    , Msg(..)
    , RamdaFunction
    , Sig(..)
    , SigClass(..)
    , SigClasses(..)
    , SigType(..)
    , TypeVar(..)
    , init
    , main
    , parseClass
    , parseClasses
    , parseSig
    , parseSigType
    , sigToString
    , update
    , view
    )

import Browser
import Char
import Element as E
import Element.Font as Font
import Html exposing (Html)
import Json.Decode as JD
import Parser exposing (..)



---- MODEL ----


type alias Model =
    { ramda : List RamdaFunction
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


type SigClasses
    = SigClasses (List SigClass)


type Sig
    = SigWithClass SigClasses (List SigType)
    | SigList (List SigType)


extractSigTypes : Sig -> List SigType
extractSigTypes sig =
    case sig of
        SigList xs ->
            xs

        SigWithClass _ xs ->
            xs


type alias RamdaFunction =
    { name : String
    , description : String
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


parseClasses : Parser.Parser SigClasses
parseClasses =
    map SigClasses
        (sequence
            { start = ""
            , separator = ","
            , end = ""
            , spaces = spaces
            , item = parseClass
            , trailing = Optional
            }
        )


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
    oneOf
        [ succeed SigWithClass
            |= backtrackable parseClasses
            |. spaces
            |. symbol "=>"
            |= parseArguments
        , succeed
            SigList
            |= parseArguments
        ]


typeVarToString : TypeVar -> String
typeVarToString (TypeVar name) =
    name


sigClassToString : SigClass -> String
sigClassToString (SigClass name var) =
    name ++ " " ++ typeVarToString var


sigClassesToString : SigClasses -> String
sigClassesToString (SigClasses sigClasses) =
    List.map sigClassToString sigClasses
        |> String.join ","


sigTypeToString : SigType -> String
sigTypeToString sigType =
    case sigType of
        Generic ->
            "*"

        GenericVar var ->
            typeVarToString var

        Boolean ->
            "Boolean"

        Number ->
            "Number"

        ListT sigType_ ->
            "[" ++ sigTypeToString sigType_ ++ "]"

        Function types ->
            "(" ++ (List.map sigTypeToString types |> String.join " -> ") ++ ")"

        Object typeKey keyVal ->
            "{" ++ sigTypeToString typeKey ++ ":" ++ " " ++ sigTypeToString keyVal ++ "}"


sigTypesToString : List SigType -> String
sigTypesToString =
    List.map sigTypeToString >> String.join " -> "


sigToString : Sig -> String
sigToString sig =
    case sig of
        SigWithClass classes types ->
            sigClassesToString classes ++ " => " ++ sigTypesToString types

        SigList types ->
            sigTypesToString types


decodeSig : String -> Result (List DeadEnd) Sig
decodeSig =
    Parser.run parseSig


sequenceResult : List (Result a b) -> List b
sequenceResult xs =
    List.foldl
        (\res agg ->
            case res of
                Ok x ->
                    x :: agg

                Err err ->
                    always agg
                        (Debug.log "err" err)
        )
        []
        xs


decodeRamdas : JD.Decoder (List (Result (List DeadEnd) RamdaFunction))
decodeRamdas =
    JD.list
        (JD.map4
            (\description name sigResult category ->
                Result.map (\sig -> RamdaFunction description name sig category) sigResult
            )
            (JD.field "name" JD.string)
            (JD.field "description" JD.string)
            (JD.map decodeSig (JD.field "sig" JD.string))
            (JD.field "category" JD.string)
        )


init : JD.Value -> ( Model, Cmd Msg )
init value =
    ( { ramda =
            JD.decodeValue decodeRamdas value
                |> Result.map sequenceResult
                |> Result.withDefault []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


viewFunction : RamdaFunction -> E.Element Msg
viewFunction { name, sig } =
    E.column [ E.spacing 3 ]
        [ E.text name
        , E.el [ Font.size 13 ] (E.text (sigToString sig))
        ]


view : Model -> Html Msg
view model =
    E.layout []
        (E.column [ E.padding 20, E.spacing 10 ] <|
            List.map viewFunction model.ramda
        )



---- PROGRAM ----


main : Program JD.Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
