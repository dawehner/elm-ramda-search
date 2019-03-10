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
    , parseSig
    , sigToString
    , update
    , view
    )

import Browser
import Char
import Element as E
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as JD
import List.Extra
import Maybe
import Parser exposing (..)
import Set



---- MODEL ----


type alias Model =
    { ramda : List RamdaFunction
    , search : Maybe String
    }


type SigType
    = Generic
    | GenericVar TypeVar
    | Boolean
    | Number
    | StringT
    | RegExp
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


convertToGenerics : Sig -> Sig
convertToGenerics sig =
    let
        replace x =
            case x of
                GenericVar _ ->
                    Generic

                _ ->
                    x
    in
    case sig of
        SigList xs ->
            SigList (List.map replace xs)

        SigWithClass _ xs ->
            SigList (List.map replace xs)


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
        , succeed StringT
            |. keyword "String"
        , succeed RegExp
            |. keyword "RegExp"
        , succeed Generic
            |. keyword "*"
        , map GenericVar typeVar
        , succeed ListT
            |. symbol "List"
            |. spaces
            |= lazy (\_ -> parseSigType)
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


parseClasses : Parser.Parser (List SigClass)
parseClasses =
    sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = spaces
        , item = parseClass
        , trailing = Optional
        }


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


sigClassesToString : List SigClass -> String
sigClassesToString sigClasses =
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

        StringT ->
            "String"

        RegExp ->
            "RegExp"

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

                Err _ ->
                    agg
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
      , search = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


sigMetric : Sig -> Sig -> Maybe Float
sigMetric sigA sigB =
    if sigA == sigB then
        Just 0.0

    else if List.Extra.isInfixOf (extractSigTypes sigA) (extractSigTypes sigB) then
        Just 0.5

    else if List.Extra.isInfixOf (extractSigTypes sigA) (extractSigTypes (convertToGenerics sigB)) then
        Just 0.3

    else
        Nothing


filterOrJustTuple : List ( Maybe a, b ) -> List ( a, b )
filterOrJustTuple =
    List.foldl
        (\( x, b ) agg ->
            case x of
                Just x_ ->
                    ( x_, b ) :: agg

                _ ->
                    agg
        )
        []


filterAndSortSearch : String -> List RamdaFunction -> Result (List DeadEnd) (List RamdaFunction)
filterAndSortSearch string list =
    let
        typeResult =
            decodeSig string
                |> Result.map
                    (\sig ->
                        List.map (\function -> ( sigMetric sig function.sig, function )) list
                            |> filterOrJustTuple
                    )

        nameResult =
            (List.filter (\function -> String.contains string function.name) list
                |> List.map (\x -> ( 0.2, x ))
            )
                |> (\x ->
                        if List.length x > 0 then
                            Ok x

                        else
                            Err []
                   )

        descriptionResult =
            (List.filter (\function -> String.contains string function.description) list
                |> List.map (\x -> ( 0.1, x ))
            )
                |> (\x ->
                        if List.length x > 0 then
                            Ok x

                        else
                            Err []
                   )
    in
    (case ( typeResult, nameResult, descriptionResult ) of
        ( Ok a, Ok b, Ok c ) ->
            Ok (a ++ b ++ c)

        ( Ok a, Ok b, Err _ ) ->
            Ok (a ++ b)

        ( Ok a, Err _, Ok c ) ->
            Ok (a ++ c)

        ( Ok a, Err _, Err _ ) ->
            Ok a

        ( Err _, Ok b, Ok c ) ->
            Ok (b ++ c)

        ( Err _, Ok b, Err _ ) ->
            Ok b

        ( Err _, Err _, Ok c ) ->
            Ok c

        ( err, _, _ ) ->
            err
    )
        |> Result.map
            (\xs ->
                List.sortBy Tuple.first xs
                    |> List.map Tuple.second
                    |> List.Extra.uniqueBy .name
            )


type Msg
    = NoOp
    | SearchTerm String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SearchTerm string ->
            case string of
                "" ->
                    ( { model | search = Nothing }, Cmd.none )

                term ->
                    ( { model | search = Just term }, Cmd.none )



---- VIEW ----


viewFunction : RamdaFunction -> E.Element Msg
viewFunction { name, sig } =
    E.link [ E.pointer ]
        { url = "https://ramdajs.com/docs/#" ++ name
        , label =
            E.row [ E.spacing 10 ]
                [ E.text name
                , E.el [ Font.size 13 ] (E.text (sigToString sig))
                ]
        }


viewFunctions : List RamdaFunction -> E.Element Msg
viewFunctions functions =
    E.column [ E.padding 20, E.spacing 10, E.centerX ] <|
        List.map viewFunction functions


view : Model -> Html Msg
view model =
    E.layout [ E.width E.fill, E.centerX ]
        (E.column [ E.width E.fill, E.centerX ]
            [ Input.search []
                { label = Input.labelHidden "Search function"
                , onChange = SearchTerm
                , placeholder = Just (Input.placeholder [] (E.text "... search function"))
                , text = model.search |> Maybe.withDefault ""
                }
            , Maybe.map
                (\search ->
                    case filterAndSortSearch search model.ramda of
                        Err _ ->
                            E.text "error parsing"

                        Ok functions ->
                            viewFunctions functions
                )
                model.search
                |> Maybe.withDefault
                    (viewFunctions model.ramda)
            ]
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
