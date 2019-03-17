module Main exposing
    ( Model
    , Msg(..)
    , RamdaFunction
    , Sig(..)
    , SigClass(..)
    , SigType(..)
    , TypeVar(..)
    , main
    , parseSig
    , sigToString
    )

import Browser
import Browser.Dom
import Browser.Events
import Char
import Element as E exposing (DeviceClass(..))
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as JD
import List.Extra
import Maybe
import Parser exposing (..)
import Task



---- MODEL ----


type alias Model =
    { ramda : List RamdaFunction
    , search : Maybe String
    , windowSize :
        { width : Int
        , height : Int
        }
    }


type SigType
    = Generic
    | GenericVar TypeVar
    | Container String (List SigType)
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
        , succeed Boolean
            |. keyword "boolean"
        , succeed Boolean
            |. keyword "bool"
        , succeed Boolean
            |. keyword "Bool"
        , succeed Number
            |. keyword "Number"
        , succeed Number
            |. keyword "number"
        , succeed Number
            |. keyword "int"
        , succeed Number
            |. keyword "float"
        , succeed StringT
            |. keyword "String"
        , succeed StringT
            |. keyword "string"
        , succeed RegExp
            |. keyword "RegExp"
        , succeed Generic
            |. keyword "*"
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
        , succeed identity
            |= andThen
                (\xs ->
                    case xs of
                        x :: [] ->
                            Parser.commit <| GenericVar <| TypeVar x

                        x :: xs_ ->
                            List.map (GenericVar << TypeVar) xs_
                                |> Container x
                                |> Parser.commit

                        [] ->
                            Parser.problem "missing variable"
                )
                (sequence
                    { start = ""
                    , end = ""
                    , separator = ""
                    , spaces = spaces
                    , item = parseAnyWord
                    , trailing = Parser.Optional
                    }
                )
        ]


parseWord : Parser String
parseWord =
    succeed ()
        |. spaces
        |. chompIf Char.isUpper
        |. chompWhile (\c -> Char.isAlpha c)
        |> getChompedString


parseAnyWord : Parser String
parseAnyWord =
    succeed ()
        |. spaces
        |. chompIf Char.isAlphaNum
        |. chompWhile (\c -> Char.isAlphaNum c)
        |> getChompedString


parseTypeVar : Parser TypeVar
parseTypeVar =
    succeed ()
        |. chompIf Char.isLower
        |> getChompedString
        |> map TypeVar


parseClass : Parser.Parser SigClass
parseClass =
    succeed SigClass
        |= parseWord
        |. spaces
        |= parseTypeVar


parseClasses : Parser.Parser (List SigClass)
parseClasses =
    oneOf
        [ sequence
            { start = "("
            , separator = ","
            , end = ")"
            , spaces = spaces
            , item = parseClass
            , trailing = Optional
            }
        , map List.singleton parseClass
        ]


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

        Container containerType innerTypes ->
            "TODO"

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
                |> Result.map (List.sortBy .name)
                |> Result.withDefault []
      , search = Nothing
      , windowSize = { width = 0, height = 0 }
      }
    , Task.perform (\viewport -> WindowResize { width = round viewport.scene.width, height = round viewport.scene.height })
        Browser.Dom.getViewport
    )



---- UPDATE ----


sigMetric : Sig -> Sig -> Maybe Float
sigMetric sigA sigB =
    if sigA == sigB then
        Just 0.0

    else if List.Extra.isInfixOf (extractSigTypes sigA) (extractSigTypes sigB) then
        Just 0.3

    else if List.Extra.isInfixOf (extractSigTypes sigA) (extractSigTypes (convertToGenerics sigB)) then
        Just 0.5

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
    | WindowResize
        { width : Int
        , height : Int
        }


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

        WindowResize { width, height } ->
            ( { model | windowSize = { width = width, height = height } }, Cmd.none )



---- VIEW ----


colors =
    { c1 = E.rgb (253.0 / 255.0) (254.0 / 255.0) (255.0 / 255.0)
    , c2 = E.rgb (235.0 / 255.0) (237.0 / 255.0) (232.0 / 255.0)
    , c3 = E.rgb (181.0 / 255.0) (182.0 / 255.0) (181.0 / 255.0)
    , c4 = E.rgb (30.0 / 255.0) (17.0 / 255.0) (17.0 / 255.0)
    }


viewFunction : E.Device -> RamdaFunction -> E.Element Msg
viewFunction device { name, sig, category } =
    let
        isPhone =
            device.class == Phone

        nameElement =
            E.text name

        signatureElement =
            E.el
                [ Font.size 13
                , Font.family
                    [ Font.typeface "Fira Code"
                    , Font.typeface "Consolas"
                    , Font.typeface "Courier"
                    , Font.typeface "monospace"
                    ]
                ]
                (E.text (sigToString sig))

        categoryElement =
            E.el
                [ Font.size 15
                , Font.bold
                , Border.rounded 5
                , E.padding 5
                , Background.color colors.c3
                , E.alignRight
                ]
                (E.text category)

        rowAttributes =
            [ E.spacing 10
            , E.width E.fill
            ]
    in
    E.link
        [ E.pointer
        , Font.color colors.c4
        , E.mouseOver
            [ Background.color colors.c2
            ]
        , E.padding 10
        , E.width E.fill
        ]
        { url = "https://ramdajs.com/docs/#" ++ name
        , label =
            if isPhone then
                E.column
                    rowAttributes
                    [ E.row [ E.width E.fill ]
                        [ nameElement
                        , categoryElement
                        ]
                    , signatureElement
                    ]

            else
                E.row
                    rowAttributes
                    [ nameElement, signatureElement, categoryElement ]
        }


viewFunctions : E.Device -> List RamdaFunction -> E.Element Msg
viewFunctions device functions =
    E.column [ E.width E.fill ] <|
        List.map (viewFunction device) functions


view : Model -> Html Msg
view model =
    let
        device =
            E.classifyDevice model.windowSize
    in
    E.layout
        [ E.width E.fill
        , Background.color colors.c1
        , E.inFront
            (Input.search
                [ Input.focusedOnLoad
                ]
                { label = Input.labelHidden "Search function"
                , onChange = SearchTerm
                , placeholder = Just (Input.placeholder [] (E.text "Filter"))
                , text = model.search |> Maybe.withDefault ""
                }
            )
        , E.inFront
            (E.row
                [ E.alignBottom
                , E.width E.fill
                , E.padding 20
                , E.spacing 10
                , Background.color colors.c1
                ]
                [ E.text "A type driven search for ramda functions"
                , E.newTabLink
                    [ Font.underline
                    ]
                    { url = "https://twitter.com/da_wehner"
                    , label = E.text "@dawehner"
                    }
                , E.newTabLink [ Font.underline ]
                    { url = "http://github.com/dawehner/elm-ramda-search"
                    , label = E.text "Source"
                    }
                ]
            )
        ]
        (E.column [ E.width E.fill ]
            [ Maybe.map
                (\search ->
                    case filterAndSortSearch search model.ramda of
                        Err _ ->
                            E.text "error parsing"

                        Ok functions ->
                            viewFunctions device functions
                )
                model.search
                |> Maybe.withDefault
                    (viewFunctions device model.ramda)
            ]
        )



---- PROGRAM ----


main : Program JD.Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions =
            always
                (Browser.Events.onResize
                    (\width height -> WindowResize { width = width, height = height })
                )
        }
