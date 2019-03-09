module Tests exposing (all)

import Expect
import Main exposing (..)
import Parser
import Result
import Test exposing (..)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "test suite"
        [ describe "parseSigType"
            [ test "boolean" <|
                \_ ->
                    Expect.equal
                        (Ok Boolean)
                        (Parser.run parseSigType "Boolean")
            , test "number" <|
                \_ ->
                    Expect.equal
                        (Ok Number)
                        (Parser.run parseSigType "Number")
            , test "generic" <|
                \_ ->
                    Expect.equal
                        (Ok Generic)
                        (Parser.run parseSigType "*")
            , test "generic with a" <|
                \_ ->
                    Expect.equal
                        (Ok (GenericVar (TypeVar "a")))
                        (Parser.run parseSigType "a")
            , test "generic with b" <|
                \_ ->
                    Expect.equal
                        (Ok (GenericVar (TypeVar "b")))
                        (Parser.run parseSigType "b")
            , test "list of generic" <|
                \_ ->
                    Expect.equal
                        (Ok (ListT Generic))
                        (Parser.run parseSigType "[*]")
            , test "nested list of generic" <|
                \_ ->
                    Expect.equal
                        (Ok (ListT (ListT Generic)))
                        (Parser.run parseSigType "[[*]]")
            , test "list of boolean" <|
                \_ ->
                    Expect.equal
                        (Ok (ListT Boolean))
                        (Parser.run parseSigType "[Boolean]")
            , test "Function 1 argument" <|
                \_ ->
                    Expect.equal
                        (Ok (Function [ Generic, Generic ]))
                        (Parser.run parseSigType "(* -> *)")
            , test "Function 3 argument" <|
                \_ ->
                    Expect.equal
                        (Ok (Function [ Generic, Number, Generic ]))
                        (Parser.run parseSigType "(* -> Number -> *)")
            , test "Object with generic key and value" <|
                \_ ->
                    Expect.equal
                        (Ok (Object Generic Generic))
                        (Parser.run parseSigType "{*: *}")
            , test "Object with specific key and value" <|
                \_ ->
                    Expect.equal
                        (Ok (Object (GenericVar (TypeVar "k")) (GenericVar (TypeVar "v"))))
                        (Parser.run parseSigType "{k: v}")
            ]
        , describe "parseSig"
            [ test "simple function" <|
                \_ ->
                    Expect.equal
                        (Ok (SigList [ Number, Number ]))
                        (Parser.run parseSig "Number -> Number")
            , test "simple function 2 arguments" <|
                \_ ->
                    Expect.equal
                        (Ok (SigList [ Number, Number, Boolean ]))
                        (Parser.run parseSig "Number -> Number -> Boolean")
            , test "simple function generic" <|
                \_ ->
                    Expect.equal
                        (Ok (SigList [ Generic, Generic, Generic ]))
                        (Parser.run parseSig "* -> * -> *")
            , test "function with list" <|
                \_ ->
                    Expect.equal
                        (Ok (SigList [ ListT Generic, Generic ]))
                        (Parser.run parseSig "[*] -> *")
            , test "function with list and function" <|
                \_ ->
                    Expect.equal
                        (Ok (SigList [ Function [ Generic, Generic ], ListT Generic, ListT Generic ]))
                        (Parser.run parseSig "(* -> *) -> [*] -> [*]")
            ]
        , describe "parseClass"
            [ test "Functor class" <|
                \_ ->
                    Expect.equal (Ok (SigClass "Functor" (TypeVar "a")))
                        (Parser.run parseClass "Functor a")
            , test "Applicative class" <|
                \_ ->
                    Expect.equal (Ok (SigClass "Applicative" (TypeVar "b")))
                        (Parser.run parseClass "Applicative  b")
            , test "Applicative class" <|
                \_ ->
                    Expect.equal (Ok (SigClass "Applicative" (TypeVar "b")))
                        (Parser.run parseClass "Applicative  b")
            , test "Traversable class" <|
                \_ ->
                    Expect.equal (Ok (SigClass "Traversable" (TypeVar "t")))
                        (Parser.run parseClass "Traversable    t")
            ]
        , describe "parseClasses"
            [ test "mixed classes" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (SigClasses
                                [ SigClass "Functor" (TypeVar "a")
                                , SigClass "Traversable" (TypeVar "t")
                                , SigClass "Applicative" (TypeVar "b")
                                ]
                            )
                        )
                        (Parser.run parseClasses "Functor a, Traversable    t, Applicative b")
            ]
        ]
