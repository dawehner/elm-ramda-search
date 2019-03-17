module Tests exposing (all)

import Expect
import Main exposing (..)
import Parser
import Result
import Test exposing (..)


all : Test
all =
    describe "test suite"
        [ describe "parseSig basic"
            [ test "boolean" <|
                \_ ->
                    Expect.equal
                        (Ok (SigList [ Boolean ]))
                        (Parser.run parseSig "Boolean")
            , test "boolean small" <|
                \_ ->
                    Expect.equal
                        (Ok (SigList [ Boolean ]))
                        (Parser.run parseSig "boolean")
            , test "boolean short" <|
                \_ ->
                    Expect.equal
                        (Ok (SigList [ Boolean ]))
                        (Parser.run parseSig "bool")
            , test "number" <|
                \_ ->
                    Expect.equal
                        (Ok (SigList [ Number ]))
                        (Parser.run parseSig "Number")
            , test "number small" <|
                \_ ->
                    Expect.equal
                        (Ok (SigList [ Number ]))
                        (Parser.run parseSig "number")
            , test "generic" <|
                \_ ->
                    Expect.equal
                        (Ok (SigList [ Generic ]))
                        (Parser.run parseSig "*")
            , test "generic with a" <|
                \_ ->
                    Expect.equal
                        (Ok (SigList [ GenericVar (TypeVar "a") ]))
                        (Parser.run parseSig "a")
            , test "generic with b" <|
                \_ ->
                    Expect.equal
                        (Ok (SigList [ GenericVar (TypeVar "b") ]))
                        (Parser.run parseSig "b")
            , test "list of generic" <|
                \_ ->
                    Expect.equal
                        (Ok (SigList [ ListT Generic ]))
                        (Parser.run parseSig "[*]")
            , test "nested list of generic" <|
                \_ ->
                    Expect.equal
                        (Ok (SigList [ ListT (ListT Generic) ]))
                        (Parser.run parseSig "[[*]]")
            , test "list of boolean" <|
                \_ ->
                    Expect.equal
                        (Ok (SigList [ ListT Boolean ]))
                        (Parser.run parseSig "[Boolean]")
            , test "Function 1 argument" <|
                \_ ->
                    Expect.equal
                        (Ok (SigList [ Function [ Generic, Generic ] ]))
                        (Parser.run parseSig "(* -> *)")
            , test "Function 3 argument" <|
                \_ ->
                    Expect.equal
                        (Ok (SigList [ Function [ Generic, Number, Generic ] ]))
                        (Parser.run parseSig "(* -> Number -> *)")
            , test "Object with generic key and value" <|
                \_ ->
                    Expect.equal
                        (Ok (SigList [ Object Generic Generic ]))
                        (Parser.run parseSig "{*: *}")
            , test "Object with specific key and value" <|
                \_ ->
                    Expect.equal
                        (Ok (SigList [ Object (GenericVar (TypeVar "k")) (GenericVar (TypeVar "v")) ]))
                        (Parser.run parseSig "{k: v}")
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
            , test "function with typeclass and arguments" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (SigWithClass
                                [ SigClass "Ord" (TypeVar "b")
                                ]
                                [ Function [ GenericVar (TypeVar "a"), GenericVar (TypeVar "b") ]
                                , ListT (GenericVar (TypeVar "a"))
                                , ListT (GenericVar (TypeVar "a"))
                                ]
                            )
                        )
                        (Parser.run parseSig "Ord b => (a -> b) -> [a] -> [a]")
            ]
        , describe "parseSig for type classes"
            [ test "Functor class" <|
                \_ ->
                    Expect.equal (Ok (SigWithClass [ SigClass "Functor" (TypeVar "a") ] [ GenericVar (TypeVar "a") ]))
                        (Parser.run parseSig "Functor a => a")
            , test "Applicative class" <|
                \_ ->
                    Expect.equal (Ok (SigWithClass [ SigClass "Applicative" (TypeVar "b") ] [ GenericVar (TypeVar "b") ]))
                        (Parser.run parseSig "Applicative  b => b")
            , test "Traversable class" <|
                \_ ->
                    Expect.equal (Ok (SigWithClass [ SigClass "Traversable" (TypeVar "t") ] [ GenericVar (TypeVar "t") ]))
                        (Parser.run parseSig "Traversable    t => t")
            ]
        , test "lensIndex" <|
            \_ ->
                Expect.equal (Ok (SigList [ Number, Container "Lens" [ GenericVar (TypeVar "s"), GenericVar (TypeVar "a") ] ]))
                    (Parser.run parseSig "Number -> Lens s a")
        , describe "parseClasses"
            [ test "mixed classes" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (SigWithClass
                                [ SigClass "Functor" (TypeVar "a")
                                , SigClass "Traversable" (TypeVar "t")
                                , SigClass "Applicative" (TypeVar "b")
                                ]
                                [ GenericVar (TypeVar "a")
                                ]
                            )
                        )
                        (Parser.run parseSig
                            "(Functor a, Traversable    t, Applicative b) => a"
                        )
            ]
        , describe "sigToString"
            [ test "F" <|
                \_ ->
                    Expect.equal "* -> Boolean" (sigToString (SigList [ Generic, Boolean ]))
            , test "add" <|
                \_ ->
                    Expect.equal "Number -> Number -> Number" (sigToString (SigList [ Number, Number, Number ]))
            , test "adjust" <|
                \_ ->
                    Expect.equal "Number -> (a -> a) -> [a] -> [a]" (sigToString (SigList [ Number, Function [ GenericVar (TypeVar "a"), GenericVar (TypeVar "a") ], ListT (GenericVar (TypeVar "a")), ListT (GenericVar (TypeVar "a")) ]))
            ]
        ]
