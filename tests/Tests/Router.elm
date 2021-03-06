module Tests.Router exposing (tests)

import Data.Category exposing (Category(..))
import Data.Item as Item
import Expect
import Router
import Tagged
import Test exposing (..)


tests : Test
tests =
    describe "Router"
        [ describe "parseExternal"
            [ test "should return nothing for an unknown route" <|
                \() ->
                    Expect.equal Nothing (Router.parseExternal "https://www.google.com")
            , test "should return nothing for an invalid route" <|
                \() ->
                    Expect.equal Nothing (Router.parseExternal "foo")
            , test "should return a route to view an item" <|
                \() ->
                    Expect.equal
                        (Just (Router.ViewItem (Tagged.tag 20)))
                        (Router.parseExternal "https://news.ycombinator.com/item?id=20")
            , test "should return a route to view a user" <|
                \() ->
                    Expect.equal
                        (Just (Router.ViewUser (Tagged.tag "mthadley")))
                        (Router.parseExternal "https://news.ycombinator.com/user?id=mthadley")
            , test "should return a route to view the ask category" <|
                \() ->
                    Expect.equal
                        (Just (Router.View Ask))
                        (Router.parseExternal "https://news.ycombinator.com/ask")
            , test "should return a route to view the show category" <|
                \() ->
                    Expect.equal
                        (Just (Router.View Show))
                        (Router.parseExternal "https://news.ycombinator.com/show")
            , test "should return a route to view the jobs category" <|
                \() ->
                    Expect.equal
                        (Just (Router.View Job))
                        (Router.parseExternal "https://news.ycombinator.com/jobs")
            , test "should return a route to view the newest category" <|
                \() ->
                    Expect.equal
                        (Just (Router.View New))
                        (Router.parseExternal "https://news.ycombinator.com/newest")
            ]
        ]
