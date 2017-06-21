module CommonNavbar exposing (NavbarLink(..), navbar)

import Bootstrap.Button as Button
import Bootstrap.Navbar as Navbar
import Html exposing (text)
import Html.Attributes exposing (class, href)
import Router exposing (routes)


type NavbarLink
    = Main
    | Login
    | Comparison


linkHref link =
    case link of
        Main ->
            routes.main.page [] |> .url |> href

        Login ->
            routes.login.page [] |> .url |> href

        Comparison ->
            routes.comparison.page [] |> .url |> href


navbar : NavbarLink -> (Navbar.State -> a) -> (Navbar.Config a -> Navbar.Config a) -> Navbar.Config a
navbar currentPage wrapper items =
    Navbar.config wrapper
        |> Navbar.withAnimation
        |> Navbar.attrs [ class "sticky-top" ]
        |> Navbar.brand [ linkHref currentPage ] [ text "Game Aggregator" ]
        |> items
        |> Navbar.customItems [ links currentPage ]


links currentPage =
    Navbar.textItem []
        ([ Main, Comparison, Login ]
            |> List.filterMap
                (\l ->
                    if not (l == currentPage) then
                        Button.linkButton [ Button.outlineSecondary, Button.attrs [ linkClass l, linkHref l ] ] [] |> Just
                    else
                        Nothing
                )
        )


linkClass link =
    case link of
        Main ->
            class "fa fa-table"

        Login ->
            class "fa fa-sign-out"

        Comparison ->
            class "fa fa-link"
