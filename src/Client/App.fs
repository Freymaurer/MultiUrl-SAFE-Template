module Client.App
open Elmish
open Elmish.UrlParser
open Elmish.Navigation

type Page =
    | Home
    | About

let toPage = function
    | Home -> "#/home"
    | About -> "#/about"

type Model = {
    currentPage : Page
}

let pageParser : Parser<Page->Page,Page> =
    oneOf [
        map Home (s "/")
        map Home (s "home")
        map About (s "about")
    ]
    
let urlUpdate (result: Option<Page>) model =
    match result with
    | None ->
        model, Navigation.modifyUrl (toPage model.currentPage)
    | Some page ->
        { model with currentPage = page }, Cmd.none

let init result =
    let (model, cmd) = urlUpdate result { currentPage = Page.Home }
    model, cmd

let update _ model =
    model, Cmd.none

open Fable.React
open Fable.React.Props

let view model _ =
    let pageHtml = function
        | Page.Home -> str "Home page"
        | Page.About -> str "About page"
    div [] [
        ul [] [
            li [] [
                a [ Href "#/home" ] [ str "Home" ]
            ]
            li [] [
                a [ Href "#/about" ] [ str "About" ]
            ]
        ]
        div [] [
            pageHtml model.currentPage
        ]
    ]

open Elmish.React

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
|> Program.toNavigable (parseHash pageParser) urlUpdate
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run