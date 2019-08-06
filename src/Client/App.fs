module Client.App
open Elmish
open Elmish.UrlParser
open Elmish.Navigation

module Pages =

    type Page =
        | Home
        | Counter
    
    let toPage page =
        match page with
        | Home -> "#/home"
        | Counter -> "#/counter"
    
    let pageParser : Parser<Page->Page,Page> =
        oneOf [
            map Home (s "/")
            map Home (s "home")
            map Counter (s "counter")
        ]

    //let urlParser location = parsePath pageParser location

open Pages

type Model = {
    currentPage : Page
}
    
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
    let pageHtml page =
        match page with
        | Page.Home -> str "Home page"
        | Page.Counter -> str "Counter page"
    div [] [
        ul [] [
            li [] [
                a [ Href "#/home" ] [ str "Home" ]
            ]
            li [] [
                a [ Href "#/counter" ] [ str "Counter" ]
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