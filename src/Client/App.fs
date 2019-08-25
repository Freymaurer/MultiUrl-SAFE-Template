module Client.App

open Elmish
open Elmish.UrlParser
open Elmish.Navigation
open Thoth.Json
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Fulma

//module Pages =

    //type Page =
    //    | Home
    //    | Counter
    
    //let toPage page =
    //    match page with
    //    | Home -> "#/home"
    //    | Counter -> "#/counter"
    
    //let pageParser : Parser<Page->_,_> =
    //    oneOf [
    //        map Home (s "/")
    //        map Home (s "home")
    //        map Counter (s "counter")
    //    ]

//open Pages

//type PageModel =
//| HomeModel
//| CounterModel of Counter.Model

//type Msg =
//| HomeMsg
//| CounterMsg of Counter.Msg

//type Model = {
//    PageModel   : PageModel
//    Page        : Page
//}



//let urlUpdate (result: Page option) (model:Model) =
//    match result with
//    | None ->
//        model, Navigation.modifyUrl (toPage Page.Home)
//    | Some Page.Home ->
//        { model with
//            PageModel = PageModel.HomeModel
//            Page = Home }, Cmd.none
//    | Some Page.Counter ->
//        let m, cmd = Counter.init ()
//        { model with
//            PageModel = PageModel.CounterModel m
//            Page = Counter}, Cmd.map CounterMsg cmd

//let hydrateModel (json:string) (page: Page option) : Model * Cmd<_> =
//    // The page was rendered server-side and now react client-side kicks in.
//    // If needed, the model could be fixed up here.
//    // In this case we just deserialize the model from the json and don't need to to anything special.
//    let model: Model = Decode.Auto.unsafeFromString(json)
//    match page, model.PageModel with
//    | Some Page.Home, HomeModel -> model, Cmd.none
//    | Some Page.Counter, CounterModel _ -> model, Cmd.none
//    | _, HomeModel |  _, CounterModel _ ->
//        // unknown page or page does not match model -> go to home page
//        { PageModel = HomeModel
//          Page  =   Home }, Cmd.none


//let init page =
//    // was the page rendered server-side?
//    let stateJson: string option = !!Browser.Dom.window?__INIT_MODEL__

//    match stateJson with
//    | Some json ->
//        // SSR -> hydrate the model
//        let model, cmd = hydrateModel json page
//        model, cmd
//    | None ->
//        // no SSR -> show home page
//        let model =
//            { PageModel = HomeModel
//              Page      = Home}

//        urlUpdate page model

//let update msg currentModel =
//    match msg, currentModel.PageModel with
//    | HomeMsg, HomeModel ->
//        let nextModel = {
//            currentModel with
//                PageModel = HomeModel
//                Page = Home
//            }
//        nextModel, Cmd.none
//    | CounterMsg msg, CounterModel m ->
//        let m, cmd =
//            Counter.update msg m
//        let nextModel = {
//            currentModel with
//                PageModel = CounterModel m
//                Page = Counter
//            }
//        nextModel, Cmd.map CounterMsg cmd
//    | _ -> currentModel,Cmd.none

type Hero = string

type Route =
    | Root
    | Heroes
    | Dashboard
    | Detail of int

let toRouteUrl route =
    match route with
    | Route.Root -> "/"
    | Route.Dashboard -> "/dashboard"
    | Route.Heroes -> "/heroes"
    | Route.Detail id -> sprintf "/detail/%d" id

type Model = {
    CurrentRoute : Route option
    Heroes : Map<int,Hero>
}

type Msg =
    | Navigate of Route
    | RemoveHero of int
    | AddHero of Hero
    | UpdateHero of int*Hero

module Routing =

    open Elmish.UrlParser

    let private route =
        oneOf [
            map Route.Root (s "")
            map Route.Dashboard (s "dashboard")
            map Route.Heroes (s "heroes")
            map Route.Detail (s "detail" </> i32)
        ]

    // Take a window.Location object and return an option of Route

    let parsePath location = UrlParser.parsePath route location

let urlUpdate (route: Route option) (model:Model) =
    { model with CurrentRoute = route }, Cmd.none

let init _ =
    let model ={
        Heroes = Map.empty
        CurrentRoute = None
    }
    let route = Routing.parsePath Browser.Dom.document.location
    urlUpdate route model


let update msg model =
    match msg with
    | Navigate route ->
        model, Navigation.newUrl (toRouteUrl route)
    | _ -> 
        model, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma

let layout dispatch page =
    Hero.hero [ Hero.IsMedium; Hero.Color IsSuccess ][
        Hero.head [] [
            Tabs.tabs [ Tabs.IsBoxed; Tabs.IsCentered ] [
            a [Href "#"; OnClick (fun ev -> ev.preventDefault(); Msg.Navigate Route.Dashboard |> dispatch)] [str "Dashboard"]
            a [Href "#"; OnClick (fun ev -> ev.preventDefault(); Msg.Navigate Route.Heroes |> dispatch)] [str "Heroes"]
            ]
        ]
        Hero.body [] [
            Box.box' [ Modifiers [ Modifier.TextAlignment (Screen.All,TextAlignment.Centered) ] ] [
                p [] [ page ]
            ]
        ]
        Hero.foot [ ] [
        ]
    ]

let App model dispatch =
    match model.CurrentRoute with
    | Some Route.Root -> str "redirecting..."
    | Some Route.Dashboard -> str "this is your dashboard"
    | Some Route.Heroes -> str "test heroe"
    | Some (Route.Detail id) -> str (sprintf "detail testing")
    | None -> h1 [] [str "404 - Hero not found"]
    |> layout dispatch
    
let view model dispatch =
    App model dispatch

//let safeComponents =
//    let components =
//        span [ ]
//           [ a [ Href "https://github.com/SAFE-Stack/SAFE-template" ]
//               [ str "SAFE  "
//                 str Version.template ]
//             str ", "
//             a [ Href "http://suave.io" ] [ str "Suave" ]
//             str ", "
//             a [ Href "http://fable.io" ] [ str "Fable" ]
//             str ", "
//             a [ Href "https://elmish.github.io" ] [ str "Elmish" ]
//             str ", "
//             a [ Href "https://fulma.github.io/Fulma" ] [ str "Fulma" ]
//             str ", "
//             a [ Href "https://zaid-ajaj.github.io/Fable.Remoting/" ] [ str "Fable.Remoting" ]

//           ]

//    span [ ]
//        [ str "Version "
//          strong [ ] [ str Version.app ]
//          str " powered by: "
//          components ]

//let view model (dispatch: Msg -> unit) =
//    let pageHtml pageModel =
//        match pageModel with
//        | HomeModel -> str "you are viewing the HomeModel"
//        | CounterModel (_) -> str "you are viewing the CounterModel"
//    Hero.hero
//        [ Hero.IsHalfHeight
//        ]
//        [ Hero.head
//            [ ]
//            [ Tabs.tabs
//                [ Tabs.IsBoxed
//                  Tabs.IsCentered ]
//                [ Tabs.tab [ (if model.Page = Home then Tabs.Tab.IsActive true else Tabs.Tab.IsActive false) ]
//                    [ a [ Href "#/home" ] [ str "Home" ] ]
//                  Tabs.tab [ (if model.Page = Counter then Tabs.Tab.IsActive true else Tabs.Tab.IsActive false) ]
//                    [ a [ Href "#/counter" ] [ str "Counter" ] ]
//                ]
//            ]
//          Hero.body [ ]
//            [ Container.container
//                [ Container.IsFluid
//                  Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
//                [ Heading.h1 [ ]
//                    [ str "Test Application" ]
//                  Heading.h2 [ Heading.IsSubtitle ]
//                    [ str "using Elmish Navigation" ]
//                  Box.box'
//                    []
//                    [ Text.p
//                        [   Modifiers [ Modifier.TextSize (Screen.All,TextSize.Is4)
//                                        Modifier.TextColor IsDanger] ]
//                        [   pageHtml model.PageModel
//                        ]
//                    ]
//                  Box.box' [ ] [
//                      match model.PageModel with
//                      | HomeModel -> 
//                        yield Home.view ()
//                      | CounterModel m ->
//                        yield Counter.view { Model = m; Dispatch = (CounterMsg >> dispatch) }
//                      //| _ ->
//                      //    yield str "this does not exist yet"
//                  ]  
//                ]
//            ]
//          Hero.foot []
//            [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
//                [ safeComponents ] ] 
//        ]




open Elmish.React
open Elmish.Debug
open Elmish.Navigation

//let withReact =
//    if (!!Browser.Dom.window?__INIT_MODEL__)
//    then Program.withReactHydrate
//    else Program.withReactSynchronous


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
|> Program.toNavigable Routing.parsePath urlUpdate
//|> Program.toNavigable (parseHash pageParser) urlUpdate
#if DEBUG
|> Program.withConsoleTrace
#endif
//|> withReact "elmish-app"
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run