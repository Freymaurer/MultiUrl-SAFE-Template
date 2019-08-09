module Client.App

open Elmish
open Elmish.UrlParser
open Elmish.Navigation
open Thoth.Json
open Fable.Core.JsInterop

module Pages =

    type Page =
        | Home
        | Counter
    
    let toPage page =
        match page with
        | Home -> "#/home"
        | Counter -> "#/counter"
    
    let pageParser : Parser<Page->_,_> =
        oneOf [
            map Home (s "/")
            map Home (s "home")
            map Counter (s "counter")
        ]

open Pages

type PageModel =
| HomeModel
| CounterModel of Counter.Model

type Msg =
| HomeMsg
| CounterMsg of Counter.Msg

type Model = {
    PageModel   : PageModel
}

let urlUpdate (result: Page option) (model:Model) =
    match result with
    | None ->
        model, Navigation.modifyUrl (*(toPage Page.Home)*) (toPage Page.Home)
    | Some Page.Home ->
        { model with PageModel = PageModel.HomeModel }, Cmd.none
    // TOREMOVE FOR WORKING
    | Some Page.Counter ->
        let m, cmd = Counter.init ()
        { model with PageModel = PageModel.CounterModel m }, Cmd.map CounterMsg cmd
    //| Some Page.Counter ->
    //    //let m, cmd = Counter.init ()
    //    { model with PageModel = PageModel.CounterModel }, Cmd.none

let hydrateModel (json:string) (page: Page option) : Model * Cmd<_> =
    // The page was rendered server-side and now react client-side kicks in.
    // If needed, the model could be fixed up here.
    // In this case we just deserialize the model from the json and don't need to to anything special.
    let model: Model = Decode.Auto.unsafeFromString(json)
    match page, model.PageModel with
    | Some Page.Home, HomeModel -> model, Cmd.none
    | Some Page.Counter, CounterModel _ -> model, Cmd.none
    | _, HomeModel |  _, CounterModel _ ->
        // unknown page or page does not match model -> go to home page
        { PageModel = HomeModel }, Cmd.none


let init page =
    // was the page rendered server-side?
    let stateJson: string option = !!Browser.Dom.window?__INIT_MODEL__

    match stateJson with
    | Some json ->
        // SSR -> hydrate the model
        let model, cmd = hydrateModel json page
        model, cmd
    | None ->
        // no SSR -> show home page
        let model =
            { PageModel = HomeModel }

        urlUpdate page model

let update msg currentModel =
    match msg, currentModel.PageModel with
    | HomeMsg, HomeModel ->
        { currentModel with PageModel = HomeModel}, Cmd.none

    // TOREMOVE FOR WORKING
    | CounterMsg msg, CounterModel m ->
        let m, cmd =
            Counter.update msg m
        { currentModel with
            PageModel = CounterModel m }, Cmd.map CounterMsg cmd
    | _ -> currentModel,Cmd.none

//let update _ model =
//    model, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma

let view model (dispatch: Msg -> unit) =
    let pageHtml pageModel =
        match pageModel with
        | HomeModel -> str "you are viewing the HomeModel"
        | CounterModel (_) -> str "you are viewing the CounterModel"
    Hero.hero
        [ Hero.IsHalfHeight
        ]
        [ Hero.head [ ]
           [ ]
          Hero.body [ ]
            [ Container.container
                [ Container.IsFluid
                  Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                [ Heading.h1 [ ]
                    [ str "Test Application" ]
                  Heading.h2 [ Heading.IsSubtitle ]
                    [ str "using Elmish Navigation" ]
                  Box.box'
                    []
                    [ Text.p
                        [   Modifiers [ Modifier.TextSize (Screen.All,TextSize.Is4)
                                        Modifier.TextColor IsDanger] ]
                        [   pageHtml model.PageModel
                        ]
                    ]
                  Box.box'
                    [ ]
                    [
                      div [] [
                          a [ Href "#/home" ] [ str "Home" ]
                      ]
                      div [] [
                          a [ Href "#/counter" ] [ str "Counter" ]
                      ]
                    ]
                  Box.box' [ ] [
                      match model.PageModel with
                      | HomeModel -> 
                        yield Home.view ()
                      // TOREMOVE FOR WORKING
                      | CounterModel m ->
                        yield Counter.view { Model = m; Dispatch = (CounterMsg >> dispatch) }
                      //| _ ->
                      //    yield str "this does not exist yet"
                  ]  
                ]
            ]
        ]


open Elmish.React
open Elmish.Debug

let withReact =
    if (!!Browser.Dom.window?__INIT_MODEL__)
    then Program.withReactHydrate
    else Program.withReactSynchronous


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
|> Program.toNavigable (parseHash pageParser) urlUpdate
#if DEBUG
|> Program.withConsoleTrace
#endif
|> withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run