module CLang.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Json
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client
open Clang
open ClangParse
open ClangRender
open FParsec

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/counter">] Counter
    | [<EndPoint "/clang">] CLang

/// The Elmish application's model.
type Model =
    {
        page: Page
        counter: int
        error: string option
        clangSource: string
        clangDiganose: string
        ast: CExpr option
    }

let initModel =
    {
        page = Home
        counter = 0
        error = None
        clangSource = ""
        clangDiganose = ""
        ast = None
    }

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | SetSource of string
    | SubmitSource
    | Increment
    | Decrement
    | SetCounter of int
    | Error of exn
    | ClearError

let update message model =
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none

    | Increment ->
        { model with counter = model.counter + 1 }, Cmd.none
    | Decrement ->
        { model with counter = model.counter - 1 }, Cmd.none
    | SetCounter value ->
        { model with counter = value }, Cmd.none
    | SetSource value ->
        { model with clangSource = value}, Cmd.none

    | SubmitSource ->
        let parseResult = run pExpr model.clangSource
        match parseResult with
        | Failure _ as fail ->
            {model with clangDiganose = fail.ToString()}, Cmd.none
        | Success (ret, _, _) as succ ->
            {model with clangDiganose = succ.ToString(); ast = Some(ret)}, Cmd.none

    | Error RemoteUnauthorizedException ->
        { model with error = Some "You have been logged out."}, Cmd.none
    | Error exn ->
        { model with error = Some exn.Message }, Cmd.none
    | ClearError ->
        { model with error = None }, Cmd.none

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

type Main = Template<"wwwroot/main.html">

let homePage model dispatch =
    Main.Home().Elt()

let counterPage model dispatch =
    Main.Counter()
        .Decrement(fun _ -> dispatch Decrement)
        .Increment(fun _ -> dispatch Increment)
        .Value(model.counter, fun v -> dispatch (SetCounter v))
        .Elt()

let menuItem (model: Model) (page: Page) (text: string) =
    Main.MenuItem()
        .Active(if model.page = page then "is-active" else "")
        .Url(router.Link page)
        .Text(text)
        .Elt()

let cLangPage (model: Model) dispatch =
    Main.CLang()
        .ClangSource(model.clangSource, fun n -> dispatch (SetSource n))
        .ClangDiagnose(model.clangDiganose, fun n -> ())
        .SubmitSource(fun _ -> dispatch SubmitSource)
        .AST(
            match model.ast with
            | None -> div [] []
            | Some(x) -> renderCExpr x
        )
        .Elt()

let view model dispatch =
    Main()
        .Menu(concat [
            menuItem model Home "Home"
            menuItem model Counter "Counter"
            menuItem model CLang "CLang"
        ])
        .Body(
            cond model.page <| function
            | Home -> homePage model dispatch
            | Counter -> counterPage model dispatch
            | CLang -> cLangPage model dispatch
        )
        .Error(
            cond model.error <| function
            | None -> empty
            | Some err ->
                Main.ErrorNotification()
                    .Text(err)
                    .Hide(fun _ -> dispatch ClearError)
                    .Elt()
        )
        .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkProgram (fun _ -> initModel, Cmd.none) update view
        |> Program.withRouter router
#if DEBUG
        // |> Program.withHotReload
#endif
