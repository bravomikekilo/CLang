module CLang.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Json
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

open Either
open Clang
open ClangLint
open ClangParse
open ClangRender
open FParsec
open StateList
open StateView
open TreeView

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/clang">] CLang

/// The Elmish application's model.
type Model =
    {
        error: string option
        clangSource: string
        clangDiganose: string
        states: CState list
        selectedState: CState option
        program: CProgram option
        treeView: CInst option
    }


let initModel =
    {
        error = None
        clangSource = ""
        clangDiganose = ""
        states = []
        selectedState = None
        program = None
        treeView = None
    }

/// The Elmish application's update messages.
type Message =
    | SetSource of string
    | SetTree of CInst
    | SetState of CState
    | SubmitSource
    | ExecuteStep
    | Error of exn
    | ClearError
    
let compile (src: string): Either<CProgram, string> =
    let parseResult = run pProgram src
    match parseResult with
    | Failure _ as fail ->
        Right (fail.ToString())
    | Success (ret, _, _) ->
        let lintRet = lintProgram ret
        match lintRet with
        | Left p -> Left p
        | Right errs ->
            Right <| errs.ToString()

let initState (program: CProgram): CState =
    let mainFunc = program.Item("main")
    let callFrameName = "main ()"
    let envFrameName = "function scope main()"
    let env = [{map=Map.empty; frameName=envFrameName}]
    let newOpStack = []
    let newIstack = List.map (fun s -> (0, Stmt s)) mainFunc.body
    let frame = {op=newOpStack; i=newIstack; name=callFrameName}
    [frame], env

let parseExpr (src: string) =
    let parseResult = run pExpr src
    match parseResult with
    | Failure _ as fail -> fail.ToString()
    | Success (ret, _, _) -> ret.ToString()
    
let parseStmt (src: string) =
    let parseResult = run pStmt src
    match parseResult with
    | Failure _ as fail -> fail.ToString()
    | Success (ret, _, _) -> ret.ToString()
    
let parseFunc (src: string) =
    let parseResult = run pFunc src
    match parseResult with
    | Failure _ as fail -> fail.ToString()
    | Success (ret, _, _) -> ret.ToString()

let update message model =
    match message with
    | SetSource value ->
        { model with clangSource = value}, Cmd.none
    
    | SetState state ->
        { model with selectedState = Some(state); treeView = None }, Cmd.none
    
    | SetTree cInst ->
        { model with treeView = Some(cInst) }, Cmd.none

    | SubmitSource ->
        (*
        let ret = parseFunc model.clangSource
        {model with clangDiganose = ret}, Cmd.none
        *)
        let compileResult = compile model.clangSource
        match compileResult with
        | Right err ->
            {model with clangDiganose = err}, Cmd.none
        | Left program ->
            let state = initState program
            {model with program = Some(program); states = [state]}, Cmd.none
            
    | ExecuteStep ->
        match model.program with
        | None -> model, Cmd.none
        | Some(program) ->
            match model.states with
            | [] -> model, Cmd.none
            | s :: _ ->
                let newState = execute program s
                {model with states = newState :: model.states}, Cmd.none

    | Error RemoteUnauthorizedException ->
        { model with error = Some "You have been logged out."}, Cmd.none
    | Error exn ->
        { model with error = Some exn.Message }, Cmd.none
    | ClearError ->
        { model with error = None }, Cmd.none

/// Connects the routing system to the Elmish application.

type Main = Template<"wwwroot/main.html">

let cLangPage (model: Model) dispatch =
    Main.CLang()
        .ClangSource(model.clangSource, fun n -> dispatch (SetSource n))
        .ClangDiagnose(model.clangDiganose, fun n -> ())
        .SubmitSource(fun _ -> dispatch SubmitSource)
        .StateView(
            ecomp<StateView, _, _> [] model.selectedState (fun s -> dispatch (SetTree s))
        )
        .TreeView(
            ecomp<TreeView, _, _> [] model.treeView ignore
        ).ExecuteStep(fun _ -> dispatch ExecuteStep)
        .Elt()

let view model dispatch =
    Main()
        .Menu(
            ecomp<StateList, _, _> []
                (model.states, model.selectedState)
                (fun n -> dispatch (SetState n))
         )
        .Body(cLangPage model dispatch)
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
#if DEBUG
        // |> Program.withHotReload
#endif
