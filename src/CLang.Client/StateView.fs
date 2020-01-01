
module StateView

open System.Text.Unicode
open Bolero
open Bolero.Html
open Clang
open ClangRender

type CallFrame = Template<"wwwroot/frame.html">
let panelBlock n = div [attr.``class`` "panel-block"] n
let renderIStackFrame (frame: int * CInst) dispatch =
    let b, inst = frame
    span [] [
        a [attr.style "margin-right: 1em"] [ text <| sprintf "b:%d" b]
        a [on.click (fun _ -> dispatch inst)] [ text <| cInstName inst ]
    ]

let renderCallFrame (callFrame: Clang.CallFrame) dispatch =
    let {op=opstack; i=istack; name=frameName} = callFrame
    CallFrame().Header(
        h1 [] [text frameName]
    ).OpStack(
        concat <| List.map (fun i -> panelBlock [text <| string i]) opstack
    ).IStack(
        concat <| List.map (fun (b, inst) -> panelBlock [renderIStackFrame (b, inst) dispatch]) istack
    ).Elt()

let renderCallStack (callstack: CallStack) dispatch =
    List.map (fun frame -> renderCallFrame frame dispatch) callstack

type EnvFrame = Template<"wwwroot/envFrame.html">

let renderEnvFrame (envFrame: Clang.EnvFrame) t =
    let {map=envMap; frameName=frameName} = envFrame
    let pair (k, v) = EnvFrame.EnvEntry().content(sprintf "%s -> %d" k v).Elt()
    let pairs = concat <| List.map pair (Map.toList envMap)
    EnvFrame().Header(text frameName).Content(pairs).Elt()

let renderEnvStack (env: Env) =
    List.mapi (fun i frame -> renderEnvFrame frame (string i)) env

type StateView() =

    inherit ElmishComponent<CState option, CInst>()

    override this.View model dispatch =
        let callStackTitle = h1 [attr.``class`` "subtitle"] [text "CallStack View"]
        let envStackTitle = h1 [attr.``class`` "subtitle"] [text "EnvStack View"]
        let ulColumn title nodes = div [attr.``class`` "main-panel"] [
            title
            ul [attr.``class`` "acc-container column"] nodes
        ]
        match model with
        | None -> concat <| [ulColumn callStackTitle []; ulColumn envStackTitle []]
        | Some(state) ->
            let callstack, env = state
            let callstackNodes = renderCallStack callstack dispatch
            let envStackNodes = renderEnvStack env
            concat [ulColumn callStackTitle callstackNodes; ulColumn envStackTitle envStackNodes]

