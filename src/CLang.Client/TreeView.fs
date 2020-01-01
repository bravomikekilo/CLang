
module TreeView

open Bolero
open Bolero.Html
open Clang
open ClangRender

type TreeView() =

    inherit ElmishComponent<CInst option, unit>()

    override this.View model dispatch =
        match model with
        | None -> div [] []
        | Some inst -> renderCInst inst