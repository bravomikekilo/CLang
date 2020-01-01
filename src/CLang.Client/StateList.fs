module StateList

open Bolero
open Bolero.Html
open Clang

type StateList() =

    inherit ElmishComponent<CState list * CState option, CState>()

    override this.View model dispatch =
        let states, selected = model
        let l = List.length states
        let every index state =
            let realIndex = l - index
            let isSelected =
                match selected with
                | None -> false
                | Some(x) -> x = state
            let selectAttribute = if isSelected then [attr.``class`` "is-active"] else []
            let attribute = (on.click (fun _ -> dispatch state)) :: selectAttribute
            a attribute [text <| "state " + string realIndex]
        concat <| List.mapi every states

