import Lean.Meta.ExprLens
import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.Panel

def Lean.Syntax.contains? (pos : String.Pos) (stx : Syntax)  : Bool := Option.toBool do
  let ⟨start, stop⟩ ← stx.getRange?
  guard <| start ≤ pos
  guard <| pos ≤ stop

def Lean.Syntax.Stack.findSmallest? (stack : Syntax.Stack) (p : Syntax → Bool) : Option Syntax :=
  stack |>.map Prod.fst |>.filter p |>.head?

def Lean.Syntax.getHeadKind? (stx : Syntax) :=
  Syntax.getKind <$> stx.getHead?


open Lean Server

def insertText (cmdStx : Syntax) (msg : String) (doc : FileWorker.EditableDocument) : RequestM Lsp.WorkspaceEdit := do
  let .some pos := cmdStx.getTailPos? | panic! "could not get range"
  let text := doc.meta.text
  let textEdit : Lsp.TextEdit := { range := { start := text.utf8PosToLspPos pos, «end» := text.utf8PosToLspPos pos }, newText := msg }
  let textDocumentEdit :=
    { textDocument := { uri := doc.meta.uri, version? := doc.meta.version },
      edits := #[textEdit] }
  return .ofTextDocumentEdit textDocumentEdit

structure InsertionProps where
  pos : Lsp.Position
  label : String
  text : String
deriving RpcEncodable

@[server_rpc_method]
def makeInsertionCommand : InsertionProps → RequestM (RequestTask Lsp.WorkspaceEdit)
  | ⟨pos, _, text⟩ =>
    RequestM.withWaitFindSnapAtPos pos fun snap ↦ do
      let doc ← RequestM.readDoc
      insertText snap.stx text doc

open ProofWidgets

@[widget_module]
def InsertComponent : Component InsertionProps where
  javascript := include_str "../../build/js/motivatedProofUI.js"

open scoped Jsx in
#html <InsertComponent pos={⟨58, 0⟩} label={"Introduce variables into the context"} text={"Hello world"} />

#check RequestM


@[widget_module]
def InsertPanel : Component PanelWidgetProps where
  javascript := "
import * as React from 'react'
import { EditorContext, RpcContext, mapRpcError, useAsync } from '@leanprover/infoview'

const e = React.createElement

function ConvButton(pos) {
  const rs = React.useContext(RpcContext)
  const ec = React.useContext(EditorContext)
  const st = useAsync(async () => {
    return await rs.call('makeInsertionCommand', pos)
  }, [])

  function onClick() {
    return void (async () => {
      if (st.value) {
        await ec.api.applyEdit(st.value.edit)
      }
    })()
  }

  const txt = st.value ? st.value.label : 'no insertion'

  return st.state === 'resolved' ? e('button', {onClick}, txt)
    : st.state === 'rejected' ? e('button', {style: {color: 'red'}}, mapRpcError(st.error).message)
    : e('button', null, 'Loading..')
}

export default function(props) {
  return (
  <details open=\"true\">
    <summary>Moves for constructing a well-motivated proof.</summary>
    <div>
      ConvButton(props.pos);
    </div>
  </details>);
}
  "
