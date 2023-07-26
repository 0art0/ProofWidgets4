import Lean.Meta.ExprLens
import ProofWidgets.Component.Panel

open Lean Server

structure InsertTextResponse where
  label : String
  edit : Lsp.WorkspaceEdit
  deriving FromJson, ToJson

def insertText (cmdStx : Syntax) (doc : FileWorker.EditableDocument) : RequestM InsertTextResponse := do
  let enterval := "Click"
  let .some pos := cmdStx.getTailPos? | panic! "could not get range"

  let text := doc.meta.text
  let msg := "\n    abracadabra"

  -- insert new syntax into document
  let textEdit : Lsp.TextEdit := { range := { start := text.utf8PosToLspPos pos, «end» := text.utf8PosToLspPos pos }, newText := msg }
  let textDocumentEdit : Lsp.TextDocumentEdit := { textDocument := { uri := doc.meta.uri, version? := doc.meta.version }, edits := [textEdit].toArray }
  let edit := Lsp.WorkspaceEdit.ofTextDocumentEdit textDocumentEdit

  return { label := enterval, edit := edit }

def Lean.Syntax.contains? (pos : String.Pos) (stx : Syntax)  : Bool := Option.toBool do
  let ⟨start, stop⟩ ← stx.getRange?
  guard <| start ≤ pos
  guard <| pos ≤ stop

def Lean.Syntax.Stack.findSmallest? (stack : Syntax.Stack) (p : Syntax → Bool) : Option Syntax :=
  stack |>.map Prod.fst |>.filter p |>.head?

def Lean.Syntax.getHeadKind? (stx : Syntax) :=
  Syntax.getKind <$> stx.getHead?

#check Snapshots.Snapshot.stx
#check Syntax.findStack?
#check FileMap.lspPosToUtf8Pos

@[server_rpc_method]
def makeInsertionCommand (cursorPos : Lsp.Position) : RequestM (RequestTask InsertTextResponse) :=
  RequestM.withWaitFindSnapAtPos cursorPos fun snap ↦ do
    let doc ← RequestM.readDoc
    let text := doc.meta.text
    -- let stk := snap.stx.findStack? (Lean.Syntax.contains? <| text.lspPosToUtf8Pos cursorPos)
    insertText snap.stx doc

open ProofWidgets

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
  return e('details', {open: true}, [e('div', null, e(ConvButton, props.pos))])
}
  "

-- macro "motivated_proof" tacs:tacticSeq : tactic =>
--   `(tactic| with_panel_widgets [InsertPanel] $tacs)

macro "abracadabra" : tactic => `(tactic| sorry)

/-! # Example usage -/

example :∀ n : ℕ, n = n := by
  with_panel_widgets [InsertPanel]
    abracadabra
