import Lean.Meta.ExprLens
import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.Panel

def Lean.Syntax.contains? (pos : String.Pos) (stx : Syntax) : Bool := Option.toBool do
  let ⟨start, stop⟩ ← stx.getRange?
  guard <| start ≤ pos
  guard <| pos ≤ stop

def Lean.Syntax.Stack.findSmallest? (stack : Syntax.Stack) (p : Syntax → Bool) : Option Syntax :=
  stack |>.map Prod.fst |>.filter p |>.head?

def Lean.Syntax.getHeadKind? (stx : Syntax) :=
  Syntax.getKind <$> stx.getHead?


open Lean Server

def insertText (cmdStx : Syntax) (msg : String) (doc : FileWorker.EditableDocument) : RequestM Lsp.WorkspaceEdit := do
  let .some pos := cmdStx.getTailPos? | panic! s!"Could not get range of {cmdStx}."
  let text := doc.meta.text
  let textEdit : Lsp.TextEdit :=
    { range := { start := text.utf8PosToLspPos pos, «end» := text.utf8PosToLspPos pos },
      newText := "\n    " ++ msg }
  let textDocumentEdit : Lsp.TextDocumentEdit :=
    { textDocument := { uri := doc.meta.uri, version? := doc.meta.version },
      edits := #[textEdit] }
  return .ofTextDocumentEdit textDocumentEdit

structure InsertionButton where
  label : String
  text : String
deriving RpcEncodable

structure MotivatedProofPanelProps where
  pos : Lsp.Position
  buttons : Array InsertionButton
deriving RpcEncodable

structure InsertionCommandProps where
  pos : Lsp.Position
  text : String
deriving RpcEncodable

@[server_rpc_method]
def makeInsertionCommand : InsertionCommandProps → RequestM (RequestTask Lsp.WorkspaceEdit)
  | ⟨pos, text⟩ =>
    RequestM.withWaitFindSnapAtPos pos fun snap ↦ do
      let doc ← RequestM.readDoc
      -- let fileMap := doc.meta.text
      insertText snap.stx text doc

open ProofWidgets

def tacticButtons : Array InsertionButton :=
  #[⟨"Introduce variables into the context", "intros"⟩, ⟨"Simplify the target", "simp"⟩]

@[widget_module]
def InsertComponent : Component MotivatedProofPanelProps where
  javascript := include_str "../../build/js/motivatedProofUI.js"

open scoped Jsx in
#html <InsertComponent pos={⟨58, 0⟩} buttons={tacticButtons} />

-- syntax (name := motivatedProofMode) "motivated_proof" tacticSeq : tactic

-- open Lean Elab Tactic in
-- @[tactic motivatedProofMode]
-- def motivatedProofImpl : Tactic
--   | stx@`(tactic| motivated_proof $tacs) => do
--     savePanelWidgetInfo stx _ _
--     evalTacticSeq tacs
--   | _ => throwUnsupportedSyntax
