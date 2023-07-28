import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.Panel

open Lean Server

section Utils

syntax (name := motivatedProofMode) "motivated_proof" tacticSeq : tactic

def Lean.Syntax.contains? (pos : String.Pos) (stx : Syntax) : Bool := Option.toBool do
  let ⟨start, stop⟩ ← stx.getRange?
  guard <| start ≤ pos
  guard <| pos ≤ stop

def Lean.Syntax.Stack.findSmallest? (stack : Syntax.Stack) (p : Syntax → Bool) : Option Syntax :=
  stack |>.map Prod.fst |>.filter p |>.head?

def Lean.Syntax.getHeadKind? (stx : Syntax) :=
  Syntax.getKind <$> stx.getHead?

def String.getLastLine! (text : String) : String :=
  text |>.trim |>.splitOn "\n" |>.getLast!

def String.getLineIndentation (line : String) : Nat :=
  line |>.takeWhile (· ∈ [' ', '·', '{', '}']) |>.length

def Lean.Syntax.getIndentation (stx : Syntax) : Nat :=
  stx |>.reprint.get! |>.getLastLine! |>.getLineIndentation

instance [LE α] [DecidableRel (LE.le (α := α))] : Max α where
  max x y := if x ≤ y then y else x

namespace SyntaxTraversal

-- TODO: Discard or move to a different file

open Syntax MonadTraverser

abbrev SyntaxTraverserM := EStateM String Traverser

instance : MonadTraverser SyntaxTraverserM where
  st := inferInstance

def SyntaxTraverserM.runStx (τ : SyntaxTraverserM Syntax) (stx : Syntax) : Option Syntax := do
  let .ok res _ := τ.run (Traverser.fromSyntax stx) | none
  return res

def SyntaxTraverserM.runStxAndPrint (τ : SyntaxTraverserM Syntax) (stx : Syntax) : Option String :=
  τ.runStx stx >>= Syntax.reprint

-- Add a tactic `tac` to syntax of the form `motivated_proof $tacs`.
def extendMotivatedProof (tac : String) : SyntaxTraverserM Syntax := do
  goDown 1
  goDown 0
  goDown 0
  let cur ← getCur
  setCur <| cur.setArgs (cur.getArgs |>.push (.node .none `null #[]) |>.push (.atom .none tac))
  goUp
  goUp
  goUp
  getCur

end SyntaxTraversal

end Utils

section TextInsertion

structure InsertionCommandProps where
  pos : Lsp.Position
  text : String
deriving RpcEncodable

structure InsertionButton where
  label : String
  text : String
deriving RpcEncodable

structure InsertionResponse where
  edit : Lsp.WorkspaceEdit
  newPos : Lsp.Position
deriving RpcEncodable

#check IO
#check RequestM
#check Meta.MetaM.toIO _ _
#check Core.Context

#check FileWorker.EditableDocument


def insertText (snapStx : Syntax) (msg : String) (doc : FileWorker.EditableDocument) :
    RequestM InsertionResponse := do
  let filemap := doc.meta.text
  let .some motivatedProofBlock := snapStx.find? (·.getKind = ``motivatedProofMode) |
    IO.throwServerError "No motivated proof block at position."
  let .some newStx := (SyntaxTraversal.extendMotivatedProof msg).runStx motivatedProofBlock |
    IO.throwServerError "Failed to modify motivated proof block."
  let .some range := motivatedProofBlock.getRange? | IO.throwServerError "Failed to get syntax range."
  let textEdit : Lsp.TextEdit :=
    { range := { start := filemap.utf8PosToLspPos range.start, «end» := filemap.utf8PosToLspPos range.stop },
      newText := newStx.reprint.get! }
  let textDocumentEdit : Lsp.TextDocumentEdit :=
    { textDocument := { uri := doc.meta.uri, version? := doc.meta.version },
      edits := #[textEdit] }
  let edit := Lsp.WorkspaceEdit.ofTextDocumentEdit textDocumentEdit
  return { edit := edit, newPos := filemap.utf8PosToLspPos newStx.getRange?.get!.stop }

@[server_rpc_method]
def makeInsertionCommand : InsertionCommandProps → RequestM (RequestTask InsertionResponse)
  | ⟨pos, text⟩ =>
    RequestM.withWaitFindSnapAtPos pos fun snap ↦ do
      let doc ← RequestM.readDoc
      insertText snap.stx text doc

end TextInsertion

namespace MotivatedProofInterface

macro "◾" label:str " → " tac:tactic : term =>
  let text : StrLit := Syntax.mkStrLit tac.raw.reprint.get!
 `(term| InsertionButton.mk $label $text)

end MotivatedProofInterface

/-- The buttons that appear as proof-generating moves in the infoview panel. -/
def tacticButtons : Array InsertionButton :=
  #[ ◾ "Introduce variables into the context"  →  try (intros),
     ◾       "Use function extensionality"     →  try (apply funext),
     ◾           "Insert a sorry"              →  sorry,
     ◾         "Simplify the target"           →  simp ]

namespace MotivatedProofInterface

open ProofWidgets
open scoped Json Jsx

structure MotivatedProofPanelProps where
  pos : Lsp.Position
  buttons : Array InsertionButton
deriving RpcEncodable

@[widget_module]
def MotivatedProofPanel : Component MotivatedProofPanelProps where
  javascript := include_str "../../build/js/motivatedProofUI.js"



open Lean Elab Tactic in
@[tactic motivatedProofMode]
def motivatedProofImpl : Tactic
  | stx@`(tactic| motivated_proof $tacs) => do
    savePanelWidgetInfo stx ``MotivatedProofPanel do
      return json% { buttons : $(← rpcEncode tacticButtons) }
    evalTacticSeq tacs
  | _ => throwUnsupportedSyntax

end MotivatedProofInterface


example : 1 = 1 := by
  motivated_proof
    sorry
