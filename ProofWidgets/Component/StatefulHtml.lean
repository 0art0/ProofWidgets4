import ProofWidgets

open Lean ProofWidgets Server Jsx Elab Command

abbrev IOUnit := IO Unit
instance : TypeName IOUnit := unsafe .mk IOUnit ``IOUnit

abbrev IOHtml := IO Html
instance : TypeName IOHtml := unsafe .mk IOHtml ``IOHtml

structure ButtonProps where
  label : String
  onClick : WithRpcRef IOUnit
deriving RpcEncodable

@[server_rpc_method]
def Button.rpc (props : ButtonProps) : RequestM (RequestTask Unit) := RequestM.asTask do
  props.onClick.val

@[widget_module]
def Button : Component ButtonProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "statefulHtml.js"
  «export» := "Button"

structure StatefulHtmlProps where
  html : WithRpcRef IOHtml
deriving RpcEncodable

@[server_rpc_method]
def StatefulHtml.rpc (props : StatefulHtmlProps) : RequestM (RequestTask Html) := RequestM.asTask do
  props.html.val

@[widget_module]
def StatefulHtml : Component StatefulHtmlProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "statefulHtml.js"
  «export» := "default"

def createStatefulHtml (html : IO Html) : IO Html := do
  return <StatefulHtml html={← WithRpcRef.mk html} />

abbrev Mutable := IO.Ref

abbrev createMutableState {α} (init : α) : IO (Mutable α) := IO.mkRef init

def InteractiveM (α : Type) := ReaderT (Mutable <| Option α) IO Html

def InteractiveM.ofM {α} : ReaderT (Mutable <| Option α) IO Html → InteractiveM α :=
  id

def InteractiveM.pure {α} (a : α) : InteractiveM α := .ofM do
 (← read).set (some a)
 return <div></div>

def InteractiveM.bind {α β} (x : InteractiveM α) (f : α → InteractiveM β) : InteractiveM β := .ofM do
  let switch : Mutable (Option α) ← createMutableState none
  let state ← read
  createStatefulHtml do
    match ← switch.get with
    | none => x.run switch
    | some a => (f a).run state

instance : Monad InteractiveM where
  pure := InteractiveM.pure
  bind := InteractiveM.bind

def showButton (label : String) : InteractiveM Unit := .ofM do
  let state ← read
  return <Button label={label} onClick={← WithRpcRef.mk do state.set (some ())} />

def showHtml (html : Html) : InteractiveM Unit := .ofM do
  return html

def runInteractiveM {α} (m : InteractiveM α) : IO Html := do
  let state : Mutable (Option α) ← createMutableState none
  m.run state

#html show IO _ from do
  let count ← createMutableState (init := 0)
  createStatefulHtml <|
    return <div>
      <Button label={s!"Click me! {← count.get}"} onClick={← WithRpcRef.mk (count.modify (· + 1))} />
      <p>Count: {.text s!"{← count.get}"}</p>
    </div>

#html show IO _ from do
  createStatefulHtml <| runInteractiveM do
    showButton "Click me!"
    showButton "And again!"
    showButton "One more time!"
    showHtml <p>That's all, folks!</p>
