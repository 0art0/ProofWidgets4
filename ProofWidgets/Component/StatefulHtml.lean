import ProofWidgets
import ProofWidgets.Util

open Lean ProofWidgets Server Jsx Elab Command

abbrev IOUnit := IO Unit
instance : TypeName IOUnit := unsafe .mk IOUnit ``IOUnit

abbrev IOHtml := IO Html
instance : TypeName IOHtml := unsafe .mk IOHtml ``IOHtml

abbrev StringToIOUnit := String → IOUnit
instance : TypeName StringToIOUnit := unsafe .mk StringToIOUnit ``StringToIOUnit

structure ButtonProps where
  label : String
  onClick : WithRpcRef IOUnit
deriving RpcEncodable

structure TextInputProps where
  label : Option String := none
  placeholder : String := ""
  onChange : WithRpcRef StringToIOUnit
  value : String := ""
deriving RpcEncodable

@[server_rpc_method]
def Button.rpc (props : ButtonProps) : RequestM (RequestTask Unit) := RequestM.asTask do
  props.onClick.val

@[server_rpc_method]
def TextInput.rpc (props : TextInputProps) : RequestM (RequestTask Unit) := RequestM.asTask do
  props.onChange.val props.value

@[widget_module]
def Button : Component ButtonProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "statefulHtml.js"
  «export» := "Button"

@[widget_module]
def TextInput : Component TextInputProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "statefulHtml.js"
  «export» := "TextInput"

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

def InteractiveT (M : Type → Type) [Monad M] [MonadLiftT IO M] [MonadDrop M IO] (α : Type) :=
  ReaderT (IO.Ref (Option α)) M Html

variable {M : Type → Type} [Monad M] [MonadLiftT IO M] [MonadDrop M IO]

def InteractiveT.ofM {α} : ReaderT (IO.Ref (Option α)) M Html → InteractiveT M α := id

def InteractiveT.pure {α} (a : α) : InteractiveT M α := .ofM do
  let state ← read
  liftM (m := IO) <| state.set (some a)
  return <div></div>

def InteractiveT.bind {α β} (x : InteractiveT M α) (f : α → InteractiveT M β) : InteractiveT M β :=
  .ofM do
    let switch : IO.Ref (Option α) ← liftM (m := IO) <| IO.mkRef none
    let state ← read
    let code : M Html := do
      match ← liftM (m := IO) switch.get with
      | none => x.run switch
      | some a => (f a).run state
    createStatefulHtml (← dropM code)

instance : Monad (InteractiveT M) where
  pure := InteractiveT.pure
  bind := InteractiveT.bind

def showButton (label : String) : InteractiveT M Unit := .ofM do
  let state ← read
  return <Button label={label} onClick={← liftM (m := IO) <| WithRpcRef.mk do state.set (some ())} />

def askChoices {α} (question : String) (options : Array (String × α)) : InteractiveT M α := .ofM do
  let answer ← read
  let optionButtons ← options.mapM fun (option, value) => do
    return <div>
      <Button label={option} onClick={← liftM (m := IO) <| WithRpcRef.mk do answer.set (some value)} />
      <br />
      </div>
  return .element "div" #[] <| #[.text question] ++ optionButtons

def askBoolean (question : String) : InteractiveT M Bool :=
  askChoices question #[("Yes", true), ("No", false)]

def askString (prompt : String) (placeholder : String := "") : InteractiveT M String := .ofM do
  let answer ← read
  let value : IO.Ref (Option String) ← liftM (m := IO) <| IO.mkRef none
  let textBox : Html ← createStatefulHtml <| return (
    <TextInput
      label={some prompt}
      placeholder={placeholder}
      onChange={← liftM (m := IO) <| WithRpcRef.mk fun str ↦ value.set (some str)}
      value={(← value.get).getD ""} />
  )
  return (
    <div>
      {textBox}
      <Button label="OK" onClick={← liftM (m := IO) <| WithRpcRef.mk do answer.set (← value.get)} />
    </div>
  )

def showHtml (html : Html) : InteractiveT M Unit := .ofM do
  return html

def runInteractive {α} (m : InteractiveT M α) : M Html := do
  let state : IO.Ref (Option α) ← liftM (m := IO) <| IO.mkRef none
  m.run state

def lift {α} (act : M α) : InteractiveT M α := .ofM do
  let state ← read
  let result ← act
  liftM (m := IO) <| state.set (some result)
  return <div></div>

instance : MonadLift M (InteractiveT M) where
  monadLift := lift

#html show IO _ from do
  let count ← IO.mkRef 0
  createStatefulHtml <|
    return <div>
      <Button label={s!"Click me! {← count.get}"} onClick={← WithRpcRef.mk (count.modify (· + 1))} />
      <p>Count: {.text s!"{← count.get}"}</p>
    </div>

#html show IO _ from do
  createStatefulHtml <| runInteractive do
    showButton "Click me!"
    showButton "And again!"
    showButton "One more time!"
    let ans ← askBoolean "Continue?"
    if ans then
      let label ← askString "What should the next button say?" "Type label here"
      showButton label
      showHtml <p>Done!</p>
    else
      showHtml <p>That's all, folks!</p>
