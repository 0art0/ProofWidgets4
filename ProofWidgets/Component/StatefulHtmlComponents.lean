import ProofWidgets

open Lean ProofWidgets Server Jsx Elab Command

abbrev UnitToRequestMUnit := Unit → RequestM (RequestTask Unit)
instance : TypeName UnitToRequestMUnit := unsafe .mk UnitToRequestMUnit ``UnitToRequestMUnit

abbrev RequestMHtml := RequestM Html
instance : TypeName RequestMHtml := unsafe .mk RequestMHtml ``RequestMHtml

abbrev StringToRequestMUnit := String → RequestM (RequestTask Unit)
instance : TypeName StringToRequestMUnit := unsafe .mk StringToRequestMUnit ``StringToRequestMUnit

abbrev NatToRequestMUnit := Nat → RequestM (RequestTask Unit)
instance : TypeName NatToRequestMUnit := unsafe .mk NatToRequestMUnit ``NatToRequestMUnit

abbrev BooleanToRequestMUnit := Bool → RequestM (RequestTask Unit)
instance : TypeName BooleanToRequestMUnit := unsafe .mk BooleanToRequestMUnit ``BooleanToRequestMUnit

abbrev NumberToRequestMUnit := Nat → RequestM (RequestTask Unit)
instance : TypeName NumberToRequestMUnit := unsafe .mk NumberToRequestMUnit ``NumberToRequestMUnit

structure StatefulHtmlProps where
  html : WithRpcRef RequestMHtml
deriving RpcEncodable

@[server_rpc_method]
def StatefulHtml.rpc (props : StatefulHtmlProps) : RequestM (RequestTask Html) := RequestM.asTask do
  props.html.val

@[widget_module]
def StatefulHtml : Component StatefulHtmlProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "statefulHtml.js"
  «export» := "default"

def createStatefulHtml (html : RequestM Html) : BaseIO Html := do
  return <StatefulHtml html={← WithRpcRef.mk html} />


structure ButtonProps where
  onClick : WithRpcRef (Unit → RequestM (RequestTask Unit))
  style : Option Json := none
deriving RpcEncodable

@[server_rpc_method]
def Button.rpc (props : ButtonProps) : RequestM (RequestTask Unit) := do
  props.onClick.val ()

@[widget_module]
def Button : Component ButtonProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "statefulHtml.js"
  «export» := "Button"

structure HoverProps where
  onMouseEnter : WithRpcRef (Unit → RequestM (RequestTask Unit))
  onMouseLeave : WithRpcRef (Unit → RequestM (RequestTask Unit))
  style : Option Json := none
deriving RpcEncodable

@[server_rpc_method]
def Hover.onMouseEnter.rpc (props : HoverProps) : RequestM (RequestTask Unit) := do
  props.onMouseEnter.val ()

@[server_rpc_method]
def Hover.onMouseLeave.rpc (props : HoverProps) : RequestM (RequestTask Unit) := do
  props.onMouseLeave.val ()

@[widget_module]
def Hover : Component HoverProps where
  javascript := include_str ".." / ".." / ".lake" / "build" /"js" / "statefulHtml.js"
  «export» := "Hover"

structure TextInputProps where
  label : Option String := none
  placeholder : String := ""
  onChange : WithRpcRef (String → RequestM (RequestTask Unit))
  value : String := ""
  style : Option Json := none
deriving RpcEncodable

@[server_rpc_method]
def TextInput.rpc (props : TextInputProps) : RequestM (RequestTask Unit) := do
  props.onChange.val props.value

@[widget_module]
def TextInput : Component TextInputProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "statefulHtml.js"
  «export» := "TextInput"

structure NumberInputProps where
  placeholder : String := ""
  onChange : WithRpcRef (Nat → RequestM (RequestTask Unit))
  max : Option Nat := none
  min : Option Nat := none
  value : Option Nat := none
  width: Option Nat := none
  style : Option Json := none
deriving RpcEncodable

@[server_rpc_method]
def NumberInput.rpc (props : NumberInputProps) : RequestM (RequestTask Unit) := do
  match props.value with
  | some n => props.onChange.val n
  | none => RequestM.asTask do pure ()

@[widget_module]
def NumberInput : Component NumberInputProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "statefulHtml.js"
  «export» := "NumberInput"

structure CheckboxProps where
  checked : Bool := false
  onChange : WithRpcRef (Bool → RequestM (RequestTask Unit))
  style : Option Json := none
deriving RpcEncodable

@[server_rpc_method]
def Checkbox.rpc (props : CheckboxProps) : RequestM (RequestTask Unit) := do
  props.onChange.val props.checked

@[widget_module]
def Checkbox : Component CheckboxProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "statefulHtml.js"
  «export» := "Checkbox"

structure DropdownProps where
  options : Array String
  selectedIndex : Option Nat := none
  onChange : WithRpcRef (Nat → RequestM (RequestTask Unit))
  style : Option Json := none
deriving RpcEncodable

@[server_rpc_method]
def Dropdown.rpc (props : DropdownProps) : RequestM (RequestTask Unit) := do
  match props.selectedIndex with
  | some idx => props.onChange.val idx
  | none => props.onChange.val 0

@[widget_module]
def Dropdown : Component DropdownProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "statefulHtml.js"
  «export» := "Dropdown"

structure RadioButtonProps where
  options : Array String
  selectedIndex : Option Nat := none
  onChange : WithRpcRef (Nat → RequestM (RequestTask Unit))
  name : String
  style : Option Json := none
deriving RpcEncodable

@[server_rpc_method]
def RadioButton.rpc (props : RadioButtonProps) : RequestM (RequestTask Unit) := do
  match props.selectedIndex with
  | some idx => props.onChange.val idx
  | none => props.onChange.val 0

@[widget_module]
def RadioButton : Component RadioButtonProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "statefulHtml.js"
  «export» := "RadioButton"

structure SliderProps where
  value : Nat := 0
  onChange : WithRpcRef (Nat → RequestM (RequestTask Unit))
  min : Option Nat := none
  max : Option Nat := none
  style : Option Json := none
deriving RpcEncodable

@[server_rpc_method]
def Slider.rpc (props : SliderProps) : RequestM (RequestTask Unit) := do
  props.onChange.val props.value

@[widget_module]
def Slider : Component SliderProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "statefulHtml.js"
  «export» := "Slider"

#html show BaseIO Html from do
  let count ← IO.mkRef 0
  createStatefulHtml <| return (
    <div>
      <Button onClick={← WithRpcRef.mk (fun _ ↦ RequestM.asTask do count.modify (· + 1))}>
        <text>Click me!</text>
      </Button>
      <p>Count: {.text s!"{← count.get}"}</p>
    </div>
)

#html show BaseIO Html from do
  let text ← IO.mkRef "Hello, world!"
  createStatefulHtml <| return (
    <div>
      <TextInput
        value={← text.get}
        placeholder={← text.get}
        onChange={← WithRpcRef.mk fun s ↦ RequestM.asTask (text.set s)} />
      <p>You entered: {.text (← text.get)}</p>
    </div>
)

#check JsonRpc.Request

deriving instance ToJson, FromJson for PUnit

def sendEdit (edit : Lsp.TextEdit) : RequestM (RequestTask Unit) := do
  return (← ServerTask.mapCheap (handleServerResponse (α := Unit)) <$>
    Server.RequestM.sendServerRequest Lsp.ApplyWorkspaceEditParams Unit "workspace/applyEdit"
    { edit := .ofTextEdit (Server.FileWorker.EditableDocument.versionedIdentifier (← read).doc) edit })
where
  handleServerResponse {α} : ServerRequestResponse α → Except RequestError α := fun
    | .success res => .ok res
    | .failure code message => .error { code, message }





#html show IO Html from do
  return <Button onClick={← WithRpcRef.mk <| fun _ ↦ sendEdit {
    newText := "#check 1 + 1",
    range := { start := { line := 200, character := 0 },
                «end» := { line := 200, character := 0 } } }}>
      <Button onClick={← WithRpcRef.mk <| fun _ ↦ sendEdit {
        newText := "#check 2 + 3",
        range := { start := { line := 200, character := 0 },
                    «end» := { line := 200, character := 0 } } }}>
        {.text "New text"}
      </Button>
    </Button>

abbrev ContT (m : Type → Type) (r a : Type) := (a → m r) → m r

def ContT.pure {m r α} (a : α) : ContT m r α := fun k ↦ k a

def ContT.bind {m r α β} (x : ContT m r α) (f : α → ContT m r β) : ContT m r β :=
  fun k ↦ x (fun a ↦ f a k)

instance {m r} [Monad m] : Monad (ContT m r) where
  pure := ContT.pure
  bind := ContT.bind

instance {m r} [Monad m] : MonadLift m (ContT m r) where
  monadLift {α} (ma : m α) : ContT m r α := fun k ↦ do
    let a ← ma
    k a

def ContT.eval {m r α} (x : ContT m r α) (k : α → m r) : m r := x k

def ContT.run {m r} [Monad m] (x : ContT m r r) : m r := x.eval Pure.pure

def ContT.extract {m α} [Monad m] [MonadLiftT (ST IO.RealWorld) m] (x : ContT m Unit α) : m (Option α) := do
  let resultRef : ST IO.RealWorld (IO.Ref (Option α)) := IO.mkRef none
  let result : IO.Ref (Option α) ← resultRef
  x.eval (fun a ↦ result.set (some a))
  result.get

abbrev InteractiveT (m : Type → Type) := ReaderT (IO.Ref Html) (ContT m Unit)

def InteractiveT.run {m} [Monad m] [MonadLiftT BaseIO m] (code : InteractiveT m Unit) : m Html := do
  let htmlRef ← IO.mkRef <text>Loading...</text>
  ReaderT.run code htmlRef |>.run
  createStatefulHtml htmlRef.get

def createButton (label : String) : InteractiveT IO Unit := fun htmlRef k ↦ do
  htmlRef.set <| <Button onClick={← WithRpcRef.mk (fun () ↦ RequestM.asTask do k ())}>{.text label}</Button>

def createHtml (html : Html) : InteractiveT IO Unit := fun htmlRef k ↦ do
  htmlRef.set html
  k ()

def askBool (question : String) : InteractiveT IO Bool := fun htmlRef k ↦ do
  htmlRef.set <|
    <div>
      <p>{.text question}</p>
      <Button onClick={← WithRpcRef.mk (fun () ↦ RequestM.asTask do k true)}>{.text "Yes"}</Button>
      <Button onClick={← WithRpcRef.mk (fun () ↦ RequestM.asTask do k false)}>{.text "No"}</Button>
    </div>

def askString (question : String) : InteractiveT IO String := fun htmlRef k ↦ do
  let result ← IO.mkRef ""
  let value ← IO.mkRef ""
  htmlRef.set <|
    <div>
      <p>{.text question}</p>
      <TextInput
        placeholder={← value.get}
        value={← value.get}
        onChange={← WithRpcRef.mk (fun newVal ↦ RequestM.asTask do value.set newVal)} />
      <Button onClick={← WithRpcRef.mk (fun () ↦ RequestM.asTask do
          let val ← value.get; result.set val; k val)}>
        {.text "Submit"}
      </Button>
    </div>

#html show IO _ from do InteractiveT.run do
  let name ← askString "What is your name?"

#html show BaseIO _ from do
  let visible : IO.Ref Bool ← IO.mkRef false
  createStatefulHtml <| return <Hover
    style={json% { position: "relative", display: "inline-block" }}
    onMouseEnter={← WithRpcRef.mk (fun () ↦ RequestM.asTask do visible.set true)}
    onMouseLeave={← WithRpcRef.mk (fun () ↦ RequestM.asTask do visible.set false)}>
      <text>Hover over me!</text>
      {if ← visible.get then
        <div
          style={json% {
            position: "absolute",
            bottom: "calc(100% + 8px)",
            left: "50%",
            transform: "translateX(-50%)",
            background: "#1e1e2e",
            color: "#cdd6f4",
            padding: "6px 10px",
            borderRadius: 4,
            fontSize: 12,
            fontFamily: "system-ui, sans-serif",
            whiteSpace: "nowrap",
            boxShadow: "0 4px 12px rgba(0,0,0,0.4)",
            border: "1px solid rgba(255,255,255,0.12)",
            zIndex: 1000,
            pointerEvents: "none"
          }}>
          <text>Hello!</text>
          <div style={json% {
            position: "absolute",
            top: "100%",
            left: "50%",
            transform: "translateX(-50%)",
            width: 0,
            height: 0,
            borderLeft: "6px solid transparent",
            borderRight: "6px solid transparent",
            borderTop: "6px solid #1e1e2e"
          }}></div>
        </div>
        else
          <span></span>}
      </Hover>
