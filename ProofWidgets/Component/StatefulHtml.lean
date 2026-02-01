import ProofWidgets

set_option Elab.async false

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

def InteractiveM.cast (β : Type) (x : InteractiveM β) : InteractiveM β := x

/-
/-- Close the loop in the diagram, feeding back 'x.run' until we get a value. -/
def InteractiveM.loop (x : InteractiveM α) : InteractiveM α := .ofM do
  let switch : Mutable (Option α) ← createMutableState none
  -- let state ← read
  match ← switch.get with
  | none => createStatefulHtml do x.run switch
  | some a => (InteractiveM.pure a).run

def InteractiveM.seq (ma : InteractiveM Unit) (mb : InteractiveM β) : InteractiveM β := .ofM do
  let _ ← (InteractiveM.loop ma).run .none
  return b
-/

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
  -- createStatefulHtml <|
  m.run state


#html show IO _ from do
  let count ← createMutableState (init := 0)
  let html : IO.Ref (IO Html) ← IO.mkRef do return (
    <div>
      <Button label={s!"Click me! {← count.get}"} onClick={← WithRpcRef.mk (count.modify (· + 1))} />
      <p>Count: {.text s!"{← count.get}"}</p>
    </div>)
  createStatefulHtml <| (← html.get)

#html show IO _ from do
  let count ← createMutableState (init := 0)
  let html : IO Html := do pure (
    <div>
      <Button label={s!"Click me! {← count.get}"} onClick={← WithRpcRef.mk (count.modify (· + 1))} />
      <p>Count: {.text s!"{← count.get}"}</p>
    </div>)
  createStatefulHtml  html

-- Hom(Hom(a, -), -) iso a

def Cont (r a : Type) := ((a → r) → r)

def Cont.run {r} (x : Cont r a) (k : a → r) : r :=
  x k

def Cont.pure {r} (val : a) : Cont r a := fun k => k val

def Cont.bind (ka : Cont r a) (a2kb : a → Cont r b) : Cont r b :=
  fun b2r =>
    ka.run fun aval =>
      (a2kb aval).run b2r

def Cont.map {r a b} (x : Cont r a) (f : a → b) : Cont r b :=
  x.bind (fun a => Cont.pure (f a))

instance : Monad (Cont r) where
  pure := Cont.pure
  bind := Cont.bind




-- -- (a -> r) -> r
-- def Cont' (a : Type) := ({r : Type} → (a → r) → r)

-- def Cont'.run {r} (x : Cont' a) (k : a → r) : r :=
--   x k

-- def Cont'.pure {r} (val : r) : Cont' r := fun k => k val

-- def Cont'.bind (ka : Cont' a) (a2kb : a → Cont' b) : Cont' b :=
--   a2kb (ka.run id)

-- def Cont'.map {r s} (x : Cont' r) (f : r → s) : Cont' s :=
--   x.bind (fun a => Cont'.pure (f a))

-- instance : Monad (Cont') where
--   pure := Cont'.pure
--   bind := Cont'.bind

def Builder := Html × IO.Ref Html → IO Html

def Builder.finish (x : Builder) : IO Html := do
  let htmlRef : IO.Ref Html ← IO.mkRef (<div></div>)
  let html ← x (<div></div>, htmlRef)
  htmlRef.set html
  createStatefulHtml <| htmlRef.get

def Builder.button (label : String) : Builder :=
  fun (next, ref) => do
    return <div>
      <Button label={label} onClick={← WithRpcRef.mk do ref.set next} />
    </div>

def Builder.getBool (statement : String) (b : Bool -> Builder) : Builder :=
  fun (next, ref) => do
    let resultRef : IO.Ref (Option Bool) ← IO.mkRef none
    let html :=
      <div>
        <p>{.text statement}</p>
        <Button label="True" onClick={← WithRpcRef.mk do resultRef.set true; ref.set (← b true (next, ref)) } />
        <Button label="False" onClick={← WithRpcRef.mk do resultRef.set false; ref.set (← b false (next, ref)) } />
      </div>
    return html

def Builder.getBoolK (label : String) : Cont Builder Bool := Builder.getBool label

def Builder.seq (y : Builder) (x : Builder) : Builder :=
  fun s => do
    let html ← x s
    y (html, s.2)

def Builder.buttonUnit (label : String) (rest : Unit -> Builder) : Builder :=
  Builder.seq (rest ()) (Builder.button label)

def Builder.buttonK (label : String) : Cont Builder Unit :=
  Builder.buttonUnit label

def Builder.empty : Builder :=
  fun (next, _ref) => return next

def Builder.runK (x : Cont Builder Unit) : Builder :=
  x.run (fun _ => Builder.empty)

#html show IO _ from Builder.finish <|
  Builder.button "Click me!" |>.seq
  (Builder.button "And again!" |>.seq
    (Builder.button "One more time!"))

#html show IO _ from Builder.finish <| Builder.runK <| do
  let b <- Builder.getBoolK "foo"
  if b then Builder.buttonK "Click me!" else Builder.buttonK "And again!"


  -- (Builder.button "And again!" |>.seq
    -- (Builder.button "One more time!"))


abbrev StupidM (α : Type):= StateM (Builder → Builder) α

def StupidM.button (label : String) : StupidM Unit := do
  modify fun b => b ∘ (Builder.button label).seq

def StupidM.finish (x : StupidM Unit) : IO Html :=
  let (_, f) := x.run id
  Builder.finish (f Builder.empty)


#html show IO _ from StupidM.finish do
  .button "Click me!"
  .button "And again!"
  .button "One more time!"

#html show IO _ from do
  let count ← createMutableState (init := 0)
  let html : IO.Ref Html ← IO.mkRef (<div></div>)
  let h0 : Html := (<p> That's all, folks! </p>)
  let h1 : Html := (<Button label="one more time" onClick={← WithRpcRef.mk do count.modify (· + 1); html.set ( h0)} />)
  let h2 : Html := (<Button label="and again" onClick={← WithRpcRef.mk do count.modify (· + 1); html.set ( h1)} />)
  let h3 : Html := (<Button label="click me!" onClick={← WithRpcRef.mk do count.modify (· + 1); html.set ( h2)} />)
  html.set h3
  createStatefulHtml <| return <div>{(← html.get)}{.text s!"XXCount: {← count.get}"}</div>


-- #html show IO _ from do
--   let count ← createMutableState (init := 0)
--   let html : IO.Ref (IO Html) ← IO.mkRef do return (<div></div>)
--   let h0 : IO Html := return (<p> That's all, folks! </p>)
--   let h1 : IO Html := return (<Button label="one more time" onClick={← WithRpcRef.mk do count.modify (· + 1); html.set ( h0)} />)
--   let h2 : IO Html := return (<Button label="and again" onClick={← WithRpcRef.mk do count.modify (· + 1); html.set ( h1)} />)
--   let h3 : IO Html := return (<Button label="click me!" onClick={← WithRpcRef.mk do count.modify (· + 1); html.set ( h2)} />)
--   html.set h3
--   createStatefulHtml <| return <div>{← (← html.get)}{.text s!"XXCount: {← count.get}"}</div>


#html show IO _ from do
  createStatefulHtml <| runInteractiveM do
    showButton "Click me!"
    showButton "And again!"
    showButton "One more time!"
    showHtml <p>That's all, folks!</p>
