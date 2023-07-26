import ProofWidgets.Component.HtmlDisplay

open Lean ProofWidgets

structure NoProps where
deriving Server.RpcEncodable

@[widget_module]
def LinearDiophantine : Component NoProps where
  javascript := include_str "../../build/js/sliderDiophantine.js"

open scoped ProofWidgets.Jsx in
#html <LinearDiophantine />
