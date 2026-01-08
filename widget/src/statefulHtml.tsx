import React from "react"
import { useRpcSession, RpcPtr, DocumentPosition } from "@leanprover/infoview";
import { Html, renderHtml } from "./htmlDisplay";

interface ButtonProps {
  label: string
  onClick: RpcPtr<'IOUnit'>
}

interface StatefulHtmlProps {
  html: RpcPtr<'IOHtml'>
  pos: DocumentPosition
}

type Interaction =
  | { type: 'Button', props: ButtonProps }

type InteractionState = Interaction[]

function updateInteractionState(
    state: InteractionState,
    interaction: Interaction): InteractionState {
  return [...state, interaction]
}

const InteractionDispatchContext = React.createContext<React.Dispatch<Interaction> | null>(null)

export function Button (props: ButtonProps): JSX.Element {
  const rs = useRpcSession()
  const interactionDispatch = React.useContext(InteractionDispatchContext)
  return <button onClick={async () => {
    await rs.call<ButtonProps, null>(
      "Button.rpc", props)
      .then(() => {
        interactionDispatch?.({ type: 'Button', props })
      })
      .catch((e) => {
        console.error("Error clicking button:", e)
      })
  }}>{props.label}</button>
}

export default function StatefulHtml (props: StatefulHtmlProps): JSX.Element {
  const rs = useRpcSession()
  const [renderedContent, setRenderedContent] = React.useState<JSX.Element>(<></>)

  const [interactionState, dispatchInteraction] = React.useReducer(
    updateInteractionState,
    [] as InteractionState
  )

  React.useEffect(() => {
    rs.call<StatefulHtmlProps, Html>(
      "StatefulHtml.rpc", props)
      .then(async (result) => {
        setRenderedContent(await renderHtml(rs, props.pos, result))
      })
      .catch((e) => {
        console.error("Error fetching `StatefulHtml`:", e)
      })
  }, [interactionState, rs])

  return (
    <InteractionDispatchContext.Provider value={dispatchInteraction}>
      {renderedContent}
    </InteractionDispatchContext.Provider>
  )
}
