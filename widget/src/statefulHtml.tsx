import React from "react"
import { useRpcSession, RpcPtr, DocumentPosition } from "@leanprover/infoview";
import { Html, renderHtml } from "./htmlDisplay";

interface ButtonProps {
  label: string
  onClick: RpcPtr<'IOUnit'>
}

interface TextInputProps {
  label?: string
  placeholder: string
  onChange: RpcPtr<'StringToIOUnit'>
  value: string
}

interface StatefulHtmlProps {
  html: RpcPtr<'IOHtml'>
  pos: DocumentPosition
}

type Interaction =
  | { type: 'Button', props: ButtonProps }
  | { type: 'TextInput', props: TextInputProps }

type InteractionState = Interaction[]

function updateInteractionState(
    state: InteractionState,
    interaction: Interaction): InteractionState {
  return [...state, interaction]
}

const InteractionDispatchContext = React.createContext<React.Dispatch<Interaction> | null>(null)

export function Button(props: ButtonProps): JSX.Element {
  const rs = useRpcSession()
  const interactionDispatch = React.useContext(InteractionDispatchContext)
  const onClick = async () => {
    await rs.call<ButtonProps, null>(
      "Button.rpc", props)
      .then(() => {
        interactionDispatch?.({ type: 'Button', props })
      })
      .catch((e) => {
        console.error("Error clicking button:", e)
      })
  }
  return <button onClick={onClick}>{props.label}</button>
}

export function TextInput(props: TextInputProps): JSX.Element {
  const rs = useRpcSession()
  const interactionDispatch = React.useContext(InteractionDispatchContext)
  const onChange = async (e: React.ChangeEvent<HTMLInputElement>) => {
    await rs.call<TextInputProps, null>(
      "TextInput.rpc", { ...props, value: e.target.value }
    ).then(() => {
      interactionDispatch?.({ type: 'TextInput', props: { ...props, value: e.target.value } })
  }).catch((e) => {
      console.error("Error changing text input:", e)
    })
  }
  return (
  <div>
    {props.label && <label>{props.label}</label>}
    <input
      type="text"
      value={props.value}
      placeholder={props.placeholder}
      onChange={onChange}
    />
  </div>)
}

export default function StatefulHtml(props: StatefulHtmlProps): JSX.Element {
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
