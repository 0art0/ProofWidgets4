import React from "react"
import { useRpcSession, RpcPtr, DocumentPosition } from "@leanprover/infoview"
import { Html, renderHtml } from "./htmlDisplay"

interface ButtonProps {
  content: Html
  onClick: RpcPtr<'RequestMUnit'>
  style?: React.CSSProperties
  pos: DocumentPosition
}

interface TextInputProps {
  label?: string
  placeholder: string
  onChange: RpcPtr<'StringToRequestMUnit'>
  value: string
  style?: React.CSSProperties
}

interface NumberInputProps {
  placeholder: number
  onChange: RpcPtr<'NumberToRequestMUnit'>
  value: number
  max?: number
  min?: number
  width?: number
  style?: React.CSSProperties
}

interface CheckboxProps {
  checked: boolean
  onChange: RpcPtr<'BooleanToRequestMUnit'>
  style?: React.CSSProperties
}

interface DropdownProps {
  options: string[]
  selectedIndex?: number
  onChange: RpcPtr<'NumberToRequestMUnit'>
  style?: React.CSSProperties
}

interface RadioButtonProps {
  options: string[]
  selectedIndex?: number
  onChange: RpcPtr<'NumberToRequestMUnit'>
  name: string
  style?: React.CSSProperties
}

interface SliderProps {
  value: number
  onChange: RpcPtr<'NumberToRequestMUnit'>
  min?: number
  max?: number
  style?: React.CSSProperties
}

interface StatefulHtmlProps {
  html: RpcPtr<'RequestMHtml'>
  pos: DocumentPosition
}

type Interaction =
  | { type: 'Button', props: ButtonProps }
  | { type: 'TextInput', props: TextInputProps }
  | { type: 'NumberInput', props: NumberInputProps }
  | { type: 'CheckBox', props: CheckboxProps }
  | { type: 'Dropdown', props: DropdownProps }
  | { type: 'RadioButton', props: RadioButtonProps }
  | { type: 'Slider', props: SliderProps }
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
  const [renderedContent, setRenderedContent] = React.useState<JSX.Element>(<></>)

  React.useEffect(() => {
    renderHtml(rs, props.pos, props.content).then(setRenderedContent)
  }, [props.content, props.pos, rs])

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
  return <button onClick={onClick} style={props.style}>{renderedContent}</button>
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
      style={props.style}
    />
  </div>)
}

export function NumberInput(props: NumberInputProps): JSX.Element {
  const rs = useRpcSession()
  const interactionDispatch = React.useContext(InteractionDispatchContext)
  const onChange = async (e: React.ChangeEvent<HTMLInputElement>) => {
    const newValue = Number(e.target.value)
    await rs.call<NumberInputProps, null>(
      "NumberInput.rpc", { ...props, value: newValue }
    ).then(() => {
      interactionDispatch?.({ type: 'NumberInput', props: { ...props, value: newValue } })
    }).catch((e) => {
      console.error("Error changing number input:", e)
    })
  }
  return (
  <div>
    <input
      type="number"
      value={props.value}
      placeholder={props.placeholder.toString()}
      onChange={onChange}
      max={props.max}
      min={props.min}
      width={props.width}
      style={props.style}
    />
  </div>)
}

export function Checkbox(props: CheckboxProps): JSX.Element {
  const rs = useRpcSession()
  const interactionDispatch = React.useContext(InteractionDispatchContext)
  const onChange = async (e: React.ChangeEvent<HTMLInputElement>) => {
    const newChecked = e.target.checked
    await rs.call<CheckboxProps, null>(
      "Checkbox.rpc", { ...props, checked: newChecked }
    ).then(() => {
      interactionDispatch?.({ type: 'CheckBox', props: { ...props, checked: newChecked } })
    }).catch((e) => {
      console.error("Error changing checkbox:", e)
    })
  }
  return (
    <input
      type="checkbox"
      checked={props.checked}
      onChange={onChange}
      style={props.style}
    />
  )
}

export function Dropdown(props: DropdownProps): JSX.Element {
  const rs = useRpcSession()
  const interactionDispatch = React.useContext(InteractionDispatchContext)
  const onChange = async (e: React.ChangeEvent<HTMLSelectElement>) => {
    const newIndex = Number(e.target.value)
    await rs.call<DropdownProps, null>(
      "Dropdown.rpc", { ...props, selectedIndex: newIndex }
    ).then(() => {
      interactionDispatch?.({ type: 'Dropdown', props: { ...props, selectedIndex: newIndex } })
    }).catch((e) => {
      console.error("Error changing dropdown:", e)
    })
  }
  return (
    <select
      value={props.selectedIndex ?? 0}
      onChange={onChange}
      style={props.style}
    >
      {props.options.map((option, index) => (
        <option key={index} value={index}>
          {option}
        </option>
      ))}
    </select>
  )
}

export function RadioButton(props: RadioButtonProps): JSX.Element {
  const rs = useRpcSession()
  const interactionDispatch = React.useContext(InteractionDispatchContext)
  const onChange = async (e: React.ChangeEvent<HTMLInputElement>) => {
    const newIndex = Number(e.target.value)
    await rs.call<RadioButtonProps, null>(
      "RadioButton.rpc", { ...props, selectedIndex: newIndex }
    ).then(() => {
      interactionDispatch?.({ type: 'RadioButton', props: { ...props, selectedIndex: newIndex } })
    }).catch((e) => {
      console.error("Error changing radio button:", e)
    })
  }
  return (
    <div style={props.style}>
      {props.options.map((option, index) => (
        <label key={index} style={{ display: 'block' }}>
          <input
            type="radio"
            name={props.name}
            value={index}
            checked={(props.selectedIndex ?? 0) === index}
            onChange={onChange}
          />
          {option}
        </label>
      ))}
    </div>
  )
}

export function Slider(props: SliderProps): JSX.Element {
  const rs = useRpcSession()
  const interactionDispatch = React.useContext(InteractionDispatchContext)
  const onChange = async (e: React.ChangeEvent<HTMLInputElement>) => {
    const newValue = Number(e.target.value)
    await rs.call<SliderProps, null>(
      "Slider.rpc", { ...props, value: newValue }
    ).then(() => {
      interactionDispatch?.({ type: 'Slider', props: { ...props, value: newValue } })
    }).catch((e) => {
      console.error("Error changing slider:", e)
    })
  }
  return (
    <input
      type="range"
      value={props.value}
      onChange={onChange}
      min={props.min}
      max={props.max}
      style={props.style}
    />
  )
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
