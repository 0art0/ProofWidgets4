import * as React from 'react';
import { Position, Range, TextEdit, WorkspaceEdit } from 'vscode-languageserver-types';
import { EditorContext, RpcContext, useAsync } from '@leanprover/infoview';
import { Button } from '@mui/material';

function insertText(props:{pos:Position, text: String}) {
  const rpcSession = React.useContext(RpcContext)
  const editorCtx = React.useContext(EditorContext)
  const asyncWSEdit = useAsync<WorkspaceEdit>(async () => {
    return await rpcSession.call('makeInsertionCommand', props)
  }, [])

  return () => {
    return void (async () => {
      if (asyncWSEdit.state === "resolved") {
        await editorCtx.api.applyEdit(asyncWSEdit.value);
      }
    })()
  };
};

function InsertionButton(props:{pos:Position, label:String, text:String}): JSX.Element {
  return <Button onClick={insertText({pos: props.pos, text: props.text})}>{props.label}</Button>;
}

export default function motivatedProofPanel(props:{pos:Position, buttons:{label:String, text:String}[]}) {
  return (
  <details open={true}>
    <summary>Motivated proof interface</summary>
    { props.buttons.map ((button:{label:String, text:String}) => {
        return <InsertionButton pos={props.pos} label={button.label} text={button.text} />;
      }) }
  </details>
  );
}
