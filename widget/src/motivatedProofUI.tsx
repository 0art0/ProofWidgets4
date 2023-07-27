import * as React from 'react';
import { Position, Range, TextEdit, WorkspaceEdit } from 'vscode-languageserver-types';
import { EditorContext, RpcContext, useAsync } from '@leanprover/infoview';
import { Button } from '@mui/material';

function insertText(props:{pos:Position, label:String, text:String}) {
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

export default function(props:{pos:Position, label:String, text:String}): JSX.Element {
  return <Button onClick={insertText(props)}>{props.label}</Button>;
}
