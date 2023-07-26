import React, { useState } from 'react';
import { TextField, Slider } from '@mui/material';


export default function Calculator() {
  const [A, setA] = useState(0);
  const [B, setB] = useState(0);
  const [X, setX] = useState(0);
  const [Y, setY] = useState(0);

  function handleVarUpdate(setVar:React.Dispatch<React.SetStateAction<number>>, value:number) {
    setVar(value);
  }

  function handleInputChange(setVar:React.Dispatch<React.SetStateAction<number>>) {
    return (function(event: React.ChangeEvent<HTMLInputElement>) {
      const { name, value } = event.target;
      handleVarUpdate(setVar, parseFloat(value));
    });
  };

  function result() {
    return (A * X + B * Y);
  }

  return (
    <div>
      <TextField
        label="A"
        variant="outlined"
        type="number"
        name="A"
        value={A}
        onChange={handleInputChange(setA)}
      />
      <TextField
        label="B"
        variant="outlined"
        type="number"
        name="B"
        value={B}
        onChange={handleInputChange(setB)}
      />
      <Slider
        aria-label="X"
        value={X}
        min={-10}
        max={10}
        step={1}
        onChange={(event, value) => handleVarUpdate(setX, value as number)}
      />
      <Slider
        aria-label="Y"
        value={Y}
        min={-10}
        max={10}
        step={1}
        onChange={(event, value) => handleVarUpdate(setY, value as number)}
      />
      <div>
        Result: {A} * {X} + {B} * {Y} = {result()}
      </div>
    </div>
  );
};
