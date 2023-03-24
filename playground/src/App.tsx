import {useEffect, useState} from "react";
import { Buffer } from 'buffer'
import {WASI} from "@wasmer/wasi";

globalThis.Buffer = Buffer;
function getCodeFromUrl() {
  const urlParams = new URLSearchParams(window.location.search);
  // Decode the code from base64
  return atob(urlParams.get("code") || "");
}

function sendString(s, instance) {
    const bytes = (new TextEncoder().encode(s));
    const bufferPtr = instance.exports.allocate_buffer(bytes.length);
    const memoryArray = new Uint8Array(instance.exports.memory.buffer);
    for (let i = 0; i < bytes.length; i++) {
        memoryArray[bufferPtr + i] = bytes[i];
    }

    return [bufferPtr, bytes.length];
}

function receiveString(ptr, instance): [string, number] {
    const length = (new Uint32Array(instance.exports.memory.buffer))[ptr/4];
    const memoryBuffer = new Uint8Array(instance.exports.memory.buffer);
    const outputBuffer = new Uint8Array(length);
    for (let i = 0; i < length; i++) {
        outputBuffer[i] = memoryBuffer[ptr + i + 4];
    }
    const string = (new TextDecoder().decode(outputBuffer.buffer))
    return [string, length]
}


let instance;
let compiledCode = "";
function App() {
  const [code, setCode] = useState(getCodeFromUrl());

  useEffect(() => {
      (async function() {
          let wasi = new WASI({
              env: {}
          });
          const moduleBytes = fetch("/vicuna.wasm");
          const module = await WebAssembly.compileStreaming(moduleBytes);
          instance = await wasi.instantiate(module, {});
      })();
  }, [])

    useEffect(() => {
        if (instance) {
            const [codePtr, codeLength] = sendString(code, instance);
            let ptr = instance.exports.compile_code(codePtr, codeLength);
            const [output] = receiveString(ptr, instance);
            compiledCode = output;
        }
    }, [instance, code])

  return (
    <div className="flex">
      <div className="p-2 w-1/2">
        <h1 className="text-xl">Vicuna Code</h1>
        <textarea
          className="w-3/4 font-mono"
          rows={10}
          value={code}
          onChange={(e) => {
            setCode(e.target.value);
            // set code in url search params
            window.history.replaceState(
              {},
              "",
              `?code=${btoa(e.target.value)}`
            );
          }}
        />
      </div>
      <div className="flex flex-col p-2 w-1/2">
        <div>
          <h1 className="text-xl">Output</h1>
          <pre className="w-full min-h-[100px]">{compiledCode}</pre>
        </div>
      </div>
    </div>
  );
}

export default App;
