import { useEffect, useState } from "react";
import Module from "../../target/wasm32-unknown-emscripten/debug/vicuna";

function getCodeFromUrl() {
  const urlParams = new URLSearchParams(window.location.search);
  // Decode the code from base64
  return atob(urlParams.get("code") || "");
}

function App() {
  const [code, setCode] = useState(getCodeFromUrl());
  useEffect(() => {
    (async function () {
      const m = await Module({ locateFile: (file) => file });
      m.ccall("test");
    })();
  }, []);

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
          <pre className="w-full min-h-[100px]">{"No output"}</pre>
        </div>
      </div>
    </div>
  );
}

export default App;
