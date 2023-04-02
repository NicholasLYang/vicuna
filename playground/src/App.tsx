import { useState } from "react";
import { compile_code } from "../../crates/vicuna-wasm/pkg";

function getCodeFromUrl() {
  const urlParams = new URLSearchParams(window.location.search);
  // Decode the code from base64
  return atob(urlParams.get("code") || "");
}

function App() {
  const [code, setCode] = useState(getCodeFromUrl());

  return (
    <main>
      <nav className="w-full bg-sky-600">
        <h1 className="p-5 text-lg text-white">Vicuna Playground</h1>
      </nav>
      <div className="flex">
        <div className="p-2 w-1/2">
          <h1 className="text-xl">Code</h1>
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
            <pre className="w-full min-h-[100px]">{compile_code(code)}</pre>
          </div>
        </div>
      </div>
    </main>
  );
}

export default App;
