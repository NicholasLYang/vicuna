import { useState } from "react";
import { run_compiler } from "../../crates/vicuna-wasm/pkg";

function getCodeFromUrl() {
  const urlParams = new URLSearchParams(window.location.search);
  // Decode the code from base64
  return atob(urlParams.get("code") || "");
}

function App() {
  const [code, setCode] = useState(getCodeFromUrl());

  const output = run_compiler(code);
  return (
    <main>
      <nav className="w-full bg-sky-600">
        <h1 className="p-5 text-lg text-white">Vicuna Playground</h1>
      </nav>
      <div className="flex bg-slate-200">
        <div className="p-2 w-1/2 space-y-5">
          <h1 className="text-xl">Code</h1>
          <textarea
            className="w-3/4 font-mono p-5 rounded"
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
          <div className="space-y-5">
            <h1 className="text-xl">Output</h1>
            <div>
              <h2>JS output</h2>
              <pre className="w-full overflow-auto p-5 rounded bg-white h-[100px]">
                {output.js || ""}
              </pre>
            </div>
            <div>
              <h2>AST</h2>
              <pre className="w-full overflow-auto p-5 rounded bg-white h-[100px]">
                {output.ast || ""}
              </pre>
            </div>
            <div>
              <h2>Errors</h2>
              <pre className="w-full overflow-auto p-5 rounded bg-white h-[100px]">
                {output.errors || "No errors"}
              </pre>
            </div>
          </div>
        </div>
      </div>
    </main>
  );
}

export default App;
