import { useEffect, useState } from "react";
import { API_URL } from "./constants";

function getCodeFromUrl() {
  const urlParams = new URLSearchParams(window.location.search);
  // Decode the code from base64
  return atob(urlParams.get("code") || "");
}

function App() {
  const [code, setCode] = useState(getCodeFromUrl());
  const [output, setOutput] = useState("");
  const [errors, setErrors] = useState("");

  useEffect(() => {
    (async function () {
      const result = await fetch(`${API_URL}/compile`, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({ code }),
      });
      const payload = await result.json();
      setOutput(payload.code);
      setErrors(payload.errors);
    })();
  }, [code]);
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
          <button
            disabled={!output}
            onClick={() => {
              eval(output);
            }}
            className="p-1 bg-orange-500 disabled:bg-gray-300 w-16 rounded text-white"
          >
            Run
          </button>
          <pre className="w-full min-h-[100px]">{output || "No output"}</pre>
        </div>
        <div>
          <h1 className="text-xl">Errors</h1>
          <pre className="w-full text-red-500">{errors}</pre>
        </div>
      </div>
    </div>
  );
}

export default App;
