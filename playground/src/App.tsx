import { useEffect, useState } from "react";
import { API_URL } from "./constants";

function App() {
  const [code, setCode] = useState("");
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
          onChange={(e) => setCode(e.target.value)}
        />
      </div>
      <div className="flex flex-col p-2 w-1/2">
        <div>
          <h1 className="text-xl">Output</h1>
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
