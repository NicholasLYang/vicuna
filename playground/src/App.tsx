import { useEffect, useState } from "react";
import { API_URL } from "./constants";

function App() {
  const [code, setCode] = useState("");
  const [output, setOutput] = useState("");

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
    })();
  }, [code]);
  return (
    <div className="flex">
      <div className="p-2">
        <h1 className="text-xl">Vicuna Code</h1>
        <textarea value={code} onChange={(e) => setCode(e.target.value)} />
      </div>
      <div>
        <h1 className="text-xl">Output</h1>
        <pre>{output}</pre>
      </div>
    </div>
  );
}

export default App;
