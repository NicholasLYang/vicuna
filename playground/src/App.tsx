import { useState } from "react";
import { run_compiler } from "../../crates/vicuna-wasm/pkg";
import { Tab, Tabs, TabList, TabPanel } from "react-tabs";
import "react-tabs/style/react-tabs.css";
import prettier from "prettier/standalone";
import babel from "prettier/parser-babel";

function base64ToBytes(base64: string): Uint8Array {
  const cleanedBase64 = base64.replace(/\s/g, "");
  console.log(cleanedBase64);
  const binString = atob(cleanedBase64.substring(0, cleanedBase64.length - 1));
  return Uint8Array.from(binString, (m) => m.codePointAt(0));
}

function bytesToBase64(bytes: Uint8Array): string {
  const binString = String.fromCodePoint(...bytes);
  return btoa(binString);
}

function getCodeFromUrl() {
  const urlParams = new URLSearchParams(window.location.search);
  const codeBase64 = urlParams.get("code");
  if (!codeBase64) {
    return "";
  }

  const codeBytes = base64ToBytes(codeBase64);
  return new TextDecoder().decode(codeBytes);
}

function App() {
  const [code, setCode] = useState(getCodeFromUrl());

  const textEncoder = new TextEncoder();

  const output = run_compiler(code);
  const formattedJs =
    output.js &&
    prettier.format(output.js, {
      parser: "babel",
      plugins: [babel],
    });

  return (
    <main>
      <nav className="w-full bg-sky-600">
        <h1 className="p-5 text-lg text-white">Vicuna Playground</h1>
      </nav>
      <div className="flex bg-slate-200 min-h-screen">
        <div className="p-2 w-1/2">
          <h1 className="text-xl py-5">Code</h1>
          <textarea
            className="w-full font-mono p-5 rounded"
            rows={10}
            value={code}
            onChange={(e) => {
              setCode(e.target.value);
              // set code in url search params
              window.history.replaceState(
                {},
                "",
                `?code=${bytesToBase64(textEncoder.encode(code))}}`
              );
            }}
          />
        </div>
        <div className="flex flex-col p-2 w-1/2">
          <Tabs>
            <TabList>
              <Tab>Output</Tab>
              <Tab>AST</Tab>
              <Tab>Errors</Tab>
            </TabList>

            <TabPanel>
              <pre className="w-full overflow-auto p-5 rounded bg-white">
                {formattedJs || ""}
              </pre>
            </TabPanel>
            <TabPanel>
              <pre className="w-full overflow-auto p-5 rounded bg-white">
                {output.ast || ""}
              </pre>
            </TabPanel>
            <TabPanel>
              <pre className="w-full overflow-auto p-5 rounded bg-white">
                {output.errors || "No errors"}
              </pre>
            </TabPanel>
          </Tabs>
        </div>
      </div>
    </main>
  );
}

export default App;
