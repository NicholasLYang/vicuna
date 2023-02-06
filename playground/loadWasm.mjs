import { init, WASI } from "@wasmer/wasi";
import { readFile } from "node:fs/promises";

export async function loadWasm() {
  await init();

  let wasi = new WASI({
    env: {},
  });
  const moduleBytes = await readFile("../target/wasm32-wasi/debug/vicuna.wasm");
  const module = await WebAssembly.compile(moduleBytes);
  // Instantiate the WASI module
  let instance = await wasi.instantiate(module, {});
  const [codePtr, codeLength] = sendString(code, instance);

  let ptr = instance.exports.compile_code(codePtr, codeLength);
  console.log(receiveString(ptr, instance));
  console.log(wasi.getStdoutString());
}

loadWasm();


function sendString(s, instance) {
  const bytes = (new TextEncoder().encode(s));
  const bufferPtr = instance.exports.allocate_buffer(bytes.length);
  const memoryArray = new Uint8Array(instance.exports.memory.buffer);
  for (let i = 0; i < bytes.length; i++) {
    memoryArray[bufferPtr + i] = bytes[i];
  }

  return [bufferPtr, bytes.length];
}

function receiveString(ptr, instance) {
  const length = (new Uint32Array(instance.exports.memory.buffer))[ptr/4];
  const memoryBuffer = new Uint8Array(instance.exports.memory.buffer);
  const outputBuffer = new Uint8Array(length);
  for (let i = 0; i < length; i++) {
    outputBuffer[i] = memoryBuffer[ptr + i + 4];
  }

  return (new TextDecoder().decode(outputBuffer.buffer))
}
