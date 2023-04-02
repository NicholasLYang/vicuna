import * as wasm from "./vicuna_wasm_bg.wasm";
import { __wbg_set_wasm } from "./vicuna_wasm_bg.js";
__wbg_set_wasm(wasm);
export * from "./vicuna_wasm_bg.js";

wasm.__wbindgen_start();
