import * as wasm from "./vicuna_bg.wasm";
import { __wbg_set_wasm } from "./vicuna_bg.js";
__wbg_set_wasm(wasm);
export * from "./vicuna_bg.js";
