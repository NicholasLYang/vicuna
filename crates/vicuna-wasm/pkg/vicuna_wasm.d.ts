/* tslint:disable */
/* eslint-disable */
/**
*/
export function init(): void;
/**
* @param {string} source
* @returns {WasmOutput}
*/
export function run_compiler(source: string): WasmOutput;
/**
*/
export class WasmOutput {
  free(): void;
/**
*/
  readonly ast: string | undefined;
/**
*/
  readonly errors: string;
/**
*/
  readonly js: string | undefined;
}
