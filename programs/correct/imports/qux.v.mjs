function print(...args) {
  function printHelper(indent, ...args) {
    const indentStr = " ".repeat(indent);
    for (const arg of args) {
      if (Array.isArray(arg)) {
        console.log(`${indentStr}[`);
        for (const elem of arg) {
          printHelper(indent + 2, elem);
        }
        console.log(`${indentStr}]`);
      } else if (arg.hasOwnProperty("__type__")) {
        if (arg.hasOwnProperty("__enum__")) {
          console.log(`${indentStr}${arg.__enum__}::${arg.__type__}`);
        } else {
          console.log(`${indentStr}${arg.__type__}`);
        }
      } else {
        console.log(`${indentStr}${arg}`);
      }
    }
  }

  printHelper(0, ...args);
}
let __match__;
let c = (10|0);
export { c };
