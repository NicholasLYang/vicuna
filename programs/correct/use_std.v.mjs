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
function is_some(a) {
__match__ = a;
switch (__match__.__type__) {
case "Some": {
const a = __match__[0];
return true;
break;
}
case "None": {
return false;
break;
}
}}
print(is_some({ "__type__": "Some",  "__enum__": "Option", 0: (1|0)}));
print(is_some({ "__type__": "None",  "__enum__": "Option", }));
