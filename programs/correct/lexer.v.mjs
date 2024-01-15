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
function Integer(a0) {
const out = [];
out[0] = a0;
out.type = "Integer";
return out;
}
function tokenize(input) {
let out = [];
for (const s of input) {
__match__ = s;
switch (__match__) {
case "+": {
out.push({ "__type__": "Plus",  "__enum__": "Token", });
break;
}
case "-": {
out.push({ "__type__": "Minus",  "__enum__": "Token", });
break;
}
case "*": {
out.push({ "__type__": "Star",  "__enum__": "Token", });
break;
}
case "/": {
out.push({ "__type__": "Slash",  "__enum__": "Token", });
break;
}
default: {
const s = __match__;
if (s.matches(/[0-9]/)) {
print("integer found");
} else {
}
break;
}
};
}
print(out);
}
tokenize("+-*/");
