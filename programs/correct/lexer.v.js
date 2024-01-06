function print(...args) {
  for (const arg of args) {
    if (Array.isArray(arg)) {
      console.log("[");
      for (const elem of arg) {
        print(elem);
      }
      console.log("]");
    } else if (arg.hasOwnProperty("__type__")) {
      if (arg.hasOwnProperty("__enum__")) {
        console.log(`${arg.__enum__}::${arg.__type__}`);
      } else {
        console.log(arg.__type__);
      }
    } else {
      console.log(arg);
    }
  }
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
};
}
print(out);
}
tokenize("+-*/");
