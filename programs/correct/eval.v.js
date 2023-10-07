let __match__;
function Binary(a0, a1, a2) {
const out = [];
out[0] = a0;
out[1] = a1;
out[2] = a2;
out.type = "Binary";
return out;
}
function Number(a0) {
const out = [];
out[0] = a0;
out.type = "Number";
return out;
}
function eval(expr) {
__match__ = expr;
switch (__match__.__type__) {
case "Binary": {
const lhs = __match__[0];
const rhs = __match__[1];
const op = __match__[2];
__match__ = op;
switch (__match__.__type__) {
case "Add": {
return eval(lhs)+eval(rhs);
break;
}
case "Sub": {
return eval(lhs)-eval(rhs);
break;
}
case "Mul": {
return eval(lhs)*eval(rhs);
break;
}
}break;
}
case "Number": {
const n = __match__[0];
return n;
break;
}
}}
let lhs = { "__type__": "Number", 0: (1|0)};
let rhs = { "__type__": "Number", 0: (2|0)};
let expr = { "__type__": "Binary", 0: lhs, 1: rhs, 2: { "__type__": "Add", }};
console.log(eval(expr));
let lhs = { "__type__": "Number", 0: (5|0)};
let rhs = { "__type__": "Number", 0: (81|0)};
let expr = { "__type__": "Binary", 0: lhs, 1: rhs, 2: { "__type__": "Mul", }};
console.log(eval(expr));
