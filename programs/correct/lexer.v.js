let __match__;
function Integer(a0) {
const out = [];
out[0] = a0;
out.type = "Integer";
return out;
}
function tokenize(input) {
let i = (0|0);
for (const s of input) {
console.log(s);
console.log(i);
(i=i+(1|0));
}
}
tokenize("hello world");
