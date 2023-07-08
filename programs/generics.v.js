let __match__;
function Some(a0) {
const out = [];
out[0] = a0;
out.type = "Some";
return out;
}
let b = { "__type__": "Some", 0: (20|0)};
let a = Some((10|0));
let s = { "__type__": "Span", start: (0|0), end: (20|0), inner: "Hello, world!"};
let e = { "__type__": "Expr", span: { "__type__": "Span", start: (10|0), end: (20|0), inner: "Hello, world!"}, value: (10|0)};
