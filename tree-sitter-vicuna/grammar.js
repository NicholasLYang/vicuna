module.exports = grammar({
  name: "vicuna",

  rules: {
    // TODO: add the actual grammar rules
    source_file: ($) => seq(repeat(seq($.statement))),
    statement: ($) =>
      choice(seq($.expression, token(";")), $.if_expression, $.let_declaration),
    let_declaration: ($) =>
      seq(
        token("let"),
        $.variable,
        token("="),
        choice(seq($.expression, token(";")), $.if_expression)
      ),
    if_expression: ($) =>
      seq(
        token("if"),
        field("condition", $.expression),
        field("then_block", $.block),
        optional(field("else_block", seq(token("else"), $.block)))
      ),
    block: ($) =>
      seq(
        token("{"),
        repeat(seq($.statement, token(";"))),
        optional(choice($.expression, $.if_expression)),
        token("}")
      ),
    expression: ($) => $._arithmetic_expression,
    _arithmetic_expression: ($) =>
      choice(
        $.primary_expression,
        $.call_expression,
        $.unary_expression,
        $.binary_expression
      ),
    binary_expression: ($) =>
      choice(
        prec.left(2, seq($.expression, "*", $.expression)),
        prec.left(1, seq($.expression, "+", $.expression)),
        prec.left(2, seq($.expression, "/", $.expression)),
        prec.left(1, seq($.expression, "-", $.expression))
      ),
    unary_expression: ($) =>
      prec.right(3, choice(seq("!", $.expression), seq("-", $.expression))),
    call_expression: ($) =>
      prec.left(
        4,
        seq(field("callee", $.expression), repeat1(field("call", $.call)))
      ),
    call: ($) =>
      seq(
        token("("),
        repeat(seq(field("argument", $.expression), token(","))),
        optional(field("argument", $.expression)),
        token(")")
      ),
    primary_expression: ($) => prec(8, choice($.variable, $.value)),
    variable: ($) => /[A-Za-z_][A-Za-z0-9_]*/,
    value: ($) => choice($.float, $.integer, $.string, $.boolean),
    boolean: ($) => choice("true", "false"),
    float: ($) => /[0-9]+\.[0-9]+/,
    integer: ($) => /[0-9]+/,
    string: ($) => /"[^"]*"/,
  },
});
