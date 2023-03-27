module.exports = grammar({
  name: "vicuna",

  rules: {
    source_file: ($) => seq(repeat(choice($.statement))),
    function: ($) =>
      seq("fn", $.identifier, $.parameter_list, $.expression_block),
    parameter_list: ($) =>
      seq("(", repeat(seq($.identifier, ":", $.type_sig, ",")), optional(seq($.identifier, ":", $.type_sig)), ")"),
    statement: ($) =>
      choice(
        $.if_statement,
        $.let_declaration,
        $.let_if_declaration,
        $.expression_statement,
        $.function
      ),
    if_statement: ($) =>
      seq(
        token("if"),
        field("condition", $.expression),
        field("then_block", seq(token("{"), repeat($.statement), token("}"))),
        optional(
          field(
            "else_block",
            seq(token("else"), token("{"), repeat($.statement), token("}"))
          )
        )
      ),
    let_declaration: ($) =>
      seq(token("let"), $.identifier, token("="), $.expression, token(";")),
    let_if_declaration: ($) =>
      seq(
        token("let"),
        $.identifier,
        token("="),
        token("if"),
        field("condition", $.expression),
        field("then_block", $.expression_block),
        optional(field("else_block", seq(token("else"), $.expression_block)))
      ),
    expression_block: ($) =>
      seq(
        token("{"),
        repeat($.statement),
        optional($.expression),
        token("}")
      ),
    expression_statement: ($) => seq($.expression, token(";")),
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
    primary_expression: ($) => prec(8, choice($.identifier, $.value)),
    type_sig: ($) => choice("i32", "f32", "string", "bool"),
    identifier: ($) => /[A-Za-z_][A-Za-z0-9_]*/,
    value: ($) => choice($.float, $.integer, $.string, $.boolean),
    boolean: ($) => choice("true", "false"),
    float: ($) => /[0-9]+\.[0-9]+/,
    integer: ($) => /[0-9]+/,
    string: ($) => /"[^"]*"/,
  },
});
