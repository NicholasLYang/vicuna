module.exports = grammar({
  name: "vicuna",

  rules: {
    // TODO: add the actual grammar rules
    source_file: ($) => seq(repeat(seq($._expression, "\n")), optional($._expression)),
    _expression: ($) => $._arithmetic_expression,
    _arithmetic_expression: ($) =>
      choice(
        $.primary_expression,
        $.call_expression,
        $.unary_expression,
        $.binary_expression
      ),
    binary_expression: ($) =>
      choice(
        prec.left(2, seq($._expression, "*", $._expression)),
        prec.left(1, seq($._expression, "+", $._expression)),
        prec.left(2, seq($._expression, "/", $._expression)),
        prec.left(1, seq($._expression, "-", $._expression))
      ),
    unary_expression: ($) =>
      prec.right(3, choice(seq("!", $._expression), seq("-", $._expression))),
    call_expression: ($) =>
      prec.left(
        4,
        seq(
          $.primary_expression,
          repeat(choice(seq(token("("), $._expression, token(")"))))
        )
      ),
    primary_expression: ($) => choice($.variable, $.value),
    variable: ($) => /[A-Za-z_][A-Za-z0-9_]*/,
    value: ($) =>
      choice($.float, $.integer, $.string, token("true"), token("false")),
    float: ($) => /[0-9]+\.[0-9]+/,
    integer: ($) => /[0-9]+/,
    string: ($) => /"[^"]*"/,
  },
});
