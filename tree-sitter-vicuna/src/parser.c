#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 13
#define STATE_COUNT 51
#define LARGE_STATE_COUNT 16
#define SYMBOL_COUNT 25
#define ALIAS_COUNT 0
#define TOKEN_COUNT 15
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 0
#define MAX_ALIAS_SEQUENCE_LENGTH 3
#define PRODUCTION_ID_COUNT 1

enum {
  anon_sym_LF = 1,
  anon_sym_STAR = 2,
  anon_sym_PLUS = 3,
  anon_sym_SLASH = 4,
  anon_sym_DASH = 5,
  anon_sym_BANG = 6,
  anon_sym_LPAREN = 7,
  anon_sym_RPAREN = 8,
  sym_variable = 9,
  anon_sym_true = 10,
  anon_sym_false = 11,
  sym_float = 12,
  sym_integer = 13,
  sym_string = 14,
  sym_source_file = 15,
  sym__expression = 16,
  sym__arithmetic_expression = 17,
  sym_binary_expression = 18,
  sym_unary_expression = 19,
  sym_call_expression = 20,
  sym_primary_expression = 21,
  sym_value = 22,
  aux_sym_source_file_repeat1 = 23,
  aux_sym_call_expression_repeat1 = 24,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_LF] = "\n",
  [anon_sym_STAR] = "*",
  [anon_sym_PLUS] = "+",
  [anon_sym_SLASH] = "/",
  [anon_sym_DASH] = "-",
  [anon_sym_BANG] = "!",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [sym_variable] = "variable",
  [anon_sym_true] = "true",
  [anon_sym_false] = "false",
  [sym_float] = "float",
  [sym_integer] = "integer",
  [sym_string] = "string",
  [sym_source_file] = "source_file",
  [sym__expression] = "_expression",
  [sym__arithmetic_expression] = "_arithmetic_expression",
  [sym_binary_expression] = "binary_expression",
  [sym_unary_expression] = "unary_expression",
  [sym_call_expression] = "call_expression",
  [sym_primary_expression] = "primary_expression",
  [sym_value] = "value",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
  [aux_sym_call_expression_repeat1] = "call_expression_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [anon_sym_LF] = anon_sym_LF,
  [anon_sym_STAR] = anon_sym_STAR,
  [anon_sym_PLUS] = anon_sym_PLUS,
  [anon_sym_SLASH] = anon_sym_SLASH,
  [anon_sym_DASH] = anon_sym_DASH,
  [anon_sym_BANG] = anon_sym_BANG,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [sym_variable] = sym_variable,
  [anon_sym_true] = anon_sym_true,
  [anon_sym_false] = anon_sym_false,
  [sym_float] = sym_float,
  [sym_integer] = sym_integer,
  [sym_string] = sym_string,
  [sym_source_file] = sym_source_file,
  [sym__expression] = sym__expression,
  [sym__arithmetic_expression] = sym__arithmetic_expression,
  [sym_binary_expression] = sym_binary_expression,
  [sym_unary_expression] = sym_unary_expression,
  [sym_call_expression] = sym_call_expression,
  [sym_primary_expression] = sym_primary_expression,
  [sym_value] = sym_value,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
  [aux_sym_call_expression_repeat1] = aux_sym_call_expression_repeat1,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [anon_sym_LF] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_STAR] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PLUS] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SLASH] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DASH] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_BANG] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RPAREN] = {
    .visible = true,
    .named = false,
  },
  [sym_variable] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_true] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_false] = {
    .visible = true,
    .named = false,
  },
  [sym_float] = {
    .visible = true,
    .named = true,
  },
  [sym_integer] = {
    .visible = true,
    .named = true,
  },
  [sym_string] = {
    .visible = true,
    .named = true,
  },
  [sym_source_file] = {
    .visible = true,
    .named = true,
  },
  [sym__expression] = {
    .visible = false,
    .named = true,
  },
  [sym__arithmetic_expression] = {
    .visible = false,
    .named = true,
  },
  [sym_binary_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_unary_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_call_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_primary_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_value] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_source_file_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_call_expression_repeat1] = {
    .visible = false,
    .named = false,
  },
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
};

static const uint16_t ts_non_terminal_alias_map[] = {
  0,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(4);
      if (lookahead == '!') ADVANCE(10);
      if (lookahead == '"') ADVANCE(1);
      if (lookahead == '(') ADVANCE(11);
      if (lookahead == ')') ADVANCE(12);
      if (lookahead == '*') ADVANCE(6);
      if (lookahead == '+') ADVANCE(7);
      if (lookahead == '-') ADVANCE(9);
      if (lookahead == '/') ADVANCE(8);
      if (lookahead == 'f') ADVANCE(13);
      if (lookahead == 't') ADVANCE(17);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(24);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(20);
      END_STATE();
    case 1:
      if (lookahead == '"') ADVANCE(25);
      if (lookahead != 0) ADVANCE(1);
      END_STATE();
    case 2:
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(23);
      END_STATE();
    case 3:
      if (eof) ADVANCE(4);
      if (lookahead == '\n') ADVANCE(5);
      if (lookahead == '(') ADVANCE(11);
      if (lookahead == '*') ADVANCE(6);
      if (lookahead == '+') ADVANCE(7);
      if (lookahead == '-') ADVANCE(9);
      if (lookahead == '/') ADVANCE(8);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(3)
      END_STATE();
    case 4:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 5:
      ACCEPT_TOKEN(anon_sym_LF);
      if (lookahead == '\n') ADVANCE(5);
      END_STATE();
    case 6:
      ACCEPT_TOKEN(anon_sym_STAR);
      END_STATE();
    case 7:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 8:
      ACCEPT_TOKEN(anon_sym_SLASH);
      END_STATE();
    case 9:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 10:
      ACCEPT_TOKEN(anon_sym_BANG);
      END_STATE();
    case 11:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 12:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 13:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'a') ADVANCE(16);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(20);
      END_STATE();
    case 14:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'e') ADVANCE(21);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(20);
      END_STATE();
    case 15:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'e') ADVANCE(22);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(20);
      END_STATE();
    case 16:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'l') ADVANCE(18);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(20);
      END_STATE();
    case 17:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'r') ADVANCE(19);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(20);
      END_STATE();
    case 18:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 's') ADVANCE(15);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(20);
      END_STATE();
    case 19:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'u') ADVANCE(14);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(20);
      END_STATE();
    case 20:
      ACCEPT_TOKEN(sym_variable);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(20);
      END_STATE();
    case 21:
      ACCEPT_TOKEN(anon_sym_true);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(20);
      END_STATE();
    case 22:
      ACCEPT_TOKEN(anon_sym_false);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(20);
      END_STATE();
    case 23:
      ACCEPT_TOKEN(sym_float);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(23);
      END_STATE();
    case 24:
      ACCEPT_TOKEN(sym_integer);
      if (lookahead == '.') ADVANCE(2);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(24);
      END_STATE();
    case 25:
      ACCEPT_TOKEN(sym_string);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 0},
  [2] = {.lex_state = 0},
  [3] = {.lex_state = 0},
  [4] = {.lex_state = 0},
  [5] = {.lex_state = 0},
  [6] = {.lex_state = 0},
  [7] = {.lex_state = 0},
  [8] = {.lex_state = 0},
  [9] = {.lex_state = 0},
  [10] = {.lex_state = 0},
  [11] = {.lex_state = 0},
  [12] = {.lex_state = 0},
  [13] = {.lex_state = 0},
  [14] = {.lex_state = 0},
  [15] = {.lex_state = 0},
  [16] = {.lex_state = 0},
  [17] = {.lex_state = 3},
  [18] = {.lex_state = 3},
  [19] = {.lex_state = 3},
  [20] = {.lex_state = 3},
  [21] = {.lex_state = 3},
  [22] = {.lex_state = 3},
  [23] = {.lex_state = 3},
  [24] = {.lex_state = 3},
  [25] = {.lex_state = 0},
  [26] = {.lex_state = 0},
  [27] = {.lex_state = 0},
  [28] = {.lex_state = 3},
  [29] = {.lex_state = 0},
  [30] = {.lex_state = 3},
  [31] = {.lex_state = 3},
  [32] = {.lex_state = 3},
  [33] = {.lex_state = 0},
  [34] = {.lex_state = 3},
  [35] = {.lex_state = 3},
  [36] = {.lex_state = 3},
  [37] = {.lex_state = 3},
  [38] = {.lex_state = 0},
  [39] = {.lex_state = 3},
  [40] = {.lex_state = 0},
  [41] = {.lex_state = 3},
  [42] = {.lex_state = 3},
  [43] = {.lex_state = 3},
  [44] = {.lex_state = 3},
  [45] = {.lex_state = 0},
  [46] = {.lex_state = 0},
  [47] = {.lex_state = 0},
  [48] = {.lex_state = 0},
  [49] = {.lex_state = 0},
  [50] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_STAR] = ACTIONS(1),
    [anon_sym_PLUS] = ACTIONS(1),
    [anon_sym_SLASH] = ACTIONS(1),
    [anon_sym_DASH] = ACTIONS(1),
    [anon_sym_BANG] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [sym_variable] = ACTIONS(1),
    [anon_sym_true] = ACTIONS(1),
    [anon_sym_false] = ACTIONS(1),
    [sym_float] = ACTIONS(1),
    [sym_integer] = ACTIONS(1),
    [sym_string] = ACTIONS(1),
  },
  [1] = {
    [sym_source_file] = STATE(50),
    [sym__expression] = STATE(31),
    [sym__arithmetic_expression] = STATE(31),
    [sym_binary_expression] = STATE(31),
    [sym_unary_expression] = STATE(31),
    [sym_call_expression] = STATE(31),
    [sym_primary_expression] = STATE(17),
    [sym_value] = STATE(22),
    [aux_sym_source_file_repeat1] = STATE(2),
    [ts_builtin_sym_end] = ACTIONS(3),
    [anon_sym_DASH] = ACTIONS(5),
    [anon_sym_BANG] = ACTIONS(5),
    [sym_variable] = ACTIONS(7),
    [anon_sym_true] = ACTIONS(9),
    [anon_sym_false] = ACTIONS(9),
    [sym_float] = ACTIONS(11),
    [sym_integer] = ACTIONS(9),
    [sym_string] = ACTIONS(11),
  },
  [2] = {
    [sym__expression] = STATE(36),
    [sym__arithmetic_expression] = STATE(36),
    [sym_binary_expression] = STATE(36),
    [sym_unary_expression] = STATE(36),
    [sym_call_expression] = STATE(36),
    [sym_primary_expression] = STATE(17),
    [sym_value] = STATE(22),
    [aux_sym_source_file_repeat1] = STATE(3),
    [ts_builtin_sym_end] = ACTIONS(13),
    [anon_sym_DASH] = ACTIONS(5),
    [anon_sym_BANG] = ACTIONS(5),
    [sym_variable] = ACTIONS(7),
    [anon_sym_true] = ACTIONS(9),
    [anon_sym_false] = ACTIONS(9),
    [sym_float] = ACTIONS(11),
    [sym_integer] = ACTIONS(9),
    [sym_string] = ACTIONS(11),
  },
  [3] = {
    [sym__expression] = STATE(43),
    [sym__arithmetic_expression] = STATE(43),
    [sym_binary_expression] = STATE(43),
    [sym_unary_expression] = STATE(43),
    [sym_call_expression] = STATE(43),
    [sym_primary_expression] = STATE(24),
    [sym_value] = STATE(34),
    [aux_sym_source_file_repeat1] = STATE(3),
    [ts_builtin_sym_end] = ACTIONS(15),
    [anon_sym_DASH] = ACTIONS(17),
    [anon_sym_BANG] = ACTIONS(17),
    [sym_variable] = ACTIONS(20),
    [anon_sym_true] = ACTIONS(23),
    [anon_sym_false] = ACTIONS(23),
    [sym_float] = ACTIONS(26),
    [sym_integer] = ACTIONS(23),
    [sym_string] = ACTIONS(26),
  },
  [4] = {
    [sym__expression] = STATE(42),
    [sym__arithmetic_expression] = STATE(42),
    [sym_binary_expression] = STATE(42),
    [sym_unary_expression] = STATE(42),
    [sym_call_expression] = STATE(42),
    [sym_primary_expression] = STATE(24),
    [sym_value] = STATE(34),
    [anon_sym_DASH] = ACTIONS(29),
    [anon_sym_BANG] = ACTIONS(29),
    [sym_variable] = ACTIONS(31),
    [anon_sym_true] = ACTIONS(33),
    [anon_sym_false] = ACTIONS(33),
    [sym_float] = ACTIONS(35),
    [sym_integer] = ACTIONS(33),
    [sym_string] = ACTIONS(35),
  },
  [5] = {
    [sym__expression] = STATE(32),
    [sym__arithmetic_expression] = STATE(32),
    [sym_binary_expression] = STATE(32),
    [sym_unary_expression] = STATE(32),
    [sym_call_expression] = STATE(32),
    [sym_primary_expression] = STATE(17),
    [sym_value] = STATE(22),
    [anon_sym_DASH] = ACTIONS(5),
    [anon_sym_BANG] = ACTIONS(5),
    [sym_variable] = ACTIONS(7),
    [anon_sym_true] = ACTIONS(9),
    [anon_sym_false] = ACTIONS(9),
    [sym_float] = ACTIONS(11),
    [sym_integer] = ACTIONS(9),
    [sym_string] = ACTIONS(11),
  },
  [6] = {
    [sym__expression] = STATE(49),
    [sym__arithmetic_expression] = STATE(49),
    [sym_binary_expression] = STATE(49),
    [sym_unary_expression] = STATE(49),
    [sym_call_expression] = STATE(49),
    [sym_primary_expression] = STATE(25),
    [sym_value] = STATE(29),
    [anon_sym_DASH] = ACTIONS(37),
    [anon_sym_BANG] = ACTIONS(37),
    [sym_variable] = ACTIONS(39),
    [anon_sym_true] = ACTIONS(41),
    [anon_sym_false] = ACTIONS(41),
    [sym_float] = ACTIONS(43),
    [sym_integer] = ACTIONS(41),
    [sym_string] = ACTIONS(43),
  },
  [7] = {
    [sym__expression] = STATE(45),
    [sym__arithmetic_expression] = STATE(45),
    [sym_binary_expression] = STATE(45),
    [sym_unary_expression] = STATE(45),
    [sym_call_expression] = STATE(45),
    [sym_primary_expression] = STATE(25),
    [sym_value] = STATE(29),
    [anon_sym_DASH] = ACTIONS(37),
    [anon_sym_BANG] = ACTIONS(37),
    [sym_variable] = ACTIONS(39),
    [anon_sym_true] = ACTIONS(41),
    [anon_sym_false] = ACTIONS(41),
    [sym_float] = ACTIONS(43),
    [sym_integer] = ACTIONS(41),
    [sym_string] = ACTIONS(43),
  },
  [8] = {
    [sym__expression] = STATE(48),
    [sym__arithmetic_expression] = STATE(48),
    [sym_binary_expression] = STATE(48),
    [sym_unary_expression] = STATE(48),
    [sym_call_expression] = STATE(48),
    [sym_primary_expression] = STATE(25),
    [sym_value] = STATE(29),
    [anon_sym_DASH] = ACTIONS(37),
    [anon_sym_BANG] = ACTIONS(37),
    [sym_variable] = ACTIONS(39),
    [anon_sym_true] = ACTIONS(41),
    [anon_sym_false] = ACTIONS(41),
    [sym_float] = ACTIONS(43),
    [sym_integer] = ACTIONS(41),
    [sym_string] = ACTIONS(43),
  },
  [9] = {
    [sym__expression] = STATE(46),
    [sym__arithmetic_expression] = STATE(46),
    [sym_binary_expression] = STATE(46),
    [sym_unary_expression] = STATE(46),
    [sym_call_expression] = STATE(46),
    [sym_primary_expression] = STATE(25),
    [sym_value] = STATE(29),
    [anon_sym_DASH] = ACTIONS(37),
    [anon_sym_BANG] = ACTIONS(37),
    [sym_variable] = ACTIONS(39),
    [anon_sym_true] = ACTIONS(41),
    [anon_sym_false] = ACTIONS(41),
    [sym_float] = ACTIONS(43),
    [sym_integer] = ACTIONS(41),
    [sym_string] = ACTIONS(43),
  },
  [10] = {
    [sym__expression] = STATE(44),
    [sym__arithmetic_expression] = STATE(44),
    [sym_binary_expression] = STATE(44),
    [sym_unary_expression] = STATE(44),
    [sym_call_expression] = STATE(44),
    [sym_primary_expression] = STATE(24),
    [sym_value] = STATE(34),
    [anon_sym_DASH] = ACTIONS(29),
    [anon_sym_BANG] = ACTIONS(29),
    [sym_variable] = ACTIONS(31),
    [anon_sym_true] = ACTIONS(33),
    [anon_sym_false] = ACTIONS(33),
    [sym_float] = ACTIONS(35),
    [sym_integer] = ACTIONS(33),
    [sym_string] = ACTIONS(35),
  },
  [11] = {
    [sym__expression] = STATE(30),
    [sym__arithmetic_expression] = STATE(30),
    [sym_binary_expression] = STATE(30),
    [sym_unary_expression] = STATE(30),
    [sym_call_expression] = STATE(30),
    [sym_primary_expression] = STATE(17),
    [sym_value] = STATE(22),
    [anon_sym_DASH] = ACTIONS(5),
    [anon_sym_BANG] = ACTIONS(5),
    [sym_variable] = ACTIONS(7),
    [anon_sym_true] = ACTIONS(9),
    [anon_sym_false] = ACTIONS(9),
    [sym_float] = ACTIONS(11),
    [sym_integer] = ACTIONS(9),
    [sym_string] = ACTIONS(11),
  },
  [12] = {
    [sym__expression] = STATE(39),
    [sym__arithmetic_expression] = STATE(39),
    [sym_binary_expression] = STATE(39),
    [sym_unary_expression] = STATE(39),
    [sym_call_expression] = STATE(39),
    [sym_primary_expression] = STATE(17),
    [sym_value] = STATE(22),
    [anon_sym_DASH] = ACTIONS(5),
    [anon_sym_BANG] = ACTIONS(5),
    [sym_variable] = ACTIONS(7),
    [anon_sym_true] = ACTIONS(9),
    [anon_sym_false] = ACTIONS(9),
    [sym_float] = ACTIONS(11),
    [sym_integer] = ACTIONS(9),
    [sym_string] = ACTIONS(11),
  },
  [13] = {
    [sym__expression] = STATE(40),
    [sym__arithmetic_expression] = STATE(40),
    [sym_binary_expression] = STATE(40),
    [sym_unary_expression] = STATE(40),
    [sym_call_expression] = STATE(40),
    [sym_primary_expression] = STATE(25),
    [sym_value] = STATE(29),
    [anon_sym_DASH] = ACTIONS(37),
    [anon_sym_BANG] = ACTIONS(37),
    [sym_variable] = ACTIONS(39),
    [anon_sym_true] = ACTIONS(41),
    [anon_sym_false] = ACTIONS(41),
    [sym_float] = ACTIONS(43),
    [sym_integer] = ACTIONS(41),
    [sym_string] = ACTIONS(43),
  },
  [14] = {
    [sym__expression] = STATE(41),
    [sym__arithmetic_expression] = STATE(41),
    [sym_binary_expression] = STATE(41),
    [sym_unary_expression] = STATE(41),
    [sym_call_expression] = STATE(41),
    [sym_primary_expression] = STATE(24),
    [sym_value] = STATE(34),
    [anon_sym_DASH] = ACTIONS(29),
    [anon_sym_BANG] = ACTIONS(29),
    [sym_variable] = ACTIONS(31),
    [anon_sym_true] = ACTIONS(33),
    [anon_sym_false] = ACTIONS(33),
    [sym_float] = ACTIONS(35),
    [sym_integer] = ACTIONS(33),
    [sym_string] = ACTIONS(35),
  },
  [15] = {
    [sym__expression] = STATE(47),
    [sym__arithmetic_expression] = STATE(47),
    [sym_binary_expression] = STATE(47),
    [sym_unary_expression] = STATE(47),
    [sym_call_expression] = STATE(47),
    [sym_primary_expression] = STATE(25),
    [sym_value] = STATE(29),
    [anon_sym_DASH] = ACTIONS(37),
    [anon_sym_BANG] = ACTIONS(37),
    [sym_variable] = ACTIONS(39),
    [anon_sym_true] = ACTIONS(41),
    [anon_sym_false] = ACTIONS(41),
    [sym_float] = ACTIONS(43),
    [sym_integer] = ACTIONS(41),
    [sym_string] = ACTIONS(43),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 2,
    ACTIONS(45), 4,
      sym_variable,
      anon_sym_true,
      anon_sym_false,
      sym_integer,
    ACTIONS(15), 5,
      ts_builtin_sym_end,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_float,
      sym_string,
  [14] = 4,
    ACTIONS(51), 1,
      anon_sym_LPAREN,
    STATE(19), 1,
      aux_sym_call_expression_repeat1,
    ACTIONS(47), 2,
      ts_builtin_sym_end,
      anon_sym_LF,
    ACTIONS(49), 4,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
  [31] = 4,
    ACTIONS(57), 1,
      anon_sym_LPAREN,
    STATE(18), 1,
      aux_sym_call_expression_repeat1,
    ACTIONS(53), 2,
      ts_builtin_sym_end,
      anon_sym_LF,
    ACTIONS(55), 4,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
  [48] = 4,
    ACTIONS(51), 1,
      anon_sym_LPAREN,
    STATE(18), 1,
      aux_sym_call_expression_repeat1,
    ACTIONS(60), 2,
      ts_builtin_sym_end,
      anon_sym_LF,
    ACTIONS(62), 4,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
  [65] = 4,
    ACTIONS(60), 1,
      anon_sym_LF,
    ACTIONS(64), 1,
      anon_sym_LPAREN,
    STATE(21), 1,
      aux_sym_call_expression_repeat1,
    ACTIONS(62), 4,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
  [81] = 4,
    ACTIONS(53), 1,
      anon_sym_LF,
    ACTIONS(66), 1,
      anon_sym_LPAREN,
    STATE(21), 1,
      aux_sym_call_expression_repeat1,
    ACTIONS(55), 4,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
  [97] = 2,
    ACTIONS(69), 2,
      ts_builtin_sym_end,
      anon_sym_LF,
    ACTIONS(71), 5,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
  [109] = 2,
    ACTIONS(73), 2,
      ts_builtin_sym_end,
      anon_sym_LF,
    ACTIONS(75), 5,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
  [121] = 4,
    ACTIONS(47), 1,
      anon_sym_LF,
    ACTIONS(64), 1,
      anon_sym_LPAREN,
    STATE(20), 1,
      aux_sym_call_expression_repeat1,
    ACTIONS(49), 4,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
  [137] = 3,
    ACTIONS(77), 1,
      anon_sym_LPAREN,
    STATE(27), 1,
      aux_sym_call_expression_repeat1,
    ACTIONS(47), 5,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_RPAREN,
  [151] = 3,
    ACTIONS(79), 1,
      anon_sym_LPAREN,
    STATE(26), 1,
      aux_sym_call_expression_repeat1,
    ACTIONS(53), 5,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_RPAREN,
  [165] = 3,
    ACTIONS(77), 1,
      anon_sym_LPAREN,
    STATE(26), 1,
      aux_sym_call_expression_repeat1,
    ACTIONS(60), 5,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_RPAREN,
  [179] = 2,
    ACTIONS(82), 2,
      ts_builtin_sym_end,
      anon_sym_LF,
    ACTIONS(84), 5,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
  [191] = 1,
    ACTIONS(69), 6,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
  [200] = 2,
    ACTIONS(86), 2,
      ts_builtin_sym_end,
      anon_sym_LF,
    ACTIONS(88), 4,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
  [211] = 4,
    ACTIONS(13), 1,
      ts_builtin_sym_end,
    ACTIONS(90), 1,
      anon_sym_LF,
    ACTIONS(92), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(94), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [226] = 2,
    ACTIONS(96), 2,
      ts_builtin_sym_end,
      anon_sym_LF,
    ACTIONS(98), 4,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
  [237] = 1,
    ACTIONS(73), 6,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
  [246] = 2,
    ACTIONS(69), 1,
      anon_sym_LF,
    ACTIONS(71), 5,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
  [257] = 2,
    ACTIONS(82), 1,
      anon_sym_LF,
    ACTIONS(84), 5,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
  [268] = 4,
    ACTIONS(90), 1,
      anon_sym_LF,
    ACTIONS(100), 1,
      ts_builtin_sym_end,
    ACTIONS(92), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(94), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [283] = 2,
    ACTIONS(73), 1,
      anon_sym_LF,
    ACTIONS(75), 5,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
  [294] = 1,
    ACTIONS(82), 6,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
  [303] = 3,
    ACTIONS(86), 2,
      ts_builtin_sym_end,
      anon_sym_LF,
    ACTIONS(88), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(92), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
  [316] = 3,
    ACTIONS(106), 1,
      anon_sym_RPAREN,
    ACTIONS(102), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(104), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [328] = 2,
    ACTIONS(86), 1,
      anon_sym_LF,
    ACTIONS(88), 4,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
  [338] = 3,
    ACTIONS(86), 1,
      anon_sym_LF,
    ACTIONS(88), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(108), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
  [350] = 3,
    ACTIONS(90), 1,
      anon_sym_LF,
    ACTIONS(108), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(110), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [362] = 2,
    ACTIONS(96), 1,
      anon_sym_LF,
    ACTIONS(98), 4,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
  [372] = 1,
    ACTIONS(86), 5,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_RPAREN,
  [380] = 3,
    ACTIONS(112), 1,
      anon_sym_RPAREN,
    ACTIONS(102), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(104), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [392] = 1,
    ACTIONS(96), 5,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_RPAREN,
  [400] = 3,
    ACTIONS(114), 1,
      anon_sym_RPAREN,
    ACTIONS(102), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(104), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [412] = 2,
    ACTIONS(102), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(86), 3,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_RPAREN,
  [422] = 1,
    ACTIONS(116), 1,
      ts_builtin_sym_end,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(16)] = 0,
  [SMALL_STATE(17)] = 14,
  [SMALL_STATE(18)] = 31,
  [SMALL_STATE(19)] = 48,
  [SMALL_STATE(20)] = 65,
  [SMALL_STATE(21)] = 81,
  [SMALL_STATE(22)] = 97,
  [SMALL_STATE(23)] = 109,
  [SMALL_STATE(24)] = 121,
  [SMALL_STATE(25)] = 137,
  [SMALL_STATE(26)] = 151,
  [SMALL_STATE(27)] = 165,
  [SMALL_STATE(28)] = 179,
  [SMALL_STATE(29)] = 191,
  [SMALL_STATE(30)] = 200,
  [SMALL_STATE(31)] = 211,
  [SMALL_STATE(32)] = 226,
  [SMALL_STATE(33)] = 237,
  [SMALL_STATE(34)] = 246,
  [SMALL_STATE(35)] = 257,
  [SMALL_STATE(36)] = 268,
  [SMALL_STATE(37)] = 283,
  [SMALL_STATE(38)] = 294,
  [SMALL_STATE(39)] = 303,
  [SMALL_STATE(40)] = 316,
  [SMALL_STATE(41)] = 328,
  [SMALL_STATE(42)] = 338,
  [SMALL_STATE(43)] = 350,
  [SMALL_STATE(44)] = 362,
  [SMALL_STATE(45)] = 372,
  [SMALL_STATE(46)] = 380,
  [SMALL_STATE(47)] = 392,
  [SMALL_STATE(48)] = 400,
  [SMALL_STATE(49)] = 412,
  [SMALL_STATE(50)] = 422,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [7] = {.entry = {.count = 1, .reusable = false}}, SHIFT(22),
  [9] = {.entry = {.count = 1, .reusable = false}}, SHIFT(28),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [13] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [15] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [17] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(10),
  [20] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(34),
  [23] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(35),
  [26] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(35),
  [29] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [31] = {.entry = {.count = 1, .reusable = false}}, SHIFT(34),
  [33] = {.entry = {.count = 1, .reusable = false}}, SHIFT(35),
  [35] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [37] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [39] = {.entry = {.count = 1, .reusable = false}}, SHIFT(29),
  [41] = {.entry = {.count = 1, .reusable = false}}, SHIFT(38),
  [43] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [45] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [47] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_call_expression, 1),
  [49] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_call_expression, 1),
  [51] = {.entry = {.count = 1, .reusable = false}}, SHIFT(13),
  [53] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_call_expression_repeat1, 2),
  [55] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_call_expression_repeat1, 2),
  [57] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_call_expression_repeat1, 2), SHIFT_REPEAT(13),
  [60] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_call_expression, 2),
  [62] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_call_expression, 2),
  [64] = {.entry = {.count = 1, .reusable = false}}, SHIFT(8),
  [66] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_call_expression_repeat1, 2), SHIFT_REPEAT(8),
  [69] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_primary_expression, 1),
  [71] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_primary_expression, 1),
  [73] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_call_expression_repeat1, 3),
  [75] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_call_expression_repeat1, 3),
  [77] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [79] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_call_expression_repeat1, 2), SHIFT_REPEAT(9),
  [82] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_value, 1),
  [84] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_value, 1),
  [86] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_expression, 3),
  [88] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_binary_expression, 3),
  [90] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [92] = {.entry = {.count = 1, .reusable = false}}, SHIFT(11),
  [94] = {.entry = {.count = 1, .reusable = false}}, SHIFT(12),
  [96] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_expression, 2),
  [98] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unary_expression, 2),
  [100] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 2),
  [102] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [104] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [106] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [108] = {.entry = {.count = 1, .reusable = false}}, SHIFT(14),
  [110] = {.entry = {.count = 1, .reusable = false}}, SHIFT(4),
  [112] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [114] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
  [116] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef _WIN32
#define extern __declspec(dllexport)
#endif

extern const TSLanguage *tree_sitter_vicuna(void) {
  static const TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .state_count = STATE_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .production_id_count = PRODUCTION_ID_COUNT,
    .field_count = FIELD_COUNT,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .parse_table = &ts_parse_table[0][0],
    .small_parse_table = ts_small_parse_table,
    .small_parse_table_map = ts_small_parse_table_map,
    .parse_actions = ts_parse_actions,
    .symbol_names = ts_symbol_names,
    .symbol_metadata = ts_symbol_metadata,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .alias_sequences = &ts_alias_sequences[0][0],
    .lex_modes = ts_lex_modes,
    .lex_fn = ts_lex,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
