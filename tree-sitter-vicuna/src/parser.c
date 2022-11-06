#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 13
#define STATE_COUNT 34
#define LARGE_STATE_COUNT 10
#define SYMBOL_COUNT 30
#define ALIAS_COUNT 0
#define TOKEN_COUNT 17
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 3
#define MAX_ALIAS_SEQUENCE_LENGTH 4
#define PRODUCTION_ID_COUNT 9

enum {
  anon_sym_SEMI = 1,
  anon_sym_LF = 2,
  anon_sym_STAR = 3,
  anon_sym_PLUS = 4,
  anon_sym_SLASH = 5,
  anon_sym_DASH = 6,
  anon_sym_BANG = 7,
  anon_sym_LPAREN = 8,
  anon_sym_COMMA = 9,
  anon_sym_RPAREN = 10,
  sym_variable = 11,
  anon_sym_true = 12,
  anon_sym_false = 13,
  sym_float = 14,
  sym_integer = 15,
  sym_string = 16,
  sym_source_file = 17,
  sym__expression = 18,
  sym__arithmetic_expression = 19,
  sym_binary_expression = 20,
  sym_unary_expression = 21,
  sym_call_expression = 22,
  sym_call = 23,
  sym_primary_expression = 24,
  sym_value = 25,
  sym_boolean = 26,
  aux_sym_source_file_repeat1 = 27,
  aux_sym_call_expression_repeat1 = 28,
  aux_sym_call_repeat1 = 29,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_SEMI] = ";",
  [anon_sym_LF] = "\n",
  [anon_sym_STAR] = "*",
  [anon_sym_PLUS] = "+",
  [anon_sym_SLASH] = "/",
  [anon_sym_DASH] = "-",
  [anon_sym_BANG] = "!",
  [anon_sym_LPAREN] = "(",
  [anon_sym_COMMA] = ",",
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
  [sym_call] = "call",
  [sym_primary_expression] = "primary_expression",
  [sym_value] = "value",
  [sym_boolean] = "boolean",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
  [aux_sym_call_expression_repeat1] = "call_expression_repeat1",
  [aux_sym_call_repeat1] = "call_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [anon_sym_SEMI] = anon_sym_SEMI,
  [anon_sym_LF] = anon_sym_LF,
  [anon_sym_STAR] = anon_sym_STAR,
  [anon_sym_PLUS] = anon_sym_PLUS,
  [anon_sym_SLASH] = anon_sym_SLASH,
  [anon_sym_DASH] = anon_sym_DASH,
  [anon_sym_BANG] = anon_sym_BANG,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_COMMA] = anon_sym_COMMA,
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
  [sym_call] = sym_call,
  [sym_primary_expression] = sym_primary_expression,
  [sym_value] = sym_value,
  [sym_boolean] = sym_boolean,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
  [aux_sym_call_expression_repeat1] = aux_sym_call_expression_repeat1,
  [aux_sym_call_repeat1] = aux_sym_call_repeat1,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [anon_sym_SEMI] = {
    .visible = true,
    .named = false,
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
  [anon_sym_COMMA] = {
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
  [sym_call] = {
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
  [sym_boolean] = {
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
  [aux_sym_call_repeat1] = {
    .visible = false,
    .named = false,
  },
};

enum {
  field_argument = 1,
  field_call = 2,
  field_callee = 3,
};

static const char * const ts_field_names[] = {
  [0] = NULL,
  [field_argument] = "argument",
  [field_call] = "call",
  [field_callee] = "callee",
};

static const TSFieldMapSlice ts_field_map_slices[PRODUCTION_ID_COUNT] = {
  [1] = {.index = 0, .length = 1},
  [2] = {.index = 1, .length = 2},
  [3] = {.index = 3, .length = 2},
  [4] = {.index = 5, .length = 1},
  [5] = {.index = 6, .length = 1},
  [6] = {.index = 7, .length = 1},
  [7] = {.index = 8, .length = 2},
  [8] = {.index = 10, .length = 2},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_call, 0},
  [1] =
    {field_call, 1, .inherited = true},
    {field_callee, 0},
  [3] =
    {field_call, 0, .inherited = true},
    {field_call, 1, .inherited = true},
  [5] =
    {field_argument, 0},
  [6] =
    {field_argument, 1},
  [7] =
    {field_argument, 1, .inherited = true},
  [8] =
    {field_argument, 0, .inherited = true},
    {field_argument, 1, .inherited = true},
  [10] =
    {field_argument, 1, .inherited = true},
    {field_argument, 2},
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
      if (lookahead == '!') ADVANCE(11);
      if (lookahead == '"') ADVANCE(2);
      if (lookahead == '(') ADVANCE(12);
      if (lookahead == ')') ADVANCE(14);
      if (lookahead == '*') ADVANCE(7);
      if (lookahead == '+') ADVANCE(8);
      if (lookahead == ',') ADVANCE(13);
      if (lookahead == '-') ADVANCE(10);
      if (lookahead == '/') ADVANCE(9);
      if (lookahead == ';') ADVANCE(5);
      if (lookahead == 'f') ADVANCE(15);
      if (lookahead == 't') ADVANCE(19);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(26);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(22);
      END_STATE();
    case 1:
      if (lookahead == '\n') ADVANCE(6);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(1)
      END_STATE();
    case 2:
      if (lookahead == '"') ADVANCE(27);
      if (lookahead != 0) ADVANCE(2);
      END_STATE();
    case 3:
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(25);
      END_STATE();
    case 4:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 5:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 6:
      ACCEPT_TOKEN(anon_sym_LF);
      if (lookahead == '\n') ADVANCE(6);
      END_STATE();
    case 7:
      ACCEPT_TOKEN(anon_sym_STAR);
      END_STATE();
    case 8:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 9:
      ACCEPT_TOKEN(anon_sym_SLASH);
      END_STATE();
    case 10:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 11:
      ACCEPT_TOKEN(anon_sym_BANG);
      END_STATE();
    case 12:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 13:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 14:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 15:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'a') ADVANCE(18);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(22);
      END_STATE();
    case 16:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'e') ADVANCE(23);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(22);
      END_STATE();
    case 17:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'e') ADVANCE(24);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(22);
      END_STATE();
    case 18:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'l') ADVANCE(20);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(22);
      END_STATE();
    case 19:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'r') ADVANCE(21);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(22);
      END_STATE();
    case 20:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 's') ADVANCE(17);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(22);
      END_STATE();
    case 21:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'u') ADVANCE(16);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(22);
      END_STATE();
    case 22:
      ACCEPT_TOKEN(sym_variable);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(22);
      END_STATE();
    case 23:
      ACCEPT_TOKEN(anon_sym_true);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(22);
      END_STATE();
    case 24:
      ACCEPT_TOKEN(anon_sym_false);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(22);
      END_STATE();
    case 25:
      ACCEPT_TOKEN(sym_float);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(25);
      END_STATE();
    case 26:
      ACCEPT_TOKEN(sym_integer);
      if (lookahead == '.') ADVANCE(3);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(26);
      END_STATE();
    case 27:
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
  [17] = {.lex_state = 0},
  [18] = {.lex_state = 0},
  [19] = {.lex_state = 0},
  [20] = {.lex_state = 0},
  [21] = {.lex_state = 0},
  [22] = {.lex_state = 0},
  [23] = {.lex_state = 0},
  [24] = {.lex_state = 0},
  [25] = {.lex_state = 0},
  [26] = {.lex_state = 0},
  [27] = {.lex_state = 0},
  [28] = {.lex_state = 0},
  [29] = {.lex_state = 0},
  [30] = {.lex_state = 0},
  [31] = {.lex_state = 0},
  [32] = {.lex_state = 0},
  [33] = {.lex_state = 1},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_SEMI] = ACTIONS(1),
    [anon_sym_STAR] = ACTIONS(1),
    [anon_sym_PLUS] = ACTIONS(1),
    [anon_sym_SLASH] = ACTIONS(1),
    [anon_sym_DASH] = ACTIONS(1),
    [anon_sym_BANG] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [sym_variable] = ACTIONS(1),
    [anon_sym_true] = ACTIONS(1),
    [anon_sym_false] = ACTIONS(1),
    [sym_float] = ACTIONS(1),
    [sym_integer] = ACTIONS(1),
    [sym_string] = ACTIONS(1),
  },
  [1] = {
    [sym_source_file] = STATE(32),
    [sym__expression] = STATE(27),
    [sym__arithmetic_expression] = STATE(27),
    [sym_binary_expression] = STATE(27),
    [sym_unary_expression] = STATE(27),
    [sym_call_expression] = STATE(27),
    [sym_primary_expression] = STATE(10),
    [sym_value] = STATE(19),
    [sym_boolean] = STATE(13),
    [aux_sym_source_file_repeat1] = STATE(6),
    [ts_builtin_sym_end] = ACTIONS(3),
    [anon_sym_DASH] = ACTIONS(5),
    [anon_sym_BANG] = ACTIONS(5),
    [sym_variable] = ACTIONS(7),
    [anon_sym_true] = ACTIONS(9),
    [anon_sym_false] = ACTIONS(9),
    [sym_float] = ACTIONS(11),
    [sym_integer] = ACTIONS(13),
    [sym_string] = ACTIONS(11),
  },
  [2] = {
    [sym__expression] = STATE(31),
    [sym__arithmetic_expression] = STATE(31),
    [sym_binary_expression] = STATE(31),
    [sym_unary_expression] = STATE(31),
    [sym_call_expression] = STATE(31),
    [sym_primary_expression] = STATE(10),
    [sym_value] = STATE(19),
    [sym_boolean] = STATE(13),
    [aux_sym_call_repeat1] = STATE(2),
    [anon_sym_DASH] = ACTIONS(15),
    [anon_sym_BANG] = ACTIONS(15),
    [anon_sym_RPAREN] = ACTIONS(18),
    [sym_variable] = ACTIONS(20),
    [anon_sym_true] = ACTIONS(23),
    [anon_sym_false] = ACTIONS(23),
    [sym_float] = ACTIONS(26),
    [sym_integer] = ACTIONS(29),
    [sym_string] = ACTIONS(26),
  },
  [3] = {
    [sym__expression] = STATE(29),
    [sym__arithmetic_expression] = STATE(29),
    [sym_binary_expression] = STATE(29),
    [sym_unary_expression] = STATE(29),
    [sym_call_expression] = STATE(29),
    [sym_primary_expression] = STATE(10),
    [sym_value] = STATE(19),
    [sym_boolean] = STATE(13),
    [aux_sym_call_repeat1] = STATE(2),
    [anon_sym_DASH] = ACTIONS(5),
    [anon_sym_BANG] = ACTIONS(5),
    [anon_sym_RPAREN] = ACTIONS(32),
    [sym_variable] = ACTIONS(7),
    [anon_sym_true] = ACTIONS(9),
    [anon_sym_false] = ACTIONS(9),
    [sym_float] = ACTIONS(11),
    [sym_integer] = ACTIONS(13),
    [sym_string] = ACTIONS(11),
  },
  [4] = {
    [sym__expression] = STATE(30),
    [sym__arithmetic_expression] = STATE(30),
    [sym_binary_expression] = STATE(30),
    [sym_unary_expression] = STATE(30),
    [sym_call_expression] = STATE(30),
    [sym_primary_expression] = STATE(10),
    [sym_value] = STATE(19),
    [sym_boolean] = STATE(13),
    [aux_sym_source_file_repeat1] = STATE(4),
    [ts_builtin_sym_end] = ACTIONS(34),
    [anon_sym_DASH] = ACTIONS(36),
    [anon_sym_BANG] = ACTIONS(36),
    [sym_variable] = ACTIONS(39),
    [anon_sym_true] = ACTIONS(42),
    [anon_sym_false] = ACTIONS(42),
    [sym_float] = ACTIONS(45),
    [sym_integer] = ACTIONS(48),
    [sym_string] = ACTIONS(45),
  },
  [5] = {
    [sym__expression] = STATE(26),
    [sym__arithmetic_expression] = STATE(26),
    [sym_binary_expression] = STATE(26),
    [sym_unary_expression] = STATE(26),
    [sym_call_expression] = STATE(26),
    [sym_primary_expression] = STATE(10),
    [sym_value] = STATE(19),
    [sym_boolean] = STATE(13),
    [aux_sym_call_repeat1] = STATE(3),
    [anon_sym_DASH] = ACTIONS(5),
    [anon_sym_BANG] = ACTIONS(5),
    [anon_sym_RPAREN] = ACTIONS(51),
    [sym_variable] = ACTIONS(7),
    [anon_sym_true] = ACTIONS(9),
    [anon_sym_false] = ACTIONS(9),
    [sym_float] = ACTIONS(11),
    [sym_integer] = ACTIONS(13),
    [sym_string] = ACTIONS(11),
  },
  [6] = {
    [sym__expression] = STATE(28),
    [sym__arithmetic_expression] = STATE(28),
    [sym_binary_expression] = STATE(28),
    [sym_unary_expression] = STATE(28),
    [sym_call_expression] = STATE(28),
    [sym_primary_expression] = STATE(10),
    [sym_value] = STATE(19),
    [sym_boolean] = STATE(13),
    [aux_sym_source_file_repeat1] = STATE(4),
    [ts_builtin_sym_end] = ACTIONS(53),
    [anon_sym_DASH] = ACTIONS(5),
    [anon_sym_BANG] = ACTIONS(5),
    [sym_variable] = ACTIONS(7),
    [anon_sym_true] = ACTIONS(9),
    [anon_sym_false] = ACTIONS(9),
    [sym_float] = ACTIONS(11),
    [sym_integer] = ACTIONS(13),
    [sym_string] = ACTIONS(11),
  },
  [7] = {
    [sym__expression] = STATE(23),
    [sym__arithmetic_expression] = STATE(23),
    [sym_binary_expression] = STATE(23),
    [sym_unary_expression] = STATE(23),
    [sym_call_expression] = STATE(23),
    [sym_primary_expression] = STATE(10),
    [sym_value] = STATE(19),
    [sym_boolean] = STATE(13),
    [anon_sym_DASH] = ACTIONS(5),
    [anon_sym_BANG] = ACTIONS(5),
    [sym_variable] = ACTIONS(7),
    [anon_sym_true] = ACTIONS(9),
    [anon_sym_false] = ACTIONS(9),
    [sym_float] = ACTIONS(11),
    [sym_integer] = ACTIONS(13),
    [sym_string] = ACTIONS(11),
  },
  [8] = {
    [sym__expression] = STATE(25),
    [sym__arithmetic_expression] = STATE(25),
    [sym_binary_expression] = STATE(25),
    [sym_unary_expression] = STATE(25),
    [sym_call_expression] = STATE(25),
    [sym_primary_expression] = STATE(10),
    [sym_value] = STATE(19),
    [sym_boolean] = STATE(13),
    [anon_sym_DASH] = ACTIONS(5),
    [anon_sym_BANG] = ACTIONS(5),
    [sym_variable] = ACTIONS(7),
    [anon_sym_true] = ACTIONS(9),
    [anon_sym_false] = ACTIONS(9),
    [sym_float] = ACTIONS(11),
    [sym_integer] = ACTIONS(13),
    [sym_string] = ACTIONS(11),
  },
  [9] = {
    [sym__expression] = STATE(24),
    [sym__arithmetic_expression] = STATE(24),
    [sym_binary_expression] = STATE(24),
    [sym_unary_expression] = STATE(24),
    [sym_call_expression] = STATE(24),
    [sym_primary_expression] = STATE(10),
    [sym_value] = STATE(19),
    [sym_boolean] = STATE(13),
    [anon_sym_DASH] = ACTIONS(5),
    [anon_sym_BANG] = ACTIONS(5),
    [sym_variable] = ACTIONS(7),
    [anon_sym_true] = ACTIONS(9),
    [anon_sym_false] = ACTIONS(9),
    [sym_float] = ACTIONS(11),
    [sym_integer] = ACTIONS(13),
    [sym_string] = ACTIONS(11),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 4,
    ACTIONS(57), 1,
      anon_sym_LPAREN,
    STATE(11), 1,
      aux_sym_call_expression_repeat1,
    STATE(17), 1,
      sym_call,
    ACTIONS(55), 8,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [20] = 4,
    ACTIONS(57), 1,
      anon_sym_LPAREN,
    STATE(12), 1,
      aux_sym_call_expression_repeat1,
    STATE(17), 1,
      sym_call,
    ACTIONS(59), 8,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [40] = 4,
    ACTIONS(63), 1,
      anon_sym_LPAREN,
    STATE(12), 1,
      aux_sym_call_expression_repeat1,
    STATE(17), 1,
      sym_call,
    ACTIONS(61), 8,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [60] = 1,
    ACTIONS(66), 9,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [72] = 1,
    ACTIONS(68), 9,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [84] = 1,
    ACTIONS(70), 9,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [96] = 1,
    ACTIONS(72), 9,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [108] = 1,
    ACTIONS(74), 9,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [120] = 1,
    ACTIONS(76), 9,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [132] = 1,
    ACTIONS(78), 9,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [144] = 2,
    ACTIONS(82), 4,
      sym_variable,
      anon_sym_true,
      anon_sym_false,
      sym_integer,
    ACTIONS(80), 5,
      ts_builtin_sym_end,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_float,
      sym_string,
  [158] = 2,
    ACTIONS(86), 4,
      sym_variable,
      anon_sym_true,
      anon_sym_false,
      sym_integer,
    ACTIONS(84), 5,
      anon_sym_DASH,
      anon_sym_BANG,
      anon_sym_RPAREN,
      sym_float,
      sym_string,
  [172] = 1,
    ACTIONS(88), 9,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [184] = 2,
    ACTIONS(92), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(90), 6,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [197] = 1,
    ACTIONS(90), 8,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [208] = 1,
    ACTIONS(94), 8,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [219] = 4,
    ACTIONS(98), 1,
      anon_sym_COMMA,
    ACTIONS(100), 1,
      anon_sym_RPAREN,
    ACTIONS(92), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(96), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [234] = 4,
    ACTIONS(53), 1,
      ts_builtin_sym_end,
    ACTIONS(102), 1,
      anon_sym_SEMI,
    ACTIONS(92), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(96), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [249] = 4,
    ACTIONS(102), 1,
      anon_sym_SEMI,
    ACTIONS(104), 1,
      ts_builtin_sym_end,
    ACTIONS(92), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(96), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [264] = 4,
    ACTIONS(98), 1,
      anon_sym_COMMA,
    ACTIONS(106), 1,
      anon_sym_RPAREN,
    ACTIONS(92), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(96), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [279] = 3,
    ACTIONS(102), 1,
      anon_sym_SEMI,
    ACTIONS(92), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(96), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [291] = 3,
    ACTIONS(98), 1,
      anon_sym_COMMA,
    ACTIONS(92), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(96), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [303] = 1,
    ACTIONS(108), 1,
      ts_builtin_sym_end,
  [307] = 1,
    ACTIONS(110), 1,
      anon_sym_LF,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(10)] = 0,
  [SMALL_STATE(11)] = 20,
  [SMALL_STATE(12)] = 40,
  [SMALL_STATE(13)] = 60,
  [SMALL_STATE(14)] = 72,
  [SMALL_STATE(15)] = 84,
  [SMALL_STATE(16)] = 96,
  [SMALL_STATE(17)] = 108,
  [SMALL_STATE(18)] = 120,
  [SMALL_STATE(19)] = 132,
  [SMALL_STATE(20)] = 144,
  [SMALL_STATE(21)] = 158,
  [SMALL_STATE(22)] = 172,
  [SMALL_STATE(23)] = 184,
  [SMALL_STATE(24)] = 197,
  [SMALL_STATE(25)] = 208,
  [SMALL_STATE(26)] = 219,
  [SMALL_STATE(27)] = 234,
  [SMALL_STATE(28)] = 249,
  [SMALL_STATE(29)] = 264,
  [SMALL_STATE(30)] = 279,
  [SMALL_STATE(31)] = 291,
  [SMALL_STATE(32)] = 303,
  [SMALL_STATE(33)] = 307,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [7] = {.entry = {.count = 1, .reusable = false}}, SHIFT(19),
  [9] = {.entry = {.count = 1, .reusable = false}}, SHIFT(15),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [13] = {.entry = {.count = 1, .reusable = false}}, SHIFT(13),
  [15] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 7), SHIFT_REPEAT(8),
  [18] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 7),
  [20] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 7), SHIFT_REPEAT(19),
  [23] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 7), SHIFT_REPEAT(15),
  [26] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 7), SHIFT_REPEAT(13),
  [29] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 7), SHIFT_REPEAT(13),
  [32] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [34] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [36] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(8),
  [39] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(19),
  [42] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(15),
  [45] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(13),
  [48] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(13),
  [51] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [53] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [55] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__arithmetic_expression, 1),
  [57] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [59] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_call_expression, 2, .production_id = 2),
  [61] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_call_expression_repeat1, 2, .production_id = 3),
  [63] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_call_expression_repeat1, 2, .production_id = 3), SHIFT_REPEAT(5),
  [66] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_value, 1),
  [68] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_call, 4, .production_id = 8),
  [70] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_boolean, 1),
  [72] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_call, 3, .production_id = 6),
  [74] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_call_expression_repeat1, 1, .production_id = 1),
  [76] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_call, 3, .production_id = 5),
  [78] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_primary_expression, 1),
  [80] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 3),
  [82] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 3),
  [84] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 4),
  [86] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 4),
  [88] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_call, 2),
  [90] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_expression, 3),
  [92] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [94] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_expression, 2),
  [96] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [98] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [100] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [102] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [104] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 2),
  [106] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [108] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [110] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
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
    .field_names = ts_field_names,
    .field_map_slices = ts_field_map_slices,
    .field_map_entries = ts_field_map_entries,
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
