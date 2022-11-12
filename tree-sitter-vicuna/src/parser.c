#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 13
#define STATE_COUNT 42
#define LARGE_STATE_COUNT 7
#define SYMBOL_COUNT 34
#define ALIAS_COUNT 0
#define TOKEN_COUNT 19
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 3
#define MAX_ALIAS_SEQUENCE_LENGTH 4
#define PRODUCTION_ID_COUNT 9

enum {
  anon_sym_SEMI = 1,
  anon_sym_LF = 2,
  anon_sym_let = 3,
  anon_sym_EQ = 4,
  anon_sym_STAR = 5,
  anon_sym_PLUS = 6,
  anon_sym_SLASH = 7,
  anon_sym_DASH = 8,
  anon_sym_BANG = 9,
  anon_sym_LPAREN = 10,
  anon_sym_COMMA = 11,
  anon_sym_RPAREN = 12,
  sym_variable = 13,
  anon_sym_true = 14,
  anon_sym_false = 15,
  sym_float = 16,
  sym_integer = 17,
  sym_string = 18,
  sym_source_file = 19,
  sym_statement = 20,
  sym_let_declaration = 21,
  sym_expression = 22,
  sym__arithmetic_expression = 23,
  sym_binary_expression = 24,
  sym_unary_expression = 25,
  sym_call_expression = 26,
  sym_call = 27,
  sym_primary_expression = 28,
  sym_value = 29,
  sym_boolean = 30,
  aux_sym_source_file_repeat1 = 31,
  aux_sym_call_expression_repeat1 = 32,
  aux_sym_call_repeat1 = 33,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_SEMI] = ";",
  [anon_sym_LF] = "\n",
  [anon_sym_let] = "let",
  [anon_sym_EQ] = "=",
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
  [sym_statement] = "statement",
  [sym_let_declaration] = "let_declaration",
  [sym_expression] = "expression",
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
  [anon_sym_let] = anon_sym_let,
  [anon_sym_EQ] = anon_sym_EQ,
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
  [sym_statement] = sym_statement,
  [sym_let_declaration] = sym_let_declaration,
  [sym_expression] = sym_expression,
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
  [anon_sym_let] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ] = {
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
  [sym_statement] = {
    .visible = true,
    .named = true,
  },
  [sym_let_declaration] = {
    .visible = true,
    .named = true,
  },
  [sym_expression] = {
    .visible = true,
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
      if (eof) ADVANCE(6);
      if (lookahead == '!') ADVANCE(15);
      if (lookahead == '"') ADVANCE(2);
      if (lookahead == '(') ADVANCE(16);
      if (lookahead == ')') ADVANCE(18);
      if (lookahead == '*') ADVANCE(11);
      if (lookahead == '+') ADVANCE(12);
      if (lookahead == ',') ADVANCE(17);
      if (lookahead == '-') ADVANCE(14);
      if (lookahead == '/') ADVANCE(13);
      if (lookahead == ';') ADVANCE(7);
      if (lookahead == '=') ADVANCE(10);
      if (lookahead == 'f') ADVANCE(19);
      if (lookahead == 'l') ADVANCE(20);
      if (lookahead == 't') ADVANCE(24);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(32);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(28);
      END_STATE();
    case 1:
      if (lookahead == '!') ADVANCE(15);
      if (lookahead == '"') ADVANCE(2);
      if (lookahead == ')') ADVANCE(18);
      if (lookahead == '-') ADVANCE(14);
      if (lookahead == 'f') ADVANCE(19);
      if (lookahead == 't') ADVANCE(24);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(1)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(32);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(28);
      END_STATE();
    case 2:
      if (lookahead == '"') ADVANCE(33);
      if (lookahead != 0) ADVANCE(2);
      END_STATE();
    case 3:
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(3)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(28);
      END_STATE();
    case 4:
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(31);
      END_STATE();
    case 5:
      if (eof) ADVANCE(6);
      if (lookahead == '\n') ADVANCE(8);
      if (lookahead == '\t' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(5)
      END_STATE();
    case 6:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 7:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 8:
      ACCEPT_TOKEN(anon_sym_LF);
      if (lookahead == '\n') ADVANCE(8);
      END_STATE();
    case 9:
      ACCEPT_TOKEN(anon_sym_let);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(28);
      END_STATE();
    case 10:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 11:
      ACCEPT_TOKEN(anon_sym_STAR);
      END_STATE();
    case 12:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 13:
      ACCEPT_TOKEN(anon_sym_SLASH);
      END_STATE();
    case 14:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 15:
      ACCEPT_TOKEN(anon_sym_BANG);
      END_STATE();
    case 16:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 17:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 18:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 19:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'a') ADVANCE(23);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(28);
      END_STATE();
    case 20:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'e') ADVANCE(26);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(28);
      END_STATE();
    case 21:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'e') ADVANCE(29);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(28);
      END_STATE();
    case 22:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'e') ADVANCE(30);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(28);
      END_STATE();
    case 23:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'l') ADVANCE(25);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(28);
      END_STATE();
    case 24:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'r') ADVANCE(27);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(28);
      END_STATE();
    case 25:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 's') ADVANCE(22);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(28);
      END_STATE();
    case 26:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 't') ADVANCE(9);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(28);
      END_STATE();
    case 27:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'u') ADVANCE(21);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(28);
      END_STATE();
    case 28:
      ACCEPT_TOKEN(sym_variable);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(28);
      END_STATE();
    case 29:
      ACCEPT_TOKEN(anon_sym_true);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(28);
      END_STATE();
    case 30:
      ACCEPT_TOKEN(anon_sym_false);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(28);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(sym_float);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(31);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(sym_integer);
      if (lookahead == '.') ADVANCE(4);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(32);
      END_STATE();
    case 33:
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
  [4] = {.lex_state = 1},
  [5] = {.lex_state = 1},
  [6] = {.lex_state = 1},
  [7] = {.lex_state = 1},
  [8] = {.lex_state = 1},
  [9] = {.lex_state = 1},
  [10] = {.lex_state = 1},
  [11] = {.lex_state = 0},
  [12] = {.lex_state = 0},
  [13] = {.lex_state = 0},
  [14] = {.lex_state = 0},
  [15] = {.lex_state = 0},
  [16] = {.lex_state = 0},
  [17] = {.lex_state = 0},
  [18] = {.lex_state = 1},
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
  [32] = {.lex_state = 5},
  [33] = {.lex_state = 5},
  [34] = {.lex_state = 0},
  [35] = {.lex_state = 0},
  [36] = {.lex_state = 0},
  [37] = {.lex_state = 0},
  [38] = {.lex_state = 0},
  [39] = {.lex_state = 5},
  [40] = {.lex_state = 0},
  [41] = {.lex_state = 3},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_SEMI] = ACTIONS(1),
    [anon_sym_let] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
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
    [sym_source_file] = STATE(40),
    [sym_statement] = STATE(37),
    [sym_let_declaration] = STATE(38),
    [sym_expression] = STATE(25),
    [sym__arithmetic_expression] = STATE(31),
    [sym_binary_expression] = STATE(31),
    [sym_unary_expression] = STATE(31),
    [sym_call_expression] = STATE(31),
    [sym_primary_expression] = STATE(31),
    [sym_value] = STATE(24),
    [sym_boolean] = STATE(29),
    [aux_sym_source_file_repeat1] = STATE(3),
    [ts_builtin_sym_end] = ACTIONS(3),
    [anon_sym_let] = ACTIONS(5),
    [anon_sym_DASH] = ACTIONS(7),
    [anon_sym_BANG] = ACTIONS(7),
    [sym_variable] = ACTIONS(9),
    [anon_sym_true] = ACTIONS(11),
    [anon_sym_false] = ACTIONS(11),
    [sym_float] = ACTIONS(13),
    [sym_integer] = ACTIONS(15),
    [sym_string] = ACTIONS(13),
  },
  [2] = {
    [sym_statement] = STATE(34),
    [sym_let_declaration] = STATE(38),
    [sym_expression] = STATE(25),
    [sym__arithmetic_expression] = STATE(31),
    [sym_binary_expression] = STATE(31),
    [sym_unary_expression] = STATE(31),
    [sym_call_expression] = STATE(31),
    [sym_primary_expression] = STATE(31),
    [sym_value] = STATE(24),
    [sym_boolean] = STATE(29),
    [aux_sym_source_file_repeat1] = STATE(2),
    [ts_builtin_sym_end] = ACTIONS(17),
    [anon_sym_let] = ACTIONS(19),
    [anon_sym_DASH] = ACTIONS(22),
    [anon_sym_BANG] = ACTIONS(22),
    [sym_variable] = ACTIONS(25),
    [anon_sym_true] = ACTIONS(28),
    [anon_sym_false] = ACTIONS(28),
    [sym_float] = ACTIONS(31),
    [sym_integer] = ACTIONS(34),
    [sym_string] = ACTIONS(31),
  },
  [3] = {
    [sym_statement] = STATE(35),
    [sym_let_declaration] = STATE(38),
    [sym_expression] = STATE(25),
    [sym__arithmetic_expression] = STATE(31),
    [sym_binary_expression] = STATE(31),
    [sym_unary_expression] = STATE(31),
    [sym_call_expression] = STATE(31),
    [sym_primary_expression] = STATE(31),
    [sym_value] = STATE(24),
    [sym_boolean] = STATE(29),
    [aux_sym_source_file_repeat1] = STATE(2),
    [ts_builtin_sym_end] = ACTIONS(37),
    [anon_sym_let] = ACTIONS(5),
    [anon_sym_DASH] = ACTIONS(7),
    [anon_sym_BANG] = ACTIONS(7),
    [sym_variable] = ACTIONS(9),
    [anon_sym_true] = ACTIONS(11),
    [anon_sym_false] = ACTIONS(11),
    [sym_float] = ACTIONS(13),
    [sym_integer] = ACTIONS(15),
    [sym_string] = ACTIONS(13),
  },
  [4] = {
    [sym_expression] = STATE(19),
    [sym__arithmetic_expression] = STATE(31),
    [sym_binary_expression] = STATE(31),
    [sym_unary_expression] = STATE(31),
    [sym_call_expression] = STATE(31),
    [sym_primary_expression] = STATE(31),
    [sym_value] = STATE(24),
    [sym_boolean] = STATE(29),
    [aux_sym_call_repeat1] = STATE(6),
    [anon_sym_DASH] = ACTIONS(7),
    [anon_sym_BANG] = ACTIONS(7),
    [anon_sym_RPAREN] = ACTIONS(39),
    [sym_variable] = ACTIONS(9),
    [anon_sym_true] = ACTIONS(11),
    [anon_sym_false] = ACTIONS(11),
    [sym_float] = ACTIONS(13),
    [sym_integer] = ACTIONS(15),
    [sym_string] = ACTIONS(13),
  },
  [5] = {
    [sym_expression] = STATE(22),
    [sym__arithmetic_expression] = STATE(31),
    [sym_binary_expression] = STATE(31),
    [sym_unary_expression] = STATE(31),
    [sym_call_expression] = STATE(31),
    [sym_primary_expression] = STATE(31),
    [sym_value] = STATE(24),
    [sym_boolean] = STATE(29),
    [aux_sym_call_repeat1] = STATE(5),
    [anon_sym_DASH] = ACTIONS(41),
    [anon_sym_BANG] = ACTIONS(41),
    [anon_sym_RPAREN] = ACTIONS(44),
    [sym_variable] = ACTIONS(46),
    [anon_sym_true] = ACTIONS(49),
    [anon_sym_false] = ACTIONS(49),
    [sym_float] = ACTIONS(52),
    [sym_integer] = ACTIONS(55),
    [sym_string] = ACTIONS(52),
  },
  [6] = {
    [sym_expression] = STATE(17),
    [sym__arithmetic_expression] = STATE(31),
    [sym_binary_expression] = STATE(31),
    [sym_unary_expression] = STATE(31),
    [sym_call_expression] = STATE(31),
    [sym_primary_expression] = STATE(31),
    [sym_value] = STATE(24),
    [sym_boolean] = STATE(29),
    [aux_sym_call_repeat1] = STATE(5),
    [anon_sym_DASH] = ACTIONS(7),
    [anon_sym_BANG] = ACTIONS(7),
    [anon_sym_RPAREN] = ACTIONS(58),
    [sym_variable] = ACTIONS(9),
    [anon_sym_true] = ACTIONS(11),
    [anon_sym_false] = ACTIONS(11),
    [sym_float] = ACTIONS(13),
    [sym_integer] = ACTIONS(15),
    [sym_string] = ACTIONS(13),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 9,
    ACTIONS(9), 1,
      sym_variable,
    ACTIONS(15), 1,
      sym_integer,
    STATE(14), 1,
      sym_expression,
    STATE(24), 1,
      sym_value,
    STATE(29), 1,
      sym_boolean,
    ACTIONS(7), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(11), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(13), 2,
      sym_float,
      sym_string,
    STATE(31), 5,
      sym__arithmetic_expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_call_expression,
      sym_primary_expression,
  [35] = 9,
    ACTIONS(9), 1,
      sym_variable,
    ACTIONS(15), 1,
      sym_integer,
    STATE(21), 1,
      sym_expression,
    STATE(24), 1,
      sym_value,
    STATE(29), 1,
      sym_boolean,
    ACTIONS(7), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(11), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(13), 2,
      sym_float,
      sym_string,
    STATE(31), 5,
      sym__arithmetic_expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_call_expression,
      sym_primary_expression,
  [70] = 9,
    ACTIONS(9), 1,
      sym_variable,
    ACTIONS(15), 1,
      sym_integer,
    STATE(15), 1,
      sym_expression,
    STATE(24), 1,
      sym_value,
    STATE(29), 1,
      sym_boolean,
    ACTIONS(7), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(11), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(13), 2,
      sym_float,
      sym_string,
    STATE(31), 5,
      sym__arithmetic_expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_call_expression,
      sym_primary_expression,
  [105] = 9,
    ACTIONS(9), 1,
      sym_variable,
    ACTIONS(15), 1,
      sym_integer,
    STATE(13), 1,
      sym_expression,
    STATE(24), 1,
      sym_value,
    STATE(29), 1,
      sym_boolean,
    ACTIONS(7), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(11), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(13), 2,
      sym_float,
      sym_string,
    STATE(31), 5,
      sym__arithmetic_expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_call_expression,
      sym_primary_expression,
  [140] = 4,
    ACTIONS(62), 1,
      anon_sym_LPAREN,
    STATE(11), 1,
      aux_sym_call_expression_repeat1,
    STATE(20), 1,
      sym_call,
    ACTIONS(60), 7,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [159] = 3,
    STATE(11), 1,
      aux_sym_call_expression_repeat1,
    STATE(20), 1,
      sym_call,
    ACTIONS(65), 8,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [176] = 5,
    ACTIONS(71), 1,
      anon_sym_LPAREN,
    STATE(12), 1,
      aux_sym_call_expression_repeat1,
    STATE(20), 1,
      sym_call,
    ACTIONS(69), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(67), 5,
      anon_sym_SEMI,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [197] = 4,
    ACTIONS(71), 1,
      anon_sym_LPAREN,
    STATE(12), 1,
      aux_sym_call_expression_repeat1,
    STATE(20), 1,
      sym_call,
    ACTIONS(73), 7,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [216] = 4,
    ACTIONS(71), 1,
      anon_sym_LPAREN,
    STATE(12), 1,
      aux_sym_call_expression_repeat1,
    STATE(20), 1,
      sym_call,
    ACTIONS(67), 7,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [235] = 2,
    ACTIONS(75), 5,
      ts_builtin_sym_end,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_float,
      sym_string,
    ACTIONS(77), 5,
      anon_sym_let,
      sym_variable,
      anon_sym_true,
      anon_sym_false,
      sym_integer,
  [250] = 7,
    ACTIONS(71), 1,
      anon_sym_LPAREN,
    ACTIONS(81), 1,
      anon_sym_COMMA,
    ACTIONS(83), 1,
      anon_sym_RPAREN,
    STATE(12), 1,
      aux_sym_call_expression_repeat1,
    STATE(20), 1,
      sym_call,
    ACTIONS(69), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(79), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [274] = 2,
    ACTIONS(87), 4,
      sym_variable,
      anon_sym_true,
      anon_sym_false,
      sym_integer,
    ACTIONS(85), 5,
      anon_sym_DASH,
      anon_sym_BANG,
      anon_sym_RPAREN,
      sym_float,
      sym_string,
  [288] = 7,
    ACTIONS(71), 1,
      anon_sym_LPAREN,
    ACTIONS(81), 1,
      anon_sym_COMMA,
    ACTIONS(89), 1,
      anon_sym_RPAREN,
    STATE(12), 1,
      aux_sym_call_expression_repeat1,
    STATE(20), 1,
      sym_call,
    ACTIONS(69), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(79), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [312] = 1,
    ACTIONS(91), 8,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [323] = 6,
    ACTIONS(71), 1,
      anon_sym_LPAREN,
    ACTIONS(93), 1,
      anon_sym_SEMI,
    STATE(12), 1,
      aux_sym_call_expression_repeat1,
    STATE(20), 1,
      sym_call,
    ACTIONS(69), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(79), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [344] = 6,
    ACTIONS(71), 1,
      anon_sym_LPAREN,
    ACTIONS(81), 1,
      anon_sym_COMMA,
    STATE(12), 1,
      aux_sym_call_expression_repeat1,
    STATE(20), 1,
      sym_call,
    ACTIONS(69), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(79), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [365] = 1,
    ACTIONS(95), 8,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [376] = 1,
    ACTIONS(97), 8,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [387] = 6,
    ACTIONS(71), 1,
      anon_sym_LPAREN,
    ACTIONS(99), 1,
      anon_sym_SEMI,
    STATE(12), 1,
      aux_sym_call_expression_repeat1,
    STATE(20), 1,
      sym_call,
    ACTIONS(69), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(79), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [408] = 1,
    ACTIONS(101), 8,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [419] = 1,
    ACTIONS(103), 8,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [430] = 1,
    ACTIONS(105), 8,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [441] = 1,
    ACTIONS(107), 8,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [452] = 1,
    ACTIONS(109), 8,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [463] = 1,
    ACTIONS(111), 8,
      anon_sym_SEMI,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [474] = 2,
    ACTIONS(113), 1,
      ts_builtin_sym_end,
    ACTIONS(115), 1,
      anon_sym_LF,
  [481] = 2,
    ACTIONS(115), 1,
      anon_sym_LF,
    ACTIONS(117), 1,
      ts_builtin_sym_end,
  [488] = 1,
    ACTIONS(119), 1,
      anon_sym_SEMI,
  [492] = 1,
    ACTIONS(121), 1,
      anon_sym_SEMI,
  [496] = 1,
    ACTIONS(123), 1,
      anon_sym_EQ,
  [500] = 1,
    ACTIONS(125), 1,
      anon_sym_SEMI,
  [504] = 1,
    ACTIONS(99), 1,
      anon_sym_SEMI,
  [508] = 1,
    ACTIONS(115), 1,
      anon_sym_LF,
  [512] = 1,
    ACTIONS(127), 1,
      ts_builtin_sym_end,
  [516] = 1,
    ACTIONS(129), 1,
      sym_variable,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(7)] = 0,
  [SMALL_STATE(8)] = 35,
  [SMALL_STATE(9)] = 70,
  [SMALL_STATE(10)] = 105,
  [SMALL_STATE(11)] = 140,
  [SMALL_STATE(12)] = 159,
  [SMALL_STATE(13)] = 176,
  [SMALL_STATE(14)] = 197,
  [SMALL_STATE(15)] = 216,
  [SMALL_STATE(16)] = 235,
  [SMALL_STATE(17)] = 250,
  [SMALL_STATE(18)] = 274,
  [SMALL_STATE(19)] = 288,
  [SMALL_STATE(20)] = 312,
  [SMALL_STATE(21)] = 323,
  [SMALL_STATE(22)] = 344,
  [SMALL_STATE(23)] = 365,
  [SMALL_STATE(24)] = 376,
  [SMALL_STATE(25)] = 387,
  [SMALL_STATE(26)] = 408,
  [SMALL_STATE(27)] = 419,
  [SMALL_STATE(28)] = 430,
  [SMALL_STATE(29)] = 441,
  [SMALL_STATE(30)] = 452,
  [SMALL_STATE(31)] = 463,
  [SMALL_STATE(32)] = 474,
  [SMALL_STATE(33)] = 481,
  [SMALL_STATE(34)] = 488,
  [SMALL_STATE(35)] = 492,
  [SMALL_STATE(36)] = 496,
  [SMALL_STATE(37)] = 500,
  [SMALL_STATE(38)] = 504,
  [SMALL_STATE(39)] = 508,
  [SMALL_STATE(40)] = 512,
  [SMALL_STATE(41)] = 516,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [5] = {.entry = {.count = 1, .reusable = false}}, SHIFT(41),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [9] = {.entry = {.count = 1, .reusable = false}}, SHIFT(24),
  [11] = {.entry = {.count = 1, .reusable = false}}, SHIFT(30),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [15] = {.entry = {.count = 1, .reusable = false}}, SHIFT(29),
  [17] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [19] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(41),
  [22] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(7),
  [25] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(24),
  [28] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(30),
  [31] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(29),
  [34] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(29),
  [37] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [39] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [41] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 7), SHIFT_REPEAT(7),
  [44] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 7),
  [46] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 7), SHIFT_REPEAT(24),
  [49] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 7), SHIFT_REPEAT(30),
  [52] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 7), SHIFT_REPEAT(29),
  [55] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 7), SHIFT_REPEAT(29),
  [58] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [60] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_call_expression_repeat1, 2, .production_id = 3),
  [62] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_call_expression_repeat1, 2, .production_id = 3), SHIFT_REPEAT(4),
  [65] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_call_expression, 2, .production_id = 2),
  [67] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_expression, 3),
  [69] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [71] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [73] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_expression, 2),
  [75] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 3),
  [77] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 3),
  [79] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [81] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [83] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [85] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 4),
  [87] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 4),
  [89] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [91] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_call_expression_repeat1, 1, .production_id = 1),
  [93] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_let_declaration, 4),
  [95] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_call, 4, .production_id = 8),
  [97] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_primary_expression, 1),
  [99] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_statement, 1),
  [101] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_call, 3, .production_id = 6),
  [103] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_call, 2),
  [105] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_call, 3, .production_id = 5),
  [107] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_value, 1),
  [109] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_boolean, 1),
  [111] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expression, 1),
  [113] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 2),
  [115] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [117] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 3),
  [119] = {.entry = {.count = 1, .reusable = true}}, SHIFT(39),
  [121] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [123] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [125] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [127] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [129] = {.entry = {.count = 1, .reusable = true}}, SHIFT(36),
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
