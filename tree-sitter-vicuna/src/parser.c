#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 13
#define STATE_COUNT 71
#define LARGE_STATE_COUNT 9
#define SYMBOL_COUNT 40
#define ALIAS_COUNT 0
#define TOKEN_COUNT 22
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 5
#define MAX_ALIAS_SEQUENCE_LENGTH 5
#define PRODUCTION_ID_COUNT 11

enum {
  anon_sym_SEMI = 1,
  anon_sym_let = 2,
  anon_sym_EQ = 3,
  anon_sym_if = 4,
  anon_sym_else = 5,
  anon_sym_LBRACE = 6,
  anon_sym_RBRACE = 7,
  anon_sym_STAR = 8,
  anon_sym_PLUS = 9,
  anon_sym_SLASH = 10,
  anon_sym_DASH = 11,
  anon_sym_BANG = 12,
  anon_sym_LPAREN = 13,
  anon_sym_COMMA = 14,
  anon_sym_RPAREN = 15,
  sym_variable = 16,
  anon_sym_true = 17,
  anon_sym_false = 18,
  sym_float = 19,
  sym_integer = 20,
  sym_string = 21,
  sym_source_file = 22,
  sym_statement = 23,
  sym_let_declaration = 24,
  sym_if_expression = 25,
  sym_block = 26,
  sym_expression = 27,
  sym__arithmetic_expression = 28,
  sym_binary_expression = 29,
  sym_unary_expression = 30,
  sym_call_expression = 31,
  sym_call = 32,
  sym_primary_expression = 33,
  sym_value = 34,
  sym_boolean = 35,
  aux_sym_source_file_repeat1 = 36,
  aux_sym_block_repeat1 = 37,
  aux_sym_call_expression_repeat1 = 38,
  aux_sym_call_repeat1 = 39,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_SEMI] = ";",
  [anon_sym_let] = "let",
  [anon_sym_EQ] = "=",
  [anon_sym_if] = "if",
  [anon_sym_else] = "else",
  [anon_sym_LBRACE] = "{",
  [anon_sym_RBRACE] = "}",
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
  [sym_if_expression] = "if_expression",
  [sym_block] = "block",
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
  [aux_sym_block_repeat1] = "block_repeat1",
  [aux_sym_call_expression_repeat1] = "call_expression_repeat1",
  [aux_sym_call_repeat1] = "call_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [anon_sym_SEMI] = anon_sym_SEMI,
  [anon_sym_let] = anon_sym_let,
  [anon_sym_EQ] = anon_sym_EQ,
  [anon_sym_if] = anon_sym_if,
  [anon_sym_else] = anon_sym_else,
  [anon_sym_LBRACE] = anon_sym_LBRACE,
  [anon_sym_RBRACE] = anon_sym_RBRACE,
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
  [sym_if_expression] = sym_if_expression,
  [sym_block] = sym_block,
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
  [aux_sym_block_repeat1] = aux_sym_block_repeat1,
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
  [anon_sym_let] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_if] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_else] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LBRACE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RBRACE] = {
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
  [sym_if_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_block] = {
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
  [aux_sym_block_repeat1] = {
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
  field_else_block = 4,
  field_then_block = 5,
};

static const char * const ts_field_names[] = {
  [0] = NULL,
  [field_argument] = "argument",
  [field_call] = "call",
  [field_callee] = "callee",
  [field_else_block] = "else_block",
  [field_then_block] = "then_block",
};

static const TSFieldMapSlice ts_field_map_slices[PRODUCTION_ID_COUNT] = {
  [1] = {.index = 0, .length = 1},
  [2] = {.index = 1, .length = 2},
  [3] = {.index = 3, .length = 1},
  [4] = {.index = 4, .length = 2},
  [5] = {.index = 6, .length = 1},
  [6] = {.index = 7, .length = 1},
  [7] = {.index = 8, .length = 1},
  [8] = {.index = 9, .length = 2},
  [9] = {.index = 11, .length = 3},
  [10] = {.index = 14, .length = 2},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_call, 0},
  [1] =
    {field_call, 1, .inherited = true},
    {field_callee, 0},
  [3] =
    {field_then_block, 2},
  [4] =
    {field_call, 0, .inherited = true},
    {field_call, 1, .inherited = true},
  [6] =
    {field_argument, 0},
  [7] =
    {field_argument, 1},
  [8] =
    {field_argument, 1, .inherited = true},
  [9] =
    {field_argument, 0, .inherited = true},
    {field_argument, 1, .inherited = true},
  [11] =
    {field_else_block, 3},
    {field_else_block, 4},
    {field_then_block, 2},
  [14] =
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
      if (eof) ADVANCE(7);
      if (lookahead == '!') ADVANCE(19);
      if (lookahead == '"') ADVANCE(3);
      if (lookahead == '(') ADVANCE(20);
      if (lookahead == ')') ADVANCE(22);
      if (lookahead == '*') ADVANCE(15);
      if (lookahead == '+') ADVANCE(16);
      if (lookahead == ',') ADVANCE(21);
      if (lookahead == '-') ADVANCE(18);
      if (lookahead == '/') ADVANCE(17);
      if (lookahead == ';') ADVANCE(8);
      if (lookahead == '=') ADVANCE(10);
      if (lookahead == 'e') ADVANCE(29);
      if (lookahead == 'f') ADVANCE(23);
      if (lookahead == 'i') ADVANCE(28);
      if (lookahead == 'l') ADVANCE(24);
      if (lookahead == 't') ADVANCE(31);
      if (lookahead == '{') ADVANCE(13);
      if (lookahead == '}') ADVANCE(14);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(40);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 1:
      if (lookahead == '!') ADVANCE(19);
      if (lookahead == '"') ADVANCE(3);
      if (lookahead == ')') ADVANCE(22);
      if (lookahead == '-') ADVANCE(18);
      if (lookahead == 'f') ADVANCE(23);
      if (lookahead == 't') ADVANCE(31);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(1)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(40);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 2:
      if (lookahead == '!') ADVANCE(19);
      if (lookahead == '"') ADVANCE(3);
      if (lookahead == '-') ADVANCE(18);
      if (lookahead == 'f') ADVANCE(23);
      if (lookahead == 'i') ADVANCE(28);
      if (lookahead == 't') ADVANCE(31);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(2)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(40);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 3:
      if (lookahead == '"') ADVANCE(41);
      if (lookahead != 0) ADVANCE(3);
      END_STATE();
    case 4:
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(4)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 5:
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(39);
      END_STATE();
    case 6:
      if (eof) ADVANCE(7);
      if (lookahead == '!') ADVANCE(19);
      if (lookahead == '"') ADVANCE(3);
      if (lookahead == '-') ADVANCE(18);
      if (lookahead == ';') ADVANCE(8);
      if (lookahead == 'f') ADVANCE(23);
      if (lookahead == 'i') ADVANCE(28);
      if (lookahead == 'l') ADVANCE(24);
      if (lookahead == 't') ADVANCE(31);
      if (lookahead == '}') ADVANCE(14);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(6)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(40);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 7:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 8:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 9:
      ACCEPT_TOKEN(anon_sym_let);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 10:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 11:
      ACCEPT_TOKEN(anon_sym_if);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 12:
      ACCEPT_TOKEN(anon_sym_else);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 13:
      ACCEPT_TOKEN(anon_sym_LBRACE);
      END_STATE();
    case 14:
      ACCEPT_TOKEN(anon_sym_RBRACE);
      END_STATE();
    case 15:
      ACCEPT_TOKEN(anon_sym_STAR);
      END_STATE();
    case 16:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 17:
      ACCEPT_TOKEN(anon_sym_SLASH);
      END_STATE();
    case 18:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 19:
      ACCEPT_TOKEN(anon_sym_BANG);
      END_STATE();
    case 20:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 21:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 22:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 23:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'a') ADVANCE(30);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 24:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'e') ADVANCE(34);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 25:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'e') ADVANCE(12);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 26:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'e') ADVANCE(37);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 27:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'e') ADVANCE(38);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 28:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'f') ADVANCE(11);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 29:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'l') ADVANCE(32);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 30:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'l') ADVANCE(33);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'r') ADVANCE(35);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 's') ADVANCE(25);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 's') ADVANCE(27);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 't') ADVANCE(9);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(sym_variable);
      if (lookahead == 'u') ADVANCE(26);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(sym_variable);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(anon_sym_true);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(anon_sym_false);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(sym_float);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(39);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(sym_integer);
      if (lookahead == '.') ADVANCE(5);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(40);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(sym_string);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 6},
  [2] = {.lex_state = 6},
  [3] = {.lex_state = 6},
  [4] = {.lex_state = 6},
  [5] = {.lex_state = 6},
  [6] = {.lex_state = 6},
  [7] = {.lex_state = 6},
  [8] = {.lex_state = 6},
  [9] = {.lex_state = 2},
  [10] = {.lex_state = 2},
  [11] = {.lex_state = 1},
  [12] = {.lex_state = 1},
  [13] = {.lex_state = 1},
  [14] = {.lex_state = 1},
  [15] = {.lex_state = 1},
  [16] = {.lex_state = 1},
  [17] = {.lex_state = 1},
  [18] = {.lex_state = 1},
  [19] = {.lex_state = 0},
  [20] = {.lex_state = 0},
  [21] = {.lex_state = 0},
  [22] = {.lex_state = 6},
  [23] = {.lex_state = 6},
  [24] = {.lex_state = 6},
  [25] = {.lex_state = 0},
  [26] = {.lex_state = 0},
  [27] = {.lex_state = 0},
  [28] = {.lex_state = 0},
  [29] = {.lex_state = 0},
  [30] = {.lex_state = 6},
  [31] = {.lex_state = 6},
  [32] = {.lex_state = 0},
  [33] = {.lex_state = 6},
  [34] = {.lex_state = 6},
  [35] = {.lex_state = 6},
  [36] = {.lex_state = 6},
  [37] = {.lex_state = 0},
  [38] = {.lex_state = 0},
  [39] = {.lex_state = 0},
  [40] = {.lex_state = 0},
  [41] = {.lex_state = 0},
  [42] = {.lex_state = 0},
  [43] = {.lex_state = 0},
  [44] = {.lex_state = 0},
  [45] = {.lex_state = 0},
  [46] = {.lex_state = 0},
  [47] = {.lex_state = 0},
  [48] = {.lex_state = 0},
  [49] = {.lex_state = 0},
  [50] = {.lex_state = 0},
  [51] = {.lex_state = 1},
  [52] = {.lex_state = 0},
  [53] = {.lex_state = 0},
  [54] = {.lex_state = 0},
  [55] = {.lex_state = 0},
  [56] = {.lex_state = 0},
  [57] = {.lex_state = 0},
  [58] = {.lex_state = 0},
  [59] = {.lex_state = 0},
  [60] = {.lex_state = 0},
  [61] = {.lex_state = 0},
  [62] = {.lex_state = 0},
  [63] = {.lex_state = 0},
  [64] = {.lex_state = 0},
  [65] = {.lex_state = 0},
  [66] = {.lex_state = 0},
  [67] = {.lex_state = 4},
  [68] = {.lex_state = 0},
  [69] = {.lex_state = 0},
  [70] = {.lex_state = 4},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_SEMI] = ACTIONS(1),
    [anon_sym_let] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
    [anon_sym_if] = ACTIONS(1),
    [anon_sym_else] = ACTIONS(1),
    [anon_sym_LBRACE] = ACTIONS(1),
    [anon_sym_RBRACE] = ACTIONS(1),
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
    [sym_source_file] = STATE(66),
    [sym_statement] = STATE(5),
    [sym_let_declaration] = STATE(24),
    [sym_if_expression] = STATE(24),
    [sym_expression] = STATE(55),
    [sym__arithmetic_expression] = STATE(44),
    [sym_binary_expression] = STATE(44),
    [sym_unary_expression] = STATE(44),
    [sym_call_expression] = STATE(44),
    [sym_primary_expression] = STATE(44),
    [sym_value] = STATE(40),
    [sym_boolean] = STATE(37),
    [aux_sym_source_file_repeat1] = STATE(5),
    [ts_builtin_sym_end] = ACTIONS(3),
    [anon_sym_let] = ACTIONS(5),
    [anon_sym_if] = ACTIONS(7),
    [anon_sym_DASH] = ACTIONS(9),
    [anon_sym_BANG] = ACTIONS(9),
    [sym_variable] = ACTIONS(11),
    [anon_sym_true] = ACTIONS(13),
    [anon_sym_false] = ACTIONS(13),
    [sym_float] = ACTIONS(15),
    [sym_integer] = ACTIONS(17),
    [sym_string] = ACTIONS(15),
  },
  [2] = {
    [sym_statement] = STATE(65),
    [sym_let_declaration] = STATE(24),
    [sym_if_expression] = STATE(61),
    [sym_expression] = STATE(52),
    [sym__arithmetic_expression] = STATE(44),
    [sym_binary_expression] = STATE(44),
    [sym_unary_expression] = STATE(44),
    [sym_call_expression] = STATE(44),
    [sym_primary_expression] = STATE(44),
    [sym_value] = STATE(40),
    [sym_boolean] = STATE(37),
    [aux_sym_block_repeat1] = STATE(4),
    [anon_sym_let] = ACTIONS(19),
    [anon_sym_if] = ACTIONS(21),
    [anon_sym_RBRACE] = ACTIONS(23),
    [anon_sym_DASH] = ACTIONS(9),
    [anon_sym_BANG] = ACTIONS(9),
    [sym_variable] = ACTIONS(11),
    [anon_sym_true] = ACTIONS(13),
    [anon_sym_false] = ACTIONS(13),
    [sym_float] = ACTIONS(15),
    [sym_integer] = ACTIONS(17),
    [sym_string] = ACTIONS(15),
  },
  [3] = {
    [sym_statement] = STATE(3),
    [sym_let_declaration] = STATE(24),
    [sym_if_expression] = STATE(24),
    [sym_expression] = STATE(55),
    [sym__arithmetic_expression] = STATE(44),
    [sym_binary_expression] = STATE(44),
    [sym_unary_expression] = STATE(44),
    [sym_call_expression] = STATE(44),
    [sym_primary_expression] = STATE(44),
    [sym_value] = STATE(40),
    [sym_boolean] = STATE(37),
    [aux_sym_source_file_repeat1] = STATE(3),
    [ts_builtin_sym_end] = ACTIONS(25),
    [anon_sym_let] = ACTIONS(27),
    [anon_sym_if] = ACTIONS(30),
    [anon_sym_DASH] = ACTIONS(33),
    [anon_sym_BANG] = ACTIONS(33),
    [sym_variable] = ACTIONS(36),
    [anon_sym_true] = ACTIONS(39),
    [anon_sym_false] = ACTIONS(39),
    [sym_float] = ACTIONS(42),
    [sym_integer] = ACTIONS(45),
    [sym_string] = ACTIONS(42),
  },
  [4] = {
    [sym_statement] = STATE(65),
    [sym_let_declaration] = STATE(24),
    [sym_if_expression] = STATE(59),
    [sym_expression] = STATE(48),
    [sym__arithmetic_expression] = STATE(44),
    [sym_binary_expression] = STATE(44),
    [sym_unary_expression] = STATE(44),
    [sym_call_expression] = STATE(44),
    [sym_primary_expression] = STATE(44),
    [sym_value] = STATE(40),
    [sym_boolean] = STATE(37),
    [aux_sym_block_repeat1] = STATE(8),
    [anon_sym_let] = ACTIONS(19),
    [anon_sym_if] = ACTIONS(21),
    [anon_sym_RBRACE] = ACTIONS(48),
    [anon_sym_DASH] = ACTIONS(9),
    [anon_sym_BANG] = ACTIONS(9),
    [sym_variable] = ACTIONS(11),
    [anon_sym_true] = ACTIONS(13),
    [anon_sym_false] = ACTIONS(13),
    [sym_float] = ACTIONS(15),
    [sym_integer] = ACTIONS(17),
    [sym_string] = ACTIONS(15),
  },
  [5] = {
    [sym_statement] = STATE(3),
    [sym_let_declaration] = STATE(24),
    [sym_if_expression] = STATE(24),
    [sym_expression] = STATE(55),
    [sym__arithmetic_expression] = STATE(44),
    [sym_binary_expression] = STATE(44),
    [sym_unary_expression] = STATE(44),
    [sym_call_expression] = STATE(44),
    [sym_primary_expression] = STATE(44),
    [sym_value] = STATE(40),
    [sym_boolean] = STATE(37),
    [aux_sym_source_file_repeat1] = STATE(3),
    [ts_builtin_sym_end] = ACTIONS(50),
    [anon_sym_let] = ACTIONS(5),
    [anon_sym_if] = ACTIONS(7),
    [anon_sym_DASH] = ACTIONS(9),
    [anon_sym_BANG] = ACTIONS(9),
    [sym_variable] = ACTIONS(11),
    [anon_sym_true] = ACTIONS(13),
    [anon_sym_false] = ACTIONS(13),
    [sym_float] = ACTIONS(15),
    [sym_integer] = ACTIONS(17),
    [sym_string] = ACTIONS(15),
  },
  [6] = {
    [sym_statement] = STATE(65),
    [sym_let_declaration] = STATE(24),
    [sym_if_expression] = STATE(64),
    [sym_expression] = STATE(54),
    [sym__arithmetic_expression] = STATE(44),
    [sym_binary_expression] = STATE(44),
    [sym_unary_expression] = STATE(44),
    [sym_call_expression] = STATE(44),
    [sym_primary_expression] = STATE(44),
    [sym_value] = STATE(40),
    [sym_boolean] = STATE(37),
    [aux_sym_block_repeat1] = STATE(8),
    [anon_sym_let] = ACTIONS(19),
    [anon_sym_if] = ACTIONS(21),
    [anon_sym_RBRACE] = ACTIONS(52),
    [anon_sym_DASH] = ACTIONS(9),
    [anon_sym_BANG] = ACTIONS(9),
    [sym_variable] = ACTIONS(11),
    [anon_sym_true] = ACTIONS(13),
    [anon_sym_false] = ACTIONS(13),
    [sym_float] = ACTIONS(15),
    [sym_integer] = ACTIONS(17),
    [sym_string] = ACTIONS(15),
  },
  [7] = {
    [sym_statement] = STATE(65),
    [sym_let_declaration] = STATE(24),
    [sym_if_expression] = STATE(62),
    [sym_expression] = STATE(53),
    [sym__arithmetic_expression] = STATE(44),
    [sym_binary_expression] = STATE(44),
    [sym_unary_expression] = STATE(44),
    [sym_call_expression] = STATE(44),
    [sym_primary_expression] = STATE(44),
    [sym_value] = STATE(40),
    [sym_boolean] = STATE(37),
    [aux_sym_block_repeat1] = STATE(6),
    [anon_sym_let] = ACTIONS(19),
    [anon_sym_if] = ACTIONS(21),
    [anon_sym_RBRACE] = ACTIONS(54),
    [anon_sym_DASH] = ACTIONS(9),
    [anon_sym_BANG] = ACTIONS(9),
    [sym_variable] = ACTIONS(11),
    [anon_sym_true] = ACTIONS(13),
    [anon_sym_false] = ACTIONS(13),
    [sym_float] = ACTIONS(15),
    [sym_integer] = ACTIONS(17),
    [sym_string] = ACTIONS(15),
  },
  [8] = {
    [sym_statement] = STATE(65),
    [sym_let_declaration] = STATE(24),
    [sym_if_expression] = STATE(24),
    [sym_expression] = STATE(55),
    [sym__arithmetic_expression] = STATE(44),
    [sym_binary_expression] = STATE(44),
    [sym_unary_expression] = STATE(44),
    [sym_call_expression] = STATE(44),
    [sym_primary_expression] = STATE(44),
    [sym_value] = STATE(40),
    [sym_boolean] = STATE(37),
    [aux_sym_block_repeat1] = STATE(8),
    [anon_sym_let] = ACTIONS(56),
    [anon_sym_if] = ACTIONS(59),
    [anon_sym_RBRACE] = ACTIONS(62),
    [anon_sym_DASH] = ACTIONS(64),
    [anon_sym_BANG] = ACTIONS(64),
    [sym_variable] = ACTIONS(67),
    [anon_sym_true] = ACTIONS(70),
    [anon_sym_false] = ACTIONS(70),
    [sym_float] = ACTIONS(73),
    [sym_integer] = ACTIONS(76),
    [sym_string] = ACTIONS(73),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 11,
    ACTIONS(7), 1,
      anon_sym_if,
    ACTIONS(11), 1,
      sym_variable,
    ACTIONS(17), 1,
      sym_integer,
    STATE(23), 1,
      sym_if_expression,
    STATE(37), 1,
      sym_boolean,
    STATE(40), 1,
      sym_value,
    STATE(57), 1,
      sym_expression,
    ACTIONS(9), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(15), 2,
      sym_float,
      sym_string,
    STATE(44), 5,
      sym__arithmetic_expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_call_expression,
      sym_primary_expression,
  [41] = 11,
    ACTIONS(11), 1,
      sym_variable,
    ACTIONS(17), 1,
      sym_integer,
    ACTIONS(21), 1,
      anon_sym_if,
    STATE(23), 1,
      sym_if_expression,
    STATE(37), 1,
      sym_boolean,
    STATE(40), 1,
      sym_value,
    STATE(57), 1,
      sym_expression,
    ACTIONS(9), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(15), 2,
      sym_float,
      sym_string,
    STATE(44), 5,
      sym__arithmetic_expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_call_expression,
      sym_primary_expression,
  [82] = 11,
    ACTIONS(82), 1,
      anon_sym_RPAREN,
    ACTIONS(84), 1,
      sym_variable,
    ACTIONS(93), 1,
      sym_integer,
    STATE(11), 1,
      aux_sym_call_repeat1,
    STATE(37), 1,
      sym_boolean,
    STATE(40), 1,
      sym_value,
    STATE(56), 1,
      sym_expression,
    ACTIONS(79), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(87), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(90), 2,
      sym_float,
      sym_string,
    STATE(44), 5,
      sym__arithmetic_expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_call_expression,
      sym_primary_expression,
  [123] = 11,
    ACTIONS(11), 1,
      sym_variable,
    ACTIONS(17), 1,
      sym_integer,
    ACTIONS(96), 1,
      anon_sym_RPAREN,
    STATE(13), 1,
      aux_sym_call_repeat1,
    STATE(37), 1,
      sym_boolean,
    STATE(40), 1,
      sym_value,
    STATE(47), 1,
      sym_expression,
    ACTIONS(9), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(15), 2,
      sym_float,
      sym_string,
    STATE(44), 5,
      sym__arithmetic_expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_call_expression,
      sym_primary_expression,
  [164] = 11,
    ACTIONS(11), 1,
      sym_variable,
    ACTIONS(17), 1,
      sym_integer,
    ACTIONS(98), 1,
      anon_sym_RPAREN,
    STATE(11), 1,
      aux_sym_call_repeat1,
    STATE(37), 1,
      sym_boolean,
    STATE(40), 1,
      sym_value,
    STATE(46), 1,
      sym_expression,
    ACTIONS(9), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(15), 2,
      sym_float,
      sym_string,
    STATE(44), 5,
      sym__arithmetic_expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_call_expression,
      sym_primary_expression,
  [205] = 9,
    ACTIONS(11), 1,
      sym_variable,
    ACTIONS(17), 1,
      sym_integer,
    STATE(37), 1,
      sym_boolean,
    STATE(40), 1,
      sym_value,
    STATE(50), 1,
      sym_expression,
    ACTIONS(9), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(15), 2,
      sym_float,
      sym_string,
    STATE(44), 5,
      sym__arithmetic_expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_call_expression,
      sym_primary_expression,
  [240] = 9,
    ACTIONS(11), 1,
      sym_variable,
    ACTIONS(17), 1,
      sym_integer,
    STATE(25), 1,
      sym_expression,
    STATE(37), 1,
      sym_boolean,
    STATE(40), 1,
      sym_value,
    ACTIONS(9), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(15), 2,
      sym_float,
      sym_string,
    STATE(44), 5,
      sym__arithmetic_expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_call_expression,
      sym_primary_expression,
  [275] = 9,
    ACTIONS(11), 1,
      sym_variable,
    ACTIONS(17), 1,
      sym_integer,
    STATE(37), 1,
      sym_boolean,
    STATE(40), 1,
      sym_value,
    STATE(49), 1,
      sym_expression,
    ACTIONS(9), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(15), 2,
      sym_float,
      sym_string,
    STATE(44), 5,
      sym__arithmetic_expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_call_expression,
      sym_primary_expression,
  [310] = 9,
    ACTIONS(11), 1,
      sym_variable,
    ACTIONS(17), 1,
      sym_integer,
    STATE(28), 1,
      sym_expression,
    STATE(37), 1,
      sym_boolean,
    STATE(40), 1,
      sym_value,
    ACTIONS(9), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(15), 2,
      sym_float,
      sym_string,
    STATE(44), 5,
      sym__arithmetic_expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_call_expression,
      sym_primary_expression,
  [345] = 9,
    ACTIONS(11), 1,
      sym_variable,
    ACTIONS(17), 1,
      sym_integer,
    STATE(29), 1,
      sym_expression,
    STATE(37), 1,
      sym_boolean,
    STATE(40), 1,
      sym_value,
    ACTIONS(9), 2,
      anon_sym_DASH,
      anon_sym_BANG,
    ACTIONS(13), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(15), 2,
      sym_float,
      sym_string,
    STATE(44), 5,
      sym__arithmetic_expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_call_expression,
      sym_primary_expression,
  [380] = 2,
    ACTIONS(100), 7,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_RBRACE,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_float,
      sym_string,
    ACTIONS(102), 7,
      anon_sym_let,
      anon_sym_if,
      anon_sym_else,
      sym_variable,
      anon_sym_true,
      anon_sym_false,
      sym_integer,
  [399] = 2,
    ACTIONS(104), 7,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_RBRACE,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_float,
      sym_string,
    ACTIONS(106), 7,
      anon_sym_let,
      anon_sym_if,
      anon_sym_else,
      sym_variable,
      anon_sym_true,
      anon_sym_false,
      sym_integer,
  [418] = 2,
    ACTIONS(108), 7,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_RBRACE,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_float,
      sym_string,
    ACTIONS(110), 7,
      anon_sym_let,
      anon_sym_if,
      anon_sym_else,
      sym_variable,
      anon_sym_true,
      anon_sym_false,
      sym_integer,
  [437] = 2,
    ACTIONS(114), 6,
      anon_sym_let,
      anon_sym_if,
      sym_variable,
      anon_sym_true,
      anon_sym_false,
      sym_integer,
    ACTIONS(112), 7,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_RBRACE,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_float,
      sym_string,
  [455] = 2,
    ACTIONS(116), 6,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_float,
      sym_string,
    ACTIONS(118), 6,
      anon_sym_let,
      anon_sym_if,
      sym_variable,
      anon_sym_true,
      anon_sym_false,
      sym_integer,
  [472] = 2,
    ACTIONS(120), 6,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_float,
      sym_string,
    ACTIONS(122), 6,
      anon_sym_let,
      anon_sym_if,
      sym_variable,
      anon_sym_true,
      anon_sym_false,
      sym_integer,
  [489] = 4,
    ACTIONS(126), 1,
      anon_sym_LPAREN,
    STATE(26), 1,
      aux_sym_call_expression_repeat1,
    STATE(41), 1,
      sym_call,
    ACTIONS(124), 9,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [510] = 3,
    STATE(32), 1,
      aux_sym_call_expression_repeat1,
    STATE(41), 1,
      sym_call,
    ACTIONS(128), 10,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [529] = 3,
    ACTIONS(134), 1,
      anon_sym_else,
    ACTIONS(130), 5,
      ts_builtin_sym_end,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_float,
      sym_string,
    ACTIONS(132), 6,
      anon_sym_let,
      anon_sym_if,
      sym_variable,
      anon_sym_true,
      anon_sym_false,
      sym_integer,
  [548] = 4,
    ACTIONS(126), 1,
      anon_sym_LPAREN,
    STATE(26), 1,
      aux_sym_call_expression_repeat1,
    STATE(41), 1,
      sym_call,
    ACTIONS(136), 9,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [569] = 5,
    ACTIONS(126), 1,
      anon_sym_LPAREN,
    STATE(26), 1,
      aux_sym_call_expression_repeat1,
    STATE(41), 1,
      sym_call,
    ACTIONS(138), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(136), 7,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [592] = 2,
    ACTIONS(140), 6,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_float,
      sym_string,
    ACTIONS(142), 6,
      anon_sym_let,
      anon_sym_if,
      sym_variable,
      anon_sym_true,
      anon_sym_false,
      sym_integer,
  [609] = 2,
    ACTIONS(144), 6,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_float,
      sym_string,
    ACTIONS(146), 6,
      anon_sym_let,
      anon_sym_if,
      sym_variable,
      anon_sym_true,
      anon_sym_false,
      sym_integer,
  [626] = 4,
    ACTIONS(150), 1,
      anon_sym_LPAREN,
    STATE(32), 1,
      aux_sym_call_expression_repeat1,
    STATE(41), 1,
      sym_call,
    ACTIONS(148), 9,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [647] = 2,
    ACTIONS(100), 5,
      ts_builtin_sym_end,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_float,
      sym_string,
    ACTIONS(102), 6,
      anon_sym_let,
      anon_sym_if,
      sym_variable,
      anon_sym_true,
      anon_sym_false,
      sym_integer,
  [663] = 2,
    ACTIONS(108), 5,
      ts_builtin_sym_end,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_float,
      sym_string,
    ACTIONS(110), 6,
      anon_sym_let,
      anon_sym_if,
      sym_variable,
      anon_sym_true,
      anon_sym_false,
      sym_integer,
  [679] = 2,
    ACTIONS(104), 5,
      ts_builtin_sym_end,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_float,
      sym_string,
    ACTIONS(106), 6,
      anon_sym_let,
      anon_sym_if,
      sym_variable,
      anon_sym_true,
      anon_sym_false,
      sym_integer,
  [695] = 2,
    ACTIONS(62), 5,
      anon_sym_RBRACE,
      anon_sym_DASH,
      anon_sym_BANG,
      sym_float,
      sym_string,
    ACTIONS(153), 6,
      anon_sym_let,
      anon_sym_if,
      sym_variable,
      anon_sym_true,
      anon_sym_false,
      sym_integer,
  [711] = 1,
    ACTIONS(155), 10,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [724] = 1,
    ACTIONS(157), 10,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [737] = 1,
    ACTIONS(159), 10,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [750] = 1,
    ACTIONS(161), 10,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [763] = 1,
    ACTIONS(163), 10,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [776] = 1,
    ACTIONS(165), 10,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [789] = 1,
    ACTIONS(167), 10,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [802] = 1,
    ACTIONS(169), 10,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [815] = 1,
    ACTIONS(171), 10,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_SLASH,
      anon_sym_DASH,
      anon_sym_LPAREN,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [828] = 7,
    ACTIONS(126), 1,
      anon_sym_LPAREN,
    ACTIONS(175), 1,
      anon_sym_COMMA,
    ACTIONS(177), 1,
      anon_sym_RPAREN,
    STATE(26), 1,
      aux_sym_call_expression_repeat1,
    STATE(41), 1,
      sym_call,
    ACTIONS(138), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(173), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [852] = 7,
    ACTIONS(126), 1,
      anon_sym_LPAREN,
    ACTIONS(175), 1,
      anon_sym_COMMA,
    ACTIONS(179), 1,
      anon_sym_RPAREN,
    STATE(26), 1,
      aux_sym_call_expression_repeat1,
    STATE(41), 1,
      sym_call,
    ACTIONS(138), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(173), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [876] = 7,
    ACTIONS(126), 1,
      anon_sym_LPAREN,
    ACTIONS(181), 1,
      anon_sym_SEMI,
    ACTIONS(183), 1,
      anon_sym_RBRACE,
    STATE(26), 1,
      aux_sym_call_expression_repeat1,
    STATE(41), 1,
      sym_call,
    ACTIONS(138), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(173), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [900] = 7,
    ACTIONS(126), 1,
      anon_sym_LPAREN,
    ACTIONS(185), 1,
      anon_sym_LBRACE,
    STATE(26), 1,
      aux_sym_call_expression_repeat1,
    STATE(27), 1,
      sym_block,
    STATE(41), 1,
      sym_call,
    ACTIONS(138), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(173), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [924] = 7,
    ACTIONS(126), 1,
      anon_sym_LPAREN,
    ACTIONS(185), 1,
      anon_sym_LBRACE,
    STATE(26), 1,
      aux_sym_call_expression_repeat1,
    STATE(41), 1,
      sym_call,
    STATE(58), 1,
      sym_block,
    ACTIONS(138), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(173), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [948] = 2,
    ACTIONS(189), 4,
      sym_variable,
      anon_sym_true,
      anon_sym_false,
      sym_integer,
    ACTIONS(187), 5,
      anon_sym_DASH,
      anon_sym_BANG,
      anon_sym_RPAREN,
      sym_float,
      sym_string,
  [962] = 7,
    ACTIONS(48), 1,
      anon_sym_RBRACE,
    ACTIONS(126), 1,
      anon_sym_LPAREN,
    ACTIONS(181), 1,
      anon_sym_SEMI,
    STATE(26), 1,
      aux_sym_call_expression_repeat1,
    STATE(41), 1,
      sym_call,
    ACTIONS(138), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(173), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [986] = 7,
    ACTIONS(52), 1,
      anon_sym_RBRACE,
    ACTIONS(126), 1,
      anon_sym_LPAREN,
    ACTIONS(181), 1,
      anon_sym_SEMI,
    STATE(26), 1,
      aux_sym_call_expression_repeat1,
    STATE(41), 1,
      sym_call,
    ACTIONS(138), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(173), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [1010] = 7,
    ACTIONS(126), 1,
      anon_sym_LPAREN,
    ACTIONS(181), 1,
      anon_sym_SEMI,
    ACTIONS(191), 1,
      anon_sym_RBRACE,
    STATE(26), 1,
      aux_sym_call_expression_repeat1,
    STATE(41), 1,
      sym_call,
    ACTIONS(138), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(173), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [1034] = 6,
    ACTIONS(126), 1,
      anon_sym_LPAREN,
    ACTIONS(181), 1,
      anon_sym_SEMI,
    STATE(26), 1,
      aux_sym_call_expression_repeat1,
    STATE(41), 1,
      sym_call,
    ACTIONS(138), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(173), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [1055] = 6,
    ACTIONS(126), 1,
      anon_sym_LPAREN,
    ACTIONS(175), 1,
      anon_sym_COMMA,
    STATE(26), 1,
      aux_sym_call_expression_repeat1,
    STATE(41), 1,
      sym_call,
    ACTIONS(138), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(173), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [1076] = 6,
    ACTIONS(126), 1,
      anon_sym_LPAREN,
    ACTIONS(193), 1,
      anon_sym_SEMI,
    STATE(26), 1,
      aux_sym_call_expression_repeat1,
    STATE(41), 1,
      sym_call,
    ACTIONS(138), 2,
      anon_sym_STAR,
      anon_sym_SLASH,
    ACTIONS(173), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
  [1097] = 2,
    ACTIONS(195), 1,
      anon_sym_else,
    ACTIONS(130), 2,
      anon_sym_SEMI,
      anon_sym_RBRACE,
  [1105] = 2,
    ACTIONS(120), 1,
      anon_sym_SEMI,
    ACTIONS(183), 1,
      anon_sym_RBRACE,
  [1112] = 2,
    ACTIONS(197), 1,
      anon_sym_LBRACE,
    STATE(22), 1,
      sym_block,
  [1119] = 2,
    ACTIONS(48), 1,
      anon_sym_RBRACE,
    ACTIONS(120), 1,
      anon_sym_SEMI,
  [1126] = 2,
    ACTIONS(52), 1,
      anon_sym_RBRACE,
    ACTIONS(120), 1,
      anon_sym_SEMI,
  [1133] = 2,
    ACTIONS(185), 1,
      anon_sym_LBRACE,
    STATE(22), 1,
      sym_block,
  [1140] = 2,
    ACTIONS(120), 1,
      anon_sym_SEMI,
    ACTIONS(191), 1,
      anon_sym_RBRACE,
  [1147] = 1,
    ACTIONS(199), 1,
      anon_sym_SEMI,
  [1151] = 1,
    ACTIONS(201), 1,
      ts_builtin_sym_end,
  [1155] = 1,
    ACTIONS(203), 1,
      sym_variable,
  [1159] = 1,
    ACTIONS(205), 1,
      anon_sym_EQ,
  [1163] = 1,
    ACTIONS(207), 1,
      anon_sym_EQ,
  [1167] = 1,
    ACTIONS(209), 1,
      sym_variable,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(9)] = 0,
  [SMALL_STATE(10)] = 41,
  [SMALL_STATE(11)] = 82,
  [SMALL_STATE(12)] = 123,
  [SMALL_STATE(13)] = 164,
  [SMALL_STATE(14)] = 205,
  [SMALL_STATE(15)] = 240,
  [SMALL_STATE(16)] = 275,
  [SMALL_STATE(17)] = 310,
  [SMALL_STATE(18)] = 345,
  [SMALL_STATE(19)] = 380,
  [SMALL_STATE(20)] = 399,
  [SMALL_STATE(21)] = 418,
  [SMALL_STATE(22)] = 437,
  [SMALL_STATE(23)] = 455,
  [SMALL_STATE(24)] = 472,
  [SMALL_STATE(25)] = 489,
  [SMALL_STATE(26)] = 510,
  [SMALL_STATE(27)] = 529,
  [SMALL_STATE(28)] = 548,
  [SMALL_STATE(29)] = 569,
  [SMALL_STATE(30)] = 592,
  [SMALL_STATE(31)] = 609,
  [SMALL_STATE(32)] = 626,
  [SMALL_STATE(33)] = 647,
  [SMALL_STATE(34)] = 663,
  [SMALL_STATE(35)] = 679,
  [SMALL_STATE(36)] = 695,
  [SMALL_STATE(37)] = 711,
  [SMALL_STATE(38)] = 724,
  [SMALL_STATE(39)] = 737,
  [SMALL_STATE(40)] = 750,
  [SMALL_STATE(41)] = 763,
  [SMALL_STATE(42)] = 776,
  [SMALL_STATE(43)] = 789,
  [SMALL_STATE(44)] = 802,
  [SMALL_STATE(45)] = 815,
  [SMALL_STATE(46)] = 828,
  [SMALL_STATE(47)] = 852,
  [SMALL_STATE(48)] = 876,
  [SMALL_STATE(49)] = 900,
  [SMALL_STATE(50)] = 924,
  [SMALL_STATE(51)] = 948,
  [SMALL_STATE(52)] = 962,
  [SMALL_STATE(53)] = 986,
  [SMALL_STATE(54)] = 1010,
  [SMALL_STATE(55)] = 1034,
  [SMALL_STATE(56)] = 1055,
  [SMALL_STATE(57)] = 1076,
  [SMALL_STATE(58)] = 1097,
  [SMALL_STATE(59)] = 1105,
  [SMALL_STATE(60)] = 1112,
  [SMALL_STATE(61)] = 1119,
  [SMALL_STATE(62)] = 1126,
  [SMALL_STATE(63)] = 1133,
  [SMALL_STATE(64)] = 1140,
  [SMALL_STATE(65)] = 1147,
  [SMALL_STATE(66)] = 1151,
  [SMALL_STATE(67)] = 1155,
  [SMALL_STATE(68)] = 1159,
  [SMALL_STATE(69)] = 1163,
  [SMALL_STATE(70)] = 1167,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [5] = {.entry = {.count = 1, .reusable = false}}, SHIFT(67),
  [7] = {.entry = {.count = 1, .reusable = false}}, SHIFT(16),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [11] = {.entry = {.count = 1, .reusable = false}}, SHIFT(40),
  [13] = {.entry = {.count = 1, .reusable = false}}, SHIFT(45),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
  [17] = {.entry = {.count = 1, .reusable = false}}, SHIFT(37),
  [19] = {.entry = {.count = 1, .reusable = false}}, SHIFT(70),
  [21] = {.entry = {.count = 1, .reusable = false}}, SHIFT(14),
  [23] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [25] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [27] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(67),
  [30] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(16),
  [33] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(15),
  [36] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(40),
  [39] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(45),
  [42] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(37),
  [45] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(37),
  [48] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [50] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [52] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [54] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [56] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_block_repeat1, 2), SHIFT_REPEAT(70),
  [59] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_block_repeat1, 2), SHIFT_REPEAT(14),
  [62] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_block_repeat1, 2),
  [64] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_block_repeat1, 2), SHIFT_REPEAT(15),
  [67] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_block_repeat1, 2), SHIFT_REPEAT(40),
  [70] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_block_repeat1, 2), SHIFT_REPEAT(45),
  [73] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_block_repeat1, 2), SHIFT_REPEAT(37),
  [76] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_block_repeat1, 2), SHIFT_REPEAT(37),
  [79] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 8), SHIFT_REPEAT(15),
  [82] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 8),
  [84] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 8), SHIFT_REPEAT(40),
  [87] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 8), SHIFT_REPEAT(45),
  [90] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 8), SHIFT_REPEAT(37),
  [93] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 8), SHIFT_REPEAT(37),
  [96] = {.entry = {.count = 1, .reusable = true}}, SHIFT(39),
  [98] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [100] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_block, 3),
  [102] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_block, 3),
  [104] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_block, 4),
  [106] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_block, 4),
  [108] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_block, 2),
  [110] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_block, 2),
  [112] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_if_expression, 5, .production_id = 9),
  [114] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_if_expression, 5, .production_id = 9),
  [116] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_let_declaration, 4),
  [118] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_let_declaration, 4),
  [120] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_statement, 1),
  [122] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_statement, 1),
  [124] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_expression, 2),
  [126] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [128] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_call_expression, 2, .production_id = 2),
  [130] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_if_expression, 3, .production_id = 3),
  [132] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_if_expression, 3, .production_id = 3),
  [134] = {.entry = {.count = 1, .reusable = false}}, SHIFT(60),
  [136] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_expression, 3),
  [138] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [140] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_statement, 2),
  [142] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_statement, 2),
  [144] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_let_declaration, 5),
  [146] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_let_declaration, 5),
  [148] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_call_expression_repeat1, 2, .production_id = 4),
  [150] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_call_expression_repeat1, 2, .production_id = 4), SHIFT_REPEAT(12),
  [153] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_block_repeat1, 2),
  [155] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_value, 1),
  [157] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_call, 4, .production_id = 10),
  [159] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_call, 2),
  [161] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_primary_expression, 1),
  [163] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_call_expression_repeat1, 1, .production_id = 1),
  [165] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_call, 3, .production_id = 6),
  [167] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_call, 3, .production_id = 7),
  [169] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expression, 1),
  [171] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_boolean, 1),
  [173] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [175] = {.entry = {.count = 1, .reusable = true}}, SHIFT(51),
  [177] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [179] = {.entry = {.count = 1, .reusable = true}}, SHIFT(42),
  [181] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [183] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [185] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [187] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 5),
  [189] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_call_repeat1, 2, .production_id = 5),
  [191] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [193] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [195] = {.entry = {.count = 1, .reusable = true}}, SHIFT(63),
  [197] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [199] = {.entry = {.count = 1, .reusable = true}}, SHIFT(36),
  [201] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [203] = {.entry = {.count = 1, .reusable = true}}, SHIFT(68),
  [205] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [207] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [209] = {.entry = {.count = 1, .reusable = true}}, SHIFT(69),
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
