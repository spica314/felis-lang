# How to Implement

This document summarizes the implementation procedure for `neco-json`.

## 0. Goals

- Provide a minimal JSON AST and parser without serde.
- Keep the lexer strict to the JSON specification (no trailing commas, no leading zeros).
- Keep the public API small: `parse_json` plus the AST and error types.

## 1. Tokenization

1. Define `TokenKind` and `Token` in `src/lexer.rs`.
2. Implement `Lexer::lex` to emit tokens for `{ } [ ] : ,`, strings, numbers, booleans, and null.
3. Validate string escapes (`\"`, `\\`, `\/`, `\b`, `\f`, `\n`, `\r`, `\t`, `\uXXXX`).
4. Reject invalid characters and control characters in strings.

## 2. AST

- `JsonValue` represents `Null`, `Bool`, `Number`, `String`, `Array`, and `Object`.
- Store object members as `Vec<(String, JsonValue)>` to preserve ordering.

## 3. Parsing

1. Implement a recursive descent parser in `src/parser.rs`.
2. `parse_value` dispatches based on the current token.
3. Parse arrays and objects with explicit separators and no trailing commas.
4. Require EOF after parsing the top-level value.

## 4. Tests

- Add basic success cases (scalars, arrays, objects, escapes).
- Add failure cases for trailing commas, invalid escapes, and leading zeros.
