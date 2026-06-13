# neco (Felis ver.)

This workspace is a small Felis-written bootstrap/demo compiler. It is not a
complete self-hosting Felis parser.

`neco-felis-parser` currently recognizes enough source text to find a direct
`IO::sys_exit <integer-literal>` call in simple programs. The lexer token model is
intentionally narrow and uses fixed identifier tokens for the subset it needs:

- keywords: `#entrypoint`, `#fn`, `#let`, `#use`, `#with`
- identifiers: `std_core`, `io`, `IO`, `main`, `sys_exit`, `i32`
- punctuation: `(`, `)`, `{`, `}`, `:`, `;`, `::`, `<-`, `_`
- decimal integer literals and end of file

Most documented Felis syntax is still outside this implementation, including
general identifiers, string literals, char literals, `#pub`, `#type`,
`#struct`, `#match`, `#if`, `#loop`, `#letref`, `.`, `->`, and `=>`.
