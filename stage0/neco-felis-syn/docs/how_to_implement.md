# How to Implement

This document summarizes the implementation procedure for `neco-felis-syn`.

## 0. Prerequisites and Preparation

- The Felis Language AST and parser adopt Trees that Grow. Design every node so that it accepts `P: Phase` and stores `ext: P::XxxExt`.
- `lib.rs` is the top-level export surface. Whenever you add a node, extend the `pub use` section so other crates can access it.
- Split implementations per module. When you introduce a new syntactic category, create a file under `src/{items,terms,proc_terms,statements,position,parsing}` and re-export it from `lib.rs`.

## 1. Implement Lexing

1. **Define the token type.** Add a struct in `parsing/token.rs` and derive `Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash`. Include `Pos` as a field when you need to store location information.
2. **Add a variant to the `Token` enum.** Keep the existing order to make downstream pattern matching consistent.
3. **Extend `Token::lex`.** Insert a branch that emits the new token and make sure it updates `line` and `column`. Pay attention to whitespace handling and the precedence for multi-character operators.
4. **Prepare parser helpers.** When you only need to consume a single token, implement `Parse` for `TokenXxx`. For keywords or tokens that take arguments, provide static helpers such as `parse_keyword`.
5. **Add lexer tests.** Call `Token::lex` under `#[cfg(test)]` and assert the resulting tokens directly, so changes to the grammar immediately fail the test. Adding the minimal `.fe` input that exercises the new token lets subsequent syntax tests reuse it.

## 2. Design the AST Nodes

1. **Represent sequences with structs.** Name fields in snake_case, keep mandatory children as-is, represent `?` with `Option<T>`, and model `*`/`+` with `Vec<T>`. Place `ext: P::NodeExt` at the end.
2. **Represent choices with enums.** Use PascalCase for variant names and order them from the most specific form to the most general (the order you want to try matches).
3. **Add accessors when helpful.** Consider downstream consumers and provide helpers such as `name()` or `fields()` to keep later stages concise.
4. **Re-export from `lib.rs`.** Add the new node to the `pub use` list so other modules can refer to it as `crate::NewNode`.

Example: define a new `ItemDefinition` node like this.

```rust
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemDefinition<P: Phase> {
    pub keyword_definition: TokenKeyword,
    pub name: TokenVariable,
    pub colon: TokenColon,
    pub type_: Box<Term<P>>,
    pub brace_l: TokenBraceL,
    pub body: Box<Term<P>>,
    pub brace_r: TokenBraceR,
    pub ext: P::ItemDefinitionExt,
}
```

## 3. Implement the `Parse` Trait

1. **Write with backtracking in mind.**
   - Copy the read position with `let mut k = *i;` and return `Ok(None)` when the parse fails partway so the caller can try the next alternative.
   - Return `Err(ParseError::Unknown("…"))` when a mandatory element is missing so tests can expose the issue.
2. **Follow the PEG order for child nodes.** Use `while let Some(x) = Child::parse(tokens, &mut k)? { ... }` for repetition. Because the return type is `Option<Self>`, exit early with `Ok(None)` if the collected elements do not satisfy the rule.
3. **Tune the priority of choices.** In enum `Parse` implementations, chain `if let Some(...)` from the most specific form to the most general. Whenever you change the order, update the related tests.
4. **Honor Trees that Grow.** During the parsing phase assign `ext: ()`. When another phase needs data, extend the `Phase` trait in `parsing/phase.rs` with `type NewNodeExt` and set it to `()` in `PhaseParse`.

### Implementation Example: Sequence

```rust
impl Parse for ItemDefinition<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        let Some(keyword_definition) =
            TokenKeyword::parse_keyword(tokens, &mut k, "definition")?
        else {
            return Ok(None); // Delegate to other Item variants when the first token is not #definition
        };

        let Some(name) = TokenVariable::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected name after #definition"));
        };

        let Some(colon) = TokenColon::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected ':' after definition name"));
        };

        let Some(type_) = Term::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected type after ':'"));
        };

        let Some(brace_l) = TokenBraceL::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected '{' before definition body"));
        };

        let Some(body) = Term::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected definition body"));
        };

        let Some(brace_r) = TokenBraceR::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected closing '}'"));
        };

        let definition = ItemDefinition {
            keyword_definition,
            name,
            colon,
            type_: Box::new(type_),
            brace_l,
            body: Box::new(body),
            brace_r,
            ext: (),
        };

        *i = k;
        Ok(Some(definition))
    }
}
```

### Implementation Example: Ordered Choice

```rust
impl Parse for Item<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        if let Some(entrypoint) = ItemEntrypoint::parse(tokens, i)? {
            return Ok(Some(Item::Entrypoint(entrypoint)));
        }
        if let Some(use_builtin) = ItemUseBuiltin::parse(tokens, i)? {
            return Ok(Some(Item::UseBuiltin(use_builtin)));
        }
        if let Some(inductive) = ItemInductive::parse(tokens, i)? {
            return Ok(Some(Item::Inductive(inductive)));
        }
        if let Some(definition) = ItemDefinition::parse(tokens, i)? {
            return Ok(Some(Item::Definition(definition)));
        }
        if let Some(theorem) = ItemTheorem::parse(tokens, i)? {
            return Ok(Some(Item::Theorem(theorem)));
        }
        if let Some(proc_) = ItemProc::parse(tokens, i)? {
            return Ok(Some(Item::Proc(Box::new(proc_))));
        }
        if let Some(type_) = ItemType::parse(tokens, i)? {
            return Ok(Some(Item::Type(type_)));
        }
        Ok(None)
    }
}
```

## 4. Consult the Grammar (PEG Format)

Use the PEG-style definition below as the source of truth during implementation and align nodes with their `Parse` implementations. `?` means optional, `*` means zero or more, and `+` means one or more.

Each rule should be expressed as either a sequence or an ordered choice (sequences may include `?`, `*`, `+`), and struct or enum names should map directly to the grammar entries.

```
File              <- Item*

Item              <- ItemEntrypoint
                   / ItemUseBuiltin
                   / ItemInductive
                   / ItemDefinition
                   / ItemTheorem
                   / ItemProc
                   / ItemType
                   / ItemSubmodule
                   / ItemUse

ItemEntrypoint    <- #entrypoint Variable ";"
ItemUseBuiltin    <- #use_builtin String #as Variable ";"
ItemInductive     <- #inductive Variable ":" Term "{" ItemInductiveBranch* "}"
ItemInductiveBranch <- Variable ":" Term ","              // trailing comma required
ItemDefinition    <- #definition Variable ":" Term "{" Term "}"
ItemTheorem       <- #theorem    Variable ":" Term "{" Term "}"
ItemProc          <- ""#proc" Variable ":" Term ItemProcBlock
ItemProcBlock     <- "{" Statements "}"
ItemType          <- "#type" Variable "{" ItemTypeConstructor* "}"
ItemStructField   <- Variable ":" Term ","
ItemTypeConstructor <- Variable "{" ItemStructField* "}" ","?
ItemSubmodule     <- "#submodule" Variable ";"
ItemUse           <- "#use" ItemUseScopePrefix* Variable ";"
ItemUseScopePrefix <- Variable "::"

Statements        <- Statement ";" Statements
                   / Statement
                   / ε
Statement         <- StatementLetMut
                   / StatementLet
                   / StatementMethodChainAssign
                   / StatementAssign
                   / StatementLoop
                   / StatementBreak
                   / StatementReturn
                   / ProcTerm                            // expression statement

StatementLetMut   <- "#let" "#mut" Variable "@" Variable "=" ProcTerm
StatementLet      <- "#let" Variable "=" ProcTerm
StatementAssign   <- Variable "<-" ProcTerm
StatementMethodChainAssign
                   <- ProcTermMethodChain "<-" NumberOrVariable
                    / ProcTermFieldAccess ". set" NumberOrVariable NumberOrVariable
StatementLoop     <- "#loop" "{" Statements "}"
StatementBreak    <- "#break" ";"
StatementReturn   <- "#return" ProcTerm ";"

NumberOrVariable <- Number / Variable

Term              <- TermMatch
                   / TermArrowDep
                   / TermArrowNodep
                   / TermApply
                   / TermStruct
                   / TermVariable
                   / TermNumber
                   / TermUnit
                   / TermParen
TermMatch         <- "#match" Variable "{" TermMatchBranch* "}"
TermMatchBranch   <- Pattern "=>" Term ","
Pattern           <- Variable Variable*                // constructor pattern when arguments are present
TermArrowDep      <- "(" TermVariable ":" Term ")" "->" Term
TermArrowNodep    <- TermPrimaryArrow "->" Term
TermPrimaryArrow  <- TermUnit
                   / TermParen
                   / TermVariable
                   / TermNumber
TermApply         <- TermApplyElem TermApplyElem+                   // f arg1 arg2 ...
TermApplyElem     <- TermUnit / TermParen / TermVariable / TermNumber
TermStruct        <- #struct "{" TermStructField* "}"
TermUnit          <- "()"
TermParen         <- "(" Term ")"
TermNumber        <- Number
TermVariable      <- Variable

ProcTerm          <- ProcTermIf
                   / ProcTermMatch
                   / ProcTermApply
                   / ProcTermFieldAccess
                   / ProcTermMethodChain
                   / ProcTermStructValue
                   / ProcTermVariable
ProcTermMatch     <- "#match" Variable "{" ProcTermMatchBranch* "}"
ProcTermMatchBranch <- ProcTermMatchPattern "=>" Statement ","
ProcTermMatchPattern <- Variable "::" Variable "{" ProcTermMatchField* "}"
ProcTermMatchField <- Variable ":" Variable ","?
                   / ProcTermNumber
                   / ProcTermUnit
                   / ProcTermParen
ProcTermIf        <- "#if" Statements "{" Statements "}" ProcTermIfPartElse?
ProcTermIfPartElse <- #else "{" Statements "}"
ProcTermApply     <- ProcTermApplyElem+
ProcTermApplyElem <- ProcTermUnit / ProcTermParen / ProcTermVariable / ProcTermNumber
ProcTermFieldAccess <- Variable "." Variable ProcTermSimple?       // no whitespace allowed before '.'
ProcTermMethodChain <- Variable " ." VariableOrKeyword ProcTermSimple? // whitespace required before '.'
ProcTermStructValue <- Variable "{" ProcTermStructField* "}"
ProcTermStructField <- Variable ":" ProcTerm ","
TypeArg           <- Variable
VariableOrKeyword <- Variable / #keyword

ProcTermSimple    <- ProcTermNumber / ProcTermVariable
```

## 5. Implementation Patterns

- **Recursion in `Statements`:** `Statements::parse` reads one `Statement`, checks the semicolon, and builds the linked list (`StatementsThen`). If there is no trailing semicolon, terminate with a single node.
- **Trailing commas in struct fields:** When trailing commas are allowed, define the field type with `Option<TokenComma>` and store the result of `TokenComma::parse` directly. When the comma is mandatory (for example `ItemInductiveBranch`), keep `TokenComma` as a required field.
- **Field access vs. method chains:** Use `TokenOperator::parse_operator_after_non_whitespace` and `parse_operator_after_whitespace` to differentiate `a.b` from `a . b`. Reuse these helpers when you add new dotted syntax.
- **Constructor calls with type arguments:** For `Type Arg1 Arg2 ::method ...`, greedily parse zero or more `TypeArg` (variables) before `::`, then allow the method token to come from either a variable or a keyword.
- **Extension points (`Ext` variants):** `Statement::Ext(P::StatementExt)` and `ProcTerm::Ext(P::ProcTermExt)` let you attach phase-specific information without changing the grammar. Prefer filling the `Ext` variants over adding new enum variants whenever possible.

## 6. Development Flow

1. **Finalize the specification.** Merge your changes into the PEG above and list the tokens and nodes you need.
2. **Implement in the order tokens → AST → parser.** Working from the lower layers to the upper ones keeps the diff minimal.
3. **Update the Trees that Grow types.** When a new node needs an extension field, add `type NewNodeExt` to the `Phase` trait in `parsing/phase.rs` and assign `()` in `PhaseParse`.
4. **Align the tests.** Add or update the affected `.fe` files, refresh the accompanying Rust tests, and run `cargo test --workspace --offline`.
5. **Perform the final check.** Review the re-exports in `lib.rs` and run `cargo clippy --all-targets --all-features -- -D warnings` to ensure there are no unused imports.
