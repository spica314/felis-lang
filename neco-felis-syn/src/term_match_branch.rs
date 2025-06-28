use crate::{
    Parse, ParseError, Phase, PhaseParse, Term,
    token::{Token, TokenOperator, TokenVariable},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Pattern {
    Variable(TokenVariable),
    Constructor(TokenVariable, Vec<TokenVariable>),
}

impl Pattern {
    /// Check if this pattern is a variable pattern
    pub fn is_variable(&self) -> bool {
        matches!(self, Pattern::Variable(_))
    }

    /// Check if this pattern is a constructor pattern
    pub fn is_constructor(&self) -> bool {
        matches!(self, Pattern::Constructor(_, _))
    }

    /// Get the variable if this is a variable pattern
    pub fn as_variable(&self) -> Option<&TokenVariable> {
        match self {
            Pattern::Variable(var) => Some(var),
            _ => None,
        }
    }

    /// Get the constructor name and arguments if this is a constructor pattern
    pub fn as_constructor(&self) -> Option<(&TokenVariable, &[TokenVariable])> {
        match self {
            Pattern::Constructor(name, args) => Some((name, args)),
            _ => None,
        }
    }
}

impl Parse for Pattern {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        // Parse first variable (constructor or simple variable)
        let Some(first) = TokenVariable::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        // Check if there are arguments (making it a constructor pattern)
        let mut args = Vec::new();
        while let Some(arg) = TokenVariable::parse(tokens, &mut k)? {
            args.push(arg);

            // Check if next token is => to stop parsing arguments
            let mut test_k = k;
            if TokenOperator::parse_operator(tokens, &mut test_k, "=>")
                .unwrap_or(None)
                .is_some()
            {
                break;
            }
        }

        let pattern = if args.is_empty() {
            Pattern::Variable(first)
        } else {
            Pattern::Constructor(first, args)
        };

        *i = k;
        Ok(Some(pattern))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TermMatchBranch<P: Phase> {
    pub pattern: Pattern,
    pub arrow: TokenOperator,
    pub body: Box<Term<P>>,
    pub ext: P::TermMatchBranchExt,
}

impl<P: Phase> TermMatchBranch<P> {
    /// Get the pattern of this match branch
    pub fn pattern(&self) -> &Pattern {
        &self.pattern
    }

    /// Get the body term of this match branch
    pub fn body(&self) -> &Term<P> {
        &self.body
    }
}

impl Parse for TermMatchBranch<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        // Parse pattern
        let Some(pattern) = Pattern::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        // Parse => operator
        let Some(arrow) = TokenOperator::parse_operator(tokens, &mut k, "=>")? else {
            return Err(ParseError::Unknown("expected => after match pattern"));
        };

        // Parse body term
        let Some(body) = Term::parse(tokens, &mut k)? else {
            return Err(ParseError::Unknown("expected term after =>"));
        };

        let branch = TermMatchBranch {
            pattern,
            arrow,
            body: Box::new(body),
            ext: (),
        };

        *i = k;
        Ok(Some(branch))
    }
}
