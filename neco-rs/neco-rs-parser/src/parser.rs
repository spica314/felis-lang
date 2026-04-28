use crate::{Result, Token};

pub trait Parse: Sized {
    type ParseOption;

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        option: Option<Self::ParseOption>,
    ) -> Result<Option<Self>>;

    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>> {
        Self::parse_with_option(tokens, i, None)
    }
}
