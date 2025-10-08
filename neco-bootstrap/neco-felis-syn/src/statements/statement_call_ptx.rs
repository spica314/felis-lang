use crate::{
    Parse, ParseError, Phase, PhaseParse,
    token::{Token, TokenKeyword, TokenNumber, TokenVariable},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementCallPtx<P: Phase> {
    pub keyword_call_ptx: TokenKeyword,
    pub function_name: TokenVariable,
    pub arg: TokenVariable,
    pub grid_dim_x: TokenNumber,
    pub grid_dim_y: TokenNumber,
    pub grid_dim_z: TokenNumber,
    pub block_dim_x: TokenNumber,
    pub block_dim_y: TokenNumber,
    pub block_dim_z: TokenNumber,
    pub ext: P::StatementCallPtxExt,
}

impl Parse for StatementCallPtx<PhaseParse> {
    fn parse(tokens: &[Token], i: &mut usize) -> Result<Option<Self>, ParseError> {
        let mut k = *i;

        let Some(keyword_call_ptx) = TokenKeyword::parse_keyword(tokens, &mut k, "call_ptx")?
        else {
            return Ok(None);
        };

        let Some(function_name) = TokenVariable::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let Some(arg) = TokenVariable::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        // Parse the 6 dimension parameters
        let Some(grid_dim_x) = TokenNumber::parse(tokens, &mut k)? else {
            return Ok(None);
        };
        let Some(grid_dim_y) = TokenNumber::parse(tokens, &mut k)? else {
            return Ok(None);
        };
        let Some(grid_dim_z) = TokenNumber::parse(tokens, &mut k)? else {
            return Ok(None);
        };
        let Some(block_dim_x) = TokenNumber::parse(tokens, &mut k)? else {
            return Ok(None);
        };
        let Some(block_dim_y) = TokenNumber::parse(tokens, &mut k)? else {
            return Ok(None);
        };
        let Some(block_dim_z) = TokenNumber::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let statement = StatementCallPtx {
            keyword_call_ptx,
            function_name,
            arg,
            grid_dim_x,
            grid_dim_y,
            grid_dim_z,
            block_dim_x,
            block_dim_y,
            block_dim_z,
            ext: (),
        };
        *i = k;
        Ok(Some(statement))
    }
}
