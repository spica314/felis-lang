use crate::{Keyword, Parser};

pub(crate) fn parse_visibility(parser: &mut Parser) -> crate::syntax::Visibility {
    if parser.consume_keyword(Keyword::Pub) {
        crate::syntax::Visibility::Public
    } else {
        crate::syntax::Visibility::Private
    }
}
