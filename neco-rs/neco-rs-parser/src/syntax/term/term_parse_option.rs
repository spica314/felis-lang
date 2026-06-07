#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct TermParseOption {
    pub stop_at_left_brace: bool,
    pub stop_at_match_arm_boundary: bool,
}
