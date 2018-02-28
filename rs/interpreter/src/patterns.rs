use pcws_syntax::cst::Program;

use binding::BindingsReified;

// ================================================================================================

pub enum PatternsExpanded {}

pub trait PatternMatchingPass {
    type Target;

    fn expand_patterns(self) -> Self::Target;
}

impl PatternMatchingPass for Program<BindingsReified> {
    type Target = Program<PatternsExpanded>;

    fn expand_patterns(self) -> Program<PatternsExpanded> {
        unimplemented!()
    }
}

// ================================================================================================
