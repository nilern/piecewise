#[derive(Debug, Trace, Finalize)]
pub enum Value {
    Int(isize),
    Float(f64),
    Char(char),
    Bool(bool)
}
