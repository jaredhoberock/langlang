#[derive(Debug, Clone, PartialEq)]
pub struct SourceLocation {
    pub line: i32,
    pub column: i32,
}

impl SourceLocation {
    pub fn new() -> Self {
        SourceLocation {
            line: 1,
            column: 1,
        }
    }

    pub fn advance_line(&mut self) {
        self.line += 1;
        self.column = 1;
    }

    pub fn advance_column(&mut self) {
        self.column += 1;
    }
}

impl std::fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
      write!(f, "Line: {}, Column: {}", self.line, self.column)
    }
}
