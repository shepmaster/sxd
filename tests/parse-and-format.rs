#![deny(rust_2018_idioms)]

use formatter::Formatter;
use pull_parser::Parser;
use token::Source;

type BoxError = Box<dyn std::error::Error>;
type Result<T = (), E = BoxError> = std::result::Result<T, E>;

#[test]
fn round_trips_xml() -> Result {
    let input = r#"
<?xml version="1.0"?>
<a name="a">
  <b1 name="b1" xmlns:ns1="1" xmlns:ns2="2"/>
  <!--hello-->
  <b3 name="b3"/>
  <?hello world?>
</a>
"#
    .trim();

    let mut parser = Parser::new(input.as_bytes());
    let mut output = Vec::new();
    let mut fmt = Formatter::new(&mut output);

    while let Some(token) = parser.next_str() {
        let token = token?;
        fmt.write_token(token)?;
    }

    let output = String::from_utf8(output)?;

    assert_eq!(input, output);

    Ok(())
}
