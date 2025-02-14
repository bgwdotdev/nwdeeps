import gleam/option.{type Option}

pub type Error {
  RegexpScanToEvent(event: String, line: String, parsed: List(Option(String)))
  UnknownHitType(String)
  UnknownValueType(String)
}
