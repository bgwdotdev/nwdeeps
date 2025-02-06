import gleam/option.{type Option}

pub type Error {
  RegexpScanToEvent(List(Option(String)))
  UnknownHitType(String)
  UnknownValueType(String)
}
