import gleam/option.{type Option}

pub type FileReason

pub type Error {
  RegexpScanToEvent(event: String, line: String, parsed: List(Option(String)))
  UnknownHitType(String)
  UnknownValueType(String)
  File(FileReason)
  LogDirEmpty(directory: String)
}
