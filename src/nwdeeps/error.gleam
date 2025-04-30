import gleam/option.{type Option}
import gleam/otp/actor
import gleam/string

pub type FileReason

pub type Error {
  LogPath(Nil)
  Actor(error: actor.StartError, actor: String)
  RegexpScanToEvent(event: String, line: String, parsed: List(Option(String)))
  UnknownHitType(String)
  UnknownValueType(String)
  File(FileReason)
  LogDirEmpty(directory: String)
  Header(error: String)
}

pub fn to_string(error: Error) {
  case error {
    LogPath(Nil) ->
      "set env var $NWDEEPS_PATH to the log folder for neverwinter nights"

    Actor(error, actor) ->
      "failed to start: " <> actor <> " reason: " <> string.inspect(error)
    _ -> todo
  }
}
