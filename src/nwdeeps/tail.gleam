import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import nwdeeps/error
import nwdeeps/log
import nwdeeps/meter

type State {
  State(
    subj: Subject(meter.Event),
    folder: String,
    file_name: String,
    file: File,
    last_update: Int,
  )
}

type Log {
  Log(full_path: String, mtime: Int)
}

pub fn start(
  subj: Subject(meter.Event),
  log_folder: String,
) -> Result(Nil, error.Error) {
  use log <- result.try(log_folder |> newest_log)
  use file <- result.try(open(log.full_path))

  let state =
    State(
      subj:,
      folder: log_folder,
      file_name: log.full_path,
      file:,
      last_update: 0,
    )
  fn() { loop(state) } |> process.start(True)
  Ok(Nil)
}

fn newest_log(folder: String) -> Result(Log, error.Error) {
  folder
  |> read_logs_folder
  |> result.map(list.sort(_, fn(a, b) { int.compare(b.mtime, a.mtime) }))
  |> result.then(fn(logs) {
    case logs {
      [log, ..] -> Ok(log)
      [] -> Error(error.LogDirEmpty(directory: folder))
    }
  })
}

// TODO: refactor panics into helpful errors handled by meter actor?
fn loop(state: State) {
  case state.file |> get_line {
    Ok(Data(line)) -> {
      line
      |> log.parse
      |> option.map(meter.new_log)
      |> option.map(process.send(state.subj, _))
      State(..state, last_update: 0)
      |> loop
    }
    Ok(Eof) -> {
      case state.last_update > 60 * 5 {
        True -> {
          let assert Ok(log) = newest_log(state.folder) as "failed to find log"
          case state.file_name == log.full_path {
            True -> loop(State(..state, last_update: 0))
            False -> {
              let assert Ok(Nil) = close(state.file)
                as "failed to close log file"
              let assert Ok(file) = open(log.full_path)
                as "failed to open log file"
              State(..state, file_name: log.full_path, file:, last_update: 0)
              |> loop
            }
          }
          loop(state)
        }
        False -> {
          process.sleep(16)
          State(..state, last_update: state.last_update + 1)
          |> loop
        }
      }
    }
    Error(_) -> panic as "failed to read file"
  }
}

fn read_logs_folder(path: String) -> Result(List(Log), error.Error) {
  use files <- result.try(path |> list_dir)
  files
  |> list.filter(is_client_log)
  |> list.map(file_to_log(_, path))
  |> result.all
}

fn is_client_log(file: String) -> Bool {
  case file {
    "nwclientLog" <> _ -> True
    _ -> False
  }
}

fn file_to_log(file: String, folder: String) -> Result(Log, error.Error) {
  case read_file_mtime(string.join([folder, file], "/")) {
    Ok(mtime) -> Ok(Log(full_path: folder <> file, mtime:))
    Error(e) -> Error(e)
  }
}

//
// IO
//

type File

type Line {
  Data(String)
  Eof
}

@external(erlang, "nwdeeps_ffi", "open")
fn open(path: String) -> Result(File, error.Error)

@external(erlang, "nwdeeps_ffi", "close")
fn close(file: File) -> Result(Nil, error.Error)

@external(erlang, "nwdeeps_ffi", "list_dir")
fn list_dir(path: String) -> Result(List(String), error.Error)

@external(erlang, "nwdeeps_ffi", "read_file_mtime")
fn read_file_mtime(path: String) -> Result(Int, error.Error)

@external(erlang, "nwdeeps_ffi", "get_line")
fn get_line(file: File) -> Result(Line, Nil)
