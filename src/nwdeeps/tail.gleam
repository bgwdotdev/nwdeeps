import gleam/erlang/process.{type Subject}
import gleam/option
import nwdeeps/error
import nwdeeps/log
import nwdeeps/meter

pub fn start(
  subj: Subject(meter.Event),
  path: String,
) -> Result(Nil, error.Error) {
  let assert Ok(file) = open(path)
  fn() { loop(file, subj) } |> process.start(True)
  Ok(Nil)
}

fn loop(file: File, subj: Subject(meter.Event)) {
  case file |> get_line {
    Ok(Data(line)) -> {
      line
      |> log.parse
      |> option.map(meter.new_log)
      |> option.map(process.send(subj, _))
      loop(file, subj)
    }
    Ok(Eof) -> {
      process.sleep(16)
      loop(file, subj)
    }
    Error(_) -> panic as "file read errored"
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

@external(erlang, "nwdeeps_ffi", "get_line")
fn get_line(file: File) -> Result(Line, Nil)
