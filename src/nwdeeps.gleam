import envoy
import gleam/erlang/process.{type Subject}
import gleam/io
import gleam/result
import nwdeeps/error
import nwdeeps/meter
import nwdeeps/tail
import shore

fn print_dps(subj: Subject(meter.Event)) {
  meter.print_dps() |> process.send(subj, _)
  process.sleep(1000)
  print_dps(subj)
}

pub fn main() {
  case run() {
    Ok(Nil) -> Nil
    Error(e) -> {
      error.to_string(e)
      Nil
    }
  }
}

fn run() -> Result(Nil, error.Error) {
  use logs <- result.try(
    envoy.get("NWDEEPS_PATH") |> result.map_error(error.LogPath),
  )
  let exit = process.new_subject()

  use meter <- result.try(
    shore.Spec(
      init: meter.init,
      update: meter.update,
      view: meter.view,
      keybinds: shore.default_keybinds(),
      exit:,
    )
    |> shore.start
    |> result.map_error(error.Actor(_, "meter")),
  )
  use _ <- result.map(tail.start(meter, logs))
  //meter |> print_dps |> process.start(True)
  exit |> process.receive_forever()
}
