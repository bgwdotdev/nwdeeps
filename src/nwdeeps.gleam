import envoy
import gleam/erlang/process
import gleam/io
import gleam/result
import nwdeeps/error
import nwdeeps/meter
import nwdeeps/tail
import shore

pub fn main() {
  case run() {
    Ok(Nil) -> Nil
    Error(e) -> {
      error.to_string(e) |> io.println
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
    shore.spec(
      init: meter.init,
      update: meter.update,
      view: meter.view,
      keybinds: shore.default_keybinds(),
      exit:,
      redraw: shore.on_timer(33),
    )
    |> shore.start
    |> result.map_error(error.Actor(_, "meter")),
  )
  use _ <- result.map(tail.start(meter, logs))
  exit |> process.receive_forever()
}
