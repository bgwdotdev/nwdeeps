import envoy
import gleam/erlang/process.{type Subject}
import gleam/io
import nwdeeps/meter
import nwdeeps/tail

fn print_dps(subj: Subject(meter.Event)) {
  meter.print_dps() |> process.send(subj, _)
  process.sleep(1000)
  print_dps(subj)
}

pub fn main() {
  io.println("==nwdeeps==")
  let assert Ok(log_file) = envoy.get("NWDEEPS_PATH")
    as "set env var $NWDEEPS_PATH to the log folder for neverwinter nights"

  let assert Ok(subj) = meter.start()
  let assert Ok(Nil) = tail.start(subj, log_file)
  print_dps(subj) |> process.start(True)
  process.sleep_forever()
}
