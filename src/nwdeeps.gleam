import gleam/bit_array
import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/regexp
import gleam/result
import gleam/string
import nwdeeps/error
import nwdeeps/meter
import nwdeeps/parse
import nwdeeps/tail

fn print_dps(subj: Subject(meter.Event)) {
  meter.print_dps() |> process.send(subj, _)
  process.sleep(1000)
  print_dps(subj)
}

pub fn main() {
  io.println("==nwdeeps==")
  let log_file =
    "/home/bgw/.var/app/com.valvesoftware.Steam/.local/share/Steam/steamapps/compatdata/704450/pfx/drive_c/users/steamuser/Documents/Neverwinter Nights/logs/nwclientLog1.txt"
  //let log_file = "nwclientLog1.txt"

  let assert Ok(subj) = meter.start()
  let assert Ok(Nil) = tail.start(subj, log_file)
  process.start(print_dps(subj), True)
  process.sleep_forever()
}
