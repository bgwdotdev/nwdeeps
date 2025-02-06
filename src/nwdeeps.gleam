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

fn one_of(regexs: List(parse.Parse), line: String) -> Option(Event) {
  case regexs {
    [] -> None
    [x, ..xs] ->
      case regexp.scan(x.regexp, line) {
        [match] -> {
          let assert Ok(m) = match_to_event(x.event, match)
          Some(m)
        }
        _ -> one_of(xs, line)
      }
  }
}

fn match_to_event(
  parse: parse.Event,
  match: regexp.Match,
) -> Result(Event, error.Error) {
  case parse {
    parse.Attack ->
      case match.submatches {
        [
          Some(_date),
          Some(time),
          Some(source),
          Some(target),
          Some(hit),
          Some(roll),
        ] ->
          hit
          |> hit_to_bool
          |> result.map(fn(hit) {
            DoAttack(time:, source:, target:, hit:, roll:)
          })
        x -> Error(error.RegexpScanToEvent(x))
      }
    parse.Damage ->
      case match.submatches {
        [
          Some(_date),
          Some(time),
          Some(source),
          Some(target),
          Some(value),
          Some(roll),
        ] ->
          value
          |> int.parse
          |> result.map(fn(value) {
            DoDamage(time:, source:, target:, value:, roll:)
          })
          |> result.map_error(fn(_) { error.UnknownValueType(value) })
        x -> Error(error.RegexpScanToEvent(x))
      }
    parse.Initiative ->
      case match.submatches {
        [Some(_date), Some(time), Some(source), Some(value), Some(roll)] ->
          value
          |> int.parse
          |> result.map(fn(value) {
            DoInitiative(time:, source:, value:, roll:)
          })
          |> result.map_error(fn(_) { error.UnknownValueType(value) })
        x -> Error(error.RegexpScanToEvent(x))
      }
    _ -> todo as "event not implemented yet"
  }
}

fn hit_to_bool(hit: String) -> Result(Bool, error.Error) {
  case hit {
    "hit" -> Ok(True)
    "critical hit" -> Ok(True)
    "miss" -> Ok(False)
    x -> Error(error.UnknownHitType(x))
  }
}

//
// Event
//

pub type Event {
  Event
  DoAttack(
    time: String,
    source: String,
    target: String,
    hit: Bool,
    roll: String,
  )
  DoDamage(
    time: String,
    source: String,
    target: String,
    value: Int,
    roll: String,
  )
  DoInitiative(time: String, source: String, value: Int, roll: String)
}
