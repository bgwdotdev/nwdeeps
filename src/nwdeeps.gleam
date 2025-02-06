import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/regexp
import gleam/result
import gleam/string
import nwdeeps/error
import nwdeeps/parse

pub fn main() {
  io.println("==nwdeeps==")
  let file = open("nwclientLog1.txt")
  let assert Ok(f) = file

  let regexs = [parse.initiative(), parse.attack(), parse.damage()]

  let events =
    fn(_) { f |> get_line |> one_of(regexs, _) }
    |> list.map(list.range(0, 200), _)
    |> option.values
    |> io.debug

  let dps: dict.Dict(String, Int) = dict.new()

  let dps =
    fn(dps: dict.Dict(String, Int), event: Event) {
      case event {
        DoDamage(_, source, _, value, _) ->
          fn(v) {
            case v {
              Some(v) -> v + value
              None -> value
            }
          }
          |> dict.upsert(dps, source, _)
        _ -> dps
      }
    }
    |> list.fold(events, dps, _)

  dps
  |> dict.to_list
  |> list.sort(fn(a, b) { int.compare(a.1, b.1) })
  |> list.reverse
  |> list.each(fn(i) { io.println(i.0 <> ": " <> int.to_string(i.1)) })
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

pub type File {
  File
}

@external(erlang, "nwdeeps_ffi", "open")
fn open(path: String) -> Result(File, error.Error)

@external(erlang, "nwdeeps_ffi", "get_line")
fn get_line(file: File) -> String

fn match_to_event(
  parse: parse.ParseEvent,
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
