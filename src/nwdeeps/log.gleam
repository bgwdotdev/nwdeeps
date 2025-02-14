import gleam/int
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/regexp
import gleam/result
import gleam/string
import nwdeeps/error
import nwdeeps/parse

pub type Log {
  Attack(time: String, source: String, target: String, hit: Bool, roll: String)
  Damage(time: String, source: String, target: String, value: Int, roll: String)
  Initiative(time: String, source: String, value: Int, roll: String)
  Experience(time: String, value: Int)
  Reset
}

pub fn parse(line: String) -> Option(Log) {
  [
    parse.initiative(),
    parse.attack(),
    parse.damage(),
    parse.experience(),
    parse.reset(),
  ]
  |> one_of(line)
}

fn one_of(regexs: List(parse.Parse), line: String) -> Option(Log) {
  case regexs {
    [] -> None
    [x, ..xs] ->
      case regexp.scan(x.regexp, line) {
        [match] -> {
          case match_to_event(x.event, match) {
            Ok(m) -> Some(m)
            Error(e) -> {
              io.debug(e)
              one_of([], line)
            }
          }
        }
        _ -> one_of(xs, line)
      }
  }
}

fn match_to_event(
  parse: parse.Event,
  match: regexp.Match,
) -> Result(Log, error.Error) {
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
          |> result.map(fn(hit) { Attack(time:, source:, target:, hit:, roll:) })
        x ->
          Error(error.RegexpScanToEvent(
            event: string.inspect(parse),
            line: match.content,
            parsed: x,
          ))
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
            Damage(time:, source:, target:, value:, roll:)
          })
          |> result.map_error(fn(_) { error.UnknownValueType(value) })
        x ->
          Error(error.RegexpScanToEvent(
            event: string.inspect(parse),
            line: match.content,
            parsed: x,
          ))
      }
    parse.Initiative ->
      case match.submatches {
        [Some(_date), Some(time), Some(source), Some(value), Some(roll)] ->
          value
          |> int.parse
          |> result.map(fn(value) { Initiative(time:, source:, value:, roll:) })
          |> result.map_error(fn(_) { error.UnknownValueType(value) })
        x ->
          Error(error.RegexpScanToEvent(
            event: string.inspect(parse),
            line: match.content,
            parsed: x,
          ))
      }
    parse.Experience -> {
      case match.submatches {
        [Some(_date), Some(time), Some(value)] ->
          value
          |> int.parse
          |> result.map(fn(value) { Experience(time:, value:) })
          |> result.map_error(fn(_) { error.UnknownValueType(value) })
        x ->
          Error(error.RegexpScanToEvent(
            event: string.inspect(parse),
            line: match.content,
            parsed: x,
          ))
      }
    }
    parse.Reset -> Ok(Reset)
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
