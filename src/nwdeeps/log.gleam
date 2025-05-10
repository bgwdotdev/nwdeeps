import gleam/int
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/regexp
import gleam/result
import gleam/string
import gleam/time/timestamp.{type Timestamp}
import nwdeeps/error
import nwdeeps/parse

pub type Log {
  Attack(
    time: Timestamp,
    source: String,
    target: String,
    hit: Bool,
    roll: String,
  )
  Damage(
    time: Timestamp,
    source: String,
    target: String,
    value: Int,
    roll: String,
  )
  Initiative(time: Timestamp, source: String, value: Int, roll: String)
  Experience(time: Timestamp, value: Int)
  Reset
  ResetCd
  ActiveCd(time: Timestamp, active: String, seconds: Int)
  DoneResting
  Charge(time: Timestamp, active: String)
  Loading
}

pub fn parse(line: String) -> Option(Log) {
  [
    parse.initiative(),
    parse.attack(),
    parse.damage(),
    parse.experience(),
    parse.reset(),
    parse.reset_cd(),
    parse.active_cd(),
    parse.done_resting(),
    parse.charge(),
    parse.loading(),
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
        [Some(time), Some(source), Some(target), Some(hit), Some(roll)] -> {
          use time <- result.try(parse.header(time))
          use hit <- result.try(hit_to_bool(hit))
          Attack(time:, source:, target:, hit:, roll:) |> Ok
        }
        x ->
          Error(error.RegexpScanToEvent(
            event: string.inspect(parse),
            line: match.content,
            parsed: x,
          ))
      }
    parse.Damage ->
      case match.submatches {
        [Some(time), Some(source), Some(target), Some(value), Some(roll)] -> {
          use time <- result.try(parse.header(time))
          use value <- result.try(
            value
            |> int.parse
            |> result.map_error(fn(_) { error.UnknownValueType(value) }),
          )
          Damage(time:, source:, target:, value:, roll:) |> Ok
        }
        x ->
          Error(error.RegexpScanToEvent(
            event: string.inspect(parse),
            line: match.content,
            parsed: x,
          ))
      }
    parse.Initiative ->
      case match.submatches {
        [Some(time), Some(source), Some(value), Some(roll)] -> {
          use time <- result.try(parse.header(time))
          use value <- result.try(
            value
            |> int.parse
            |> result.map_error(fn(_) { error.UnknownValueType(value) }),
          )
          Initiative(time:, source:, value:, roll:) |> Ok
        }
        x ->
          Error(error.RegexpScanToEvent(
            event: string.inspect(parse),
            line: match.content,
            parsed: x,
          ))
      }
    parse.Experience -> {
      case match.submatches {
        [Some(time), Some(value)] -> {
          use time <- result.try(parse.header(time))
          use value <- result.try(
            value
            |> int.parse
            |> result.map_error(fn(_) { error.UnknownValueType(value) }),
          )
          Experience(time:, value:) |> Ok
        }
        x ->
          Error(error.RegexpScanToEvent(
            event: string.inspect(parse),
            line: match.content,
            parsed: x,
          ))
      }
    }
    parse.ActiveCd -> {
      case match.submatches {
        [Some(time), Some(active), Some(minutes), Some(seconds)] -> {
          {
            use time <- result.try(parse.header(time))
            use minutes <- result.try(
              minutes
              |> int.parse
              |> result.map_error(fn(_) { error.UnknownValueType(minutes) }),
            )
            use seconds <- result.map(
              seconds
              |> int.parse
              |> result.map_error(fn(_) { error.UnknownValueType(seconds) }),
            )
            ActiveCd(time:, active:, seconds: minutes * 60 + seconds)
          }
          |> result.map_error(fn(_) {
            error.UnknownValueType(minutes <> seconds)
          })
        }
        [Some(time), Some(active), Some(minutes)] -> {
          use time <- result.try(parse.header(time))
          use minutes <- result.try(
            minutes
            |> int.parse
            |> result.map_error(fn(_) { error.UnknownValueType(minutes) }),
          )
          ActiveCd(time:, active:, seconds: minutes * 60) |> Ok
        }
        [Some(time), Some(active), None, Some(seconds)] -> {
          use time <- result.try(parse.header(time))
          use seconds <- result.try(
            seconds
            |> int.parse
            |> result.map_error(fn(_) { error.UnknownValueType(seconds) }),
          )
          ActiveCd(time:, active:, seconds:) |> Ok
        }
        x ->
          Error(error.RegexpScanToEvent(
            event: string.inspect(parse),
            line: match.content,
            parsed: x,
          ))
      }
    }
    parse.Reset -> Ok(Reset)
    parse.ResetCd -> Ok(ResetCd)
    parse.DoneResting -> Ok(DoneResting)
    parse.Charge ->
      case match.submatches {
        [Some(time), Some(active)] -> {
          use time <- result.try(parse.header(time))
          Charge(time:, active:) |> Ok
        }
        x ->
          Error(error.RegexpScanToEvent(
            event: string.inspect(parse),
            line: match.content,
            parsed: x,
          ))
      }

    parse.Loading ->
      case match.submatches {
        [Some(_time)] -> Ok(Loading)
        x ->
          Error(error.RegexpScanToEvent(
            event: string.inspect(parse),
            line: match.content,
            parsed: x,
          ))
      }

    e ->
      Error(error.RegexpNotImplemented(
        "event not implemented yet: " <> string.inspect(e),
      ))
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
