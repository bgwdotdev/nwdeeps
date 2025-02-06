import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/time/duration
import gleam/time/timestamp.{type Timestamp}
import nwdeeps/error
import nwdeeps/log

pub opaque type Event {
  Log(log.Log)
  PrintDps
}

pub fn new_log(event: log.Log) -> Event {
  Log(event)
}

pub fn print_dps() -> Event {
  PrintDps
}

pub opaque type State {
  State(
    current: List(log.Log),
    previous: Dict(String, List(log.Log)),
    round_start: Timestamp,
    last_log: Timestamp,
  )
}

pub fn start() {
  let state =
    State(
      current: [],
      previous: dict.new(),
      round_start: timestamp.system_time(),
      last_log: timestamp.system_time(),
    )
  actor.start(state, loop)
}

fn loop(msg: Event, state: State) -> actor.Next(Event, State) {
  case msg {
    Log(log) -> {
      let state = case log {
        log.Initiative(time, _, _, _) -> {
          io.println("==new fight==")
          State(
            current: [],
            previous: dict.insert(state.previous, time, state.current),
            round_start: timestamp.system_time(),
            last_log: timestamp.system_time(),
          )
        }
        x ->
          State(
            ..state,
            current: [x, ..state.current],
            last_log: timestamp.system_time(),
          )
      }
      actor.continue(state)
    }
    PrintDps -> {
      io.println("")
      clear_terminal()
      io.println("==dps==")
      fn(dps: dict.Dict(String, Int), log: log.Log) {
        case log {
          log.Damage(_, source, _, value, _) ->
            dict.upsert(dps, source, fn(v) {
              case v {
                Some(v) -> v + value
                None -> value
              }
            })
          _ -> dps
        }
      }
      |> list.fold(state.current, dict.new(), _)
      |> dict.to_list
      |> list.map(to_dps(_, state))
      |> list.sort(fn(a, b) { int.compare(a.dps, b.dps) })
      |> list.reverse
      |> list.index_map(fn(dps, idx) {
        io.println(
          int.to_string(idx)
          <> ". "
          <> dps.source
          <> ": "
          <> int.to_string(dps.dps)
          <> " ("
          <> int.to_string(dps.damage)
          <> ")",
        )
      })

      timestamp.difference(state.round_start, state.last_log)
      |> duration.to_seconds
      actor.continue(state)
    }
  }
}

type Dps {
  Dps(source: String, dps: Int, damage: Int)
}

fn to_dps(i: #(String, Int), state: State) -> Dps {
  let round_duration =
    timestamp.difference(state.round_start, state.last_log)
    |> duration.to_seconds
  let dps = i.1 / float.truncate(round_duration)
  Dps(source: i.0, dps:, damage: i.1)
}

@external(erlang, "nwdeeps_ffi", "clear_terminal")
fn clear_terminal() -> Nil
