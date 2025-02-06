import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import nwdeeps/error
import nwdeeps/log

pub opaque type Event {
  Log(log.Log)
  Dps
}

pub fn new_log(event: log.Log) -> Event {
  Log(event)
}

pub fn print_dps() -> Event {
  Dps
}

pub opaque type State {
  State(current: List(log.Log), previous: Dict(String, List(log.Log)))
}

pub fn start() {
  let state = State(current: [], previous: dict.new())
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
          )
        }
        x -> State(..state, current: [x, ..state.current])
      }
      actor.continue(state)
    }
    Dps -> {
      io.println("")
      //clear_terminal()
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
      |> list.sort(fn(a, b) { int.compare(a.1, b.1) })
      |> list.reverse
      |> list.each(fn(i) { io.println(i.0 <> ": " <> int.to_string(i.1)) })
      actor.continue(state)
    }
  }
}

@external(erlang, "nwdeeps_ffi", "clear_terminal")
fn clear_terminal() -> Nil
