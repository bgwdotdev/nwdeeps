import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string
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
    xp_start: Timestamp,
    xp_total: Int,
  )
}

pub fn start() {
  let state =
    State(
      current: [],
      previous: dict.new(),
      round_start: timestamp.system_time(),
      last_log: timestamp.system_time(),
      xp_start: timestamp.system_time(),
      xp_total: 0,
    )
  actor.start(state, loop)
}

fn loop(msg: Event, state: State) -> actor.Next(Event, State) {
  case msg {
    Log(log) -> {
      let state = case log {
        //log.Initiative(time, _, _, _) -> {
        //  io.println("==new fight==")
        //  State(
        //    current: [],
        //    previous: dict.insert(state.previous, time, state.current),
        //    round_start: timestamp.system_time(),
        //    last_log: timestamp.system_time(),
        //  )
        //}
        log.Reset ->
          State(..state, xp_total: 0, xp_start: timestamp.system_time())
        log.Experience(_, xp) -> State(..state, xp_total: state.xp_total + xp)
        x -> {
          let current_time = timestamp.system_time()
          let diff =
            current_time
            |> timestamp.difference(state.last_log, _)
            |> duration.to_seconds
            |> float.round
          case diff {
            diff if diff > 12 ->
              State(
                ..state,
                current: [],
                previous: dict.insert(
                  state.previous,
                  timestamp.to_rfc3339(current_time, duration.seconds(0)),
                  state.current,
                ),
                round_start: timestamp.system_time(),
                last_log: timestamp.system_time(),
              )

            _ ->
              State(
                ..state,
                current: [x, ..state.current],
                last_log: timestamp.system_time(),
              )
          }
        }
      }
      actor.continue(state)
    }
    PrintDps -> {
      // dps
      let meters =
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
        |> list.sort(fn(a, b) { int.compare(a.damage, b.damage) })
        |> list.reverse

      let top =
        meters
        |> list.first
        |> result.unwrap(Dps("", 0, 0, 0))

      // time since last message
      let diff =
        timestamp.system_time()
        |> timestamp.difference(state.last_log, _)
        |> duration.to_seconds
        |> float.round

      // XP

      let xph = {
        let session =
          timestamp.difference(state.xp_start, timestamp.system_time())
          |> duration.to_seconds
        //|> float.round
        let xph = int.to_float(state.xp_total) /. session *. 3600.0
        " XP/h: " <> int.to_string(float.round(xph)) <> "\n\n"
      }

      let title = " DPS | Last Update: " <> int.to_string(diff) <> "\n\n"
      let meters_print =
        meters
        |> list.index_map(fn(dps, _idx) {
          let done =
            int.to_float(dps.damage) /. int.to_float(top.damage) *. 10.0
          let spaces = 10.0 -. done

          "["
          <> string.repeat("=", float.round(done *. 2.0))
          <> string.repeat(" ", float.round(spaces *. 2.0))
          <> "] "
          <> string.repeat(
            " ",
            { top.dpr |> int.to_string |> string.length }
              - { dps.dpr |> int.to_string |> string.length },
          )
          <> int.to_string(dps.dpr)
          <> " : "
          <> string.repeat(
            " ",
            { top.damage |> int.to_string |> string.length }
              - { dps.damage |> int.to_string |> string.length },
          )
          <> int.to_string(dps.damage)
          <> " : "
          <> dps.source
          <> "\n"
        })
        |> string.concat

      clear_terminal()
      io.print(title <> xph <> meters_print)
      //let _ = meters |> list.index_map(to_svg) |> print_svg |> io.debug

      timestamp.difference(state.round_start, state.last_log)
      |> duration.to_seconds
      actor.continue(state)
    }
  }
}

type Dps {
  Dps(source: String, dps: Int, dpr: Int, damage: Int)
}

fn to_dps(i: #(String, Int), state: State) -> Dps {
  let fight_duration =
    timestamp.difference(state.round_start, state.last_log)
    |> duration.to_seconds
  let dps = i.1 / float.truncate(fight_duration)
  let dpr = dps * 6
  Dps(source: i.0, dps:, dpr:, damage: i.1)
}

fn print_svg(svg: List(String)) -> String {
  list.flatten([
    ["<svg id='dps-chart' width='500' height='200'>"],
    svg,
    ["</svg>"],
  ])
  |> string.concat
}

fn to_svg(dps: Dps, idx: Int) -> String {
  let i = idx * 10
  "<g class='bar'>"
  <> {
    "<rect x='0' y='"
    <> int.to_string(i)
    <> "' width='"
    <> int.to_string(dps.damage)
    <> "' height='10' fill='"
    <> name_to_color(dps.source)
    <> "'></rect>"
  }
  <> {
    "  <text x='0' y='"
    <> { i + 10 |> int.to_string() }
    <> "'>"
    <> dps.source
    <> "</text>"
  }
  <> "</g>"
}

fn hash_string(name: String) -> Int {
  name
  |> string.to_graphemes
  |> list.fold(0, fn(hash, char) {
    hash |> int.multiply(31) |> int.add(to_ascii_int(char))
  })
}

fn name_to_color(name: String) -> String {
  let hue = name |> hash_string |> int.absolute_value |> io.debug
  "hsl(" <> int.to_string(hue % 360) <> ", 70%, 50%)"
}

@external(erlang, "nwdeeps_ffi", "to_ascii_int")
fn to_ascii_int(char: String) -> Int

@external(erlang, "nwdeeps_ffi", "clear_terminal")
fn clear_terminal() -> Nil

@external(erlang, "nwdeeps_ffi", "top_terminal")
fn top_terminal() -> Nil
