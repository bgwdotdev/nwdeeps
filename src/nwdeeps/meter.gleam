import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/order
import gleam/result
import gleam/string
import gleam/time/duration
import gleam/time/timestamp.{type Timestamp}
import nwdeeps/log
import shore
import shore/key
import shore/layout
import shore/style
import shore/ui

// MODEL 

pub opaque type State {
  State(
    page: Page,
    current: Combat,
    previous: Dict(String, Combat),
    xp_start: Timestamp,
    xp_total: Int,
    cds: Dict(String, Cd),
    last_loading: Timestamp,
    curve: Bool,
    curve_filter: String,
  )
}

type Combat {
  Combat(logs: List(log.Log), round_start: Timestamp, last_log: Timestamp)
}

type Page {
  ShowDps
  ShowHistory
  ShowLog(String, Combat)
}

type Cd {
  Cd(duration: Int, start: Timestamp)
}

pub fn init() -> #(State, List(fn() -> Event)) {
  let current =
    Combat(
      logs: [],
      round_start: timestamp.from_unix_seconds(0),
      last_log: timestamp.from_unix_seconds(0),
    )
  let state =
    State(
      page: ShowDps,
      current:,
      previous: dict.new(),
      xp_start: timestamp.system_time(),
      xp_total: 0,
      cds: dict.new(),
      last_loading: timestamp.system_time(),
      curve: False,
      curve_filter: "",
    )
  let cmd = []
  #(state, cmd)
}

// UPDATE 

pub opaque type Event {
  Log(log.Log)
  SetPage(Page)
  LoadLog(String)
  SetFilter(String)
  ToggleCurve
}

pub fn new_log(event: log.Log) -> Event {
  Log(event)
}

pub fn update(state: State, msg: Event) -> #(State, List(fn() -> Event)) {
  case msg {
    Log(log) -> {
      let state = case log {
        log.Reset ->
          State(..state, xp_total: 0, xp_start: timestamp.system_time())
        log.ResetCd -> State(..state, cds: dict.new())
        log.Experience(_, xp) -> State(..state, xp_total: state.xp_total + xp)
        log.DoneResting ->
          State(
            ..state,
            cds: dict.map_values(state.cds, fn(_, _) {
              Cd(0, timestamp.system_time())
            }),
          )
        log.ActiveCd(_, active, seconds) ->
          State(
            ..state,
            cds: dict.upsert(state.cds, active, fn(_) {
              Cd(seconds, timestamp.system_time())
            }),
          )

        log.Charge(_, active) ->
          State(
            ..state,
            cds: dict.upsert(state.cds, active, fn(v) {
              let now = timestamp.system_time()
              case v {
                Some(cd) -> {
                  Cd(
                    duration: timestamp.difference(cd.start, now)
                      |> duration.to_seconds
                      |> float.round,
                    start: now,
                  )
                }
                None -> Cd(duration: 0, start: now)
              }
            }),
          )
        log.Loading -> State(..state, last_loading: timestamp.system_time())

        log.Attack(time:, ..) as x
        | log.Damage(time:, ..) as x
        | log.Initiative(time:, ..) as x -> {
          let diff =
            time
            |> timestamp.difference(state.current.last_log, _)
            |> duration.to_seconds
            |> float.round
          case diff {
            diff if diff > 12 ->
              State(
                ..state,
                current: Combat([], time, time),
                previous: dict.insert(
                  state.previous,
                  timestamp.to_rfc3339(time, duration.seconds(0)),
                  state.current,
                ),
              )

            _ ->
              State(
                ..state,
                current: Combat(
                  ..state.current,
                  logs: [x, ..state.current.logs],
                  last_log: time,
                ),
              )
          }
        }
      }
      #(state, [])
    }
    SetPage(page) -> #(State(..state, page:), [])
    LoadLog(log_name) -> {
      case dict.get(state.previous, log_name) {
        Ok(log) -> #(State(..state, page: ShowLog(log_name, log)), [])
        Error(Nil) -> #(state, [])
      }
    }
    SetFilter(name) -> #(State(..state, curve_filter: name), [])
    ToggleCurve -> #(State(..state, curve: !state.curve), [])
  }
}

// VIEW

pub fn view(state: State) -> shore.Node(Event) {
  let time = time(state)
  layout.grid(
    gap: 0,
    rows: [style.Px(5), style.Fill, style.Px(3), style.Pct(30), style.Px(1)],
    cols: [style.Pct(66), style.Fill],
    cells: [
      layout.cell(
        content: ui.box(
          [
            ui.table_kv(style.Px(40), [
              view_update(time),
              view_xph(state, time),
              view_loading(state, time),
            ]),
          ],
          Some("stats"),
        ),
        row: #(0, 0),
        col: #(1, 1),
      ),
      layout.cell(
        content: ui.box([view_cds(state, time)], Some("cooldowns")),
        row: #(1, 3),
        col: #(1, 1),
      ),
      layout.cell(
        content: ui.bar2(style.Blue, ui.row(view_page_keybinds(state))),
        row: #(4, 4),
        col: #(0, 1),
      ),
    ]
      |> list.append(view_page(state), _),
  )
}

fn view_page(state: State) -> List(layout.Cell(Event)) {
  let height = case state.curve {
    True -> #(0, 1)
    False -> #(0, 3)
  }
  let curve = fn(log) {
    case state.curve {
      True -> [
        view_graph_filter(state)
          |> layout.cell(row: #(2, 2), col: #(0, 0)),
        view_graph(state, log)
          |> layout.cell(row: #(3, 3), col: #(0, 0)),
      ]
      False -> []
    }
  }
  case state.page {
    ShowDps -> [
      view_dps(meters(state.current))
        |> layout.cell(row: height, col: #(0, 0)),
      ..curve(state.current)
    ]
    ShowHistory -> [
      ui.box(view_history(state), Some("logs"))
      |> layout.cell(row: #(0, 3), col: #(0, 0)),
    ]

    ShowLog(log_name, log) -> [
      ui.box(view_dps_log(log_name, meters(log)), Some("DPS: " <> log_name))
        |> layout.cell(row: height, col: #(0, 0)),
      ..curve(log)
    ]
  }
}

fn view_page_keybinds(state: State) -> List(shore.Node(Event)) {
  case state.page {
    ShowDps -> view_dps_keybinds()
    ShowHistory -> view_history_keybinds()
    ShowLog(..) -> view_log_keybinds()
  }
}

// ShowDps

fn meters(combat: Combat) -> List(Dps) {
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
  |> list.fold(combat.logs, dict.new(), _)
  |> dict.to_list
  |> list.map(to_dps(_, combat.round_start, combat.last_log))
  |> list.sort(fn(a, b) { int.compare(a.damage, b.damage) })
  |> list.reverse
}

fn view_xph(state: State, time: Time) -> List(String) {
  let session =
    timestamp.difference(state.xp_start, time.now) |> duration.to_seconds
  let xph = int.to_float(state.xp_total) /. session *. 3600.0
  ["XP/h", int.to_string(float.round(xph))]
}

fn view_loading(state: State, time: Time) -> List(String) {
  let time_since =
    timestamp.difference(state.last_loading, time.now)
    |> duration.to_seconds
    |> float.round
  ["Loading", int.to_string(time_since)]
}

fn view_cds(state: State, time: Time) -> shore.Node(Event) {
  let cds =
    state.cds
    |> dict.to_list
    |> list.sort(fn(a, b) { string.compare(a.0, b.0) })
    |> list.map(fn(cd) {
      let left =
        timestamp.difference({ cd.1 }.start, time.now)
        |> duration.to_seconds
        |> float.round
        |> int.subtract({ cd.1 }.duration, _)
        |> int.max(0)

      [int.to_string(left), cd.0]
    })

  [["cooldown", "ability"], ..cds] |> ui.table(style.Px(40), _)
}

fn view_update(time: Time) -> List(String) {
  ["Last Update", int.to_string(time.diff)]
}

fn view_meters(meters: List(Dps), top: Dps) -> shore.Node(Event) {
  layout.grid(
    gap: 0,
    rows: list.repeat(style.Px(1), list.length(meters)),
    cols: [style.Fill, style.Px(3), style.Px(7), style.Px(7), style.Px(20)],
    cells: meters
      |> list.index_map(fn(dps, idx) { view_meter(idx, dps, top) })
      |> list.flatten,
  )
}

fn view_meter(row: Int, dps: Dps, top: Dps) -> List(layout.Cell(Event)) {
  // right align numbers
  let align = fn(a, b) {
    { a |> int.to_string |> string.length }
    - { b |> int.to_string |> string.length }
    |> string.repeat(" ", _)
  }
  let dpr_right = align(top.dpr, dps.dpr)
  let damage_right = align(top.damage, dps.damage)
  [
    layout.cell(
      row: #(row, row),
      col: #(0, 0),
      content: ui.progress(style.Fill, top.damage, dps.damage, style.Blue),
    ),
    layout.cell(
      row: #(row, row),
      col: #(2, 2),
      content: ui.text(dpr_right <> int.to_string(dps.dpr)),
    ),
    layout.cell(
      row: #(row, row),
      col: #(3, 3),
      content: ui.text(damage_right <> int.to_string(dps.damage)),
    ),
    layout.cell(row: #(row, row), col: #(4, 4), content: ui.text(dps.source)),
  ]
}

fn view_dps(meters: List(Dps)) -> shore.Node(Event) {
  let top = top_dps(meters)
  ui.box([view_meters(meters, top)], Some("DPS"))
}

fn view_dps_log(title: String, meters: List(Dps)) -> List(shore.Node(Event)) {
  let top = top_dps(meters)
  [ui.text_styled(title, Some(style.Blue), None), view_meters(meters, top)]
}

fn view_dps_keybinds() -> List(shore.Node(Event)) {
  [
    ui.button("l: logs", key.Char("l"), SetPage(ShowHistory)),
    ui.button("r: reset xp", key.Char("r"), Log(log.Reset)),
    ui.button("c: reset cd", key.Char("c"), Log(log.ResetCd)),
    ui.button("d: toggle dps curve", key.Char("d"), ToggleCurve),
  ]
}

fn view_log_keybinds() -> List(shore.Node(Event)) {
  [
    ui.button("l: logs", key.Char("l"), SetPage(ShowHistory)),
    ui.button("d: toggle dps curve", key.Char("d"), ToggleCurve),
  ]
}

fn view_history_keybinds() -> List(shore.Node(Event)) {
  [
    ui.button("l: dps", key.Char("l"), SetPage(ShowDps)),
    ui.text_styled("  0-9: log", Some(style.Black), Some(style.Blue)),
  ]
}

// ShowHistory

fn view_history(state: State) -> List(shore.Node(Event)) {
  [
    dict.to_list(state.previous)
      |> list.sort(fn(a, b) { string.compare(b.0, a.0) })
      |> list.index_map(fn(x, idx) { [int.to_string(idx), x.0] })
      |> list.prepend(["log", "time"])
      |> ui.table(style.Px(50), _),
    dict.to_list(state.previous)
      |> list.sort(fn(a, b) { string.compare(b.0, a.0) })
      |> list.take(10)
      |> list.index_map(fn(x, idx) {
        ui.keybind(key.Char(int.to_string(idx)), LoadLog(x.0))
      })
      |> ui.col,
  ]
}

// HELPERS 

type Dps {
  Dps(source: String, dps: Int, dpr: Int, damage: Int)
}

fn to_dps(
  i: #(String, Int),
  round_start: Timestamp,
  round_end: Timestamp,
) -> Dps {
  let fight_duration =
    timestamp.difference(round_start, round_end)
    |> duration.to_seconds
  let dps = i.1 / float.truncate(fight_duration)
  let dpr = dps * 6
  Dps(source: i.0, dps:, dpr:, damage: i.1)
}

fn top_dps(meters: List(Dps)) -> Dps {
  meters
  |> list.first
  |> result.unwrap(Dps("", 0, 0, 0))
}

fn time(state: State) -> Time {
  let now = timestamp.system_time()
  let diff =
    now
    |> timestamp.difference(state.current.last_log, _)
    |> duration.to_seconds
    |> float.round
  Time(now:, diff:)
}

type Time {
  Time(now: Timestamp, diff: Int)
}

// DPS GRAPH

type Sample {
  Sample(time: Timestamp, dps: List(Float), current: List(Int))
}

fn graph(combat: Combat, filter: String) -> List(Float) {
  let sample = Sample(timestamp.from_unix_seconds(0), [], [])
  let sample =
    list.fold(combat.logs, sample, fn(acc, log) {
      case log {
        log.Damage(time:, value:, ..) as l if l.source == filter -> {
          let diff =
            acc.time
            |> timestamp.difference(time)
            |> duration.compare(duration.seconds(6))
          case diff {
            order.Gt -> {
              let total = list.length(acc.current) |> int.to_float
              let dps =
                acc.current
                |> list.map(int.to_float)
                |> float.sum
              let avg = dps /. total
              Sample(time, [avg, ..acc.dps], [value])
            }
            _ -> {
              Sample(acc.time, acc.dps, [value, ..acc.current])
            }
          }
        }
        _ -> acc
      }
    })
  sample.dps |> list.reverse
}

fn view_graph(state: State, combat: Combat) -> shore.Node(Event) {
  let g =
    combat |> graph(state.curve_filter) |> ui.graph(style.Fill, style.Fill, _)
  ui.box([g], Some("curve"))
}

fn view_graph_filter(state: State) -> shore.Node(Event) {
  [ui.input(">", state.curve_filter, style.Fill, SetFilter)]
  |> ui.box(Some("name"))
}
