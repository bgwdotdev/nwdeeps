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
import shore
import shore/key

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
    )
  let cmd = []
  #(state, cmd)
}

// UPDATE 

pub opaque type Event {
  Log(log.Log)
  SetPage(Page)
  LoadLog(String)
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
  }
}

// VIEW

pub fn view(state: State) -> shore.Node(Event) {
  shore.Split(shore.Split2(
    shore.Vertical,
    shore.Ratio2(shore.Px(2), shore.Fill),
    shore.Split1(shore.Div([shore.Text("", None, None)], shore.Col)),
    shore.Split1(view_page(state)),
  ))
}

fn view_page(state: State) -> shore.Node(Event) {
  case state.page {
    ShowDps -> shore.Div(view_dps(state, meters(state.current)), shore.Col)
    ShowHistory -> shore.Div(view_history(state), shore.Col)
    ShowLog(log_name, log) ->
      shore.Div(view_dps_log(log_name, meters(log)), shore.Col)
  }
}

// ShowDps

fn meters(combat: Combat) -> List(Dps) {
  //fn meters(state: State) -> List(Dps) {
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

fn view_xph(state: State, time: Time) -> shore.Node(Event) {
  let session =
    timestamp.difference(state.xp_start, time.now) |> duration.to_seconds
  let xph = int.to_float(state.xp_total) /. session *. 3600.0
  { "XP/h: " <> int.to_string(float.round(xph)) } |> shore.Text(None, None)
}

fn view_loading(state: State, time: Time) -> shore.Node(Event) {
  let time_since =
    timestamp.difference(state.last_loading, time.now)
    |> duration.to_seconds
    |> float.round
  { "Loading: " <> int.to_string(time_since) } |> shore.Text(None, None)
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

  [["cooldown", "ability"], ..cds] |> shore.Table(40, _)
}

fn view_title(time: Time) -> shore.Node(Event) {
  { "DPS | Last Update: " <> int.to_string(time.diff) }
  |> shore.Text(None, None)
}

fn view_meters(meters: List(Dps), top: Dps) -> shore.Node(Event) {
  meters |> list.map(view_meter(_, top)) |> shore.Div(shore.In)
}

fn view_meter(dps: Dps, top: Dps) -> shore.Node(Event) {
  // right align numbers
  let align = fn(a, b) {
    { a |> int.to_string |> string.length }
    - { b |> int.to_string |> string.length }
    |> string.repeat(" ", _)
  }
  let dpr_right = align(top.dpr, dps.dpr)
  let damage_right = align(top.damage, dps.damage)
  [
    shore.Progress(30, top.damage, dps.damage, shore.Blue),
    shore.Text(dpr_right <> int.to_string(dps.dpr), None, None),
    shore.Text(damage_right <> int.to_string(dps.damage), None, None),
    shore.Text(dps.source, None, None),
  ]
  |> shore.Div(shore.Row)
}

fn view_dps(state: State, meters: List(Dps)) -> List(shore.Node(Event)) {
  let time = time(state)
  let top = top_dps(meters)

  [
    view_title(time),
    view_xph(state, time),
    view_loading(state, time),
    shore.BR,
    view_meters(meters, top),
    shore.BR,
    view_cds(state, time),
    shore.KeyBind(key.Char("r"), Log(log.Reset)),
    shore.KeyBind(key.Char("c"), Log(log.ResetCd)),
    shore.KeyBind(key.Char("l"), SetPage(ShowHistory)),
  ]
}

fn view_dps_log(title: String, meters: List(Dps)) -> List(shore.Node(Event)) {
  let top = top_dps(meters)
  [
    shore.Text(title, Some(shore.Blue), None),
    view_meters(meters, top),
    shore.KeyBind(key.Char("l"), SetPage(ShowHistory)),
  ]
}

// ShowHistory

fn view_history(state: State) -> List(shore.Node(Event)) {
  [
    dict.to_list(state.previous)
      |> list.sort(fn(a, b) { string.compare(b.0, a.0) })
      |> list.index_map(fn(x, idx) { [int.to_string(idx), x.0] })
      |> list.prepend(["log", "time"])
      |> shore.Table(50, _),
    shore.KeyBind(key.Char("l"), SetPage(ShowDps)),
    dict.to_list(state.previous)
      |> list.sort(fn(a, b) { string.compare(b.0, a.0) })
      |> list.take(10)
      |> list.index_map(fn(x, idx) {
        shore.KeyBind(key.Char(int.to_string(idx)), LoadLog(x.0))
      })
      |> shore.Div(shore.Col),
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
