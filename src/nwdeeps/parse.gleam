import gleam/int
import gleam/list
import gleam/regexp
import gleam/result
import gleam/string
import gleam/time/calendar
import gleam/time/timestamp
import nwdeeps/error

pub type Parse {
  Parse(event: Event, regexp: regexp.Regexp)
}

pub type Event {
  Initiative
  Attack
  AOO
  Damage
  Heal
  Experience
  Reset
  ResetCd
  CurseSongCd
  ActiveCd
  DoneResting
  Charge
  Loading
}

// EXAMPLE
// 
// > [CHAT WINDOW TEXT] [Tue Feb  4 22:11:40] 
// 
const header_regex = "^\\[CHAT WINDOW TEXT\\] \\[(.*)\\] "

const name_regex = "([a-zA-Z\\(\\)\\'\\-\\s]*)"

pub fn header(header: String) -> Result(timestamp.Timestamp, error.Error) {
  let parse = {
    use _, rest <- parser_try(header |> parse_day |> skip)
    use month, rest <- parser_try(rest |> parse_month |> skip)
    use date, rest <- parser_try(rest |> parse_date |> skip)
    use time, _ <- parser_try(rest |> parse_time |> skip)
    ParseOk(
      timestamp.from_calendar(
        date: calendar.Date(2025, month, date),
        time:,
        offset: calendar.utc_offset,
      ),
      "",
    )
  }
  case parse {
    ParseOk(timestamp, _) -> Ok(timestamp)
    ParseError(e) -> Error(error.Header(e))
  }
}

fn parser_try(parser: Parser(a), fun: fn(a, String) -> Parser(b)) -> Parser(b) {
  case parser {
    ParseOk(value:, rest:) -> fun(value, rest)
    ParseError(e) -> ParseError(e)
  }
}

pub type Parser(v) {
  ParseError(String)
  ParseOk(value: v, rest: String)
}

fn skip(p: Parser(v)) -> Parser(v) {
  case p {
    ParseOk(value:, rest: " " <> xs) -> skip(ParseOk(value, xs))
    ParseOk(value:, rest:) -> ParseOk(value, rest)
    ParseError(e) -> ParseError(e)
  }
}

fn parse_day(str: String) -> Parser(Nil) {
  case str {
    "Mon" <> xs -> ParseOk(Nil, xs)
    "Tue" <> xs -> ParseOk(Nil, xs)
    "Wed" <> xs -> ParseOk(Nil, xs)
    "Thu" <> xs -> ParseOk(Nil, xs)
    "Fri" <> xs -> ParseOk(Nil, xs)
    "Sat" <> xs -> ParseOk(Nil, xs)
    "Sun" <> xs -> ParseOk(Nil, xs)
    x -> ParseError("error: parse day failure: " <> x)
  }
}

fn parse_month(str: String) -> Parser(calendar.Month) {
  case str {
    "Jan" <> xs -> ParseOk(calendar.January, xs)
    "Feb" <> xs -> ParseOk(calendar.February, xs)
    "Mar" <> xs -> ParseOk(calendar.March, xs)
    "Apr" <> xs -> ParseOk(calendar.April, xs)
    "May" <> xs -> ParseOk(calendar.May, xs)
    "Jun" <> xs -> ParseOk(calendar.June, xs)
    "Jul" <> xs -> ParseOk(calendar.July, xs)
    "Aug" <> xs -> ParseOk(calendar.August, xs)
    "Sep" <> xs -> ParseOk(calendar.September, xs)
    "Oct" <> xs -> ParseOk(calendar.October, xs)
    "Nov" <> xs -> ParseOk(calendar.November, xs)
    "Dec" <> xs -> ParseOk(calendar.December, xs)
    x -> ParseError("error: parse month failure: " <> x)
  }
}

fn parse_date(str: String) -> Parser(Int) {
  do_parse_date(str, "")
}

fn do_parse_date(str: String, acc: String) -> Parser(Int) {
  case str |> string.pop_grapheme {
    Ok(#(" ", xs)) ->
      case int.parse(acc) {
        Ok(acc) -> ParseOk(acc, xs)
        Error(Nil) -> ParseError("date acc: " <> str)
      }
    Ok(#(x, xs)) -> do_parse_date(xs, acc <> x)
    Error(e) -> ParseError(string.inspect(e))
  }
}

fn parse_time(str: String) -> Parser(calendar.TimeOfDay) {
  case str |> string.split(":") |> list.map(int.parse) |> result.all {
    Ok(x) ->
      case x {
        [h, m, s] -> ParseOk(calendar.TimeOfDay(h, m, s, 0), "")
        e -> ParseError("time length too long: " <> string.inspect(e))
      }
    Error(e) -> ParseError("time" <> string.inspect(e))
  }
}

/// EXAMPLE
/// 
/// > F : Initiative Roll : 11 : (12 - 1 = 11)
/// 
pub fn initiative() -> Parse {
  let regex = name_regex <> " : Initiative Roll : (\\d*) : \\((.*)\\)$"
  let assert Ok(regex) = regexp.from_string(header_regex <> regex)
  Parse(event: Initiative, regexp: regex)
}

/// EXAMPLE
/// 
/// > F attacks Arcane Archer : *hit* : (13 + 33 = 46)
/// 
/// > Summoned Zombie Tyrant attacks Arcane Archer : *miss* : (1 + 30 = 31)
/// 
/// > Summoned Zombie Tyrant attacks Arcane Archer : *critical hit* : (20 + 25 = 45 : Threat Roll: 17 + 25 = 42)
///
pub fn attack() -> Parse {
  let regex =
    name_regex
    <> " attacks "
    <> name_regex
    <> " : \\*(critical hit|hit|miss)\\* : \\((.*)\\)$"
  let assert Ok(regex) = regexp.from_string(header_regex <> regex)
  Parse(event: Attack, regexp: regex)
}

/// EXAMPLE
///
/// > Attack Of Opportunity : F attacks Arcane Archer : *miss* : (1 + 40 = 41)
///
pub fn aoo() -> Parse {
  let regex =
    "Attack Of Opportunity : "
    <> name_regex
    <> " attacks "
    <> name_regex
    <> " : \\*(critical hit|hit|miss)\\* : \\((.*)\\)$"
  let assert Ok(regex) = regexp.from_string(header_regex <> regex)
  Parse(event: AOO, regexp: regex)
}

/// EXAMPLE
///
/// > F damages Arcane Archer: 27 (19 Physical 8 Entropy)
///
/// > Summoned Zombie Dread Tyrant damages Arcane Archer: 31 (31 Physical)
///
/// > Arcane Archer damages Summoned Zombie Dread Tyrant: 9 (2 Physical 7 Fire)
///
pub fn damage() -> Parse {
  let regex = name_regex <> " damages " <> name_regex <> ": (\\d*) \\((.*)\\)$"
  let assert Ok(regex) = regexp.from_string(header_regex <> regex)
  Parse(event: Damage, regexp: regex)
}

/// EXAMPLE
///
/// > F : Healed 0 hit points.
///
pub fn heal() -> Parse {
  let regex = name_regex <> " : Healed (\\d*) hit points\\.$"
  let assert Ok(regex) = regexp.from_string(header_regex <> regex)
  Parse(event: Heal, regexp: regex)
}

/// EXAMPLE
///
/// > Experience Points Gained:  13
///
pub fn experience() -> Parse {
  let regex = "Experience Points Gained:  (\\d*)$"
  let assert Ok(regex) = regexp.from_string(header_regex <> regex)
  Parse(event: Experience, regexp: regex)
}

/// EXAMPLE
///
/// > Invalid or inaccessible command '-reset'. Type -help for a list of commands.
///
pub fn reset() -> Parse {
  let regex =
    "Invalid or inaccessible command '-reset'. Type -help for a list of commands."
  let assert Ok(regex) = regexp.from_string(header_regex <> regex)
  Parse(event: Reset, regexp: regex)
}

/// EXAMPLE
///
/// > Invalid or inaccessible command '-reset'. Type -help for a list of commands.
///
pub fn reset_cd() -> Parse {
  let regex =
    "Invalid or inaccessible command '-resetcd'. Type -help for a list of commands."
  let assert Ok(regex) = regexp.from_string(header_regex <> regex)
  Parse(event: ResetCd, regexp: regex)
}

/// EXAMPLE
///
/// > Curse Song has regained a charge. (11 / 17)
///
pub fn curse_song_cd() -> Parse {
  let regex = "Curse Song has regained a charge. (\\d* / \\d*)"
  let assert Ok(regex) = regexp.from_string(header_regex <> regex)
  Parse(event: CurseSongCd, regexp: regex)
}

/// EXAMPLE
///
/// > Dirge of Terror has a timer of 3 minutes. You may not use Dirge of Terror again for this period of time.
///
/// > Atrocity: Whisper has a timer of 1 minute and 40 seconds. You may not use Atrocity: Whisper again for this period of time.
///
/// > loadoutfit has a timer of 6 seconds. You may not use loadoutfit again for this period of time.
///
pub fn active_cd() -> Parse {
  let regex =
    "(.*?) has a timer of (?:(\\d+) minutes?)?(?: and )?(?:(\\d*) seconds?)?. You may not use .*? again for this period of time.$"
  let assert Ok(regex) = regexp.from_string(header_regex <> regex)
  Parse(event: ActiveCd, regexp: regex)
}

/// EXAMPLE
///
/// > Done resting.
///
pub fn done_resting() -> Parse {
  let regex = "Done resting."
  let assert Ok(regex) = regexp.from_string(header_regex <> regex)
  Parse(event: DoneResting, regexp: regex)
}

/// EXAMPLE
///
/// > Imbue Arrow has regained a charge.
///
/// > Bard Song has regained a charge. (17 / 18)
///
pub fn charge() -> Parse {
  let regex = "(.*?) has regained a charge\\.(?: \\(\\d* / \\d*\\))?$"
  let assert Ok(regex) = regexp.from_string(header_regex <> regex)
  Parse(event: Charge, regexp: regex)
}

/// EXAMPLE
///
/// > Area Setting: [Magic] [State] [Teleport] [Transfer] [Death]
///
pub fn loading() -> Parse {
  let regex = "Area Setting: .*"
  let assert Ok(regex) = regexp.from_string(header_regex <> regex)
  Parse(event: Loading, regexp: regex)
}
