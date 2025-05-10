import gleam/list
import gleam/regexp
import gleam/result
import gleam/time/calendar
import gleam/time/timestamp
import gleeunit
import gleeunit/should
import nwdeeps/parse

pub fn main() {
  gleeunit.main()
}

type Test {
  Test(expect: parse.Event, text: String)
}

pub fn header_parse_test() {
  let text = "Tue Feb  4 22:11:40"
  let actual =
    text
    |> parse.header
    |> result.map(timestamp.to_calendar(_, calendar.utc_offset))
  let date = calendar.Date(2025, calendar.February, 4)
  let time = calendar.TimeOfDay(22, 11, 40, 0)
  let expect = Ok(#(date, time))
  should.equal(actual, expect)
}

const tests = [
  Test(
    expect: parse.Initiative,
    text: "[CHAT WINDOW TEXT] [Tue Feb  4 22:11:25] F : Initiative Roll : 11 : (12 - 1 = 11)",
  ),
  Test(
    expect: parse.Attack,
    text: "[CHAT WINDOW TEXT] [Tue Feb  4 22:11:36] F attacks Arcane-a Archer : *hit* : (14 + 38 = 52)",
  ),
  Test(
    expect: parse.AOO,
    text: "[CHAT WINDOW TEXT] [Tue Feb  4 22:11:35] Attack Of Opportunity : F attacks Arcane Archer : *miss* : (1 + 38 = 39)",
  ),
  Test(
    expect: parse.Damage,
    text: "[CHAT WINDOW TEXT] [Tue Feb  4 22:11:36] Summoned Zombie Dread Tyrant damages Arcane Archer: 2 (2 Poison)",
  ),
  Test(
    expect: parse.Damage,
    text: "[CHAT WINDOW TEXT] [Sat Feb  8 18:01:39] Kerzesh damages Kuo-Toa Dart Thrower: 34 (24 Physical 6 Fire 4 Positive Energy)",
  ),
  Test(
    expect: parse.Heal,
    text: "[CHAT WINDOW TEXT] [Tue Feb  4 22:11:34] F : Healed 0 hit points.",
  ),
  Test(
    expect: parse.Experience,
    text: "[CHAT WINDOW TEXT] [Sat Feb  8 12:23:30] Experience Points Gained:  10",
  ),
  Test(
    expect: parse.Reset,
    text: "[CHAT WINDOW TEXT] [Sat Feb  8 16:19:32] Invalid or inaccessible command '-reset'. Type -help for a list of commands.",
  ),
  Test(
    expect: parse.ActiveCd,
    text: "[CHAT WINDOW TEXT] [Mon Feb 17 17:35:32] Atrocity: Whisper has a timer of 1 minute and 40 seconds. You may not use Atrocity: Whisper again for this period of time.",
  ),
  Test(
    expect: parse.ActiveCd,
    text: "[CHAT WINDOW TEXT] [Mon Feb 17 17:37:49] Dirge of Awakening has a timer of 15 minutes. You may not use Dirge of Awakening again for this period of time.",
  ),
  Test(
    expect: parse.ActiveCd,
    text: "[CHAT WINDOW TEXT] [Mon Feb 17 17:08:10] loadoutfit has a timer of 6 seconds. You may not use loadoutfit again for this period of time.",
  ),
  Test(
    expect: parse.DoneResting,
    text: "[CHAT WINDOW TEXT] [Mon Feb 17 17:31:35] Done resting.",
  ),
  Test(
    expect: parse.Charge,
    text: "[CHAT WINDOW TEXT] [Tue Feb 18 17:02:08] Bard Song has regained a charge. (17 / 18)",
  ),
  Test(
    expect: parse.Charge,
    text: "[CHAT WINDOW TEXT] [Tue Feb 18 17:01:23] Imbue Arrow has regained a charge.",
  ),
  Test(
    expect: parse.Loading,
    text: "[CHAT WINDOW TEXT] [Sun Mar  9 10:59:56] Area Setting: [Magic] [State] [Teleport] [Transfer] [Death]",
  ),
]

fn do_test(parser: parse.Parse) {
  fn(t: Test) {
    case regexp.check(parser.regexp, t.text) {
      True -> should.equal(parser.event, t.expect)
      False -> should.not_equal(parser.event, t.expect)
    }
  }
  |> list.map(tests, _)
}

pub fn initiative_test() {
  do_test(parse.initiative())
}

pub fn attack_test() {
  do_test(parse.attack())
}

pub fn aoo_test() {
  do_test(parse.aoo())
}

pub fn damage_test() {
  do_test(parse.damage())
}

pub fn heal_test() {
  do_test(parse.heal())
}

pub fn experience_test() {
  do_test(parse.experience())
}

pub fn reset_test() {
  do_test(parse.reset())
}

pub fn active_cd_test() {
  do_test(parse.active_cd())
}

pub fn done_resting_test() {
  do_test(parse.done_resting())
}

pub fn charge_test() {
  do_test(parse.charge())
}

pub fn loading_test() {
  do_test(parse.loading())
}
