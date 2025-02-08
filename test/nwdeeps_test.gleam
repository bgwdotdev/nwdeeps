import gleam/io
import gleam/list
import gleam/regexp
import gleeunit
import gleeunit/should
import nwdeeps/parse

pub fn main() {
  gleeunit.main()
}

const log = [
  "[CHAT WINDOW TEXT] [Tue Feb  4 22:11:25] F : Initiative Roll : 11 : (12 - 1 = 11)",
  "[CHAT WINDOW TEXT] [Tue Feb  4 22:11:36] F attacks Arcane-a Archer : *hit* : (14 + 38 = 52)",
  "[CHAT WINDOW TEXT] [Tue Feb  4 22:11:35] Attack Of Opportunity : F attacks Arcane Archer : *miss* : (1 + 38 = 39)",
  //"[CHAT WINDOW TEXT] [Tue Feb  4 22:11:36] Summoned Zombie Dread Tyrant damages Arcane Archer: 2 (2 Poison)",
  "[CHAT WINDOW TEXT] [Sat Feb  8 18:01:39] Kerzesh damages Kuo-Toa Dart Thrower: 34 (24 Physical 6 Fire 4 Positive Energy)",
  "[CHAT WINDOW TEXT] [Tue Feb  4 22:11:34] F : Healed 0 hit points.",
  "[CHAT WINDOW TEXT] [Sat Feb  8 12:23:30] Experience Points Gained:  10",
  "[CHAT WINDOW TEXT] [Sat Feb  8 16:19:32] Invalid or inaccessible command '-reset'. Type -help for a list of commands.",
]

const damage = [
  "[CHAT WINDOW TEXT] [Tue Feb  4 22:11:36] Summoned Zombie Dread Tyrant damages Arcane Archer: 2 (2 Poison)",
]

pub fn initiative_test() {
  let parser = parse.initiative()
  let target = [True, False, False, False, False, False, False]
  let results = log |> list.map(regexp.check(parser.regexp, _))
  should.equal(results, target)
}

pub fn attack_test() {
  let parser = parse.attack()
  let target = [False, True, False, False, False, False, False]
  let results = log |> list.map(regexp.check(parser.regexp, _))
  should.equal(results, target)
}

pub fn aoo_test() {
  let parser = parse.aoo()
  let target = [False, False, True, False, False, False, False]
  let results = log |> list.map(regexp.check(parser.regexp, _))
  should.equal(results, target)
}

pub fn damage_test() {
  let parser = parse.damage()
  let target = [False, False, False, True, False, False, False]
  let results = log |> list.map(regexp.check(parser.regexp, _))
  should.equal(results, target)
}

pub fn heal_test() {
  let parser = parse.heal()
  let target = [False, False, False, False, True, False, False]
  let results = log |> list.map(regexp.check(parser.regexp, _))
  should.equal(results, target)
}

pub fn experience_test() {
  let parser = parse.experience()
  let target = [False, False, False, False, False, True, False]
  let results = log |> list.map(regexp.check(parser.regexp, _))
  should.equal(results, target)
}

pub fn reset_test() {
  let parser = parse.reset()
  let target = [False, False, False, False, False, False, True]
  let results = log |> list.map(regexp.check(parser.regexp, _))
  should.equal(results, target)
}
