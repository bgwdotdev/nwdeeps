import gleam/regexp

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
  CurseSongCd
  ActiveCd
  DoneResting
}

// EXAMPLE
// 
// > [CHAT WINDOW TEXT] [Tue Feb  4 22:11:40] 
// 
const header = "^\\[CHAT WINDOW TEXT\\] \\[(.*) (\\d\\d:\\d\\d:\\d\\d)\\] "

const name = "([a-zA-Z\\(\\)\\'\\-\\s]*)"

//const timer = "(\\d*) minutes?"

/// EXAMPLE
/// 
/// > F : Initiative Roll : 11 : (12 - 1 = 11)
/// 
pub fn initiative() -> Parse {
  let regex = name <> " : Initiative Roll : (\\d*) : \\((.*)\\)$"
  let assert Ok(regex) = regexp.from_string(header <> regex)
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
    name
    <> " attacks "
    <> name
    <> " : \\*(critical hit|hit|miss)\\* : \\((.*)\\)$"
  let assert Ok(regex) = regexp.from_string(header <> regex)
  Parse(event: Attack, regexp: regex)
}

/// EXAMPLE
///
/// > Attack Of Opportunity : F attacks Arcane Archer : *miss* : (1 + 40 = 41)
///
pub fn aoo() -> Parse {
  let regex =
    "Attack Of Opportunity : "
    <> name
    <> " attacks "
    <> name
    <> " : \\*(critical hit|hit|miss)\\* : \\((.*)\\)$"
  let assert Ok(regex) = regexp.from_string(header <> regex)
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
  let regex = name <> " damages " <> name <> ": (\\d*) \\((.*)\\)$"
  let assert Ok(regex) = regexp.from_string(header <> regex)
  Parse(event: Damage, regexp: regex)
}

/// EXAMPLE
///
/// > F : Healed 0 hit points.
///
pub fn heal() -> Parse {
  let regex = name <> " : Healed (\\d*) hit points\\.$"
  let assert Ok(regex) = regexp.from_string(header <> regex)
  Parse(event: Heal, regexp: regex)
}

/// EXAMPLE
///
/// > Experience Points Gained:  13
///
pub fn experience() -> Parse {
  let regex = "Experience Points Gained:  (\\d*)$"
  let assert Ok(regex) = regexp.from_string(header <> regex)
  Parse(event: Experience, regexp: regex)
}

/// EXAMPLE
///
/// > Invalid or inaccessible command '-reset'. Type -help for a list of commands.
///
pub fn reset() -> Parse {
  let regex =
    "Invalid or inaccessible command '-reset'. Type -help for a list of commands."
  let assert Ok(regex) = regexp.from_string(header <> regex)
  Parse(event: Reset, regexp: regex)
}

/// EXAMPLE
///
/// > Curse Song has regained a charge. (11 / 17)
///
pub fn curse_song_cd() -> Parse {
  let regex = "Curse Song has regained a charge. (\\d* / \\d*)"
  let assert Ok(regex) = regexp.from_string(header <> regex)
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
  let assert Ok(regex) = regexp.from_string(header <> regex)
  Parse(event: ActiveCd, regexp: regex)
}

/// EXAMPLE
///
/// > Done resting.
///
pub fn done_resting() -> Parse {
  let regex = "Done resting."
  let assert Ok(regex) = regexp.from_string(header <> regex)
  Parse(event: DoneResting, regexp: regex)
}
