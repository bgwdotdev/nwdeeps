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
}

// EXAMPLE
// 
// > [CHAT WINDOW TEXT] [Tue Feb  4 22:11:40] 
// 
const header = "^\\[CHAT WINDOW TEXT\\] \\[(.*) (\\d\\d:\\d\\d:\\d\\d)\\] "

/// EXAMPLE
/// 
/// > F : Initiative Roll : 11 : (12 - 1 = 11)
/// 
pub fn initiative() -> Parse {
  let regex = "([a-zA-Z\\s]*) : Initiative Roll : (\\d*) : \\((.*)\\)$"
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
    "([a-zA-Z\\s]*) attacks ([a-zA-Z\\s]*) : \\*(critical hit|hit|miss)\\* : \\((.*)\\)$"
  let assert Ok(regex) = regexp.from_string(header <> regex)
  Parse(event: Attack, regexp: regex)
}

/// EXAMPLE
///
/// > Attack Of Opportunity : F attacks Arcane Archer : *miss* : (1 + 40 = 41)
///
pub fn aoo() -> Parse {
  let regex =
    "Attack Of Opportunity : ([a-zA-Z\\s]*) attacks ([a-zA-Z\\s]*) : \\*(critical hit|hit|miss)\\* : \\((.*)\\)$"
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
  let regex = "([a-zA-Z\\s]*) damages ([a-zA-Z\\s]*): (\\d*) \\((.*)\\)$"
  let assert Ok(regex) = regexp.from_string(header <> regex)
  Parse(event: Damage, regexp: regex)
}

/// EXAMPLE
///
/// > F : Healed 0 hit points.
///
pub fn heal() -> Parse {
  let regex = "([a-zA-Z\\s]*) : Healed (\\d*) hit points\\.$"
  let assert Ok(regex) = regexp.from_string(header <> regex)
  Parse(event: Heal, regexp: regex)
}
