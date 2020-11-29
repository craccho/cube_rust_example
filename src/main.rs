use std::fmt;
use regex::Regex;
use std::collections::HashMap;

type CornerPosition = usize;
type EdgePosition = usize;
type CornerSticker = u8;
type EdgeSticker = u8;
type CornerTwist = u8;
type EdgeFlip = u8;
type Corner = [CornerSticker; 8];
type Edge = [EdgeSticker; 12];

#[derive(PartialEq,Clone)]
struct Cube {
    corner: Corner,
    edge: Edge,
}
type CornerTurn = [(CornerPosition, CornerTwist); 4];
type EdgeTurn = [(EdgePosition, EdgeFlip); 4];

type CornerPerm = Vec<Vec<CornerSticker>>;
type EdgePerm = Vec<Vec<EdgeSticker>>;

#[derive(Clone,Debug)]
struct Perm {
    name: Option<String>,
    cp: CornerPerm,
    ep: EdgePerm,
}

impl  Perm {
    fn new() -> Perm {
        Perm {
            name: None,
            cp: Vec::new(),
            ep: Vec::new(),
        }
    }

    fn inverse(&self, name: &str) -> Perm {
        let cp = self.cp.iter().rev().cloned().map(|p| p.iter().rev().cloned().collect()).collect();
        let ep = self.ep.iter().rev().cloned().map(|p| p.iter().rev().cloned().collect()).collect();
        Perm { name: Some(String::from(name)), cp, ep }
    }

    fn double(&self) -> Perm {
        let mut cube = Cube::SOLVED;
        let name = self.name.as_ref().map(|name| format!("{}2", name)).unwrap_or_default();


        cube.apply(self);
        cube.apply(self);
        cube.perm(&name)
    }

    fn join(perms: Vec<Perm>) -> Perm {
        let mut cube = Cube::SOLVED;
        let name = perms.iter().flat_map(|p| p.name.as_ref().map(|s| s.to_string())).collect::<Vec<_>>().join(" ");

        for perm in perms {
            cube.apply(&perm);
        }
        cube.perm(name.as_str())
    }

    fn from_turn(turn: &Turn) -> Perm {
        let mut cube = Cube {
            corner: Cube::SOLVED.corner.clone(),
            edge: Cube::SOLVED.edge.clone(),
        };

        cube.turn(turn);
        cube.perm(turn.name)
    }

    fn turns() -> HashMap<&'static str, Perm> {
        let mut turns = HashMap::new();

        let u = Perm::from_turn(&Turn::U);
        let ui = u.inverse("U'");
        let u2 = u.double();
        let d = Perm::from_turn(&Turn::D);
        let di = d.inverse("D'");
        let d2 = d.double();
        let f = Perm::from_turn(&Turn::F);
        let fi = f.inverse("F'");
        let f2 = f.double();
        let b = Perm::from_turn(&Turn::B);
        let bi = b.inverse("B'");
        let b2 = b.double();
        let r = Perm::from_turn(&Turn::R);
        let ri = r.inverse("R'");
        let r2 = r.double();
        let l = Perm::from_turn(&Turn::L);
        let li = l.inverse("L'");
        let l2 = l.double();

        turns.insert("U", u);
        turns.insert("U'", ui);
        turns.insert("U2", u2);
        turns.insert("D", d);
        turns.insert("D'", di);
        turns.insert("D2", d2);
        turns.insert("R", r);
        turns.insert("R'", ri);
        turns.insert("R2", r2);
        turns.insert("L", l);
        turns.insert("L'", li);
        turns.insert("L2", l2);
        turns.insert("F", f);
        turns.insert("F'", fi);
        turns.insert("F2", f2);
        turns.insert("B", b);
        turns.insert("B'", bi);
        turns.insert("B2", b2);

        turns
    }

    fn from_scramble(scramble: &str) -> Perm {
        let re = Regex::new(r"([UDFBRL]['2]?)").unwrap();
        let mut perm: Perm = Perm::new();

        let turns = Perm::turns();

        let perms = re.captures_iter(scramble).map(|cap| {
            println!("{}", &cap[1]);
            turns.get(&cap[1]).unwrap_or(&Perm::new()).clone()
        }).collect();

        Perm::join(perms)
    }
}

#[derive(Debug,Hash,Eq,PartialEq)]
struct Turn {
    name: &'static str,
    cp: CornerTurn,
    ep: EdgeTurn,
}

impl Turn {
    const fn inverse(&self, name: &'static str) -> Turn {
        let cp = &self.cp;
        let ep = &self.ep;
        Turn {
            name,
            cp: [cp[3], cp[2], cp[1], cp[0]],
            ep: [ep[3], ep[2], ep[1], ep[0]],
        }
    }

    const R: Turn = Turn {
        name: "R",
        cp: [( 7, 1), ( 0, 2), ( 3, 1), ( 4, 2)],
        ep: [( 4, 0), ( 0, 0), ( 7, 0), ( 8, 0)],
    };
    const RI: Turn = Turn::R.inverse("R'");

    const L: Turn = Turn {
        name: "L",
        cp: [( 2, 2), ( 1, 1), ( 6, 2), ( 5, 1)],
        ep: [( 6, 0), ( 2, 0), ( 5, 0), (10, 0)],
    };
    const LI: Turn = Turn::L.inverse("L'");

    const U: Turn = Turn {
        name: "U",
        cp: [( 1, 0), ( 2, 0), ( 3, 0), ( 0, 0)],
        ep: [( 3, 0), ( 0, 0), ( 1, 0), ( 2, 0)],
    };
    const UI: Turn = Turn::U.inverse("U'");

    const D: Turn = Turn {
        name: "D",
        cp: [( 7, 0), ( 4, 0), ( 5, 0), ( 6, 0)],
        ep: [(11, 0), ( 8, 0), ( 9, 0), (10, 0)],
    };
    const DI: Turn = Turn::D.inverse("D'");

    const F: Turn = Turn {
        name: "F",
        cp: [( 1, 2), ( 0, 1), ( 7, 2), ( 6, 1)],
        ep: [( 5, 1), ( 1, 1), ( 4, 1), (11, 1)],
    };
    const FI: Turn = Turn::F.inverse("F'");

    const B: Turn  = Turn {
        name: "B",
        cp: [( 3, 2), ( 2, 1), ( 5, 2), ( 4, 1)],
        ep: [( 6, 1), ( 9, 1), ( 7, 1), ( 3, 1)],
    };
    const BI: Turn = Turn::B.inverse("B'");

}

impl Cube {
    const SOLVED: Cube = Cube {
        corner: [0,1,2,3,20,21,22,23],
        edge: [0,1,2,3,4,6,12,14,20,21,22,23],
    };
    const CT: [[CornerSticker; 24]; 3] =
        [
            [ 0, 1,  2,  3,  4,  5,  6, 7, 8,  9, 10,11,12, 13, 14,15, 16, 17,18,19, 20, 21,22,23],
            [18, 6, 10, 14, 17, 22, 11, 0, 5, 21, 15, 1, 9, 20, 19, 2, 13, 23, 7, 3, 16, 12, 8, 4],
            [ 7,11, 15, 19, 23,  8,  1,18,22, 12,  2, 6,21, 16,  3,10, 20,  4, 0,14, 13,  9, 5,17],
        ];
    const CP: [(CornerPosition, CornerTwist); 24] =
        [ (0, 0), (1, 0), (2, 0), (3, 0),
          (7, 1), (6, 2), (1, 1), (0, 2),
          (6, 1), (5, 2), (2, 1), (1, 2),
          (5, 1), (4, 2), (3, 1), (2, 2),
          (4, 1), (7, 2), (0, 1), (3, 2),
          (4, 0), (5, 0), (6, 0), (7, 0) ];
    const EF: [[EdgeSticker; 24]; 2] =
        [
            [ 0, 1,  2,  3,  4,  5, 6, 7, 8,  9, 10,11, 12, 13, 14,15, 16, 17,18,19, 20, 21,22,23],
            [19, 7, 11, 15, 18, 23, 8, 1, 6, 22, 12, 2, 10, 21, 16, 3, 14, 20, 4, 0, 17, 13, 9, 5],
        ];
    const EP: [(EdgePosition, EdgeFlip); 24] =
        [ (0, 0), ( 1, 0), ( 2, 0), ( 3, 0),
          (4, 0), (11, 1), ( 5, 0), ( 1, 1),
          (5, 1), (10, 1), ( 6, 1), ( 2, 1),
          (6, 0), ( 9, 1), ( 7, 0), ( 3, 1),
          (7, 1), ( 8, 1), ( 4, 1), ( 0, 1),
          (8, 0), ( 9, 0), (10, 0), (11, 0) ];

    fn turn(&mut self, t: &Turn) {
        let l = t.cp.len();
        let sources: Vec<u8>  = (0..l).map(|i| {
            let j = (i + l - 1) % l;
            Cube::CT[t.cp[j].1 as usize][self.corner[t.cp[j].0 as usize] as usize]
        }).collect();
        for i in 0..l {
            self.corner[t.cp[i].0 as usize] = sources[i];
        }
        let l = t.ep.len();
        let sources: Vec<u8>  = (0..l).map(|i| {
            let j = (i + l - 1) % l;
            Cube::EF[t.ep[j].1 as usize][self.edge[t.ep[j].0 as usize] as usize]
        }).collect();
        for i in 0..l {
            self.edge[t.ep[i].0 as usize] = sources[i];
        }
    }

    fn apply(&mut self, p: &Perm) {
        let l = p.cp.len();
        for i in 0..l {
            let cp = &p.cp[i];
            let l = cp.len();
            let next: Vec<(CornerPosition, CornerSticker)> = (0..l).map(|i| {
                let j = (i + l - 1) % l;
                let pi = Cube::CP[cp[i] as usize];
                let pj = Cube::CP[cp[j] as usize];
                let sj = Cube::CT[((pj.1 + 3 - pi.1) % 3) as usize][self.corner[pj.0] as usize];

                (pi.0, sj)
            }).collect();
            for i in 0..l {
                self.corner[next[i].0] = next[i].1;
            }
        }

        let l = p.ep.len();
        for i in 0..l {
            let ep = &p.ep[i];
            let l = ep.len();
            let next: Vec<(EdgePosition, EdgeSticker)> = (0..l).map(|i| {
                let j = (i + l - 1) % l;
                let pi = Cube::EP[ep[i] as usize];
                let pj = Cube::EP[ep[j] as usize];
                let sj = Cube::EF[((pj.1 + 2 - pi.1) % 2) as usize][self.edge[pj.0] as usize];

                (pi.0, sj)
            }).collect();
            for i in 0..l {
                self.edge[next[i].0] = next[i].1
            }
        }
    }

    fn solved(&self) -> bool {
        *self == Cube::SOLVED
    }

    fn perm(&self, name: &str) -> Perm {
        let mut cube = self.clone();
        let mut perm = Perm {
            name: None,
            cp: Vec::new(),
            ep: Vec::new(),
        };

        while !cube.solved() {
            for i in 0..8 {
                let s1 = Cube::SOLVED.corner[i];
                let mut s2 = cube.corner[i];
                while s1 != s2 {
                    let p = vec![s1, s2];
                    let cp = Perm {name: None, cp: vec![p.clone()], ep: vec![]};
                    cube.apply(&cp);
                    perm.cp.push(p);
                    s2 = cube.corner[i];
                }
            }
            for i in 0..12 {
                let s1 = Cube::SOLVED.edge[i];
                let s2 = cube.edge[i];
                if s1 != s2 {
                    let p = vec![s1, s2];
                    let ep = Perm {name: None, ep: vec![p.clone()], cp: vec![]};
                    cube.apply(&ep);
                    perm.ep.push(p);
                }
            }
        }
        perm.inverse(&name.to_string())
    }

    fn solve(&self) -> Vec<Perm> {
        let mut solutions: Vec<Vec<Perm>> = Vec::new();
        let single_turns: &Vec<Perm> = &Perm::turns().values().cloned().collect();
        let inital = vec![Perm::new()];
        let mut perms: Vec<(Vec<Perm>, Cube)> = vec![(inital, self.clone())];
        while solutions.len() == 0 {
            let mut nexts = Vec::new();
            for (turns, cube) in &perms {
                if cube.solved() {
                    solutions.push(turns.clone());
                } else {
                    for turn in single_turns {
                        let es = &"#".to_string();
                        let turn_name = turn.name.as_ref().unwrap_or(es);
                        let tnfc = turn_name.chars().next().unwrap();
                        let target_name = turns.last().unwrap().name.as_ref().unwrap_or(es);
                        let gnfc = target_name.chars().next().unwrap();
                        if tnfc == gnfc { continue; }
                        let mut cube = cube.clone();
                        cube.apply(&turn);
                        let mut nt = turns.clone();
                        nt.push(turn.clone());
                        println!("{}", nt.iter().map(|t| t.name.clone().unwrap_or_default()).collect::<Vec<String>>().join(" "));
                        nexts.push((nt, cube));
                    }
                }
            }
            perms = nexts
        }
        solutions[0].clone()
    }
}

impl fmt::Display for Cube {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {:?}", self.corner, self.edge)
    }
}

impl fmt::Display for Perm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.name {
            Some(name) => write!(f, "{}", name),
            None => write!(f, "cp:{:?} ep:{:?}", self.cp, self.ep),
        }
    }
}

impl fmt::Display for Turn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

fn main() {

    let mut cube: Cube = Cube::SOLVED;
    let sequence = vec![Turn::R, Turn::U, Turn::RI, Turn::UI];
    let sequence2 = vec![Turn::L, Turn::B, Turn::R, Turn::U, Turn::F, Turn::D];
    let sequence3 = vec![Turn::DI, Turn::FI, Turn::UI, Turn::RI, Turn::BI, Turn::LI];
    let sequence4 = vec![
        Turn::R, Turn::RI,
        Turn::L, Turn::LI,
        Turn::U, Turn::UI,
        Turn::D, Turn::DI,
        Turn::F, Turn::FI,
        Turn::B, Turn::BI,
    ];
    let sequence5 = vec![
        Turn::R,
        Turn::L,
        Turn::U,
        Turn::D,
        Turn::F,
        Turn::B,
        Turn::BI,
        Turn::FI,
        Turn::DI,
        Turn::UI,
        Turn::LI,
        Turn::RI,
    ];
    let sequence6 = vec![ Turn::R, Turn::U, Turn::U, Turn::DI, Turn::B, Turn::DI ];
    let sequence7 = vec![ Turn::R ];
    let sc = Perm::from_scramble("R' U' F R2 U R2 U F2 L2 D' R2 U B2 U' L' U F' L R2 D' F' L' R D' R' U' F");
    let sc2 = Perm::from_scramble("R");
    let mut sequences: Vec<Perm> = vec![
        sequence,
        sequence2,
        sequence3,
        sequence4,
        sequence5,
        sequence6,
        sequence7,
    ].iter().map(|turns| {
        Perm::join(
            turns.iter().map(|t| {
                Perm::from_turn(t)
            }).collect()
        )
    }).collect();
    sequences.push(sc);
    sequences.push(sc2);

    for seq in sequences {
        let mut ct = 0;

        println!("sequence: {:?}", seq);

        loop {
            ct += 1;
            cube.apply(&seq);
            if ct == 1 {
                println!("initial state: {}", cube);
            }
            let sol = Perm::join(cube.solve());
            cube.apply(&sol);
            if cube == Cube::SOLVED {
                println!("Solved by {} !", sol);
                break;
            } else {
            }
        }
    }
}
