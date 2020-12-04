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

#[derive(Clone)]
enum Rotation {
    X, Xi, Y, Yi, Z, Zi,
}

impl Rotation {
    fn inverse(self) -> Rotation {
        match self {
            Rotation::X => Rotation::Xi,
            Rotation::Xi => Rotation::X,
            Rotation::Y => Rotation::Yi,
            Rotation::Yi => Rotation::Y,
            Rotation::Z => Rotation::Zi,
            Rotation::Zi => Rotation::Z,
        }
    }
}

fn rotate_sticker(s: u8, r: &Rotation) -> u8 {
    match r {
        Rotation::X => [
            4, 5, 6, 7,
            20, 21, 22, 23,
            9, 10, 11, 8,
            0, 1, 2, 3,
            19, 16, 17, 18,
            12, 13, 14, 15,
        ][s as usize],
        Rotation::Xi => [
            12, 13, 14, 15,
            0, 1, 2, 3,
            11, 8, 9, 10,
            20, 21, 22, 23,
            17, 18, 19, 16,
            4, 5, 6, 7,
        ][s as usize],
        Rotation::Y => [
            3, 0, 1, 2,
            16, 17, 18, 19,
            4, 5, 6, 7,
            8, 9, 10, 11,
            12, 13, 14, 15,
            21, 22, 23, 20,
        ][s as usize],
        Rotation::Yi => [
            1, 2, 3, 0,
            8, 9, 10, 11,
            12, 13, 14, 15,
            16, 17, 18, 19,
            4, 5, 6, 7,
            23, 20, 21, 22,
        ][s as usize],
        Rotation::Z => [
            8, 9, 10, 11,
            7, 4, 5, 6,
            20, 21, 22, 23,
            13, 14, 15, 12,
            0, 1, 2, 3,
            16, 17, 18, 19,
        ][s as usize],
        Rotation::Zi => [
            16, 17, 18, 19,
            5, 6, 7, 4,
            0, 1, 2, 3,
            15, 12, 13, 14,
            20, 21, 22, 23,
            8, 9, 10, 11,
        ][s as usize],
    }
}

#[derive(PartialEq,Clone)]
enum Surface {
    U, D, F, B, R, L,
}

impl Surface {
    fn rotate(&self, rotation: &Rotation) -> Self {
        match self {
            Surface::U => match rotation {
                Rotation::X => Surface::F,
                Rotation::Xi => Surface::B,
                Rotation::Y => Surface::U,
                Rotation::Yi => Surface::U,
                Rotation::Z => Surface::L,
                Rotation::Zi => Surface::R,
            },
            Surface::D => match rotation {
                Rotation::X => Surface::B,
                Rotation::Xi => Surface::F,
                Rotation::Y => Surface::D,
                Rotation::Yi => Surface::D,
                Rotation::Z => Surface::R,
                Rotation::Zi => Surface::L,
            },
            Surface::F => match rotation {
                Rotation::X => Surface::D,
                Rotation::Xi => Surface::U,
                Rotation::Y => Surface::R,
                Rotation::Yi => Surface::L,
                Rotation::Z => Surface::F,
                Rotation::Zi => Surface::F,
            },
            Surface::B => match rotation {
                Rotation::X => Surface::U,
                Rotation::Xi => Surface::D,
                Rotation::Y => Surface::L,
                Rotation::Yi => Surface::R,
                Rotation::Z => Surface::B,
                Rotation::Zi => Surface::B,
            },
            Surface::R => match rotation {
                Rotation::X => Surface::R,
                Rotation::Xi => Surface::R,
                Rotation::Y => Surface::B,
                Rotation::Yi => Surface::F,
                Rotation::Z => Surface::U,
                Rotation::Zi => Surface::D,
            },
            Surface::L => match rotation {
                Rotation::X => Surface::L,
                Rotation::Xi => Surface::L,
                Rotation::Y => Surface::F,
                Rotation::Yi => Surface::B,
                Rotation::Z => Surface::D,
                Rotation::Zi => Surface::U,
            },
        }
    }
}

#[derive(PartialEq,Clone)]
struct Orientation {
    u: Surface,
    f: Surface,
}

impl Orientation {
    fn rotate(&mut self, rotation: &Rotation) {
        self.u.rotate(rotation);
        self.f.rotate(rotation);
    }

    fn rotation(&self) -> Vec<Rotation> {
        match self.u {
            Surface::U => match self.f {
                Surface::F => vec![],
                Surface::B => vec![Rotation::Y, Rotation::Y],
                Surface::L => vec![Rotation::Yi],
                Surface::R => vec![Rotation::Y],
                _ => panic!(),
            },
            Surface::D => match self.f {
                Surface::F => vec![Rotation::Z, Rotation::Z],
                Surface::B => vec![Rotation::X, Rotation::X],
                Surface::L => vec![Rotation::X, Rotation::X, Rotation::Yi],
                Surface::R => vec![Rotation::X, Rotation::X, Rotation::Y],
                _ => panic!(),
            },
            Surface::F => match self.f {
                Surface::U => vec![Rotation::Z, Rotation::Z, Rotation::X],
                Surface::D => vec![Rotation::X],
                Surface::L => vec![Rotation::X, Rotation::Yi],
                Surface::R => vec![Rotation::X, Rotation::Y],
                _ => panic!(),
            },
            Surface::B => match self.f {
                Surface::U => vec![Rotation::Xi],
                Surface::D => vec![Rotation::Y, Rotation::Y, Rotation::X],
                Surface::L => vec![Rotation::Xi, Rotation::Yi],
                Surface::R => vec![Rotation::Xi, Rotation::Y],
                _ => panic!(),
            },
            Surface::L => match self.f {
                Surface::F => vec![Rotation::Z],
                Surface::B => vec![Rotation::Y, Rotation::Y, Rotation::Zi],
                Surface::U => vec![Rotation::Y, Rotation::Xi],
                Surface::D => vec![Rotation::Yi, Rotation::X],
                _ => panic!(),
            },
            Surface::R => match self.f {
                Surface::F => vec![Rotation::Zi],
                Surface::B => vec![Rotation::Y, Rotation::Y, Rotation::Z],
                Surface::U => vec![Rotation::Yi, Rotation::Xi],
                Surface::D => vec![Rotation::Y, Rotation::X],
                _ => panic!(),
            },

        }
    }

    fn reverse(&self) -> Vec<Rotation> {
        self.rotation().iter().rev().cloned().map(Rotation::inverse).collect()
    }
}

#[derive(PartialEq,Clone)]
struct Cube {
    corner: Corner,
    edge: Edge,
    orientation: Orientation,
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

impl Perm {
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
        let mut cube = Cube::SOLVED.clone();

        cube.turn(turn);
        cube.perm(turn.name)
    }

    fn turns(gen: u8) -> HashMap<&'static str, Perm> {
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

        if gen < 3 {
            if gen < 2 {
                if gen < 1 {
                    turns.insert("F", f);
                    turns.insert("F'", fi);
                    turns.insert("B", b);
                    turns.insert("B'", bi);
                }
                turns.insert("R", r);
                turns.insert("R'", ri);
                turns.insert("L", l);
                turns.insert("L'", li);
            }
            turns.insert("U", u);
            turns.insert("U'", ui);
            turns.insert("D", d);
            turns.insert("D'", di);
        }
        turns.insert("U2", u2);
        turns.insert("D2", d2);
        turns.insert("R2", r2);
        turns.insert("L2", l2);
        turns.insert("F2", f2);
        turns.insert("B2", b2);

        turns
    }

    fn kind(&self) -> Option<(Surface, u8)> {
        match &self.name {
            Some(name) => match name.chars().next().unwrap() {
                'U' => Some((Surface::U, 1)),
                'D' => Some((Surface::D, 1)),
                'R' => Some((Surface::R, 2)),
                'L' => Some((Surface::L, 2)),
                'F' => Some((Surface::F, 3)),
                'B' => Some((Surface::B, 3)),
                _ => None
            },
            None => None
        }
    }

    fn from_scramble(scramble: &str) -> Perm {
        let re = Regex::new(r"([UDFBRL]['2]?)").unwrap();
        let perm: Perm = Perm::new();

        let turns = Perm::turns(0);

        let perms = re.captures_iter(scramble).map(|cap| {
            println!("{}", &cap[1]);
            turns.get(&cap[1]).unwrap_or(&perm).clone()
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
        orientation: Orientation {
            u: Surface::U,
            f: Surface::F,
        }
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

    fn face_solved(&self, s: &Surface) -> bool {
        let is = match s {
            Surface::U => ([ 0, 1, 2, 3], [ 0, 1, 2, 3]),
            Surface::D => ([ 4, 5, 6, 7], [ 8, 9, 10, 11]),
            Surface::R => ([ 0, 3, 4, 7], [ 0, 4, 7, 8]),
            Surface::L => ([ 1, 2, 5, 6], [ 2, 5, 6, 10]),
            Surface::F => ([ 0, 1, 6, 7], [ 1, 4, 5, 11]),
            Surface::B => ([ 2, 3, 4, 5], [ 3, 6, 7, 9]),
        };
        for i in 0..4 {
            if
                self.corner[is.0[i]] != Cube::SOLVED.corner[is.0[i]] ||
                self.edge[is.0[i]] != Cube::SOLVED.edge[is.0[i]] {
                    return false;
                }
        }

        true
    }

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

    fn rotate(&mut self, rotation: &Rotation) {
        self.orientation.rotate(rotation);
        for i in 0..(self.corner.len()) {
            self.corner[i] = rotate_sticker(self.corner[i], rotation);
        }
        for i in 0..(self.edge.len()) {
            self.edge[i] = rotate_sticker(self.edge[i], rotation);
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

    fn eo(&self) -> u8 {
        let mut cube = self.clone();
        let mut eo = 0;
        for &e in cube.edge.iter() {
            eo += Cube::EP[e as usize].1;
        }
        eo
    }

    fn co(&self) -> u8 {
        let mut cube = self.clone();
        let mut co = 0;
        for &c in cube.corner.iter() {
            co += if Cube::CP[c as usize].1 > 0 { 1 } else { 0 };
        }
        co
    }

    fn solve(&self) -> Vec<Perm> {
        let mut solutions: Vec<Vec<&Perm>> = Vec::new();
        let single_turns: Vec<Perm> = Perm::turns(0).values().cloned().collect();
        let eo_turns: Vec<Perm> = Perm::turns(1).values().cloned().collect();
        let dr_turns: Vec<Perm> = Perm::turns(2).values().cloned().collect();
        let base_turns = [&single_turns, &eo_turns, &dr_turns];
        let ip = Perm::new();
        let initial = vec![&ip];
        let mut perms = vec![(initial, self.clone(), 0)];
        let mut ct = 0;
        let mut tick = 0;
        let mut nl = false;
        if self.solved() {
            solutions.push(vec![&ip]);
        }
        while solutions.len() == 0 {
            ct += 1;
            crif(&mut nl);
            println!("{}", ct);
            let mut nexts = Vec::new();
            for (turns, cube, mut gen) in &perms {
                let eo = cube.eo();
                if gen == 0 && eo == 0 {
                    gen = 1;
                    crif(&mut nl);
                    println!("{}", p2s(&turns));
                    println!("EO!");
                }
                if gen == 1 && cube.co() == 0 {
                    gen = 2;
                    crif(&mut nl);
                    println!("{}", p2s(&turns));
                    println!("DR!");
                }
                if ct >= 8 && gen == 0 {
                    crif(&mut nl);
                    println!("Too long to EO!");
                    continue;
                }
                for turn in base_turns[gen] {
                    let k1 = turn.kind().unwrap();
                    if cube.face_solved(&k1.0) {
                        continue;
                    }
                    let k2 = turns.last().unwrap().kind();
                    match k2 {
                        Some(k2) => {
                            if k1.0 == k2.0 { continue; };
                            if turns.len() > 1 && k1.1 == k2.1 {
                                let k2 = turns[turns.len() - 2].kind();
                                match k2 {
                                    Some(k2) => {
                                        if k1.0 == k2.0 { continue; };
                                    },
                                    None => (),
                                };
                            }
                        },
                        None => (),
                    };
                    let mut cube = cube.clone();
                    let bo = if gen == 0 { cube.eo() } else { cube.co() };
                    cube.apply(&turn);
                    let ao = if gen == 0 { cube.eo() } else { cube.co() };
                    let mut nt = turns.to_vec();
                    if ao <= bo || ao - bo < 4 {
                        nt.push(turn);
                        if cube.solved() {
                            solutions.push(nt.to_vec());
                            break;
                        }
                        nexts.push((nt.to_vec(), cube, gen));
                    }
                    tick += 1;
                    if tick % 100 == 0 {
                        print!("{}         \r", p2s(&nt));
                        nl = true;
                    }
                }
            }
            perms = nexts
        }
        solutions[0].iter().map(|&x| x.clone()).collect()
    }


    fn orient(&mut self, o: &Orientation) {
        let mut rotation = self.orientation.reverse();
        rotation.append(&mut o.rotation());

        for r in &rotation {
            self.rotate(r);
        }
    }
}

fn p2s(turns: &Vec<&Perm>) -> String {
    turns.iter().map(|t| t.name.clone().unwrap_or_default()).collect::<Vec<String>>().join(" ")
}

fn crif(nl: &mut bool) {
    if *nl {
        print!("                                                           \r");
        *nl = false;
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
