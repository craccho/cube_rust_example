use std::fmt;
use regex::Regex;
use std::collections::HashMap;
use std::time::Instant;

type CornerPosition = usize;
type EdgePosition = usize;
type CornerSticker = u8;
type EdgeSticker = u8;
type CornerTwist = u8;
type EdgeFlip = u8;
type Corner = [CornerSticker; 8];
type Edge = [EdgeSticker; 12];

type Solution<'a> = (Vec<&'a Perm>, Vec<&'a Perm>, Cube, usize, u8);


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

#[derive(PartialEq,Clone,Debug)]
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

#[derive(PartialEq,Clone,Debug)]
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

#[derive(PartialEq,Clone,Debug)]
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

    fn inverse(&self, name: Option<String>) -> Perm {
        let cp = self.cp.iter().rev().cloned().map(|p| p.iter().rev().cloned().collect()).collect();
        let ep = self.ep.iter().rev().cloned().map(|p| p.iter().rev().cloned().collect()).collect();
        Perm { name: name.or(self.name.as_ref().map(|x| iname(&x).to_string())), cp, ep }
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
        let ui = u.inverse(None);
        let u2 = u.double();
        let d = Perm::from_turn(&Turn::D);
        let di = d.inverse(None);
        let d2 = d.double();
        let f = Perm::from_turn(&Turn::F);
        let fi = f.inverse(None);
        let f2 = f.double();
        let b = Perm::from_turn(&Turn::B);
        let bi = b.inverse(None);
        let b2 = b.double();
        let r = Perm::from_turn(&Turn::R);
        let ri = r.inverse(None);
        let r2 = r.double();
        let l = Perm::from_turn(&Turn::L);
        let li = l.inverse(None);
        let l2 = l.double();

        if gen == 4 {
            let set = [
                "R", "R'", "L", "L'",
            ];
            for &perm in set.iter() {
                turns.insert(perm, Perm::from_scramble(perm));
            };
        } else {
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
        }

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
            turns.get(&cap[1]).unwrap_or(&perm).clone()
        }).collect();

        Perm::join(perms)
    }

    fn clone(&self) -> Perm {
        Perm {
            name: self.name.clone(),
            cp: self.cp.to_vec(),
            ep: self.ep.to_vec(),
        }
    }
}

#[derive(Debug,Hash,Eq,PartialEq)]
struct Turn {
    name: &'static str,
    cp: CornerTurn,
    ep: EdgeTurn,
}

fn iname(name: &str) -> &'static str {
    match name {
        "U" => "U'",
        "D" => "D'",
        "R" => "R'",
        "L" => "L'",
        "F" => "F'",
        "B" => "B'",
        "U'" => "U",
        "D'" => "D",
        "R'" => "R",
        "L'" => "L",
        "F'" => "F",
        "B'" => "B",
        "U2" => "U2",
        "D2" => "D2",
        "R2" => "R2",
        "L2" => "L2",
        "F2" => "F2",
        "B2" => "B2",
        _ => "N/A",
    }
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
                self.edge[is.1[i]] != Cube::SOLVED.edge[is.1[i]] {
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
        let sources: Vec<EdgeSticker>  = (0..l).map(|i| {
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

    fn inv_apply(&mut self, p: &Perm) {
        let p = p.inverse(None);
        let mut cm = [(0, 0); 24];
        for i in 0..8 {
            let c = self.corner[i] as usize;
            cm[c] = (i, 0);
            cm[Cube::CT[1][c] as usize] = (i, 1);
            cm[Cube::CT[2][c] as usize] = (i, 2);
        }
        let l = p.cp.len();
        for i in 0..l {
            let cp = &p.cp[i];
            let l = cp.len();
            let next: Vec<(CornerPosition, CornerSticker)> = (0..l).map(|i| {
                let j = (i + l - 1) % l;
                let pi = cm[cp[i] as usize];
                let pj = cm[cp[j] as usize];
                let sj = Cube::CT[((pj.1 + 3 - pi.1) % 3) as usize][self.corner[pj.0] as usize];

                (pi.0, sj)
            }).collect();
            for i in 0..l {
                self.corner[next[i].0] = next[i].1;
            }
        }

        let mut em = [(0, 0); 24];
        for i in 0..12 {
            let e = self.edge[i] as usize;
            em[e] = (i, 0);
            em[Cube::EF[1][e] as usize] = (i, 1);
        }
        let l = p.ep.len();
        for i in 0..l {
            let ep = &p.ep[i];
            let l = ep.len();
            let next: Vec<(EdgePosition, EdgeSticker)> = (0..l).map(|i| {
                let j = (i + l - 1) % l;
                let pi = em[ep[i] as usize];
                let pj = em[ep[j] as usize];
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
                let mut cp = vec![s1];
                while s1 != s2 {
                    let p = Perm {name: None, cp: vec![vec![s1, s2]], ep: vec![]};
                    cube.apply(&p);
                    cp.push(s2);
                    s2 = cube.corner[i];
                }
                if cp.len() > 1 {
                    perm.cp.push(cp);
                }
            }
            for i in 0..12 {
                let s1 = Cube::SOLVED.edge[i];
                let mut s2 = cube.edge[i];
                let mut ep = vec![s1];
                while s1 != s2 {
                    let p = Perm {name: None, ep: vec![vec![s1, s2]], cp: vec![]};
                    cube.apply(&p);
                    ep.push(s2);
                    s2 = cube.edge[i];
                }
                if ep.len() > 1 {
                    perm.ep.push(ep);
                }
            }
        }
        perm.inverse(Some(name.to_string()))
    }

    fn eo(&self) -> u8 {
        let cube = &self;
        let mut eo = 0;
        for &e in cube.edge.iter() {
            eo += Cube::EP[e as usize].1;
        }
        eo
    }

    fn co(&self) -> u8 {
        let cube = &self;
        let mut co = 0;
        for &c in cube.corner.iter() {
            co += if Cube::CP[c as usize].1 > 0 { 1 } else { 0 };
        }
        co
    }

    fn be(&self) -> u8 {
        (4..8).map(|ep| [1,1,1,1, 0,1,0,1, 1,1,1,1, 0,1,0,1, 1,1,1,1, 1,1,1,1,][self.edge[ep] as usize]).sum()
    }

    fn trigger(&self) -> bool {
        let table = [
            (
            [(0, 1), (3, 2), (4, 1), (7, 2)],
            [(4, 1), (5, 0), (6, 0), (7, 1), (0, 0), (8, 0)],
            ),
            (
            [(1, 2), (2, 1), (5, 2), (6, 1)],
            [(4, 0), (5, 1), (6, 1), (7, 0), (2, 0), (10, 0)],
            ),
        ];
        table.iter().any(|pat| {
            pat.0.iter().all(|cp| {
                Cube::CP[self.corner[cp.0] as usize].1 == cp.1
            }) &&
            pat.1.iter().all(|ep| {
                [1,1,1,1, 0,1,0,1, 1,1,1,1, 0,1,0,1, 1,1,1,1, 1,1,1,1,][self.edge[ep.0] as usize] == ep.1
            })
        })
    }

    fn htr(&self) -> bool {
        if self.co() > 0 {
            false
        } else {
            self.be() == 0 && {
                let be1: u8 = [1, 3, 9, 11].iter().map(|&i| [1,0,1,0, 1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1, 1,0,1,0,][self.edge[i] as usize]).sum();
                be1 == 0 && {
                    let be2: u8 = [0, 2, 8, 10].iter().map(|&i| [0,1,0,1, 1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1, 0,1,0,1,][self.edge[i] as usize]).sum();
                    be2 == 0
                }
            } && self.cp()
        }
    }

    fn cp(&self) -> bool {
        let mut c = self.clone();
        let t = [
            ["", "NA", "U2", "NA", "R2", "NA", "F2", "NA"],
            ["NA", "", "NA", "B2 L2", "NA", "L2", "NA", "D2 L2"],
            ["NA", "NA", "", "NA", "B2", "NA", "D2 B2", "NA"],
            ["NA", "NA", "NA", "", "NA", "NA", "NA", "NA"],
            ["NA", "NA", "NA", "NA", "", "NA", "D2", "NA"],
            ["NA", "NA", "NA", "NA", "NA", "", "NA", "NA"],
            ["NA", "NA", "NA", "NA", "NA", "NA", "", "NA"],
        ];
        for i in 0..7 {
            let p = (i..7).find(|&p| c.corner[p] == Cube::SOLVED.corner[i]);
            match p {
                None => { return false },
                Some(p) => {
                    match t[i][p] {
                        "" => { continue; },
                        "NA" => { return false },
                        a => { c.apply(&Perm::from_scramble(a)); }
                    };
                }
            };
        }
        true
    }

    fn solve<'a>(&self, initial: (Vec<&'a Perm>, Vec<&'a Perm>), base_turns: &'a [&Vec<Perm>; 5], limit: &mut usize, ip: usize) -> Vec<Solution<'a>> {
        let mut solutions: Vec<Solution<'a>> = Vec::new();
        let mut perms: Vec<Solution<'a>> = vec![(initial.0.to_vec(), initial.1.to_vec(), self.clone(), ip, if ip == 0 { self.eo() } else { self.co() })];
        let mut tick = 0;
        let mut nl = false;
        let p2g = [0, 1, 2, 4, 2, 3];
        if self.solved() {
            solutions.push(perms.last().unwrap().clone());
        }
        while solutions.len() == 0 {
            let mut nexts: Vec<Solution<'a>> = Vec::new();
            if perms.len() == 0 {
                break;
            }
            for (turns, inv_turns, cube, phase, no) in &perms {
                let mut phase = *phase;
                let mut no = *no;
                let len = turns.len() + inv_turns.len();
                if len >= *limit {
                    continue;
                }
                if phase == 0 {
                    if no == 0 {
                        phase = 1;
                        crif(&mut nl);
                        println!("{} ({})", p2s(turns), i2s(inv_turns));
                        println!("EO!");
                        no = cube.co();
                    }
                }
                if phase == 1 {
                    let be = cube.be();
                    if no == 0 && be == 0 {
                        phase = 4;
                        crif(&mut nl);
                        println!("{} ({})", p2s(turns), i2s(inv_turns));
                        println!("DR!");
                    } else if
                        // no == 3 && (be == 1 || be == 2) ||
                        no == 4 && be == 2
                    {
                        phase = 2;
                        crif(&mut nl);
                        println!("{} ({})", p2s(turns), i2s(inv_turns));
                        println!("Phase 2");
                    }
                }
                if phase == 2 {
                    if cube.trigger() {
                        phase = 3;
                        crif(&mut nl);
                        println!("{} ({})", p2s(turns), i2s(inv_turns));
                        println!("DR Trigger!");
                    } else {
                    }
                }
                if phase == 3 {
                    if cube.be() == 0 && no == 0 {
                        phase = 4;
                        crif(&mut nl);
                        println!("{} ({})", p2s(turns), i2s(inv_turns));
                        println!("DR!");
                    } else {
                    }
                }
                if phase == 4 && cube.htr() {
                    phase = 5;
                    crif(&mut nl);
                    println!("{} ({})", p2s(turns), i2s(inv_turns));
                    println!("HTR!");
                }
                /*
                if gen == 3 && cube.cp() {
                    gen = 4;
                    crif(&mut nl);
                    println!("{} ({})", p2s(turns), i2s(inv_turns));
                    println!("HTR-CP!");
                }
                */
                if len >= 8 && phase == 0 {
                    crif(&mut nl);
                    println!("Too long to EO!");
                    continue;
                }
                /*
                if len >= 30 {
                    continue;
                }
                */
                let mut bf = false;
                let gen = p2g[phase];
                for turn in base_turns[gen] {
                    for &inv in {
                        if phase == 5 { vec![false] } else { vec![false, true] }
                    }.iter() {
                        if len + 1 >= *limit {
                            continue;
                        }
                        let the_turns = if inv { inv_turns } else { turns };
                        let k1 = turn.kind().unwrap();
                        if cube.face_solved(&k1.0) {
                            continue;
                        }
                        match the_turns.last().map(|t| t.kind()).flatten() {
                            Some(k2) => {
                                if k1.0 == k2.0 { continue; };
                                if the_turns.len() > 1 && k1.1 == k2.1 {
                                    let k2 = the_turns[the_turns.len() - 2].kind();
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
                        let bo = if phase == 0 { cube.eo() } else { cube.co() };
                        if inv {
                            cube.inv_apply(&turn);
                        } else {
                            cube.apply(&turn);
                        }
                        let ao = if phase == 0 { cube.eo() } else { cube.co() };
                        let mut next_turns: Vec<&'a Perm> = the_turns.to_vec();
                        assert!(phase == 2 && (ao == bo) || phase != 2, "{} {}", turn, gen);
                        if
                            phase == 0 && (ao <= bo || ao <= 4) ||
                            // phase == 1 && (ao <= bo || ao <= 4) ||
                            phase >= 1
                        {
                            next_turns.push(turn);
                            let solved = cube.solved();
                            let skeleton = if inv {
                                (turns.to_vec(), next_turns.to_vec(), cube.clone(), phase, ao)
                            } else {
                                (next_turns.to_vec(), inv_turns.to_vec(), cube.clone(), phase, ao)
                            };
                            if solved {
                                let len = skeleton.0.len() + skeleton.1.len();
                                if len < *limit {
                                    *limit = len;
                                    crif(&mut nl);
                                    println!("limit: {}", limit);
                                }
                                crif(&mut nl);
                                println!("Solution: {} ({})        \r", p2s(&skeleton.0), i2s(&skeleton.1));
                                solutions.push(skeleton);
                                if phase >= 5 {
                                    bf = true;
                                    break;
                                }
                            } else {
                                if
                                    (phase == 0 && ao == 0) || // become EO
                                    (phase == 1 && ao == 4 && cube.be() == 2) || // become phase2
                                    (phase == 2 && cube.trigger()) || // become trigger
                                    ((phase == 1 || phase == 3) && ao == 0 && cube.be() == 0) || // become DR
                                    // (phase == 5) || // HTR
                                    (phase == 4 && cube.htr()) || // become HTR
                                    false
                                {
                                    let l = (skeleton.0, skeleton.1);
                                    let mut sols = cube.solve(l, base_turns, limit, phase);
                                    solutions.append(&mut sols);
                                } else {
                                    nexts.push(skeleton);
                                }
                            }
                        }
                        tick += 1;
                        if tick % 100 == 0 {
                            print!("{} ({})                    \r", p2s(&nexts.last().unwrap().0), i2s(&nexts.last().unwrap().1));
                            nl = true;
                        }
                    }
                    if bf {
                        break;
                    }
                }
            }
            perms = nexts
        };

        solutions
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

fn i2s(turns: &Vec<&Perm>) -> String {
    turns.iter().map(|t| t.name.clone().unwrap_or_default()).collect::<Vec<String>>().join(" ")
}

fn crif(nl: &mut bool) {
    if *nl {
        print!("                                                                                 \r");
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
    let mut cube = Cube::SOLVED;
    cube.apply(&Perm::from_scramble("U"));
    cube.apply(&Perm::from_scramble("D"));
    cube.apply(&Perm::from_scramble("F"));
    cube.apply(&Perm::from_scramble("F'"));
    cube.inv_apply(&Perm::from_turn(&Turn::U));
    cube.inv_apply(&Perm::from_turn(&Turn::D));
    assert!(cube.solved(), "{:?}", cube);

    let mut cube: Cube = Cube::SOLVED;
    cube.apply(&Perm::from_scramble("R U R'"));
    assert_eq!(cube.co(), 3, "{:?}", cube);

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

    let mut single_turns: Vec<Perm> = Perm::turns(0).values().cloned().collect();
    let mut eo_turns: Vec<Perm> = Perm::turns(1).values().cloned().collect();
    let mut dr_turns: Vec<Perm> = Perm::turns(2).values().cloned().collect();
    let mut htr_turns: Vec<Perm> = Perm::turns(3).values().cloned().collect();
    let mut slice_turns: Vec<Perm> = Perm::turns(4).values().cloned().collect();
    fn keyf(p: &Perm) -> Option<String> { p.name.clone() };
    single_turns.sort_by_key(keyf);
    single_turns.reverse();
    eo_turns.sort_by_key(keyf);
    dr_turns.sort_by_key(keyf);
    htr_turns.sort_by_key(keyf);
    slice_turns.sort_by_key(keyf);
    let base_turns = [&single_turns, &eo_turns, &dr_turns, &htr_turns, &slice_turns];

    cube.turn(&Turn::R);
    assert!(cube.trigger(), "{} {}", &cube.co(), &cube.be());
    cube.turn(&Turn::RI);

    // for seq in sequences {
    //     solve(&mut cube, &seq, &base_turns);
    // }
    loop {
        for i in 0..(sequences.len()) {
            println!("{}: {}", i, (&sequences)[i].name.as_ref().unwrap());
        }
        use std::io::{stdin,stdout,Write};
        let mut s=String::new();
        print!("Please enter a number or scramble: ");
        let _=stdout().flush();
        stdin().read_line(&mut s).expect("Did not enter a correct string");
        let n = s.chars().next().unwrap();
        let seq = match n.to_digit(10) {
            Some(i) => (&sequences)[i as usize].clone(),
            _ => Perm::from_scramble(&s)
        };
        solve(&mut cube, &seq, &base_turns);
    }
}

fn solve(cube: &mut Cube, seq: &Perm, base_turns: &[&Vec<Perm>; 5]) {
    println!("sequence: {:?}", seq);

    cube.apply(seq);
    println!("initial state: {}", cube);

    let initial = (vec![], vec![]);
    let now = Instant::now();
    let solutions = &cube.solve(initial, &base_turns, &mut 99, 0);
    let elapsed = now.elapsed();
    let lens: Vec<usize> = solutions.iter().map(|s| s.0.len() + s.1.len()).collect();
    let i = (0..(lens.len())).min_by(|&i, &j| lens[i].cmp(&lens[j])).unwrap();
    let solution = &solutions[i];

    let mut st: Vec<Perm> = solution.0.iter().map(|&x| x.clone()).collect();
    let inv = &mut solution.1.iter().rev().cloned().map(|x| x.inverse(None)).collect();
    st.append(inv);

    let sol = Perm::join(st);
    cube.apply(&sol);
    assert!(*cube == Cube::SOLVED);
    println!("Solved by {} in {}s!\n\n", sol, elapsed.as_secs_f32());
}
