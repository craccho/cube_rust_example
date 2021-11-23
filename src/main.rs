use memoize::memoize;
use rand::seq::SliceRandom;
use rand::thread_rng;
use regex::Regex;
use std::collections::HashMap;
use std::collections::LinkedList;
use std::fmt;
use std::time::Instant;

type CornerPosition = usize;
type EdgePosition = usize;
type CornerSticker = u8;
type EdgeSticker = u8;
type CornerTwist = u8;
type EdgeFlip = u8;
type Corner = [CornerSticker; 8];
type Edge = [EdgeSticker; 12];

#[derive(Clone, Debug, PartialEq, Eq)]
struct Node<T> {
    elem: T,
    prev: Option<Box<Node<T>>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct List<T> {
    tail: Option<Box<Node<T>>>,
    size: usize,
    vec: Vec<T>,
}

impl<T: Clone> List<T> {
    fn new() -> Self {
        List {
            tail: None,
            size: 0,
            vec: vec![],
        }
    }

    fn clone(&self) -> Self {
        List {
            tail: self.tail.clone(),
            size: self.size,
            vec: self.vec.clone(),
        }
    }

    fn pushed(&self, elem: &T) -> Self {
        let mut vec = self.vec.clone();
        vec.push(elem.clone());
        List {
            tail: Some(Box::new(Node {
                elem: elem.clone(),
                prev: self.tail.clone(),
            })),
            size: self.size + 1,
            vec: vec,
        }
    }

    fn peek(&self, i: usize) -> Option<&T> {
        if i >= self.size {
            return None;
        } else {
            if i == 0 {
                return self.tail.as_ref().map(|node| &node.elem);
            } else {
                return self.peek(i - 1);
            }
        }
    }

    fn len(&self) -> usize {
        self.size
    }

    fn iter(&self) -> std::slice::Iter<T> {
        self.vec.iter()
    }
}

type Solution<'a> = (List<&'a Perm>, List<&'a Perm>, Cube, usize, u8);

#[derive(Clone)]
enum Rotation {
    X,
    Xi,
    Y,
    Yi,
    Z,
    Zi,
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
            4, 5, 6, 7, 20, 21, 22, 23, 9, 10, 11, 8, 0, 1, 2, 3, 19, 16, 17, 18, 12, 13, 14, 15,
        ][s as usize],
        Rotation::Xi => [
            12, 13, 14, 15, 0, 1, 2, 3, 11, 8, 9, 10, 20, 21, 22, 23, 17, 18, 19, 16, 4, 5, 6, 7,
        ][s as usize],
        Rotation::Y => [
            3, 0, 1, 2, 16, 17, 18, 19, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 21, 22, 23, 20,
        ][s as usize],
        Rotation::Yi => [
            1, 2, 3, 0, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 4, 5, 6, 7, 23, 20, 21, 22,
        ][s as usize],
        Rotation::Z => [
            8, 9, 10, 11, 7, 4, 5, 6, 20, 21, 22, 23, 13, 14, 15, 12, 0, 1, 2, 3, 16, 17, 18, 19,
        ][s as usize],
        Rotation::Zi => [
            16, 17, 18, 19, 5, 6, 7, 4, 0, 1, 2, 3, 15, 12, 13, 14, 20, 21, 22, 23, 8, 9, 10, 11,
        ][s as usize],
    }
}

#[derive(PartialEq, Clone, Debug)]
enum Surface {
    U,
    D,
    F,
    B,
    R,
    L,
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

#[derive(PartialEq, Clone, Debug)]
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
        self.rotation()
            .iter()
            .rev()
            .cloned()
            .map(Rotation::inverse)
            .collect()
    }
}

#[derive(PartialEq, Clone, Debug)]
struct Cube {
    corner: Corner,
    edge: Edge,
    orientation: Orientation,
}

type CornerTurn = [(CornerPosition, CornerTwist); 4];
type EdgeTurn = [(EdgePosition, EdgeFlip); 4];

type CornerPerm = Vec<Vec<CornerSticker>>;
type EdgePerm = Vec<Vec<EdgeSticker>>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Perm {
    name: Option<String>,
    cp: CornerPerm,
    ep: EdgePerm,
}

fn inverse_perm(cp: &Vec<Vec<u8>>) -> Vec<Vec<u8>> {
    return cp
        .iter()
        .rev()
        .map(|p| p.iter().rev().cloned().collect())
        .collect();
}

#[memoize]
fn inverse(perm: Perm, name: Option<String>) -> Perm {
    let cp = inverse_perm(&perm.cp);
    let ep = inverse_perm(&perm.ep);
    Perm {
        name: name.or(perm.name.as_ref().map(|x| iname(&x).to_string())),
        cp,
        ep,
    }
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
        inverse(self.clone(), name)
    }

    fn double(&self) -> Perm {
        let mut cube = Cube::SOLVED;
        let name = self
            .name
            .as_ref()
            .map(|name| format!("{}2", name))
            .unwrap_or_default();

        cube.apply(self);
        cube.apply(self);
        cube.perm(&name)
    }

    fn join(perms: Vec<&Perm>) -> Perm {
        let mut cube = Cube::SOLVED;
        let name = perms
            .iter()
            .flat_map(|p| p.name.as_ref().map(|s| s.to_string()))
            .collect::<Vec<_>>()
            .join(" ");

        for perm in perms {
            cube.apply(perm);
        }
        cube.perm(name.as_str())
    }

    fn from_turn(turn: &Turn) -> Perm {
        let mut cube = Cube::SOLVED.clone();

        cube.turn(turn);
        cube.perm(turn.name)
    }

    fn generate_turns<'a>() -> HashMap<String, Perm> {
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
        let f2b2 = Perm::join(vec![&f2, &b2]);
        let r2l2 = Perm::join(vec![&r2, &l2]);

        turns.insert("F".to_string(), f);
        turns.insert("F'".to_string(), fi);
        turns.insert("B".to_string(), b);
        turns.insert("B'".to_string(), bi);
        turns.insert("R".to_string(), r);
        turns.insert("R'".to_string(), ri);
        turns.insert("L".to_string(), l);
        turns.insert("L'".to_string(), li);
        turns.insert("U".to_string(), u);
        turns.insert("U'".to_string(), ui);
        turns.insert("D".to_string(), d);
        turns.insert("D'".to_string(), di);
        turns.insert("U2".to_string(), u2);
        turns.insert("D2".to_string(), d2);
        turns.insert("R2".to_string(), r2);
        turns.insert("L2".to_string(), l2);
        turns.insert("F2".to_string(), f2);
        turns.insert("B2".to_string(), b2);
        turns.insert("F2B2".to_string(), f2b2);
        turns.insert("R2L2".to_string(), r2l2);

        turns
    }

    fn turns<'a>(gen: u8, turns_h: &'a HashMap<String, Perm>) -> Vec<&'a Perm> {
        let mut turns_ = Vec::new();
        let mut register = |set: &[&str]| {
            for &perm in set.iter() {
                turns_.push(turns_h.get(perm).unwrap());
            }
        };
        if gen == 6 {
            register(&["U", "U'", "U2", "D", "D'", "D2", "F2B2", "R2L2"]);
        } else if gen == 5 {
            register(&["U", "U'", "U2", "D", "D'", "D2"]);
        } else if gen == 4 {
            register(&["R", "L'", "L", "R'"]);
        } else {
            if gen < 3 {
                if gen < 2 {
                    if gen < 1 {
                        register(&["F", "B'", "B", "F'"]);
                    }
                    register(&["R", "L'", "L", "R'"]);
                }
                register(&["U", "D'", "D", "U'"]);
            }
            register(&["U2", "R2", "D2", "F2", "L2", "B2"]);
        }

        turns_
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
                _ => None,
            },
            None => None,
        }
    }

    fn from_scramble<'a>(scramble: &'a str, solver: &Solver) -> Perm {
        let re = Regex::new(r"([UDFBRL]['2]?)").unwrap();

        let perms: Vec<&Perm> = re
            .captures_iter(scramble)
            .map(|cap| solver.get_turn(cap[1].to_string()))
            .collect();

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

#[derive(Debug, Hash, Eq, PartialEq)]
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
        cp: [(7, 1), (0, 2), (3, 1), (4, 2)],
        ep: [(4, 0), (0, 0), (7, 0), (8, 0)],
    };
    const RI: Turn = Turn::R.inverse("R'");

    const L: Turn = Turn {
        name: "L",
        cp: [(2, 2), (1, 1), (6, 2), (5, 1)],
        ep: [(6, 0), (2, 0), (5, 0), (10, 0)],
    };
    const LI: Turn = Turn::L.inverse("L'");

    const U: Turn = Turn {
        name: "U",
        cp: [(1, 0), (2, 0), (3, 0), (0, 0)],
        ep: [(3, 0), (0, 0), (1, 0), (2, 0)],
    };
    const UI: Turn = Turn::U.inverse("U'");

    const D: Turn = Turn {
        name: "D",
        cp: [(7, 0), (4, 0), (5, 0), (6, 0)],
        ep: [(11, 0), (8, 0), (9, 0), (10, 0)],
    };
    const DI: Turn = Turn::D.inverse("D'");

    const F: Turn = Turn {
        name: "F",
        cp: [(1, 2), (0, 1), (7, 2), (6, 1)],
        ep: [(5, 1), (1, 1), (4, 1), (11, 1)],
    };
    const FI: Turn = Turn::F.inverse("F'");

    const B: Turn = Turn {
        name: "B",
        cp: [(3, 2), (2, 1), (5, 2), (4, 1)],
        ep: [(6, 1), (9, 1), (7, 1), (3, 1)],
    };
    const BI: Turn = Turn::B.inverse("B'");
}

type Perms<'a> = List<&'a Perm>;

#[derive(Clone)]
struct SolveState<'a> {
    phase: usize,
    eo_or_co: u8,
    turns: Perms<'a>,
    inv_turns: Perms<'a>,
    cube: Cube,
}

impl Cube {
    const SOLVED: Cube = Cube {
        corner: [0, 1, 2, 3, 20, 21, 22, 23],
        edge: [0, 1, 2, 3, 4, 6, 12, 14, 20, 21, 22, 23],
        orientation: Orientation {
            u: Surface::U,
            f: Surface::F,
        },
    };
    const CT: [[CornerSticker; 24]; 3] = [
        [
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
        ],
        [
            18, 6, 10, 14, 17, 22, 11, 0, 5, 21, 15, 1, 9, 20, 19, 2, 13, 23, 7, 3, 16, 12, 8, 4,
        ],
        [
            7, 11, 15, 19, 23, 8, 1, 18, 22, 12, 2, 6, 21, 16, 3, 10, 20, 4, 0, 14, 13, 9, 5, 17,
        ],
    ];
    const CP: [(CornerPosition, CornerTwist); 24] = [
        (0, 0),
        (1, 0),
        (2, 0),
        (3, 0),
        (7, 1),
        (6, 2),
        (1, 1),
        (0, 2),
        (6, 1),
        (5, 2),
        (2, 1),
        (1, 2),
        (5, 1),
        (4, 2),
        (3, 1),
        (2, 2),
        (4, 1),
        (7, 2),
        (0, 1),
        (3, 2),
        (4, 0),
        (5, 0),
        (6, 0),
        (7, 0),
    ];
    const EF: [[EdgeSticker; 24]; 2] = [
        [
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
        ],
        [
            19, 7, 11, 15, 18, 23, 8, 1, 6, 22, 12, 2, 10, 21, 16, 3, 14, 20, 4, 0, 17, 13, 9, 5,
        ],
    ];
    const EP: [(EdgePosition, EdgeFlip); 24] = [
        (0, 0),
        (1, 0),
        (2, 0),
        (3, 0),
        (4, 0),
        (11, 1),
        (5, 0),
        (1, 1),
        (5, 1),
        (10, 1),
        (6, 1),
        (2, 1),
        (6, 0),
        (9, 1),
        (7, 0),
        (3, 1),
        (7, 1),
        (8, 1),
        (4, 1),
        (0, 1),
        (8, 0),
        (9, 0),
        (10, 0),
        (11, 0),
    ];

    const HTRC: [[&'static str; 8]; 7] = [
        ["", "NA", "U2", "NA", "R2", "NA", "F2", "NA"],
        ["NA", "", "NA", "B2 L2", "NA", "L2", "NA", "D2 L2"],
        ["NA", "NA", "", "NA", "B2", "NA", "D2 B2", "NA"],
        ["NA", "NA", "NA", "", "NA", "NA", "NA", "NA"],
        ["NA", "NA", "NA", "NA", "", "NA", "D2", "NA"],
        ["NA", "NA", "NA", "NA", "NA", "", "NA", "NA"],
        ["NA", "NA", "NA", "NA", "NA", "NA", "", "NA"],
    ];

    fn face_solved(&self, s: &Surface) -> bool {
        let is = match s {
            Surface::U => ([0, 1, 2, 3], [0, 1, 2, 3]),
            Surface::D => ([4, 5, 6, 7], [8, 9, 10, 11]),
            Surface::R => ([0, 3, 4, 7], [0, 4, 7, 8]),
            Surface::L => ([1, 2, 5, 6], [2, 5, 6, 10]),
            Surface::F => ([0, 1, 6, 7], [1, 4, 5, 11]),
            Surface::B => ([2, 3, 4, 5], [3, 6, 7, 9]),
        };
        for i in 0..4 {
            if self.corner[is.0[i]] != Cube::SOLVED.corner[is.0[i]]
                || self.edge[is.1[i]] != Cube::SOLVED.edge[is.1[i]]
            {
                return false;
            }
        }

        true
    }

    fn turn(&mut self, t: &Turn) {
        let l = t.cp.len();
        let sources: Vec<u8> = (0..l)
            .map(|i| {
                let j = (i + l - 1) % l;
                Cube::CT[t.cp[j].1 as usize][self.corner[t.cp[j].0 as usize] as usize]
            })
            .collect();
        for i in 0..l {
            self.corner[t.cp[i].0 as usize] = sources[i];
        }
        let l = t.ep.len();
        let sources: Vec<EdgeSticker> = (0..l)
            .map(|i| {
                let j = (i + l - 1) % l;
                Cube::EF[t.ep[j].1 as usize][self.edge[t.ep[j].0 as usize] as usize]
            })
            .collect();
        for i in 0..l {
            self.edge[t.ep[i].0 as usize] = sources[i];
        }
    }

    fn apply(&mut self, p: &Perm) {
        let l = p.cp.len();
        for i in 0..l {
            let cp = &p.cp[i];
            let l = cp.len();
            let next: Vec<(CornerPosition, CornerSticker)> = (0..l)
                .map(|i| {
                    let j = (i + l - 1) % l;
                    let pi = Cube::CP[cp[i] as usize];
                    let pj = Cube::CP[cp[j] as usize];
                    let sj = Cube::CT[((pj.1 + 3 - pi.1) % 3) as usize][self.corner[pj.0] as usize];

                    (pi.0, sj)
                })
                .collect();
            for i in 0..l {
                self.corner[next[i].0] = next[i].1;
            }
        }

        let l = p.ep.len();
        for i in 0..l {
            let ep = &p.ep[i];
            let l = ep.len();
            let next: Vec<(EdgePosition, EdgeSticker)> = (0..l)
                .map(|i| {
                    let j = (i + l - 1) % l;
                    let pi = Cube::EP[ep[i] as usize];
                    let pj = Cube::EP[ep[j] as usize];
                    let sj = Cube::EF[((pj.1 + 2 - pi.1) % 2) as usize][self.edge[pj.0] as usize];

                    (pi.0, sj)
                })
                .collect();
            for i in 0..l {
                self.edge[next[i].0] = next[i].1
            }
        }
    }

    fn inv_apply(&mut self, p: &Perm) {
        // let p = p.inverse(None);
        let mut cm = [(0, 0); 24];
        for i in 0..8 {
            let c = self.corner[i] as usize;
            cm[c] = (i, 0);
            cm[Cube::CT[1][c] as usize] = (i, 1);
            cm[Cube::CT[2][c] as usize] = (i, 2);
        }
        let l = p.cp.len();
        for ii in 0..l {
            let i = l - ii - 1;
            let cp = &p.cp[i];
            let l = cp.len();
            let next: Vec<(CornerPosition, CornerSticker)> = (0..l)
                .map(|i| {
                    let j = (i + l + 1) % l;
                    let pi = cm[cp[i] as usize];
                    let pj = cm[cp[j] as usize];
                    let sj = Cube::CT[((pj.1 + 3 - pi.1) % 3) as usize][self.corner[pj.0] as usize];

                    (pi.0, sj)
                })
                .collect();
            for ii in 0..l {
                let i = l - ii - 1;
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
        for ii in 0..l {
            let i = l - ii - 1;
            let ep = &p.ep[i];
            let l = ep.len();
            let next: Vec<(EdgePosition, EdgeSticker)> = (0..l)
                .map(|i| {
                    let j = (i + l + 1) % l;
                    let pi = em[ep[i] as usize];
                    let pj = em[ep[j] as usize];
                    let sj = Cube::EF[((pj.1 + 2 - pi.1) % 2) as usize][self.edge[pj.0] as usize];

                    (pi.0, sj)
                })
                .collect();
            for ii in 0..l {
                let i = l - ii - 1;
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
                    let p = Perm {
                        name: None,
                        cp: vec![vec![s1, s2]],
                        ep: vec![],
                    };
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
                    let p = Perm {
                        name: None,
                        ep: vec![vec![s1, s2]],
                        cp: vec![],
                    };
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
        (4..8)
            .map(|ep| {
                [
                    1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                ][self.edge[ep] as usize]
            })
            .sum()
    }

    fn pre_trigger(&self, solver: &Solver) -> bool {
        let u_table = &solver.u_table;
        let d_table = &solver.d_table;
        let e_table = &solver.e_table;
        [u_table, d_table, e_table].iter().all(|table| {
            table.iter().any(|pat| {
                pat.0
                    .iter()
                    .all(|cp| Cube::CP[self.corner[cp.0] as usize].1 == cp.1)
                    && pat.1.iter().all(|ep| {
                        [
                            1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                        ][self.edge[ep.0] as usize]
                            == ep.1
                    })
            })
        })
    }
    const TRIGGER_TABLE: [([(usize, u8); 4], [(usize, i32); 6]); 2] = [
        (
            [(0, 1), (3, 2), (4, 1), (7, 2)],
            [(4, 1), (5, 0), (6, 0), (7, 1), (0, 0), (8, 0)],
        ),
        (
            [(1, 2), (2, 1), (5, 2), (6, 1)],
            [(4, 0), (5, 1), (6, 1), (7, 0), (2, 0), (10, 0)],
        ),
    ];
    fn trigger(&self) -> bool {
        Cube::TRIGGER_TABLE.iter().any(|pat| {
            pat.0
                .iter()
                .all(|cp| Cube::CP[self.corner[cp.0] as usize].1 == cp.1)
                && pat.1.iter().all(|ep| {
                    [
                        1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                    ][self.edge[ep.0] as usize]
                        == ep.1
                })
        })
    }

    fn htr(&self, solver: &Solver) -> bool {
        if self.co() > 0 {
            false
        } else {
            self.be() == 0
                && {
                    let be1: u8 = [1, 3, 9, 11]
                        .iter()
                        .map(|&i| {
                            [
                                1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
                                1, 0,
                            ][self.edge[i] as usize]
                        })
                        .sum();
                    be1 == 0 && {
                        let be2: u8 = [0, 2, 8, 10]
                            .iter()
                            .map(|&i| {
                                [
                                    0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
                                    1, 0, 1,
                                ][self.edge[i] as usize]
                            })
                            .sum();
                        be2 == 0
                    }
                }
                && self.cp(solver)
        }
    }

    fn cp(&self, solver: &Solver) -> bool {
        let mut c = self.clone();
        for i in 0..7 {
            let p = (i..7).find(|&p| c.corner[p] == Cube::SOLVED.corner[i]);
            match p {
                None => return false,
                Some(p) => {
                    match Cube::HTRC[i][p] {
                        "" => {
                            continue;
                        }
                        "NA" => return false,
                        a => {
                            c.apply(solver.get_turn(a.to_string()));
                        }
                    };
                }
            };
        }
        true
    }

    fn determin_phase(
        &self,
        phase: &mut usize,
        ao: &mut u8,
        turns: &List<&Perm>,
        inv_turns: &List<&Perm>,
        nl: &mut bool,
        solver: &Solver,
    ) -> bool {
        let mut depth = false;
        if *phase == 0 {
            if *ao == 0 {
                *phase = 1;
                crif(nl);
                println!("{} ({})", p2s(&turns), i2s(&inv_turns));
                println!("EO!");
                *ao = self.co();
                depth = true;
            }
        }
        if *phase == 1 {
            let be = self.be();
            if *ao == 0 && be == 0 {
                *phase = 4;
                crif(nl);
                println!("{} ({})", p2s(&turns), i2s(&inv_turns));
                println!("DR!");
                depth = true;
            } else if
            // no == 3 && (be == 1 || be == 2) ||
            *ao == 4 && be == 2 {
                *phase = 2;
                crif(nl);
                println!("{} ({})", p2s(&turns), i2s(&inv_turns));
                println!("Phase 2");
                depth = true;
            }
        }
        if *phase == 2 {
            if self.pre_trigger(solver) {
                *phase = 6;
                crif(nl);
                println!("{} ({})", p2s(&turns), i2s(&inv_turns));
                println!("DR Pre-Trigger!");
                depth = true;
            }
        }
        if *phase == 6 {
            if self.trigger() {
                *phase = 3;
                crif(nl);
                println!("{} ({})", p2s(&turns), i2s(&inv_turns));
                println!("DR Trigger!");
                depth = true;
            }
        }
        if *phase == 3 {
            if self.be() == 0 && *ao == 0 {
                *phase = 4;
                crif(nl);
                println!("{} ({})", p2s(&turns), i2s(&inv_turns));
                println!("DR!");
                depth = true;
            }
        }
        if *phase == 4 && self.cp(solver) {
            if self.htr(solver) {
                *phase = 5;
                crif(nl);
                println!("{} ({})", p2s(&turns), i2s(&inv_turns));
                println!("HTR!");
                depth = true;
            } else {
                *phase = 7;
                crif(nl);
                println!("{} ({})", p2s(&turns), i2s(&inv_turns));
                println!("CP!");
                depth = true;
            }
        }
        if *phase == 7 {
            if self.htr(solver) {
                *phase = 5;
                crif(nl);
                println!("{} ({})", p2s(&turns), i2s(&inv_turns));
                println!("HTR!");
                depth = true;
            }
        }
        depth
    }

    fn solve<'a>(
        &self,
        initial: (List<&'a Perm>, List<&'a Perm>),
        solver: &'a Solver,
        limit: &mut usize,
        ip: usize,
    ) -> Vec<SolveState<'a>> {
        let mut solutions: Vec<SolveState<'a>> = Vec::new();
        let mut phase = ip;
        let mut nl = false;
        let mut bo = if phase == 0 { self.eo() } else { self.co() };
        self.determin_phase(&mut phase, &mut bo, &initial.0, &initial.1, &mut nl, solver);
        let mut perms: Vec<SolveState> = vec![SolveState {
            turns: initial.0,
            inv_turns: initial.1,
            cube: self.clone(),
            phase,
            eo_or_co: bo,
        }];
        let mut tick = 0;
        let p2g = [0, 1, 2, 4, 2, 3, 5, 6];
        if self.solved() {
            solutions.append(&mut perms);
        }
        let mut fc = 0;
        while solutions.len() == 0 {
            let mut nexts: Vec<SolveState> = Vec::new();
            if perms.len() == 0 {
                break;
            }
            for SolveState {
                turns,
                inv_turns,
                cube,
                phase,
                eo_or_co,
            } in &perms
            {
                let mut phase = *phase;
                let mut no = *eo_or_co;
                let len = turns.len() + inv_turns.len();
                let mut depth = false;

                let mut bf = false;
                let gen = p2g[phase];
                if fc > *limit {
                    break;
                }
                let mut ts: Vec<&Perm> = solver.base_turns[gen].clone();
                ts.shuffle(&mut thread_rng());
                for turn in &ts {
                    for &inv in {
                        if phase == 5 || phase == 7 {
                            vec![false]
                        } else {
                            vec![false, true]
                        }
                    }
                    .iter()
                    {
                        if len + 1 >= *limit || fc > *limit {
                            continue;
                        }
                        let (turns, inv_turns) = if inv {
                            (inv_turns, turns)
                        } else {
                            (turns, inv_turns)
                        };
                        let k1 = turn.kind().unwrap();
                        if cube.face_solved(&k1.0) {
                            continue;
                        }
                        match turns.peek(0).map(|t| t.kind()).flatten() {
                            Some(k2) => {
                                if k1.0 == k2.0 {
                                    continue;
                                };
                                if turns.len() > 1 && k1.1 == k2.1 {
                                    let k2 = turns.peek(1).unwrap().kind();
                                    match k2 {
                                        Some(k2) => {
                                            if k1.0 == k2.0 {
                                                continue;
                                            };
                                        }
                                        None => (),
                                    };
                                }
                            }
                            None => (),
                        };
                        if phase == 5 {
                            match inv_turns.peek(0).map(|t| t.kind()).flatten() {
                                Some(k2) => {
                                    if k1.0 == k2.0 {
                                        continue;
                                    };
                                    if inv_turns.len() > 1 && k1.1 == k2.1 {
                                        let k2 = inv_turns.peek(1).unwrap().kind();
                                        match k2 {
                                            Some(k2) => {
                                                if k1.0 == k2.0 {
                                                    continue;
                                                };
                                            }
                                            None => (),
                                        };
                                    }
                                }
                                None => (),
                            };
                        }
                        let mut cube = cube.clone();
                        let bo = no;
                        if inv {
                            cube.inv_apply(turn);
                        } else {
                            cube.apply(turn);
                        }
                        let mut ao = if phase == 0 { cube.eo() } else { cube.co() };

                        let applied_turns = turns.clone();
                        if phase == 0 && (ao <= bo || ao <= 4) ||
                            // phase == 1 && (ao <= bo || ao <= 4) ||
                            phase >= 1
                        {
                            let applied_turns = turns.pushed(turn);
                            let (turns, inv_turns) = if inv {
                                (inv_turns, &applied_turns)
                            } else {
                                (&applied_turns, inv_turns)
                            };
                            depth = cube.determin_phase(
                                &mut phase, &mut ao, &turns, &inv_turns, &mut nl, solver,
                            );
                            if len >= 8 && phase == 0 {
                                crif(&mut nl);
                                println!("Too long to EO!");
                                continue;
                            }
                            let solved = cube.solved();
                            let skeleton = SolveState {
                                turns: turns.clone(),
                                inv_turns: inv_turns.clone(),
                                cube: cube.clone(),
                                phase,
                                eo_or_co: ao,
                            };
                            if solved {
                                let len = skeleton.turns.len() + skeleton.inv_turns.len();
                                if len < *limit {
                                    *limit = len;
                                    crif(&mut nl);
                                    println!("limit: {}", limit);
                                }
                                crif(&mut nl);
                                println!(
                                    "Solution: {} ({})        \r",
                                    p2s(&skeleton.turns),
                                    i2s(&skeleton.inv_turns)
                                );
                                solutions.push(skeleton);
                                if phase == 5 {
                                    bf = true;
                                    break;
                                }
                            } else {
                                if depth {
                                    let mut sols = cube.solve(
                                        (skeleton.turns, skeleton.inv_turns),
                                        solver,
                                        limit,
                                        phase,
                                    );
                                    solutions.append(&mut sols);
                                    fc += 1;
                                    continue;
                                } else {
                                    nexts.push(skeleton);
                                }
                            }
                        }
                        tick += 1;
                        if tick % 100 == 0 {
                            print!(
                                "{} ({})                    \r",
                                p2s(&nexts.last().unwrap().turns),
                                i2s(&nexts.last().unwrap().inv_turns)
                            );
                            nl = true;
                        }
                    }
                    if bf {
                        break;
                    }
                }
            }
            perms = nexts
        }

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

fn p2s(turns: &List<&Perm>) -> String {
    turns
        .iter()
        .map(|t| t.name.clone().unwrap_or_default())
        .collect::<Vec<String>>()
        .join(" ")
}

fn i2s(turns: &List<&Perm>) -> String {
    turns
        .iter()
        .map(|t| t.name.clone().unwrap_or_default())
        .collect::<Vec<String>>()
        .join(" ")
}

fn crif(nl: &mut bool) {
    if *nl {
        print!(
            "                                                                                 \r"
        );
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
    let turns_h = Perm::generate_turns();
    let solver = &mut Solver::new(&turns_h);

    let mut cube = Cube::SOLVED;
    cube.apply(&Perm::from_scramble("U", solver));
    cube.apply(&Perm::from_scramble("D", solver));
    cube.apply(&Perm::from_scramble("F", solver));
    cube.apply(&Perm::from_scramble("F'", solver));
    cube.inv_apply(&Perm::from_turn(&Turn::U));
    cube.inv_apply(&Perm::from_turn(&Turn::D));
    assert!(cube.solved(), "{:?}", cube);

    let mut cube: Cube = Cube::SOLVED;
    cube.apply(&Perm::from_scramble("R U R'", solver));
    assert_eq!(cube.co(), 3, "{:?}", cube);

    let mut cube: Cube = Cube::SOLVED;
    let sequence = vec![Turn::R, Turn::U, Turn::RI, Turn::UI];
    let sequence2 = vec![Turn::L, Turn::B, Turn::R, Turn::U, Turn::F, Turn::D];
    let sequence3 = vec![Turn::DI, Turn::FI, Turn::UI, Turn::RI, Turn::BI, Turn::LI];
    let sequence4 = vec![
        Turn::R,
        Turn::RI,
        Turn::L,
        Turn::LI,
        Turn::U,
        Turn::UI,
        Turn::D,
        Turn::DI,
        Turn::F,
        Turn::FI,
        Turn::B,
        Turn::BI,
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
    let sequence6 = vec![Turn::R, Turn::U, Turn::U, Turn::DI, Turn::B, Turn::DI];
    let sequence7 = vec![Turn::R];
    let sc = Perm::from_scramble(
        "R' U' F R2 U R2 U F2 L2 D' R2 U B2 U' L' U F' L R2 D' F' L' R D' R' U' F",
        solver,
    );
    let sc2 = Perm::from_scramble("R", solver);
    let mut sequences: Vec<Perm> = vec![sequence, sequence2, sequence3, sequence6, sequence7]
        .iter()
        .map(|turns| {
            Perm::join(
                turns
                    .iter()
                    .map(|t| solver.get_turn(t.name.to_string()))
                    .collect(),
            )
        })
        .collect();
    sequences.push(sc);

    loop {
        for i in 0..(sequences.len()) {
            println!("{}: {}", i, (&sequences)[i].name.as_ref().unwrap());
        }
        use std::io::{stdin, stdout, Write};
        let mut s = String::new();
        print!("Please enter a number or scramble: ");
        let _ = stdout().flush();
        stdin()
            .read_line(&mut s)
            .expect("Did not enter a correct string");
        let n = s.chars().next().unwrap();
        let seq = match n.to_digit(10) {
            Some(i) => (&sequences)[i as usize].clone(),
            _ => Perm::from_scramble(&s, solver),
        };
        solver.solve(&mut cube, &seq);
    }
}

struct Solver<'a> {
    turns: HashMap<String, Perm>,
    base_turns: [Vec<&'a Perm>; 7],
    u_table: Vec<(Vec<(usize, u8)>, Vec<(usize, u8)>)>,
    d_table: Vec<(Vec<(usize, u8)>, Vec<(usize, u8)>)>,
    e_table: Vec<(Vec<(usize, u8)>, Vec<(usize, u8)>)>,
}

impl<'a> Solver<'a> {
    fn new(turns_h: &'a HashMap<String, Perm>) -> Self {
        let single_turns: Vec<&'a Perm> = Perm::turns(0, turns_h);
        let eo_turns: Vec<&'a Perm> = Perm::turns(1, turns_h);
        let dr_turns: Vec<&'a Perm> = Perm::turns(2, turns_h);
        let htr_turns: Vec<&'a Perm> = Perm::turns(3, turns_h);
        let slice_turns: Vec<&'a Perm> = Perm::turns(4, turns_h);
        let p6_turns: Vec<&'a Perm> = Perm::turns(5, turns_h);
        let cp_turns: Vec<&'a Perm> = Perm::turns(6, turns_h);
        let base_turns = [
            single_turns,
            eo_turns,
            dr_turns,
            htr_turns,
            slice_turns,
            p6_turns,
            cp_turns,
        ];

        let u_table = vec![
            (vec![(0, 1), (3, 2)], vec![(0, 0)]),
            (vec![(0, 2), (1, 1)], vec![(1, 0)]),
            (vec![(1, 2), (2, 1)], vec![(2, 0)]),
            (vec![(2, 2), (3, 1)], vec![(3, 0)]),
        ];
        let d_table = vec![
            (vec![(4, 1), (7, 2)], vec![(8, 0)]),
            (vec![(4, 2), (5, 1)], vec![(9, 0)]),
            (vec![(5, 2), (6, 1)], vec![(10, 0)]),
            (vec![(6, 2), (7, 1)], vec![(11, 0)]),
        ];
        let e_table = vec![
            (vec![], vec![(4, 1), (5, 0), (6, 0), (7, 1)]),
            (vec![], vec![(4, 0), (5, 1), (6, 1), (7, 0)]),
        ];
        let mut solver = Solver {
            turns: turns_h.clone(),
            base_turns,
            u_table,
            d_table,
            e_table,
        };
        solver.get_turn_mut("D2 L2".to_string());
        solver.get_turn_mut("B2 L2".to_string());
        solver.get_turn_mut("D2 B2".to_string());

        solver
    }

    fn get_turn(&self, turn: String) -> &Perm {
        self.turns.get(&turn).unwrap()
    }

    fn get_turn_mut(&mut self, turn: String) -> &Perm {
        if self.turns.contains_key(&turn) {
            self.turns.get(&turn).unwrap()
        } else {
            let p = Perm::from_scramble(&turn, &*self);
            self.turns.insert(turn.to_string(), p);
            self.get_turn_mut(turn)
        }
    }

    fn solve(&mut self, cube: &mut Cube, seq: &Perm) {
        println!("sequence: {:?}", seq);

        cube.apply(seq);
        println!("initial state: {}", cube);

        let initial = (List::new(), List::new());
        let now = Instant::now();
        let solutions = cube.solve(initial, self, &mut 40, 0);
        let elapsed = now.elapsed();
        let lens: Vec<usize> = solutions
            .iter()
            .map(|s| s.turns.len() + s.inv_turns.len())
            .collect();
        let i = (0..(lens.len()))
            .min_by(|&i, &j| lens[i].cmp(&lens[j]))
            .unwrap();
        let solution = solutions[i].clone();

        let mut st: Vec<&Perm> = solution.turns.iter().cloned().collect();
        let inv: &mut Vec<Perm> = &mut solution
            .inv_turns
            .iter()
            .rev()
            .map(|x| x.inverse(None))
            .collect();
        let inv: &mut Vec<&Perm> = &mut inv.iter().collect();
        st.append(inv);

        let sol = Perm::join(st);
        cube.apply(&sol);
        assert!(*cube == Cube::SOLVED);
        println!("Solved by {} in {}s!\n\n", sol, elapsed.as_secs_f32());
    }
}
