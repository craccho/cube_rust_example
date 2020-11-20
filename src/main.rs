use std::fmt;

type CornerTwist = [[u8; 24]; 3];
type EdgeFlip = [[u8; 24]; 2];
type Corner = [u8; 8];
type Edge = [u8; 12];

#[derive(PartialEq)]
struct Cube {
    corner: Corner,
    edge: Edge,
}
type CornerTurn = [[u8; 2]; 4];
type EdgeTurn = [[u8; 2]; 4];
#[derive(Clone)]
struct Turn {
    cp: CornerTurn,
    ep: EdgeTurn,
}

impl Turn {
    const fn inverse(&self) -> Turn {
        let cp = &self.cp;
        let ep = &self.ep;
        Turn {
            cp: [cp[3], cp[2], cp[1], cp[0]],
            ep: [ep[3], ep[2], ep[1], ep[0]],
        }
    }
}

impl Cube {
    const SOLVED: Cube = Cube {
        corner: [0,1,2,3,20,21,22,23],
        edge: [0,1,2,3,4,6,12,14,20,21,22,23],
    };
    const CT: CornerTwist =
        [
            [ 0, 1,  2,  3,  4,  5,  6, 7, 8,  9, 10,11,12, 13, 14,15, 16, 17,18,19, 20, 21,22,23],
            [18, 6, 10, 14, 17, 22, 11, 0, 5, 21, 15, 1, 9, 20, 19, 2, 13, 23, 7, 3, 16, 12, 8, 4],
            [ 7,11, 15, 19, 23,  8,  1,18,22, 12,  2, 6,21, 16,  3,10, 20,  4, 0,14, 13,  9, 5,17],
        ];
    const EF: EdgeFlip =
        [
            [ 0, 1,  2,  3,  4,  5, 6, 7, 8,  9, 10,11, 12, 13, 14,15, 16, 17,18,19, 20, 21,22,23],
            [19, 7, 11, 15, 18, 23, 8, 1, 6, 22, 12, 2, 10, 21, 16, 3, 14, 20, 4, 0, 17, 13, 9, 5],
        ];

    const R: Turn = Turn {
        cp: [[ 7, 1], [ 4, 2], [ 3, 1], [ 0, 2]],
        ep: [[ 4, 0], [ 8, 0], [ 7, 0], [ 0, 0]],
    };
    const RI: Turn = Cube::R.inverse();

    const L: Turn = Turn {
        cp: [[ 2, 2], [ 5, 1], [ 6, 2], [ 1, 1]],
        ep: [[ 6, 0], [10, 0], [ 5, 0], [ 2, 0]],
    };
    const LI: Turn = Cube::L.inverse();

    const U: Turn = Turn {
        cp: [[ 3, 0], [ 0, 0], [ 1, 0], [ 2, 0]],
        ep: [[ 3, 0], [ 2, 0], [ 1, 0], [ 0, 0]],
    };
    const UI: Turn = Cube::U.inverse();

    const D: Turn = Turn {
        cp: [[ 7, 0], [ 6, 0], [ 5, 0], [ 4, 0]],
        ep: [[11, 0], [10, 0], [ 9, 0], [ 8, 0]],
    };
    const DI: Turn = Cube::D.inverse();

    const F: Turn = Turn {
        cp: [[ 1, 2], [ 6, 1], [ 7, 2], [ 0, 1]],
        ep: [[ 5, 1], [11, 1], [ 4, 1], [ 1, 1]],
    };
    const FI: Turn = Cube::F.inverse();

    const B: Turn  = Turn {
        cp: [[ 3, 2], [ 4, 1], [ 5, 2], [ 2, 1]],
        ep: [[ 6, 1], [ 3, 1], [ 7, 1], [ 9, 1]],
    };
    const BI: Turn = Cube::B.inverse();

    fn turn(&mut self, t: &Turn) {
        let l = t.cp.len();
        let sources: Vec<u8>  = (0..l).map(|i| {
            let j = (i + 1) % l;
            Cube::CT[t.cp[j][1] as usize][self.corner[t.cp[j][0] as usize] as usize]
        }).collect();
        for i in 0..l {
            self.corner[t.cp[i][0] as usize] = sources[i];
        }
        let l = t.ep.len();
        let sources: Vec<u8>  = (0..l).map(|i| {
            let j = (i + 1) % l;
            Cube::EF[t.ep[j][1] as usize][self.edge[t.ep[j][0] as usize] as usize]
        }).collect();
        for i in 0..l {
            self.edge[t.ep[i][0] as usize] = sources[i];
        }
    }
}

impl fmt::Display for Cube {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {:?}", self.corner, self.edge)
    }
}

fn main() {

    let mut cube: Cube = Cube::SOLVED;
    let sequence = [Cube::R, Cube::U, Cube::RI, Cube::UI];
    let sequence2 = [Cube::L, Cube::B, Cube::R, Cube::U, Cube::F, Cube::D];
    let sequence3 = [Cube::DI, Cube::FI, Cube::UI, Cube::RI, Cube::BI, Cube::LI];
    let sequence4 = [
        Cube::R, Cube::RI,
        Cube::L, Cube::LI,
        Cube::U, Cube::UI,
        Cube::D, Cube::DI,
        Cube::F, Cube::FI,
        Cube::B, Cube::BI,
    ];
    let sequence5 = [
        Cube::R,
        Cube::L,
        Cube::U,
        Cube::D,
        Cube::F,
        Cube::B,
        Cube::BI,
        Cube::FI,
        Cube::DI,
        Cube::UI,
        Cube::LI,
        Cube::RI,
    ];
let sequence6 = [ Cube::R, Cube::U, Cube::U, Cube::DI, Cube::B, Cube::DI ];
    let mut ct = 0;

    println!("initial state: {}", cube.to_string());

    loop {
        ct += 1;
        for t in sequence6.iter() {
            cube.turn(t);
        }
        println!("{}: {}", ct, cube.to_string());
        if cube == Cube::SOLVED {
            println!("Solved in {} times!", ct);
            break;
        } else {
        }
    }
}
