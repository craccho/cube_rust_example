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
type CornerPerm = [[u8; 2]; 4];
type EdgePerm = [[u8; 2]; 4];
struct Turn {
    cp: CornerPerm,
    ep: EdgePerm,
}

impl Cube {
    const SOLVED: Cube = Cube {
        corner: [0,1,2,3,20,21,22,23],
        edge: [0,1,2,3,4,6,12,14,20,21,22,23],
    };
    const CT: CornerTwist =
        // [[ 1, 19,  8],
        //  [ 2,  7, 12],
        //  [ 3, 11, 16],
        //  [ 4, 15, 20],
        //  [21, 17, 14],
        //  [22, 13, 10],
        //  [23,  9,  6],
        //  [24,  5, 18]];
        [
            [ 0, 1,  2,  3,  4,  5,  6, 7, 8,  9, 10,11,12, 13, 14,15, 16, 17,18,19, 20, 21,22,23],
            [18, 6, 10, 14, 17, 22, 11, 0, 5, 21, 15, 1, 9, 20, 19, 2, 13, 23, 7, 3, 16, 12, 8, 4],
            [ 7,11, 15, 19, 23,  8,  1,18,22, 12,  2, 6,21, 16,  3,10, 20,  4, 0,14, 13,  9, 5,17],
        ];
    const EF: EdgeFlip =
        // [[ 1, 20],
        //  [ 2,  8],
        //  [ 3, 12],
        //  [ 4, 16],
        //  [ 5, 19],
        //  [ 7,  9],
        //  [13, 11],
        //  [15, 17],
        //  [21, 18],
        //  [22, 14],
        //  [23, 10],
        //  [24,  6]];
        [
            [ 0, 1,  2,  3,  4,  5, 6, 7, 8,  9, 10,11, 12, 13, 14,15, 16, 17,18,19, 20, 21,22,23],
            [19, 7, 11, 15, 18, 23, 8, 1, 6, 22, 12, 2, 10, 21, 16, 3, 14, 20, 4, 0, 17, 13, 9, 5],
        ];

    const R: Turn = Turn {
        cp: [[ 7, 1], [ 4, 2], [ 3, 1], [ 0, 2]],
        ep: [[ 4, 0], [ 8, 0], [ 7, 0], [ 0, 0]],
    };
    const RI: Turn = Turn {
        cp: [[ 3, 1], [ 4, 2], [ 7, 1], [ 0, 2]],
        ep: [[ 7, 0], [ 8, 0], [ 4, 0], [ 0, 0]],
    };

    const L: Turn = Turn {
        cp: [[ 2, 2], [ 5, 1], [ 6, 2], [ 1, 1]],
        ep: [[ 6, 0], [10, 0], [ 5, 0], [ 2, 0]],
    };

    const LI: Turn = Turn {
        cp: [[ 6, 2], [ 5, 1], [ 2, 2], [ 1, 1]],
        ep: [[ 5, 0], [10, 0], [ 6, 0], [ 2, 0]],
    };

    const U: Turn = Turn {
        cp: [[ 3, 0], [ 0, 0], [ 1, 0], [ 2, 0]],
        ep: [[ 3, 0], [ 2, 0], [ 1, 0], [ 0, 0]],
    };
    const UI: Turn= Turn {
        cp: [[ 1, 0], [ 0, 0], [ 3, 0], [ 2, 0]],
        ep: [[ 3, 0], [ 0, 0], [ 1, 0], [ 2, 0]],
    };

    const D: Turn = Turn {
        cp: [[ 7, 0], [ 6, 0], [ 5, 0], [ 4, 0]],
        ep: [[11, 0], [10, 0], [ 9, 0], [ 8, 0]],
    };
    const DI: Turn = Turn {
        cp: [[ 5, 0], [ 6, 0], [ 7, 0], [ 4, 0]],
        ep: [[ 9, 0], [10, 0], [11, 0], [ 8, 0]],
    };

    const F: Turn = Turn {
        cp: [[ 1, 2], [ 6, 1], [ 7, 2], [ 0, 1]],
        ep: [[ 5, 1], [11, 1], [ 4, 1], [ 1, 1]],
    };
    const FI: Turn = Turn {
        cp: [[ 7, 2], [ 6, 1], [ 1, 2], [ 0, 1]],
        ep: [[ 4, 1], [11, 1], [ 5, 1], [ 1, 1]],
    };

    const B: Turn  = Turn {
        cp: [[ 3, 2], [ 4, 1], [ 5, 2], [ 2, 1]],
        ep: [[ 6, 1], [ 3, 1], [ 7, 1], [ 9, 1]],
    };
    const BI: Turn = Turn {
        cp: [[ 5, 2], [ 4, 1], [ 3, 2], [ 2, 1]],
        ep: [[ 7, 1], [ 3, 1], [ 6, 1], [ 9, 1]],
    };

    fn turn(&mut self, t: &Turn) {
        let sources: Vec<u8>  = (0..4).map(|i| {
            let j = (i + 1) % 4;
            Cube::CT[t.cp[j][1] as usize][self.corner[t.cp[j][0] as usize] as usize]
        }).collect();
        for i in 0..4 {
            self.corner[t.cp[i][0] as usize] = sources[i];
        }
        let sources: Vec<u8>  = (0..4).map(|i| {
            let j = (i + 1) % 4;
            Cube::EF[t.ep[j][1] as usize][self.edge[t.ep[j][0] as usize] as usize]
        }).collect();
        for i in 0..4 {
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
    let sequence3 = [Cube::LI, Cube::BI, Cube::RI, Cube::UI, Cube::FI, Cube::DI];
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
    let mut ct = 0;

    println!("initial state: {}", cube.to_string());

    loop {
        ct += 1;
        for t in sequence5.iter() {
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
