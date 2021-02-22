// Write code here.
//
// To see what the code looks like after macro expansion:
//     $ cargo expand
//
// To run the code:
//     $ cargo run

use seq::seq;

const PROCS: [Proc; 256] = {
    seq!(N in 0..256 {
        [
            #(
                Proc::new(N),
            )*
        ]
    })
};

struct Proc {
    id: usize,
}

impl Proc {
    const fn new(id: usize) -> Self {
        Proc { id }
    }
}

fn main() {
    assert_eq!(PROCS[32].id, 32);
}
