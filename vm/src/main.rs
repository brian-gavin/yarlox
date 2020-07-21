#[macro_use]
extern crate log;
extern crate env_logger;

use yarlox_vm::{
    chunk::{Chunk, OpCode::*},
    vm::Vm,
};

fn main() {
    env_logger::init();
    let mut chunk1 = Chunk::new();
    let mut chunk2 = Chunk::new();
    let mut chunk3 = Chunk::new();

    // a.
    let c = chunk1.add_constant(1.0);
    chunk1.write_chunk(Constant(c), 1);
    let c = chunk1.add_constant(2.0);
    chunk1.write_chunk(Constant(c), 1);
    chunk1.write_chunk(Multiply, 1);
    let c = chunk1.add_constant(3.0);
    chunk1.write_chunk(Constant(c), 1);
    chunk1.write_chunk(Add, 1);
    chunk1.write_chunk(Return, 1);

    // b.
    let c = chunk2.add_constant(1.0);
    chunk2.write_chunk(Constant(c), 2);
    let c = chunk2.add_constant(2.0);
    chunk2.write_chunk(Constant(c), 2);
    let c = chunk2.add_constant(3.0);
    chunk2.write_chunk(Constant(c), 2);
    chunk2.write_chunk(Multiply, 2);
    chunk2.write_chunk(Add, 2);
    chunk2.write_chunk(Return, 2);

    // c is the same as a :)

    // d.
    let c = chunk3.add_constant(1.0);
    chunk3.write_chunk(Constant(c), 3);
    let c = chunk3.add_constant(2.0);
    chunk3.write_chunk(Constant(c), 3);
    let c = chunk3.add_constant(3.0);
    chunk3.write_chunk(Constant(c), 3);
    chunk3.write_chunk(Multiply, 3);
    chunk3.write_chunk(Add, 3);
    let c = chunk3.add_constant(4.0);
    chunk3.write_chunk(Constant(c), 3);
    let c = chunk3.add_constant(5.0);
    chunk3.write_chunk(Constant(c), 3);
    chunk3.write_chunk(Negate, 3);
    chunk3.write_chunk(Divide, 3);
    chunk3.write_chunk(Subtract, 3);
    chunk3.write_chunk(Return, 3);

    for chunk in vec![chunk1, chunk2, chunk3] {
        debug!("\n{}", chunk);
        let mut vm = Vm::new(chunk);
        vm.interpret().unwrap();
    }
}
