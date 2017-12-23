import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

enum Status {
    running,
    terminated,
}

type Op = (number | string)[];
type Ops = { [name: string]: (state: State, op: Op) => State };
type Registers = { [name: string]: number };
interface State {
    pc: number;
    regs: Registers;
    status: Status;
}

const lookup = (state: State, x: number | string) => (
    isNaN(+x) ? R.pipe(R.path<number>(['regs', x]), R.defaultTo(0))(state) : +x
);

let a = 0;

const ops: Ops = {
    set: (s, [x, y]) => ({ ...s, regs: { ...s.regs, [x]: lookup(s, y) }}),
    sub: (s, [x, y]) => ops.set(s, [x, lookup(s, x) - lookup(s, y)]),
    mul: (s, [x, y]) => { a++; return ops.set(s, [x, lookup(s, x) * lookup(s, y)]); },
    jnz: (s, [x, y]) => ({ ...s, pc: s.pc + (lookup(s, x) !== 0 ? lookup(s, y) - 1 : 0) }),
};

const perform = (state: State, code: string, size: number) => {
    const [op, ...ps] = R.split(' ', code);
    const s = ops[op](state, ps);
    const pc = s.pc + 1;
    return {
        ...s,
        pc,
        status: pc < 0 || pc >= size ? Status.terminated : s.status,
    };
};

const exec = (state: State) => (lines: string[]) => {
    while (state.status === Status.running) {
        state = perform(state, lines[state.pc], lines.length);
    }
};

const part1 = exec({ pc: 0, regs: {}, status: Status.running });

const isPrime = (num: number) => {
    for (let i = 2; i <= Math.sqrt(num); i++) {
        if (num % i === 0) {
            return false;
        }
    }

    return num !== 1;
};

const part2 = () => R.pipe<number, number[], number[], number>(
    R.times(n => 108100 + (n * 17)),
    R.filter<number>(n => !isPrime(n)),
    R.length,
)(1001);

(async () => {
    const input = await promisify(readFile)('day23/input.txt', 'utf8');
    const lines = R.split('\r\n', input);

    part1(lines);
    console.log(a);
    console.log(part2());
})();