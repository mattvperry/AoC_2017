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
    sounds: number[];
    status: Status;
}

const lookup = (state: State, x: number | string) => (
    isNaN(+x) ? R.pipe(R.path<number>(['regs', x]), R.defaultTo(0))(state) : +x
);

const ops: Ops = {
    set: (s, [x, y]) => ({ ...s, regs: { ...s.regs, [x]: lookup(s, y) }}),
    add: (s, [x, y]) => ops.set(s, [x, lookup(s, x) + lookup(s, y)]),
    mul: (s, [x, y]) => ops.set(s, [x, lookup(s, x) * lookup(s, y)]),
    mod: (s, [x, y]) => ops.set(s, [x, lookup(s, x) % lookup(s, y)]),
    jgz: (s, [x, y]) => ({ ...s, pc: s.pc + (lookup(s, x) > 0 ? lookup(s, y) - 1 : 0) }),
    snd: (s, [x]) => ({ ...s, sounds: [...s.sounds, lookup(s, x)] }),
    rcv: (s, [x]) => {
        if (lookup(s, x) !== 0) {
            console.log(R.last(s.sounds));
            return { ...s, status: Status.terminated };
        }

        return s;
    },
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

const part1 = exec({ pc: 0, regs: {}, sounds: [], status: Status.running });

(async () => {
    const input = await promisify(readFile)('day18/input.txt', 'utf8');
    const lines = R.split('\r\n', input);

    part1(lines);
})();