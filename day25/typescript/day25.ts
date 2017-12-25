import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

interface Turing {
    state: string;
    cursor: number;
    tape: { [idx: number]: 0 | 1 };
}

interface Move {
    set: 0 | 1;
    change: number;
    next: string;
}

type States = { [name: string]: { 0: Move; 1: Move; } };
const states: States = {
    A: {
        0: { set: 1, change: 1, next: 'B' },
        1: { set: 0, change: -1, next: 'E' },
    },
    B: {
        0: { set: 1, change: -1, next: 'C' },
        1: { set: 0, change: 1, next: 'A' },
    },
    C: {
        0: { set: 1, change: -1, next: 'D' },
        1: { set: 0, change: 1, next: 'C' },
    },
    D: {
        0: { set: 1, change: -1, next: 'E' },
        1: { set: 0, change: -1, next: 'F' },
    },
    E: {
        0: { set: 1, change: -1, next: 'A' },
        1: { set: 1, change: -1, next: 'C' },
    },
    F: {
        0: { set: 1, change: -1, next: 'E' },
        1: { set: 1, change: 1, next: 'A' },
    },
};

const machine: Turing = {
    state: 'A',
    cursor: 0,
    tape: {},
};

for (let i = 0; i < 12208951; ++i) {
    const move = states[machine.state][machine.tape[machine.cursor] || 0];
    machine.state = move.next;
    machine.tape[machine.cursor] = move.set;
    machine.cursor += move.change;
}

console.log(R.sum(R.values(machine.tape)));