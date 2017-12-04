import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

type Pos = [number, number];
type Grid<T> = { [coord: string]: T };
type Mover = (x: number, y: number) => Pos;

const moves: Array<(pos: Pos) => Pos> = R.map(R.apply, [
    (x, y) => [x, y - 1], // Up
    (x, y) => [x - 1, y], // Left
    (x, y) => [x, y + 1], // Down
    (x, y) => [x + 1, y], // Right
    (x, y) => [x + 1, y + 1],
    (x, y) => [x - 1, y - 1],
    (x, y) => [x - 1, y + 1],
    (x, y) => [x + 1, y - 1],
] as Mover[]);

function* spiral() {
    function* dirs() {
        let dir = 0;
        for (let num = 1; true; num += .5) {
            yield* R.repeat(dir, Math.floor(num));
            dir = (dir + 1) % 4;
        }
    }

    let pos: Pos = [0, 1];
    for (const dir of dirs()) {
        yield pos;
        pos = moves[dir](pos);
    }
}

const part1 = (data: number) => {
    const gen = spiral();
    return R.compose<number, number, Pos[], Pos, number>(
        R.apply(R.useWith(R.add, R.repeat(Math.abs, 2))),
        R.last,
        R.times(() => gen.next().value),
        R.inc,
    )(data);
};

function* gridGen(state: [number, Grid<number>], gen: IterableIterator<Pos>) {
    for (;;) {
        const [_, g] = state;
        const coord = gen.next().value;
        const sum = R.compose<Pos, Pos[], number[], number>(
            R.sum,
            R.map(c => g[JSON.stringify(coord)] || 0),
            R.juxt(moves),
        )(coord);
        state = [sum, { ...g, [JSON.stringify(coord)]: sum }];
        yield state;
    }
}

const part2 = (data: number) => {
    const gen = spiral();
    const state: [number, Grid<number>] = [1, { [JSON.stringify([0, 0])]: 1 }];
    for (const [sum, _] of gridGen(state, gen)) {
        if (sum > data) {
            return sum;
        }
    }
};

async function day3() {
    const input = await promisify(readFile)('day3/input.txt', 'utf8');
    const data = parseInt(input, 10);

    console.log(part1(data));
    console.log(part2(data));
}

day3();