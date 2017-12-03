import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

type Pos = [number, number];
type Grid<T> = { [coord: string]: T };
type Mover = (x: number, y: number) => Pos;

const dirs: Array<(pos: Pos) => Pos> = R.map(R.apply, [
    (x, y) => [x + 1, y], // Right
    (x, y) => [x, y - 1], // Up
    (x, y) => [x - 1, y], // Left
    (x, y) => [x, y + 1], // Down
    (x, y) => [x + 1, y + 1],
    (x, y) => [x - 1, y - 1],
    (x, y) => [x - 1, y + 1],
    (x, y) => [x + 1, y - 1],
] as Mover[]);

function* traverse() {
    function* moves() {
        let dir = 0;
        for (let num = 1; true; num += .5) {
            yield* R.repeat(dirs[dir], Math.floor(num));
            dir = (dir + 1) % 4;
        }
    }

    let pos: Pos = [0, 0];
    for (const move of moves()) {
        yield pos;
        pos = move(pos);
    }
}

const part1 = (data: number) => {
    const gen = traverse();
    return R.compose<number, Pos[], Pos, number>(
        R.apply(R.useWith(R.add, R.repeat(Math.abs, 2))),
        R.last,
        R.times(() => gen.next().value),
    )(data);
};

const part2 = (data: number) => {
    const grid: Grid<number> = {};
    const gen = traverse();

    const sumNeighbors = (
        R.compose<Pos, Pos[], number[], number>(
            R.sum,
            R.map(R.compose<Pos, string, number, number>(
                R.defaultTo(0),
                coord => grid[coord],
                JSON.stringify,
            )),
            R.juxt(dirs),
        )
    );

    for (;;) {
        const coord = gen.next().value;
        const sum = sumNeighbors(coord);
        if (sum > data) {
            return sum;
        }

        grid[JSON.stringify(coord)] = R.max(sum, 1);
    }
};

async function day3() {
    const input = await promisify(readFile)('day3/input.txt', 'utf8');
    const data = parseInt(input, 10);

    console.log(part1(data));
    console.log(part2(data));
}

day3();