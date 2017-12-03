import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

type Pos = [number, number];
type Grid<T> = { [coord: string]: T };
type Movers = Array<(x: number, y: number) => Pos>;

const dirs: Movers = [
    (x, y) => [x + 1, y], // Right
    (x, y) => [x, y - 1], // Up
    (x, y) => [x - 1, y], // Left
    (x, y) => [x, y + 1], // Down
];

const allDirs: Movers = [
    ...dirs,
    (x, y) => [x + 1, y + 1],
    (x, y) => [x - 1, y - 1],
    (x, y) => [x - 1, y + 1],
    (x, y) => [x + 1, y - 1],
];

const posToStr = JSON.stringify;
const strToPos = JSON.parse;

function* traverse() {
    const grid: Grid<boolean> = {};

    let pos: Pos = [0, 0];
    let dir = 0;
    grid[posToStr(pos)] = true;

    for (;;) {
        yield pos;

        pos = R.apply(dirs[dir])(pos);
        grid[posToStr(pos)] = true;

        const nextDir = R.mathMod(dir + 1, 4);
        const nextPos = R.apply(dirs[nextDir])(pos);
        if (grid[posToStr(nextPos)] === undefined) {
            dir = nextDir;
        }
    }
}

const part1 = (data: number) => {
    const gen = traverse();
    const [x, y] = R.compose(
        R.reduce(() => gen.next().value, [0, 0]),
        R.range(0),
    )(data);
    return Math.abs(x) + Math.abs(y);
};

const part2 = (data: number) => {
    const grid: Grid<number> = {};
    const gen = traverse();

    const sumNeighbors = (
        R.compose<Pos, Pos[], string[], number[], number[], number>(
            R.sum,
            R.map(R.defaultTo(0)),
            R.map(R.flip<string, Grid<number>, any>(R.prop)(grid)),
            R.map(posToStr),
            R.juxt(R.map(R.apply, allDirs)),
        )
    );

    for (;;) {
        const coord = gen.next().value;
        const sum = sumNeighbors(coord);
        if (sum > data) {
            return sum;
        }

        grid[posToStr(coord)] = R.max(sum, 1);
    }
};

async function day3() {
    const input = await promisify(readFile)('day3/input.txt', 'utf8');
    const data = parseInt(input, 10);

    console.log(part1(data));
    console.log(part2(data));
}

day3();