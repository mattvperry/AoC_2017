import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

import { part2 as knot } from '../../day10/typescript/day10';
import { part2 as connected, Graph } from '../../day12/typescript/day12';

type Coord = R.KeyValuePair<number, number>;
type Grid = number[];

const binary = (hex: string) => {
    const i = parseInt(hex, 16);
    const bin = i.toString(2).padStart(4, '0');
    return R.map(parseInt, Array.from(bin));
};

const makeGrid = (input: string) => {
    const hashes = R.map(knot, R.times(n => `${input}-${n}`, 128));
    return R.chain(R.compose(R.chain(binary), Array.from), hashes);
};

const part1 = R.sum;

const neighbors = (coord: string) => {
    const [x, y] = JSON.parse(coord);
    return R.map(JSON.stringify, [
        [x + 1, y],
        [x, y + 1],
        [x - 1, y],
        [x, y - 1],
    ]);
};

const occupied = R.compose<Grid, number[][], number[][], string[]>(
    R.map<number[], string>(([_, x, y]) => JSON.stringify([x, y])),
    R.filter<number[]>(([v, x, y]) => v === 1),
    R.addIndex<number, number[]>(R.map)(
        (val, idx) => [val, Math.floor(idx / 128), idx % 128],
    ),
);

const makeGraph = (cs: Set<string>) => R.reduce<string, Graph>(
    (acc, curr) => R.set(
        R.lensProp(curr),
        R.filter(c => cs.has(c), neighbors(curr)),
        acc,
    ),
    {},
    Array.from(cs),
);

const part2 = R.compose(
    connected,
    makeGraph,
    o => new Set(o),
    occupied,
);

(async () => {
    const input = await promisify(readFile)('day14/input.txt', 'utf8');
    const grid = makeGrid(input);

    console.log(part1(grid));
    console.log(part2(grid));
})();
