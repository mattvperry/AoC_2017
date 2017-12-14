import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

import { part2 as knot } from '../../day10/typescript/day10';
import { part2 as connected, Graph } from '../../day12/typescript/day12';

type Coord = R.KeyValuePair<number, number>;
type Grid = number[][];

const binary = (hex: string) => {
    const i = parseInt(hex, 16);
    const bin = i.toString(2).padStart(4, '0');
    return R.map(parseInt, Array.from(bin));
};

const makeGrid = (input: string) => {
    const hashes = R.map(knot, R.times(n => `${input}-${n}`, 128));
    return R.map(R.compose(R.chain(binary), Array.from), hashes);
};

const part1 = R.compose<Grid, number[], number>(R.sum, R.chain(r => r));

const clamp = R.clamp(0, 127);
const neighbors = (x: number, y: number): Coord[] => [
    [clamp(x + 1), clamp(y)],
    [clamp(x), clamp(y + 1)],
    [clamp(x - 1), clamp(y)],
    [clamp(x), clamp(y - 1)],
];

function* occupied(grid: Grid, coords: Coord[]): IterableIterator<Coord> {
    for (const [x, y] of coords) {
        if (grid[x][y] === 1) {
            yield [x, y];
        }
    }
}

const part2 = (grid: Grid) => {
    const graph: Graph = {};
    const coords = R.xprod(R.range(0, 128), R.range(0, 128));
    for (const [i, j] of Array.from(occupied(grid, coords))) {
        const set = new Set();
        for (const [x, y] of Array.from(occupied(grid, neighbors(i, j)))) {
            if (x === i && y === j) {
                continue;
            }

            set.add(JSON.stringify([x, y]));
        }

        graph[JSON.stringify([i, j])] = Array.from(set);
    }

    return connected(graph);
};

(async () => {
    const input = await promisify(readFile)('day14/input.txt', 'utf8');
    const grid = makeGrid(input);

    console.log(part1(grid));
    console.log(part2(grid));
})();
