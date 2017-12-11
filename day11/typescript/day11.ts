import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

type Coord = [number, number, number];
type Moves = { [dir: string]: (coord: Coord) => Coord };

const moves: Moves = {
    n: ([x, y, z]) => [x - 1, y + 1, z],
    ne: ([x, y, z]) => [x, y + 1, z - 1],
    nw: ([x, y, z]) => [x - 1, y, z + 1],
    s: ([x, y, z]) => [x + 1, y - 1, z],
    se: ([x, y, z]) => [x + 1, y, z - 1],
    sw: ([x, y, z]) => [x, y - 1, z + 1],
};

const path = (dirs: string[]) => R.scan<string, Coord>(
    (acc, curr) => moves[curr](acc),
    [0, 0, 0],
    dirs,
);

const part1 = (dirs: string[]) => {
    const end = R.last(path(dirs))!;
    return Math.max(...end.map(Math.abs));
};

const part2 = (dirs: string[]) => {
    const points = path(dirs);
    return Math.max(...R.map(p => Math.max(...p.map(Math.abs)), points));
};

(async () => {
    const input = await promisify(readFile)('day11/input.txt', 'utf8');
    const dirs = input.split(',');

    console.log(part1(dirs));
    console.log(part2(dirs));
})();
