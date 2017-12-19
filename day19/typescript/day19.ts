import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

type Coord = [number, number];
type Grid = string[][];
type Dir = (coord: Coord) => Coord;
type Dirs = { [dir: string]: Dir };

const dirs: Dirs = {
    r: ([y, x]) => [y, x + 1],
    d: ([y, x]) => [y + 1, x],
    l: ([y, x]) => [y, x - 1],
    u: ([y, x]) => [y - 1, x],
};

const lookup = (grid: Grid) => ([y, x]: Coord) => (grid[y] || [])[x] || ' ';

const solve = (grid: Grid) => {
    const letters = [];
    const at = lookup(grid);
    let steps = 1;
    let dir: Dir = dirs.r;
    let curr: Coord = [0, 0];
    while (at(curr) === ' ') {
        curr = dir(curr);
    }

    dir = dirs.d;
    for (;;) {
        const next = dir(curr);
        const val = at(next);
        if (val === ' ') {
            break;
        }

        if (val.match(/[A-Z]/)) {
            letters.push(val);
        }

        if (val !== '+') {
            steps++;
            curr = next;
            continue;
        }

        dir = R.pipe<Dirs, Dir[], Dir | undefined>(
            R.values,
            R.find(d => !R.equals(d(next), curr) && at(d(next)) !== ' '),
        )(dirs)!;

        curr = next;
        steps++;
    }

    return [letters.join(''), steps];
};

const part2 = () => {
    return '';
};

(async () => {
    const input = await promisify(readFile)('day19/input.txt', 'utf8');
    const grid = R.map(Array.from, R.split('\r\n', input));

    console.log(...solve(grid));
})();