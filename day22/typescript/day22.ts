import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

type Coord = [number, number];
type Grid = string[][];

const move = [
    ([x, y]: Coord) => [x, y - 1], // Up
    ([x, y]: Coord) => [x + 1, y], // Right
    ([x, y]: Coord) => [x, y + 1], // Down
    ([x, y]: Coord) => [x - 1, y], // Left
];

const travel = (grid: Grid, moves: number, next: (cell: string) => [number, string]) => {
    let infected = 0;
    let dir = 0;
    let [x, y] = [Math.floor(grid.length / 2), Math.floor(grid.length / 2)];
    for (let i = 0; i < moves; ++i) {
        if (grid[y] === undefined) {
            grid[y] = [];
        }
        if (grid[y][x] === undefined) {
            grid[y][x] = '.';
        }

        const [turn, set] = next(grid[y][x]);
        if (set === '#') {
            infected++;
        }

        grid[y][x] = set;
        dir = R.mathMod(dir + turn, 4);
        [x, y] = move[dir]([x, y]);
    }

    return infected;
};

const part1 = (grid: Grid) => travel(grid, 10000, cell => (
    cell === '#' ? [1, '.'] : [-1, '#']
));

const part2 = (grid: Grid) => travel(grid, 10000000, cell => {
    switch (cell) {
        case '#':
            return [1, 'F'];
        case 'W':
            return [0, '#'];
        case 'F':
            return [2, '.'];
        default:
            return [-1, 'W'];
    }
});

(async () => {
    const input = await promisify(readFile)('day22/input.txt', 'utf8');
    const grid = R.map(Array.from, R.split('\r\n', input));

    console.log(part1(grid.map(r => [...r])));
    console.log(part2(grid.map(r => [...r])));
})();