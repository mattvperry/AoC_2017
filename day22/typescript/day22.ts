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

const part1 = (grid: Grid) => {
    let [x, y] = [12, 12];
    let dir = 0;
    let infected = 0;
    for (let i = 0; i < 10000; ++i) {
        if (grid[y] === undefined) {
            grid[y] = [];
        }
        if (grid[y][x] === undefined) {
            grid[y][x] = '.';
        }
        const cell = grid[y][x];
        if (cell === '#') {
            dir = R.mathMod(dir + 1, 4) ;
            grid[y][x] = '.';
        } else {
            dir = R.mathMod(dir - 1, 4);
            grid[y][x] = '#';
            infected++;
        }

        [x, y] = move[dir]([x, y]);
    }

    return infected;
};

const part2 = (grid: Grid) => {
    let [x, y] = [12, 12];
    let dir = 0;
    let infected = 0;
    for (let i = 0; i < 10000000; ++i) {
        if (grid[y] === undefined) {
            grid[y] = [];
        }
        if (grid[y][x] === undefined) {
            grid[y][x] = '.';
        }
        const cell = grid[y][x];
        switch (cell) {
            case '#':
                dir = R.mathMod(dir + 1, 4) ;
                grid[y][x] = 'F';
                break;
            case 'W':
                grid[y][x] = '#';
                infected++;
                break;
            case 'F':
                dir = R.mathMod(dir + 2, 4) ;
                grid[y][x] = '.';
                break;
            default:
                dir = R.mathMod(dir - 1, 4);
                grid[y][x] = 'W';
                break;
        }

        [x, y] = move[dir]([x, y]);
    }

    return infected;
};

(async () => {
    const input = await promisify(readFile)('day22/input.txt', 'utf8');
    const grid = R.map(Array.from, R.split('\r\n', input));

    console.log(part2(grid));
})();