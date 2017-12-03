import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

const maximum = R.apply(Math.max);
const minimum = R.apply(Math.min);
const mapSum = (fn: (x: number[]) => number) => R.compose(R.sum, R.map(fn));

const part1 = mapSum(
    R.lift(R.subtract)(maximum, minimum),
);

const part2 = mapSum(row =>
    R.compose<number[], number[], number[], number>(
        maximum,
        R.filter<number>(x => x % 1 === 0),
        R.lift(R.divide)(row),
    )(row),
);

const readGrid = R.compose(
    R.map(R.map(parseInt)),
    R.map(R.split('\t')),
    R.split('\n'),
);

async function day2() {
    const input = await promisify(readFile)('day2/input.txt', 'utf8');
    const grid = readGrid(input);

    console.log(part1(grid));
    console.log(part2(grid));
}

day2();