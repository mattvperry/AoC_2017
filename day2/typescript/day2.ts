import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

const maximum = R.apply(Math.max);
const minimum = R.apply(Math.min);
const mapSum = (fn: (x: number[]) => number) => R.compose(R.sum, R.map(fn));

const part1 = mapSum(
    R.converge(R.subtract, [maximum, minimum]),
);

const part2 = mapSum(row =>
    R.compose<number[], number[], number[], number>(
        maximum,
        R.filter<number>(x => x % 1 === 0),
        R.lift(R.divide)(row),
    )(row),
);

async function day2() {
    const input = await promisify(readFile)('day2/input.txt', 'utf8');
    const grid = input.split('\n')
        .map(r => r.split('\t').map(n => parseInt(n.trim(), 10)));

    console.log(part1(grid));
    console.log(part2(grid));
}

day2();