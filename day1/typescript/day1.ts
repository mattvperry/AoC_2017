import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

function pairsAtDist<T>(dist: number, list: T[]) {
    return R.compose(
        R.map(R.map<number, T>(i => list[i])),
        R.map<number, number[]>(i => [i, R.mathMod(i + dist, list.length)]),
        R.range(0),
    )(list.length);
}

function sumPairs(dist: number, list: number[]) {
    const pairs = pairsAtDist(dist, list);
    const matches = R.filter<number[]>(R.apply<number, boolean>(R.equals), pairs);
    return R.sum(R.map(x => x[0], matches));
}

async function day1() {
    const input = await promisify(readFile)('day1/input.txt', 'utf8');
    const digits = R.map(parseInt, Array.from(input));

    const part1 = sumPairs(1, digits);
    const part2 = sumPairs(digits.length / 2, digits);

    console.log(part1);
    console.log(part2);
}

day1();