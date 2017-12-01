import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

const rotate = <T>(dist: number, list: T[]) => (
    R.flatten<T>(R.reverse(R.splitAt(dist, list)))
);

const sumPairs = (dist: number, list: number[]) => (
    R.compose(
        R.sum,
        R.zipWith<number, number, number>(
            (a, b) => a === b ? a : 0,
            rotate(dist, list),
        ),
    )(list)
);

async function day1() {
    const input = await promisify(readFile)('day1/input.txt', 'utf8');
    const digits = R.map(parseInt, Array.from(input));

    const part1 = sumPairs(1, digits);
    const part2 = sumPairs(digits.length / 2, digits);

    console.log(part1);
    console.log(part2);
}

day1();