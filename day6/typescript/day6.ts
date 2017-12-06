import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

type DupePair = [number, number[]];

const redist = (banks: number[]): number[] => {
    const max = Math.max(...banks);
    const idx = R.findIndex(R.equals(max), banks);
    return R.reduce(R.flip(R.call), banks, [
        R.set(R.lensIndex(idx), 0),
        ...R.map(
            n => R.over(R.lensIndex(n), R.inc),
            R.times(n => (n + idx + 1) % banks.length, max),
        ),
    ]);
};

const findDupe = (banks: number[]): DupePair => {
    let count = 0;
    const lookup = new Set();
    for (let seen = false; !seen; ++count) {
        lookup.add(banks.join(''));
        banks = redist(banks);
        seen = lookup.has(banks.join(''));
    }

    return [count, banks];
};

const part1 = R.compose(
    R.head,
    findDupe,
);

const part2 = R.compose<number[], DupePair, number[], DupePair, number>(
    R.head,
    findDupe,
    R.last,
    findDupe,
);

(async () => {
    const input = await promisify(readFile)('day6/input.txt', 'utf8');
    const banks = R.map(parseInt, R.split('\t', input));

    console.log(part1(banks));
    console.log(part2(banks));
})();