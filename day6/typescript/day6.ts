import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

type Lookup = { [key: string]: boolean };

const redistribute = (banks: number[]): number[] => {
    let max = Math.max(...banks);
    const idx = R.findIndex(R.equals(max), banks);
    banks[idx] = 0;
    for (let i = (idx + 1) % banks.length; max > 0; i = (i + 1) % banks.length) {
        banks[i]++;
        max--;
    }

    return banks;
};

const part1 = (banks: number[]) => {
    let seen = false;
    let count = 0;
    const lookup: Lookup = {};
    for (; !seen; ++count) {
        lookup[banks.join(' ')] = true;
        banks = redistribute([...banks]);
        seen = lookup[banks.join(' ')];
    }

    return count;
};

const part2 = (banks: number[]) => {
    let seen = false;
    let count = 0;
    const lookup: Lookup = {};
    for (; !seen; ++count) {
        lookup[banks.join(' ')] = true;
        banks = redistribute([...banks]);
        seen = lookup[banks.join(' ')];
    }

    return part1(banks);
};

async function day6() {
    const input = await promisify(readFile)('day6/input.txt', 'utf8');
    const banks = R.map(parseInt, input.split('\t'));

    console.log(part1(banks));
    console.log(part2(banks));
}

day6();