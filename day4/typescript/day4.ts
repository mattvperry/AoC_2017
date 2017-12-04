import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

const part1 = (data: any) => {
    return 'wat';
};

const part2 = (data: any) => {
    return 'wat';
};

async function day4() {
    const input = await promisify(readFile)('day4/input.txt', 'utf8');

    console.log(part1(input));
    console.log(part2(input));
}

day4();