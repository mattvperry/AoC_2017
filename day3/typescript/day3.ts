import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

const part1 = (data: any) => {
    return 'part1';
};

const part2 = (data: any) => {
    return 'part2';
};

const parse = (input: string) => {
    return undefined;
};

async function day3() {
    const input = await promisify(readFile)('day3/input.txt', 'utf8');
    const data = parse(input);

    console.log(part1(data));
    console.log(part2(data));
}

day3();