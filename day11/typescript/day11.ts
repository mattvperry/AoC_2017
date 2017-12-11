import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

const part1 = (input: string) => {
    return input;
};

const part2 = (input: string) => {
    return input;
};

(async () => {
    const input = await promisify(readFile)('day11/input.txt', 'utf8');

    console.log(part1(input));
    console.log(part2(input));
})();
