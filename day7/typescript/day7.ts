import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

const part1 = () => {
    return 'wat';
};

const part2 = () => {
    return 'wat';
};

(async () => {
    const input = await promisify(readFile)('day7/input.txt', 'utf8');

    console.log(part1());
    console.log(part2());
})();