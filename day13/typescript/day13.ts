import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

const part1 = () => {
    return '';
};

const part2 = () => {
    return '';
};

(async () => {
    const input = await promisify(readFile)('day13/input.txt', 'utf8');

    console.log(part1());
    console.log(part2());
})();
