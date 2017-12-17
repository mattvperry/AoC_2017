import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

function* position(num: number): IterableIterator<[number, number]> {
    let curr = 0;
    for (let i = 1; i <= num; ++i) {
        curr = ((curr + 369) % i) + 1;
        yield [i, curr];
    }
}

const part1 = () => {
    const buffer = R.reduce(
        (acc, [i, curr]) => R.insert(curr, i, acc),
        [0],
        Array.from(position(2017)),
    );

    return buffer[buffer.indexOf(2017) + 1];
};

const part2 = () => {
    let val = 0;
    for (const [i, curr] of position(50000000)) {
        val = curr === 1 ? i : val;
    }

    return val;
};

(() => {
    console.log(part1());
    console.log(part2());
})();