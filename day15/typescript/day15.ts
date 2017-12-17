import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

interface Generator {
    value: number;
    factor: number;
}

function* values({ value, factor }: Generator, criteria: number): IterableIterator<number> {
    for (;;) {
        if (value % criteria === 0) {
            yield value;
        }

        value = (value * factor) % 2147483647;
    }
}

const solve = (as: IterableIterator<number>, bs: IterableIterator<number>, num: number) => {
    let count = 0;
    for (const i of R.range(0, num)) {
        if ((as.next().value & 0xFFFF) === (bs.next().value & 0xFFFF)) {
            count++;
        }
    }

    return count;
};

const part1 = (a: Generator, b: Generator) => solve(
    values(a, 1),
    values(b, 1),
    40000000,
);

const part2 = (a: Generator, b: Generator) => solve(
    values(a, 4),
    values(b, 8),
    5000000,
);

(() => {
    const a = { value: 883, factor: 16807 };
    const b = { value: 879, factor: 48271 };

    console.log(part1(a, b));
    console.log(part2(a, b));
})();
