import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

interface Generator {
    value: number;
    factor: number;
}

const next = ({ value, factor }: Generator) => {
    return { value: (value * factor) % 2147483647, factor };
};

function* values(gen: Generator, criteria: number): IterableIterator<Generator> {
    for (;;) {
        if (gen.value % criteria === 0) {
            yield gen;
        }

        gen = next(gen);
    }
}

const solve = (as: IterableIterator<Generator>, bs: IterableIterator<Generator>, num: number) => {
    let count = 0;
    for (const i of R.range(0, num)) {
        const a = as.next().value;
        const b = bs.next().value;

        if ((a.value & 0xFFFF) === (b.value & 0xFFFF)) {
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
