import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

interface Layer {
    range: number;
    scanner: number;
}

type Firewall = { [depth: number]: Layer };

const parse = R.compose<string[], Firewall[], Firewall>(
    R.mergeAll,
    R.map(
        R.compose<string, string[], number[], Firewall>(
            ([d, r]) => ({ [d]: { range: r, scanner: 0 } }),
            R.map(parseInt),
            R.split(': '),
        ),
    ),
);

const step = R.mapObjIndexed(
    ({scanner, range}, key, obj) => ({
        range,
        scanner: (scanner + 1) % (range * 2 - 2)
    }),
);

function* simulate(firewall: Firewall): IterableIterator<[number, number]> {
    const max = Math.max(...R.map(parseInt, R.keys(firewall)));
    for (const p of R.range(0, max + 1)) {
        const layer = firewall[p];
        if (layer !== undefined && layer.scanner === 0) {
            yield [p, layer.range];
        }

        firewall = step(firewall);
    }
}

const part1 = (firewall: Firewall) => R.reduce(
    (acc, [depth, range]) => acc + depth * range,
    0,
    Array.from(simulate(firewall)),
);

const part2 = (firewall: Firewall) => {
    for (let i = 0;; ++i) {
        if (Array.from(simulate(firewall)).length === 0) {
            return i;
        }

        firewall = step(firewall);
    }
};

(async () => {
    const input = await promisify(readFile)('day13/input.txt', 'utf8');
    const firewall = parse(R.split('\r\n', input));

    console.log(part1(firewall));
    console.log(part2(firewall));
})();
