import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

interface Layer {
    depth: number;
    range: number;
    scanner: number;
}

type Firewall = { [depth: number]: Layer };

const parse = R.compose<string[], Firewall[], Firewall>(
    R.mergeAll,
    R.map(
        R.compose<string, string[], number[], Firewall>(
            ([d, r]) => ({ [d]: { depth: d, range: r, scanner: 0 } }),
            R.map(parseInt),
            R.split(': '),
        ),
    ),
);

const stepper = (fn: (s: number, k: number) => number) => R.mapObjIndexed(
    ({ scanner, range, depth }, key, obj) => ({
        depth,
        range,
        scanner: fn(scanner, parseInt(key, 10)) % (range * 2 - 2),
    }),
);

const step = stepper((s, k) => s + 1);
const sim = stepper(R.add);

const part1 = (firewall: Firewall) => R.reduce<Layer, number>(
    (acc, { depth, range }) => acc + depth * range,
    0,
    R.filter(l => l.scanner === 0, R.values(sim(firewall))),
);

const part2 = (firewall: Firewall) => {
    for (let i = 0;; ++i) {
        const sim2 = stepper((s, k) => s + k + i);
        if (R.all(({ scanner }) => scanner !== 0, R.values(sim2(firewall)))) {
            return i;
        }
    }
};

(async () => {
    const input = await promisify(readFile)('day13/input.txt', 'utf8');
    const firewall = parse(R.split('\r\n', input));

    console.log(part1(firewall));
    console.log(part2(firewall));
})();
