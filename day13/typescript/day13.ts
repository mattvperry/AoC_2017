import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

interface Layer {
    depth: number;
    range: number;
    scanner: number;
}

type Firewall = Layer[];

const parse = R.map(
    R.compose<string, string[], number[], Layer>(
        ([d, r]) => ({ depth: d, range: r, scanner: 0 }),
        R.map(parseInt),
        R.split(': '),
    ),
);

const stepper = (fn: (s: number, d: number) => number) => R.map(
    ({ scanner, depth, range }) => ({
        depth,
        range,
        scanner: fn(scanner, depth) % (range * 2 - 2),
    }),
);

const step = stepper((s, k) => s + 1);
const sim = stepper(R.add);

const part1 = (firewall: Firewall) => R.reduce<Layer, number>(
    (acc, { depth, range }) => acc + depth * range,
    0,
    R.filter(R.propEq('scanner', 0), sim(firewall)),
);

const part2 = (firewall: Firewall) => {
    for (let i = 0;; ++i) {
        const sim2 = stepper((s, d) => s + d + i);
        if (R.any(R.propEq('scanner', 0), sim2(firewall))) {
            continue;
        }

        return i;
    }
};

(async () => {
    const input = await promisify(readFile)('day13/input.txt', 'utf8');
    const firewall = parse(R.split('\r\n', input));

    console.log(part1(firewall));
    console.log(part2(firewall));
})();
