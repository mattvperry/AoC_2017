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

function* stepper(fn: (newScanner: number) => number, firewall: Firewall): IterableIterator<Layer> {
    for (const { scanner, depth, range } of firewall) {
        yield { depth, range, scanner: fn(scanner + depth) % (range * 2 - 2) };
    }
}

const part1 = (firewall: Firewall) => R.reduce<Layer, number>(
    (acc, { depth, range }) => acc + depth * range,
    0,
    R.filter(R.propEq('scanner', 0), Array.from(stepper(R.identity, firewall))),
);

const part2 = (firewall: Firewall) => {
    for (let i = 0;; ++i) {
        let ded = false;
        for (const layer of stepper(R.add(i), firewall)) {
            if (layer.scanner === 0) {
                ded = true;
                break;
            }
        }

        if (!ded) {
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
