import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

type KVP = R.KeyValuePair<number, number>;
type XProd = KVP[];

const mapSum = (fn: (x: number[]) => number) => R.compose(R.sum, R.map(fn));

const part1 = mapSum(row => Math.max(...row) - Math.min(...row));

const part2 = mapSum(row =>
    R.compose<number[], XProd, XProd, number[], number[], number>(
        r => r[0],
        R.filter<number>(d => d % 1 === 0),
        R.map(R.apply<number, number>(R.divide)),
        R.filter<KVP>(R.apply<number, boolean>(R.compose(R.not, R.equals))),
        R.xprod<number>(row),
    )(row),
);

async function day2() {
    const input = await promisify(readFile)('day2/input.txt', 'utf8');
    const grid = input.split('\n')
        .map(r => r.split('\t').map(n => parseInt(n.trim(), 10)));

    console.log(part1(grid));
    console.log(part2(grid));
}

day2();