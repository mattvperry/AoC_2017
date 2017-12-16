import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

type Programs = string[];
type Moves = { [type: string]: (ps: Programs, data: string[]) => Programs };

const programs = Array.from('abcdefghijklmnop');

const moves: Moves = {
    s: (ps, [num]) => spin(ps, +num),
    x: (ps, data) => exchange(ps, R.map(parseInt, data)),
    p: (ps, data) => exchange(ps, R.map(p => ps.indexOf(p), data)),
};

const spin = (ps: Programs, num: number) => [...ps.slice(-num), ...ps.slice(0, -num)];

const exchange = (ps: Programs, idx: number[]): Programs => {
    const [a, b] = R.map(R.lensIndex, idx);
    return R.compose<Programs, Programs, Programs>(
        R.set<string>(b, R.view(a, ps)),
        R.set<string>(a, R.view(b, ps)),
    )(ps);
};

const makeMoves = R.curry(R.reduce<string, Programs>(
    (acc, curr) => moves[curr.charAt(0)](acc, curr.slice(1).split('/')),
));

const part1 = R.compose(R.join(''), makeMoves(programs));

const part2 = (ms: string[]) => {
    let ps = programs;
    const seen = [];
    do {
        seen.push(ps);
        ps = makeMoves(ps, ms);
    } while (!R.equals(ps, programs));

    return R.join('', seen[1000000000 % seen.length]);
};

(async () => {
    const input = await promisify(readFile)('day16/input.txt', 'utf8');
    const ms = R.split(',', input);

    console.log(part1(ms));
    console.log(part2(ms));
})();