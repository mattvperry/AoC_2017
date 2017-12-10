import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

type Data = [number, number, number[]];

const sparse = (lengths: number[]) => (pos: number, skip: number, list: number[]): Data => {
    for (const length of lengths) {
        const idxs = R.times(n => (pos + n) % list.length, length);
        const swaps = R.zip(idxs, R.reverse(idxs));
        for (const [a, b] of R.take(Math.floor(length / 2), swaps)) {
            const tmp = list[a];
            list[a] = list[b];
            list[b] = tmp;
        }

        pos = (pos + length + skip++) % list.length;
    }

    return [pos, skip, list];
}

const part1 = (input: string) => {
    const hash = sparse(R.map(
       parseInt, 
       R.split(',', input)
    ));

    const [pos, skip, [x, y, ...xs]] = hash(0, 0, R.times(R.identity, 256));
    return x * y;
}

const part2 = (input: string) => {
    const hash = sparse(R.concat(
        R.map(c => c.charCodeAt(0), Array.from(input)),
        [17, 31, 73, 47, 23],
    ));

    return R.compose<number[], Data, number[], number[][], number[], string[], string>(
        R.join(''),
        R.map(n => n.toString(16)),
        R.map(R.reduce((acc, curr) => acc ^ curr, 0)),
        R.splitEvery(16),
        R.last,
        R.reduce<number, Data>(
            ([p, s, l], _) => hash(p, s, l),
            [0, 0, R.times(R.identity, 256)],
        ),
    )(R.times(R.identity, 64));
}

(async () => {
    const input = await promisify(readFile)('day10/input.txt', 'utf8');

    console.log(part1(input));
    console.log(part2(input));
})();
