import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

type Counter = { [key: string]: number };

const part1 = R.compose<string[][], Counter[], Counter[], number>(
    R.length,
    R.filter(R.compose<Counter, number[], boolean>(
        R.all(R.equals(1)),
        R.values,
    )),
    R.map(R.countBy(R.identity)),
);

const part2 = R.compose(
    part1,
    R.map(R.map(R.compose(
        R.join(''),
        R.sort(R.ascend(R.identity)),
        Array.from,
    ))),
);

async function day4() {
    const input = await promisify(readFile)('day4/input.txt', 'utf8');
    const words = R.compose(
        R.map(R.split(' ')),
        R.split('\r\n'),
    )(input);

    console.log(part1(words));
    console.log(part2(words));
}

day4();