import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

const doJumps = (change: (jump: number) => number) => (data: number[]) => {
    let offset = 0;
    for (let count = 0; true; ++count) {
        if (offset >= data.length || offset < 0) {
            return count;
        }
        const jump = data[offset];
        data[offset] += change(jump);
        offset += jump;
    }
};

const part1 = doJumps(_ => 1);

const part2 = doJumps(jump => jump >= 3 ? -1 : 1);

async function day5() {
    const input = await promisify(readFile)('day5/input.txt', 'utf8');
    const jumps = R.map(parseInt, input.split('\r\n'));

    console.log(part1([...jumps]));
    console.log(part2([...jumps]));
}

day5();