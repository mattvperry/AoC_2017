import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

interface Registers {
    [name: string]: number;
}

interface Instruction {
    register: string;
    amount: number;
    condition: (g: (r: string) => number) => boolean;
}

const ops: { [op: string]: (x: number, y: number) => boolean } = {
    ['>']: R.gt,
    ['<']: R.lt,
    ['>=']: R.gte,
    ['<=']: R.lte,
    ['==']: R.equals,
    ['!=']: R.compose(R.not, R.equals),
};

const parse = (ins: string): Instruction => {
    const [register, move, num, _, cond, op, val] = R.split(' ', ins);
    const amount = parseInt(num, 10);
    return {
        register,
        amount: move === 'inc' ? amount : -1 * amount,
        condition: getter => ops[op](getter(cond), parseInt(val, 10)),
    };
};

const solve = (instructions: Instruction[]): [number, Registers] => {
    let max = 0;
    const regs: Registers = {};
    for (const { register, amount, condition } of instructions) {
        if (!condition(reg => regs[reg] || 0)) {
            continue;
        }

        regs[register] = (regs[register] || 0) + amount;
        max = R.max(max, regs[register]);
    }

    return [max, regs];
};

(async () => {
    const input = await promisify(readFile)('day8/input.txt', 'utf8');
    const [max, regs] = R.compose(
        solve,
        R.map(parse),
        R.split('\r\n'),
    )(input);

    console.log(Math.max(...R.values(regs)));
    console.log(max);
})();