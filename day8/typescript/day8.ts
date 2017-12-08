import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

interface Registers {
    [name: string]: number;
}

interface Instruction {
    register: string;
    amount: number;
    condition: string[];
}

const getRegister = (regs: Registers, reg: string) => R.compose<Registers, number, number>(
    R.defaultTo(0),
    R.prop(reg),
)(regs);

const parse = (ins: string) => {
    const [register, op, num, _, ...condition] = R.split(' ', ins);
    const amount = parseInt(num, 10);
    return {
        register,
        amount: op === 'inc' ? amount : -1 * amount,
        condition,
    };
};

const solve = (instructions: Instruction[]): [number, Registers] => {
    let max = 0;
    const regs: Registers = {};
    for (const { register, amount, condition } of instructions) {
        const [cond, op, val] = condition;
        if (eval(`${getRegister(regs, cond)} ${op} ${val}`)) {
            regs[register] = getRegister(regs, register) + amount;
            max = R.max(max, regs[register]);
        }
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