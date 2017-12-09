import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

const solve = (chars: string[]) => {
    let garbage = false;
    let depth = 0;
    let sum = 0;
    let count = 0;
    while (chars.length > 0) {
        const c = chars.pop();
        switch(c) {
            case '!':
                chars.pop();
                break;
            case '<':
                garbage ? count++ : garbage = true;
                break;
            case '>':
                garbage = false;
                break;
            case '{':
                garbage ? count++ : sum += ++depth;
                break;
            case '}':
                garbage ? count++ : depth--;
                break;
            default:
                if (garbage) count++;
                break;
        }
    }

    return [sum, count];
}

(async () => {
    const input = await promisify(readFile)('day9/input.txt', 'utf8');
    const chars = Array.from(input).reverse();

    const [part1, part2] = solve(chars);
    console.log(part1);
    console.log(part2);
})();