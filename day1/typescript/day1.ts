import { readFile } from 'fs';
import { promisify } from 'util';

async function day1() {
    const input = await promisify(readFile)('day1/input.txt', 'utf8');
    console.log(input);
}

day1();