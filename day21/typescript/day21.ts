import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

type Image = string[][];

interface Rules {
    [from: string]: string;
}

const seed = [
    ['.', '#', '.'],
    ['.', '.', '#'],
    ['#', '#', '#'],
];

const parse = (lines: string[]): Rules => R.mergeAll(
    R.map(
        R.pipe(R.split(' => '), ([f, t]) => ({ [f]: t })),
        lines,
    ),
);

const toImage = R.pipe(R.split('/'), R.map(Array.from));

const toString = R.pipe(R.map(R.join('')), R.join('/'));

const flipH = R.map<string[], string[]>(R.reverse);

const flipV = R.reverse;

const rotateLeft = R.pipe(flipH, R.transpose);

const rotateRight = R.pipe<Image, Image, Image>(R.transpose, flipH);

const transform = (rules: Rules) => (image: Image): Image => {
    const transforms = [R.identity, flipH, flipV, rotateLeft, rotateRight];
    const projections = R.xprod(transforms, transforms);
    for (const [a, b] of projections) {
        const flat = toString(R.pipe(a, b)(image));
        if (rules[flat] !== undefined) {
            return toImage(rules[flat]);
        }
    }

    throw new Error('Missing rule.');
};

const split = (size: number) => R.pipe<Image, Image[], Image[], Image[][], Image[][], Image[]>(
    R.map(R.splitEvery(size)),
    R.transpose,
    R.map(R.splitEvery(size)),
    R.transpose,
    x => R.unnest<string[][]>(x),
);

const concat = (images: Image[]) => images.reduce(
    (a, b) => R.zipWith<string[], string[], string[]>(R.concat, a, b),
);

const combine = (size: number) => (images: Image[]): Image => {
    const rows = R.map(concat, R.splitEvery(size, images));
    return R.transpose(concat(R.map(R.transpose, rows)));
};

const count = (num: number) => (rules: Rules) => {
    let image: Image = R.map(r => [...r], seed);
    for (const _ of R.range(0, num)) {
        const size = image.length % 2 === 0 ? 2 : 3;
        image = R.pipe(
            split(size),
            R.map(transform(rules)),
            combine(image.length / size),
        )(image);
    }

    return R.countBy(c => c, R.flatten<string>(image))['#'];
};

const part1 = count(5);

const part2 = count(18);

(async () => {
    const input = await promisify(readFile)('day21/input.txt', 'utf8');
    const rules = parse(R.split('\r\n', input));

    console.log(part1(rules));
    console.log(part2(rules));
})();