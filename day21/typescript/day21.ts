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

const toImage = R.pipe(R.split('/'), R.map(Array.from));

const toString = R.pipe(R.map(R.join('')), R.join('/'));

const projections = (image: Image) => R.map(toString, R.scan(
    (acc, curr) => curr(acc),
    image,
    R.unnest<(image: Image) => Image>(R.repeat([R.transpose, R.reverse], 4)),
));

const parse = (lines: string[]): Rules => R.mergeAll(
    R.chain(
        R.pipe(
            R.split(' => '),
            ([f, t]) => R.map(
                p => ({ [p]: t }),
                projections(toImage(f)),
            ),
        ),
        lines,
    ),
);

const transform = (rules: Rules) => (image: Image): Image => (
    toImage(rules[toString(image)])
);

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

const solve = (num: number) => (rules: Rules) => {
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

const part1 = solve(5);

const part2 = solve(18);

(async () => {
    const input = await promisify(readFile)('day21/input.txt', 'utf8');
    const rules = parse(R.split('\r\n', input));

    console.log(part1(rules));
    console.log(part2(rules));
})();