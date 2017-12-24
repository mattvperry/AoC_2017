import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

type Component = [number, number];
type Bridge = Component[];

const parse = (line: string): Component => {
    const [i, o] = R.split('/', line);
    return [+i, +o];
};

const find = (port: number) => R.filter<Component>(
    ([i, o]) => i === port || o === port,
);

const construct = (end: number, cs: Component[], path: Bridge): Bridge[] => {
    const matches = find(end)(cs);
    if (matches.length === 0) {
        return [path];
    }

    return R.chain<Component, Bridge>(
        c => construct(
            c[0] === end ? c[1] : c[0],
            R.remove(cs.indexOf(c), 1, cs),
            [...path, c],
        ),
        matches,
    );
};

const strongest = R.pipe(
    R.map<Bridge, number>(
        R.reduce((acc, [i, o]) => acc + i + o, 0),
    ),
    R.reduce<number, number>(R.max, 0),
);

const solve = (cs: Component[]) => {
    const paths = construct(0, cs, []);

    const part1 = strongest(paths);

    const len = R.reduce<number, number>(R.max, 0, R.map(x => x.length, paths));
    const longest = R.filter(p => p.length === len, paths);
    const part2 = strongest(longest);

    return [part1, part2];
};

(async () => {
    const input = await promisify(readFile)('day24/input.txt', 'utf8');
    const components = R.map(parse, R.split('\r\n', input));

    console.log(solve(components));
})();