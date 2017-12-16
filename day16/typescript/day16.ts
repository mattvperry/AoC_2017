import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

interface Spin {
    kind: 'spin';
    data: number;
}

interface Exchange {
    kind: 'exchange';
    data: number[];
}

interface Partner {
    kind: 'partner';
    data: string[];
}

type Move = Spin | Exchange | Partner;
type Programs = string[];

const programs = Array.from('abcdefghijklmnop');

const parse = R.map<string, Move>(
    (m: string) => {
        switch (R.head(m)) {
            case 's':
                return { kind: 'spin', data: parseInt(R.tail(m), 10) };
            case 'x':
                return { kind: 'exchange', data: R.map(parseInt, R.split('/', R.tail(m))) };
            default:
                return { kind: 'partner', data: R.split('/', R.tail(m)) };
        }
    },
);

const spin = (ps: Programs, s: Spin) => R.compose<Programs, Programs[], Programs[], Programs>(
    x => R.flatten<string>(x),
    R.reverse,
    R.splitAt(ps.length - s.data),
)(ps);

const exchange = (ps: Programs, e: Exchange): Programs => {
    const [a, b] = R.map(R.lensIndex, e.data);
    return R.compose<Programs, Programs, Programs>(
        R.set<string>(b, R.view(a, ps)),
        R.set<string>(a, R.view(b, ps)),
    )(ps);
};

const partner = (ps: Programs, p: Partner): Programs => {
    return exchange(ps, {
        kind: 'exchange',
        data: R.map(c => R.findIndex(R.equals(c), ps), p.data),
    });
};

const makeMoves = (ps: Programs) => R.reduce<Move, Programs>(
    (acc, curr) => {
        switch (curr.kind) {
            case 'spin':
                return spin(acc, curr);
            case 'exchange':
                return exchange(acc, curr);
            default:
                return partner(acc, curr);
        }
    },
    ps,
);

const part1 = R.compose(R.join(''), makeMoves(programs));

const part2 = (moves: Move[]) => {
    let ps = [...programs];
    let count = 0;
    do {
        ps = makeMoves(ps)(moves);
        count++;
    } while (ps.join('') !== programs.join(''));

    const rem = 1000000000 % count;
    for (const _ of R.range(0, rem)) {
        ps = makeMoves(ps)(moves);
    }

    return ps.join('');
};

(async () => {
    const input = await promisify(readFile)('day16/input.txt', 'utf8');
    const moves = parse(R.split(',', input));

    console.log(part1(moves));
    console.log(part2(moves));
})();