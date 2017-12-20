import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

type Groups = { [key: string]: Particle[] };
type Vector = [number, number, number];
interface Particle {
    id: number;
    position: Vector;
    velocity: Vector;
    acceleration: Vector;
}

const parse = (line: string, id: number): Particle => {
    const [[p, pv], [v, vv], [a, av]] = R.map(R.split('='), R.split(', ', line));
    const [px, py, pz] = R.split(',', pv.slice(1, -1));
    const [vx, vy, vz] = R.split(',', vv.slice(1, -1));
    const [ax, ay, az] = R.split(',', av.slice(1, -1));
    return {
        id,
        position: [+px, +py, +pz],
        velocity: [+vx, +vy, +vz],
        acceleration: [+ax, +ay, +az],
    };
};

const add = ([x1, y1, z1]: Vector, [x2, y2, z2]: Vector): Vector => [x1 + x2, y1 + y2, z1 + z2];

const move = ({ id, position, velocity, acceleration }: Particle) => {
    const newV = add(velocity, acceleration);
    const newP = add(position, newV);
    return {
        id,
        position: newP,
        velocity: newV,
        acceleration,
    };
};

const dist = ([x, y, z]: Vector) => Math.abs(x) + Math.abs(y) + Math.abs(z);

const part1 = (particles: Particle[]) => {
    const accels = R.map(p => ({ id: p.id, a: dist(p.acceleration) }), particles);
    const min = accels.reduce((acc, curr) => curr.a < acc.a ? curr : acc);
    return min.id;
};

const part2 = (particles: Particle[]) => {
    for (let i = 0; i < 1000; ++i) {
        particles = R.pipe<Particle[], Groups, Particle[][], Particle[][], Particle[], Particle[]>(
            R.groupBy<Particle>(p => p.position.join(' ')),
            R.values,
            R.filter<Particle[]>(ps => ps.length === 1),
            ps => R.flatten<Particle>(ps),
            R.map(move),
        )(particles);
    }

    return particles.length;
};

(async () => {
    const input = await promisify(readFile)('day20/input.txt', 'utf8');
    const particles = R.addIndex(R.map)(parse, R.split('\r\n', input));

    console.log(part1(particles));
    console.log(part2(particles));
})();