import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

enum Status {
    running,
    terminated,
    waiting,
}

type Registers = { [name: string]: number };
type Params = (number | string)[];
type Ops = { [name: string]: (this: Program, op: Params) => number | void };

interface Pipe {
    push: (value: number) => void;
}

class Program implements Pipe {
    public sends = 0;
    public status: Status = Status.running;

    private pc = 0;
    private regs: Registers = {};
    private sounds: number[] = [];
    private pipe: Pipe;
    private code: string[];
    private ops: Ops = {
        set: ([x, y]) => { this.regs[x] = this.lookup(y); },
        add: ([x, y]) => { this.regs[x] += this.lookup(y); },
        mul: ([x, y]) => { this.regs[x] *= this.lookup(y); },
        mod: ([x, y]) => { this.regs[x] %= this.lookup(y); },
        jgz: ([x, y]) => this.lookup(x) > 0 ? this.lookup(y) : 1,
        snd: ([x]) => { this.sends++; this.pipe.push(this.lookup(x)); },
        rcv: ([x]) => {
            const sound = this.sounds.pop();
            if (sound === undefined) {
                this.status = Status.waiting;
                return 0;
            }

            this.regs[x] = sound;
        },
    };

    constructor(id: number, lines: string[]) {
        this.regs.p = id;
        this.code = lines;
    }

    public step() {
        const [op, ...ps] = R.split(' ', this.code[this.pc]);
        this.pc += R.defaultTo(1, this.ops[op].call(this, ps));

        if (this.pc < 0 || this.pc >= this.code.length) {
            this.status = Status.terminated;
        }
    }

    public push(value: number) {
        this.sounds.unshift(value);
        if (this.status === Status.waiting) {
            this.status = Status.running;
        }
    }

    public connect(pipe: Pipe) {
        this.pipe = pipe;
    }

    private lookup(x: number | string) {
        return isNaN(+x) ? R.defaultTo(0, this.regs[x]) : +x;
    }
}

const part2 = (lines: string[]) => {
    const p0 = new Program(0, lines);
    const p1 = new Program(1, lines);

    p0.connect(p1);
    p1.connect(p0);

    while (p0.status === Status.running || p1.status === Status.running) {
        if (p0.status === Status.running) {
            p0.step();
        }

        if (p1.status === Status.running) {
            p1.step();
        }
    }

    return p1.sends;
};

(async () => {
    const input = await promisify(readFile)('day18/input.txt', 'utf8');
    const lines = R.split('\r\n', input);

    console.log(part2(lines));
})();