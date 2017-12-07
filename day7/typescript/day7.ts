import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

interface Node {
    name: string;
    weight: number;
    children: string[];
}

interface State {
    answer?: number;
}

type Tree = { [name: string]: Node };
type Tuple = R.KeyValuePair<number, string>;

const parse = (line: string): Tree => {
    const [name, weight, _, ...nodes] = R.split(' ', line);
    return {
        [name]: {
            name,
            weight: parseInt(weight.slice(1, -1), 10),
            children: nodes.length > 0 ? [
                ...R.map(n => n.slice(0, -1), R.slice(0, nodes.length - 1, nodes)),
                nodes[nodes.length - 1],
            ] : [],
        },
    };
};

const part1 = (tree: Tree): string => {
    const nodes = R.values(tree);
    let node = R.find(
        R.propSatisfies(R.isEmpty, 'children'),
        nodes,
    );

    let name = '';
    while (node) {
        name = node.name;
        node = R.find(
            R.propSatisfies(R.contains(name), 'children'),
            nodes,
        );
    }

    return name;
};

const weigh = (name: string, tree: Tree, state: State): number => {
    const { weight, children } = tree[name];
    const weights = R.map<string, Tuple>(c => [weigh(c, tree, state), c], children);

    if (!state.answer && R.uniqBy(R.head, weights).length === 2) {
        const [bad, good] = R.compose<any, any, any, any, any>(
            R.map(R.compose(parseInt, R.head)),
            R.sortBy(R.last),
            R.toPairs,
            R.countBy(R.head),
        )(weights);

        const [_, node] = R.find(w => R.head(w) === bad, weights)!;
        state.answer = tree[node].weight + good - bad;
    }

    return weight + R.sum(R.map<Tuple, number>(R.head, weights));
};

const part2 = (tree: Tree) => {
    const state: State = {};
    weigh(part1(tree), tree, state);
    return state.answer;
};

(async () => {
    const input = await promisify(readFile)('day7/input.txt', 'utf8');
    const tree = R.reduce<Tree, Tree>(
        R.merge,
        {},
        R.map(parse, R.split('\r\n', input)),
    );

    console.log(part1(tree));
    console.log(part2(tree));
})();