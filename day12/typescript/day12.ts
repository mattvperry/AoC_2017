import { readFile } from 'fs';
import { promisify } from 'util';

import * as R from 'ramda';

export type Graph = { [node: string]: string[] };

const makeGraph = (conns: string[][]) => {
    const graph: Graph = {};
    for (const [from, _, ...tos] of conns) {
        graph[from] = (graph[from] || []).concat(tos);
    }

    return graph;
};

const findGroup = (graph: Graph, node: string) => {
    const visited = new Set();
    const toVisit = [node];

    while (toVisit.length > 0) {
        const visit = toVisit.pop()!;
        if (visited.has(visit)) {
            continue;
        }

        visited.add(visit);
        toVisit.push(...graph[visit]);
    }

    return Array.from(visited);
};

const part1 = (graph: Graph) => findGroup(graph, '0').length;

export const part2 = (graph: Graph) => {
    let count = 0;
    let nodes = R.keys(graph);
    while (nodes.length > 0) {
        const node = nodes.pop()!;
        nodes = R.difference(nodes, findGroup(graph, node));
        count++;
    }

    return count;
};

(async () => {
    const input = await promisify(readFile)('day12/input.txt', 'utf8');
    const conns = R.map(R.split(/,? /), R.split('\r\n', input));
    const graph = makeGraph(conns);

    if (!module.parent) {
        console.log(part1(graph));
        console.log(part2(graph));
    }
})();
