#!/usr/bin/env deno

// AoC 2024 Day 25

// note: this script works on Deno only

import * as path from "jsr:@std/path";

enum ElementKind {
    Key,
    Lock,
}

interface Element {
    kind: ElementKind;
    values: number[];
}

interface Parsed {
    keys: number[][],
    locks: number[][]
};

type ParseError = Error & { input: string };

const DIM = 5;

export function check(s: string): string {
    if (s !in ["#", "."]) {
        throw { input: s, cause: "invalid character" } as ParseError;
    }
    
    return s;
}

export function countClashes(parsed: Parsed): number {
    return parsed.keys.reduce((acc, key) => {
        return parsed.locks.reduce((acc, lock) => {
            const clash = findClash(key,lock);

            return clash === undefined ? acc + 1 : acc;
        }, acc);
    }, 0);
}

export function findClash(key: number[], lock: number[]): number | undefined {
    const zipped = (): [number, number][] => key.map((k, i) => [k, lock[i]]);

    for (const [i, [k, l]] of zipped().entries()) {
        if (k + l > DIM) {
            return i;
        }
    }
}

export function parseElement(s: string): Element {
    const rows = s.trim().split("\n").map(row => row.trim());

    if (rows.length != DIM + 2) {
        throw { input: s, cause: `invalid element: wrong number of columns (${rows.length})` } as ParseError;
    }

    const cols = rows[0].length;
    if (cols != DIM) {
        throw { input: s, cause: `malformed element, ${cols} columns are not the right amount (${DIM})` } as ParseError;
    }

    const kind = check(rows[0][0]) === "#" ? ElementKind.Lock : ElementKind.Key;

    const values = rows
        .slice(1, -1)
        .reduce((acc, row) => {
            if (row.length !== DIM) {
                throw { input: s, cause: `malformed element of size ${row.length}, expected ${cols}` } as ParseError;
            }

            for (const [i, item] of row.split("").entries()) {
                acc[i] += check(item) === "#" ? 1 : 0;
            }

            return acc;
        }, new Array<number>(cols).fill(0));

    return { kind, values };
}

export async function parseInput(fname: string): Promise<Parsed> {
    const content = await Deno.readTextFile(fname);

    return content.split("\n\n").map(parseElement).reduce((acc, element) => {
        switch (element.kind) {
            case ElementKind.Key:
                acc.keys.push(element.values);
                break;

            case ElementKind.Lock:
                acc.locks.push(element.values);
                break;

        }

        return acc;
    }, {keys: [], locks: []} as Parsed);
}

async function main(fname: string) {    
    const elements = await parseInput(fname);

    console.log(`part 1: ${countClashes(elements)}`);
}

if (Deno.mainModule === import.meta.url) {
    if (Deno.args.length !== 1) {
        console.error(`error: invalid number of arguments\nusage: ${path.basename(import.meta.filename!)} <input>`);
        
        Deno.exit(2);
    }

    const fname = Deno.args[0];
    
    await main(fname);
}
