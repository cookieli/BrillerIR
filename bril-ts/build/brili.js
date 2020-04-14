#!/usr/bin/env node
"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const util_1 = require("./util");
const argCounts = {
    add: 2,
    mul: 2,
    sub: 2,
    div: 2,
    id: 1,
    lt: 2,
    le: 2,
    gt: 2,
    ge: 2,
    eq: 2,
    not: 1,
    and: 2,
    or: 2,
    print: null,
    br: 3,
    jmp: 1,
    ret: 0,
    nop: 0,
};
function get(env, ident) {
    let val = env.get(ident);
    if (typeof val === 'undefined') {
        throw `undefined variable ${ident}`;
    }
    return val;
}
/**
 * Ensure that the instruction has exactly `count` arguments,
 * throwing an exception otherwise.
 */
function checkArgs(instr, count) {
    if (instr.args.length != count) {
        throw `${instr.op} takes ${count} argument(s); got ${instr.args.length}`;
    }
}
function getInt(instr, env, index) {
    let val = get(env, instr.args[index]);
    if (typeof val !== 'bigint') {
        throw `${instr.op} argument ${index} must be a number`;
    }
    return val;
}
function getBool(instr, env, index) {
    let val = get(env, instr.args[index]);
    if (typeof val !== 'boolean') {
        throw `${instr.op} argument ${index} must be a boolean`;
    }
    return val;
}
let NEXT = { "next": true };
let END = { "end": true };
/**
 * Interpret an instruction in a given environment, possibly updating the
 * environment. If the instruction branches to a new label, return that label;
 * otherwise, return "next" to indicate that we should proceed to the next
 * instruction or "end" to terminate the function.
 */
function evalInstr(instr, env) {
    // Check that we have the right number of arguments.
    if (instr.op !== "const") {
        let count = argCounts[instr.op];
        if (count === undefined) {
            throw "unknown opcode " + instr.op;
        }
        else if (count !== null) {
            checkArgs(instr, count);
        }
    }
    switch (instr.op) {
        case "const":
            // Ensure that JSON ints get represented appropriately.
            let value;
            if (typeof instr.value === "number") {
                value = BigInt(instr.value);
            }
            else {
                value = instr.value;
            }
            env.set(instr.dest, value);
            return NEXT;
        case "id": {
            let val = get(env, instr.args[0]);
            env.set(instr.dest, val);
            return NEXT;
        }
        case "add": {
            let val = getInt(instr, env, 0) + getInt(instr, env, 1);
            env.set(instr.dest, val);
            return NEXT;
        }
        case "mul": {
            let val = getInt(instr, env, 0) * getInt(instr, env, 1);
            env.set(instr.dest, val);
            return NEXT;
        }
        case "sub": {
            let val = getInt(instr, env, 0) - getInt(instr, env, 1);
            env.set(instr.dest, val);
            return NEXT;
        }
        case "div": {
            let val = getInt(instr, env, 0) / getInt(instr, env, 1);
            env.set(instr.dest, val);
            return NEXT;
        }
        case "le": {
            let val = getInt(instr, env, 0) <= getInt(instr, env, 1);
            env.set(instr.dest, val);
            return NEXT;
        }
        case "lt": {
            let val = getInt(instr, env, 0) < getInt(instr, env, 1);
            env.set(instr.dest, val);
            return NEXT;
        }
        case "gt": {
            let val = getInt(instr, env, 0) > getInt(instr, env, 1);
            env.set(instr.dest, val);
            return NEXT;
        }
        case "ge": {
            let val = getInt(instr, env, 0) >= getInt(instr, env, 1);
            env.set(instr.dest, val);
            return NEXT;
        }
        case "eq": {
            let val = getInt(instr, env, 0) === getInt(instr, env, 1);
            env.set(instr.dest, val);
            return NEXT;
        }
        case "not": {
            let val = !getBool(instr, env, 0);
            env.set(instr.dest, val);
            return NEXT;
        }
        case "and": {
            let val = getBool(instr, env, 0) && getBool(instr, env, 1);
            env.set(instr.dest, val);
            return NEXT;
        }
        case "or": {
            let val = getBool(instr, env, 0) || getBool(instr, env, 1);
            env.set(instr.dest, val);
            return NEXT;
        }
        case "print": {
            let values = instr.args.map(i => get(env, i).toString());
            console.log(...values);
            return NEXT;
        }
        case "jmp": {
            return { "label": instr.args[0] };
        }
        case "br": {
            let cond = getBool(instr, env, 0);
            if (cond) {
                return { "label": instr.args[1] };
            }
            else {
                return { "label": instr.args[2] };
            }
        }
        case "ret": {
            return END;
        }
        case "nop": {
            return NEXT;
        }
    }
    util_1.unreachable(instr);
    throw `unhandled opcode ${instr.op}`;
}
function evalFunc(func) {
    let env = new Map();
    for (let i = 0; i < func.instrs.length; ++i) {
        let line = func.instrs[i];
        if ('op' in line) {
            let action = evalInstr(line, env);
            if ('label' in action) {
                // Search for the label and transfer control.
                for (i = 0; i < func.instrs.length; ++i) {
                    let sLine = func.instrs[i];
                    if ('label' in sLine && sLine.label === action.label) {
                        break;
                    }
                }
                if (i === func.instrs.length) {
                    throw `label ${action.label} not found`;
                }
            }
            else if ('end' in action) {
                return;
            }
        }
    }
}
function evalProg(prog) {
    for (let func of prog.functions) {
        if (func.name === "main") {
            evalFunc(func);
        }
    }
}
function main() {
    return __awaiter(this, void 0, void 0, function* () {
        let prog = JSON.parse(yield util_1.readStdin());
        evalProg(prog);
    });
}
// Make unhandled promise rejections terminate.
process.on('unhandledRejection', e => { throw e; });
main();
//# sourceMappingURL=brili.js.map