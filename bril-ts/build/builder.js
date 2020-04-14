"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
/**
 * A utility for building up Bril programs.
 */
class Builder {
    constructor() {
        /**
         * The program we have built so far.
         */
        this.program = { functions: [] };
        this.curFunction = null;
        this.nextFresh = 0;
    }
    /**
     * Create a new, empty function into which further code will be generated.
     */
    buildFunction(name) {
        let func = { name, instrs: [] };
        this.program.functions.push(func);
        this.curFunction = func;
        this.nextFresh = 0;
        return func;
    }
    /**
     * Build an operation instruction that produces a result. If the name is
     * omitted, a fresh variable is chosen automatically.
     */
    buildValue(op, args, type, dest) {
        dest = dest || this.freshVar();
        let instr = { op, args, dest, type };
        this.insert(instr);
        return instr;
    }
    /**
     * Build a non-value-producing (side-effecting) operation instruction.
     */
    buildEffect(op, args) {
        let instr = { op, args };
        this.insert(instr);
        return instr;
    }
    /**
     * Build a constant instruction. As above, the destination name is optional.
     */
    buildConst(value, type, dest) {
        dest = dest || this.freshVar();
        let instr = { op: "const", value, dest, type };
        this.insert(instr);
        return instr;
    }
    /**
     * Build a constant integer value.
     */
    buildInt(value, dest) {
        return this.buildConst(value, "int", dest);
    }
    /**
     * Build a constant boolean value.
     */
    buildBool(value, dest) {
        return this.buildConst(value, "bool", dest);
    }
    /**
     * Add a label to the function at the current position.
     */
    buildLabel(name) {
        let label = { label: name };
        this.insert(label);
    }
    /**
     * Insert an instruction at the end of the current function.
     */
    insert(instr) {
        if (!this.curFunction) {
            throw "cannot build instruction/label without a function";
        }
        this.curFunction.instrs.push(instr);
    }
    /**
     * Generate an unused variable name.
     */
    freshVar() {
        let out = 'v' + this.nextFresh.toString();
        this.nextFresh += 1;
        return out;
    }
    /**
     * Generate an unused suffix.
     */
    freshSuffix() {
        let out = '.' + this.nextFresh.toString();
        this.nextFresh += 1;
        return out;
    }
}
exports.Builder = Builder;
//# sourceMappingURL=builder.js.map