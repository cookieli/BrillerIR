"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
/**
 * Read all the data from stdin as a string.
 */
function readStdin() {
    return new Promise((resolve, reject) => {
        let chunks = [];
        process.stdin.on("data", function (chunk) {
            chunks.push(chunk);
        }).on("end", function () {
            resolve(chunks.join(""));
        }).setEncoding("utf8");
    });
}
exports.readStdin = readStdin;
function unreachable(x) {
    throw "impossible case reached";
}
exports.unreachable = unreachable;
//# sourceMappingURL=util.js.map