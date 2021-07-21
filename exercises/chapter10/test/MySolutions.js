"use strict";
const addComplex = a => b => {
    return {
        real: a.real + b.real,
        imag: a.imag + b.imag
    }
}
// Note to reader: Add your solutions to this file
function volumeFn(l, w, h) {
    return l * w * h
}

exports.volumeFn = volumeFn
exports.volumeArrow = l => w => h => l * h * w
exports.cumulativeSumsComplex = a => a.reduce((acc, cur) => {
    const tot = addComplex(acc.tot)(cur)
    return { tot, v: [...acc.v, tot] }
}, { tot: { real: 0.0, imag: 0.0 }, v: [] }).v

exports.quadraticRootsImpl = pair => q => {
    const { a, b, c } = q
    const d = b * b - 4 * a * c
    const p1 = -b / (2 * a)
    const p2 = Math.sqrt(Math.abs(d)) / (2 * a)
    if (d < 0) return pair({ real: p1, imag: p2 })({ real: p1, imag: -p2 })
    return pair({ real: p1 + p2, imag: 0 })({ real: p1 - p2, imag: 0 })
}

exports.valuesOfMapJ = m => [...new Map(m).values()]

exports.quadraticRootsSetImpl = q => {
    const { a, b, c } = q
    const d = b * b - 4 * a * c
    const p1 = -b / (2 * a)
    const p2 = Math.sqrt(Math.abs(d)) / (2 * a)
    if (d < 0) return [{ real: p1, imag: p2 }, { real: p1, imag: -p2 }]
    return [{ real: p1 + p2, imag: 0 }, { real: p1 - p2, imag: 0 }]
}