'use strict'

function is(xs) {return typeof xs[Symbol.iterator] === 'function' || typeof xs.next === 'function'}
function generator(gen) {return (...args) => new Iter(gen(...args))}
const G = generator
function from(iter) {return new Iter(iter[Symbol.iterator] ? iter[Symbol.iterator]() : iter)}
const empty = G(function*() {})

const range = G(function*(start, end, skip = 1) {
  if (end === undefined) [start, end] = [0, start]
  if (skip > 0) for (let i = start; i < end; i += skip) yield i
  else for (let i = start; i > end; i += skip) yield i
})
const irange = G(function*(start = 0, skip = 1) {
  for (let i = start; ; i += skip) yield i
})
const replicate = G(function*(n, x) {for (let i = 0; i < n; ++i) yield x})
const forever = G(function*(x) {for (;;) yield x})
const iterate = G(function*(x, fn) {for (;;) {yield x; x = fn(x)}})

const _keys = Object.keys
const entries = G(function*(o) {for (const k of _keys(o)) yield [k, o[k]]})
function keys(o) {return new Iter(_keys(o)[Symbol.iterator]())}
const values = G(function*(o) {for (const k of _keys(o)) yield o[k]})

function split(n = 2, xs) {return new SplitSource(n, xs).derived}
const cycle = G(function*(xs) {
  const cache = []
  for (const x of xs) {
    cache.push(x)
    yield x
  }
  for (;;) yield* cache
})
const repeat = G(function*(n, xs) {
  if (n <= 0) return
  const cache = []
  for (const x of xs) {
    cache.push(x)
    yield x
  }
  for (let i = 1; i < n; ++i) yield* cache
})
const enumerate = G(function*(xs) {let i = 0; for (const x of xs) yield [i++, x]})
const map = G(function*(fn, xs) {for (const x of xs) yield fn(x)})
const flatMap = G(function*(fn, xs) {for (const x of xs) yield* fn(x)})
const tap = G(function*(fn, xs) {for (const x of xs) {fn(x); yield x}})
const filter = G(function*(fn, xs) {for (const x of xs) if (fn(x)) yield x})
const reject = G(function*(fn, xs) {for (const x of xs) if (!fn(x)) yield x})
const concat = G(function*(...xss) {for (const xs of xss) yield* xs})
const push = G(function*(...ys) {const xs = ys.pop(); yield* xs; yield* ys})
const unshift = G(function*(...ys) {const xs = ys.pop(); yield* ys; yield* xs})
const flatten = G(function*(xs) {for (const x of xs) yield* x})
const chunksOf = G(function*(n, xs) {
  let list = []
  for (const x of xs) {
    if (list.length >= n) {yield list; list = []}
    list.push(x)
  }
  if (list.length) yield list
})
const subsequences = G(function*(xs, n = 2) {
  if (xs[Symbol.iterator]) xs = xs[Symbol.iterator]()
  let buffer = []
  let value, done
  while (buffer.length < n && ({value, done} = xs.next()) && !done) {
    buffer.push(value)
  }
  if (!done) while (({value, done} = xs.next()) && !done) {
    yield buffer
    buffer = buffer.slice(1)
    buffer.push(value)
  }
  if (buffer.length === n) yield buffer
})
const lookahead = G(function*(xs, n = 1) {
  if (xs[Symbol.iterator]) xs = xs[Symbol.iterator]()
  let buffer = []
  let value, done
  while (buffer.length < n && ({value, done} = xs.next()) && !done) {
    buffer.push(value)
  }
  if (!done) while (({value, done} = xs.next()) && !done) {
    buffer.push(value)
    yield buffer
    buffer = buffer.slice(1)
  }
  for (let i = buffer.length - 1; i-- >= 0;) {
    yield buffer
    buffer = buffer.slice(1)
  }
})
const drop = G(function*(n, xs) {for (const x of xs) if (n <= 0) yield x; else --n})
const dropWhile = G(function*(fn, xs) {let init = true; for (const x of xs) if (!init || !fn(x)) {init = false; yield x}})
const dropLast = G(function*(n, xs) {
  if (n === 0) yield* xs; else {
    const list = []
    let i = 0
    for (const x of xs) {
      if (i >= n) yield list[i % n]
      list[i++ % n] = x
    }
  }
})
const take = G(function*(n, xs) {if (n <= 0) return; for (const x of xs) {yield x; if (--n <= 0) return}})
const takeWhile = G(function*(fn, xs) {for (const x of xs) if (fn(x)) yield x; else return})
const takeLast = G(function*(n, xs) {
  const list = []
  let i = 0
  for (const x of xs) list[i++ % n] = x
  if (n > list.length) n = list.length
  for (let j = 0; j < n; j++) yield list[(i + j) % n]
})
const zip = G(function*(...xss) {
  const its = xss.map(xs => xs[Symbol.iterator]())
  for (;;) {
    const rs = its.map(it => it.next())
    if (rs.some(r => r.done)) return
    yield rs.map(r => r.value)
  }
})

function every(fn, xs) {for (const x of xs) if (!fn(x)) return false; return true}
function some(fn, xs) {for (const x of xs) if (fn(x)) return true; return false}
function find(fn, xs) {for (const x of xs) if (fn(x)) return x}
function findLast(fn, xs) {let y; for (const x of xs) if (fn(x)) y = x; return y}
function findIndex(fn, xs) {let i = 0; for (const x of xs) {if (fn(x)) return i; ++i} return -1}
function findLastIndex(fn, xs) {let i = 0, j = -1; for (const x of xs) {if (fn(x)) j = i; ++i} return j}
function indexOf(y, xs) {let i = 0; for (const x of xs) {if (x === y) return i; ++i} return -1}
function lastIndexOf(y, xs) {let i = 0, j = -1; for (const x of xs) {if (x === y) j = i; ++i} return j}
function includes(y, xs) {for (const x of xs) if (x === y) return true; return false}
function reduce(a, fn, xs) {for (const x of xs) a = fn(a, x); return a}
function inject(a, fn, xs) {for (const x of xs) fn(a, x); return a}

function first(xs) {if (Array.isArray(xs)) return xs[0]; for (const x of xs) return x}
const head = first
function last(xs) {if (Array.isArray(xs)) return xs[xs.length - 1]; let z; for (const x of xs) z = x; return z}
function tail(xs) {return drop(1, xs)}
function init(xs) {return dropLast(1, xs)}

function count(xs) {if (Array.isArray(xs)) return xs.length; let i = 0; for (const x of xs) ++i; return i}
function pick(i, xs) {if (Array.isArray(xs)) return xs[i]; for (const x of xs) if (i-- <= 0) return x}

function sum(xs) {return reduce(0, (x, y) => x + Number(y), xs)}
function product(xs) {return reduce(1, (x, y) => x * y, xs)}
function max(xs) {return reduce(-Infinity, Math.max, xs)}
function min(xs) {return reduce(Infinity, Math.min, xs)}
function minMax(xs) {
  let min = Infinity, max = -Infinity
  for (const x of xs) {
    const b = +x
    if (b < min) min = b
    if (b > max) max = b
  }
  return [min, max]
}

function groupBy(fn, unique, xs) {
  if (!xs) [unique, xs] = [false, unique]
  return inject(new Map, unique ? (m, x) => {
    const k = fn(x), s = m.get(k)
    if (s) s.add(x)
    else m.set(k, new Set([x]))
  } : (m, x) => {
    const k = fn(x), l = m.get(k)
    if (l) l.push(x)
    else m.set(k, [x])
  }, xs)
}
function keyBy(fn, xs) {
  return inject(new Map, (m, x) => m.set(fn(x), x), xs)
}

const unique = G(function*(xs) {
  const used = new Set
  for (const x of xs) {
    if (!used.has(x)) {
      yield x
      used.add(x)
    }
  }
})

function toArray(xs) {return Array.from(xs)}
const array = toArray
function toMap(xs) {return new Map(xs)}
function toSet(xs) {return new Set(xs)}
function toObject(xs, empty = false) {
  const o = empty ? Object.create(null) : {}
  for (const [k, v] of xs) {
    o[k] = v
  }
  return o
}
const intersperse = G(function*(sep, xs) {
  let use = false
  for (const x of xs) {
    if (use) yield sep
    yield x
    use = true
  }
})
function join(sep, xs) {
  if (!xs) [sep, xs] = [',', sep]
  let s = ''
  if (sep) {
    let use = false
    for (const x of xs) {
      if (use) s += sep
      s += x
      use = true
    }
  } else {
    for (const x of xs) s += x
  }
  return s
}

const slice = G(function*(xs, start = 0, end) {
  if (Array.isArray(xs)) {
    if (start < 0) start += array.length
    if (end === undefined) end = array.length
    else if (end < 0) end += array.length
    for (let i = start; i < end; ++i) yield array[i]
  } else if (end === undefined) {
    yield* start < 0 ? takeLast(-start, xs) : drop(start, xs)
  } else if (start >= 0) {
    let i = 0
    if (end === 0) return
    else if (end > 0) {
      for (const x of xs) {
        if (i >= start) yield x
        if (++i >= end) return
      }
    } else {
      // yield* dropLast(-end, drop(start, xs))
      const list = []
      const n = -end
      for (const x of xs) {
        if (i >= start) {
          const k = (i - start) % n
          if (i - start >= n) yield list[k]
          list[k] = x
        }
        ++i
      }
    }
  } else {
    // yield* dropLast(-end, takeLast(-start, xs))
    const list = []
    let n = -start
    let i = 0
    for (const x of xs) list[i++ % n] = x
    if (n > list.length) n = list.length
    for (let j = 0; j < n + end; j++) yield list[(i + j) % n]
  }
})

class Iter {
  constructor(iter) {this.iter = iter}
  [Symbol.iterator]() {return this.iter}
  next() {return this.iter.next()}
  toArray() {return Array.from(this.iter)}
  array() {return Array.from(this.iter)}
  toMap() {return new Map(this.iter)}
  toSet() {return new Set(this.iter)}
  toObject(empty = false) {return toObject(this.iter, empty)}
  join(sep = ',') {return join(sep, this.iter)}
  intersperse(sep) {return intersperse(sep, this.iter)}

  split(n = 2) {return split(n, this.iter)}
  repeat(n) {return repeat(n, this.iter)}
  cycle() {return cycle(this.iter)}
  enumerate() {return enumerate(this.iter)}
  map(fn) {return map(fn, this.iter)}
  flatMap(fn) {return flatMap(fn, this.iter)}
  tap(fn) {return tap(fn, this.iter)}
  filter(fn) {return filter(fn, this.iter)}
  reject(fn) {return reject(fn, this.iter)}
  concat(...xss) {return concat(this.iter, ...xss)}
  push(...xs) {return push(...xs, this.iter)}
  unshift(...xs) {return unshift(...xs, this.iter)}
  flatten() {return flatten(this.iter)}
  chunksOf(n) {return chunksOf(n, this.iter)}
  lookahead(n) {return lookahead(this.iter, n)}
  subsequences(n) {return subsequences(this.iter, n)}
  drop(n) {return drop(n, this.iter)}
  dropWhile(fn) {return dropWhile(fn, this.iter)}
  dropLast(n) {return dropLast(n, this.iter)}
  take(n) {return take(n, this.iter)}
  takeWhile(fn) {return takeWhile(fn, this.iter)}
  takeLast(n) {return takeLast(n, this.iter)}
  zip(...xss) {return zip(this.iter, ...xss)}

  every(fn) {return every(fn, this.iter)}
  some(fn) {return some(fn, this.iter)}
  find(fn) {return find(fn, this.iter)}
  findLast(fn) {return findLast(fn, this.iter)}
  findIndex(fn) {return findIndex(fn, this.iter)}
  findLastIndex(fn) {return findLastIndex(fn, this.iter)}
  indexOf(x) {return indexOf(x, this.iter)}
  lastIndexOf(x) {return lastIndexOf(x, this.iter)}
  includes(x) {return includes(x, this.iter)}
  reduce(a, fn) {return reduce(a, fn, this.iter)}
  inject(a, fn) {return inject(a, fn, this.iter)}

  first() {return first(this.iter)}
  head() {return head(this.iter)}
  last() {return last(this.iter)}
  tail() {return tail(this.iter)}
  init() {return init(this.iter)}
  count() {return count(this.iter)}
  pick(i) {return pick(i, this.iter)}

  sum() {return sum(this.iter)}
  product() {return product(this.iter)}
  max() {return max(this.iter)}
  min() {return min(this.iter)}
  minMax() {return minMax(this.iter)}

  groupBy(fn, unique = false) {return groupBy(fn, unique, this.iter)}
  keyBy(fn) {return keyBy(fn, this.iter)}
  unique() {return unique(this.iter)}

  slice(start, end) {return slice(this.iter, start, end)}
}
class SplitSource {
  constructor(n, iter) {
    this.iter = iter
    this.derived = Array(n)
    for (let i = this.derived.length; i--;) {
      this.derived[i] = new SplitIter(this)
    }
  }
  [Symbol.iterator]() {return this.derived[Symbol.iterator]()}
  pull() {
    const {done, value} = this.iter.next()
    if (done) return
    for (const b of this.derived) b.push(value)
  }
}
class SplitIter extends Iter {
  constructor(source) {
    super()
    this.iter = this
    this.buffer = []
    this.source = source
  }
  [Symbol.iterator]() {return this}
  push(v) {this.buffer.push(v)}
  next() {
    if (!this.buffer.length) this.source.pull()
    return this.buffer.length ? {done: false, value: this.buffer.shift()} : {done: true}
  }
}

Object.assign(module.exports = from, {
  is, from, generator, empty,
  range, irange,
  replicate, forever, iterate,
  entries, keys, values,
  toArray, array, toMap, toSet, toObject,
  intersperse, join,

  split, repeat, cycle, enumerate,
  map, flatMap, filter, reject,
  concat, push, unshift, flatten,
  chunksOf, lookahead, subsequences,
  drop, dropWhile, dropLast,
  take, takeWhile, takeLast,
  zip,
  every, some,
  find, findLast, findIndex, findLastIndex, indexOf, lastIndexOf, includes,
  reduce, inject,
  first, head, last, tail, init,
  count, pick,
  sum, product, min, max, minMax,
  groupBy, keyBy, unique,
  slice,
})
