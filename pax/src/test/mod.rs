#![allow(unused_imports)]

#[cfg(feature = "bench")]
extern crate test;

use std::path::Path;
use std::process;
use super::*;

cfg_if! {
    if #[cfg(feature = "bench")] {
        fn npm_install(dir: &Path) {
            let node_modules = dir.join("node_modules");
            if node_modules.is_dir() { return }

            let ok = process::Command::new("npm")
                .arg("install")
                .arg("--silent")
                .current_dir(dir)
                .spawn()
                .expect("failed to start `npm install`")
                .wait()
                .unwrap()
                .success();
            if !ok {
                panic!("`npm install` did not exit successfully");
            }
        }

        #[bench]
        fn bench_cjs_simple(b: &mut test::Bencher) {
            let entry_point = Path::new("examples/simple/index.js");
            npm_install(entry_point.parent().unwrap());
            let input_options = InputOptions::default();
            let output = "/dev/null";
            let map_output = SourceMapOutput::Inline;

            b.iter(|| {
                let _ = bundle(&entry_point, input_options, &output, &map_output).unwrap();
            });
        }

        #[bench]
        fn bench_es6_simple(b: &mut test::Bencher) {
            let entry_point = Path::new("examples/es6-simple/index.mjs");
            npm_install(entry_point.parent().unwrap());
            let input_options = InputOptions {
                es6_syntax: true,
                ..InputOptions::default()
            };
            let output = "/dev/null";
            let map_output = SourceMapOutput::Inline;

            b.iter(|| {
                let _ = bundle(&entry_point, input_options, &output, &map_output).unwrap();
            });
        }

        #[bench]
        fn bench_es6_everywhere_simple(b: &mut test::Bencher) {
            let entry_point = Path::new("examples/es6-everywhere-simple/index.js");
            npm_install(entry_point.parent().unwrap());
            let input_options = InputOptions {
                es6_syntax: true,
                es6_syntax_everywhere: true,
                ..InputOptions::default()
            };
            let output = "/dev/null";
            let map_output = SourceMapOutput::Inline;

            b.iter(|| {
                let _ = bundle(&entry_point, input_options, &output, &map_output).unwrap();
            });
        }

        #[bench]
        fn bench_write_map_to(b: &mut test::Bencher) {
            let writer = Writer {
                modules: {
                    let mut modules = HashMap::new();
                    for i in 0..1000 {
                        let mut path = PathBuf::new();
                        path.push(i.to_string());
                        path.push("examples/es6-everywhere-simple/node_modules/itt/index.js");
                        modules.insert(
                            path,
                            Module {
                                source: Source {
                                    prefix: "~function() {".to_owned(),
                                    body: "\'use strict\'\n\nfunction is(xs) {return typeof xs[Symbol.iterator] === \'function\' || typeof xs.next === \'function\'}\nfunction generator(gen) {return (...args) => new Iter(gen(...args))}\nconst G = generator\nfunction from(iter) {return new Iter(iter[Symbol.iterator] ? iter[Symbol.iterator]() : iter)}\nconst empty = G(function*() {})\n\nconst range = G(function*(start, end, skip = 1) {\n  if (end === undefined) [start, end] = [0, start]\n  if (skip > 0) for (let i = start; i < end; i += skip) yield i\n  else for (let i = start; i > end; i += skip) yield i\n})\nconst irange = G(function*(start = 0, skip = 1) {\n  for (let i = start; ; i += skip) yield i\n})\nconst replicate = G(function*(n, x) {for (let i = 0; i < n; ++i) yield x})\nconst forever = G(function*(x) {for (;;) yield x})\nconst iterate = G(function*(x, fn) {for (;;) {yield x; x = fn(x)}})\n\nconst _keys = Object.keys\nconst entries = G(function*(o) {for (const k of _keys(o)) yield [k, o[k]]})\nfunction keys(o) {return new Iter(_keys(o)[Symbol.iterator]())}\nconst values = G(function*(o) {for (const k of _keys(o)) yield o[k]})\n\nfunction split(n = 2, xs) {return new SplitSource(n, xs).derived}\nconst cycle = G(function*(xs) {\n  const cache = []\n  for (const x of xs) {\n    cache.push(x)\n    yield x\n  }\n  for (;;) yield* cache\n})\nconst repeat = G(function*(n, xs) {\n  if (n <= 0) return\n  const cache = []\n  for (const x of xs) {\n    cache.push(x)\n    yield x\n  }\n  for (let i = 1; i < n; ++i) yield* cache\n})\nconst enumerate = G(function*(xs) {let i = 0; for (const x of xs) yield [i++, x]})\nconst map = G(function*(fn, xs) {for (const x of xs) yield fn(x)})\nconst flatMap = G(function*(fn, xs) {for (const x of xs) yield* fn(x)})\nconst tap = G(function*(fn, xs) {for (const x of xs) {fn(x); yield x}})\nconst filter = G(function*(fn, xs) {for (const x of xs) if (fn(x)) yield x})\nconst reject = G(function*(fn, xs) {for (const x of xs) if (!fn(x)) yield x})\nconst concat = G(function*(...xss) {for (const xs of xss) yield* xs})\nconst push = G(function*(...ys) {const xs = ys.pop(); yield* xs; yield* ys})\nconst unshift = G(function*(...ys) {const xs = ys.pop(); yield* ys; yield* xs})\nconst flatten = G(function*(xs) {for (const x of xs) yield* x})\nconst chunksOf = G(function*(n, xs) {\n  let list = []\n  for (const x of xs) {\n    if (list.length >= n) {yield list; list = []}\n    list.push(x)\n  }\n  if (list.length) yield list\n})\nconst subsequences = G(function*(xs, n = 2) {\n  if (xs[Symbol.iterator]) xs = xs[Symbol.iterator]()\n  let buffer = []\n  let value, done\n  while (buffer.length < n && ({value, done} = xs.next()) && !done) {\n    buffer.push(value)\n  }\n  if (!done) while (({value, done} = xs.next()) && !done) {\n    yield buffer\n    buffer = buffer.slice(1)\n    buffer.push(value)\n  }\n  if (buffer.length === n) yield buffer\n})\nconst lookahead = G(function*(xs, n = 1) {\n  if (xs[Symbol.iterator]) xs = xs[Symbol.iterator]()\n  let buffer = []\n  let value, done\n  while (buffer.length < n && ({value, done} = xs.next()) && !done) {\n    buffer.push(value)\n  }\n  if (!done) while (({value, done} = xs.next()) && !done) {\n    buffer.push(value)\n    yield buffer\n    buffer = buffer.slice(1)\n  }\n  for (let i = buffer.length - 1; i-- >= 0;) {\n    yield buffer\n    buffer = buffer.slice(1)\n  }\n})\nconst drop = G(function*(n, xs) {for (const x of xs) if (n <= 0) yield x; else --n})\nconst dropWhile = G(function*(fn, xs) {let init = true; for (const x of xs) if (!init || !fn(x)) {init = false; yield x}})\nconst dropLast = G(function*(n, xs) {\n  if (n === 0) yield* xs; else {\n    const list = []\n    let i = 0\n    for (const x of xs) {\n      if (i >= n) yield list[i % n]\n      list[i++ % n] = x\n    }\n  }\n})\nconst take = G(function*(n, xs) {if (n <= 0) return; for (const x of xs) {yield x; if (--n <= 0) return}})\nconst takeWhile = G(function*(fn, xs) {for (const x of xs) if (fn(x)) yield x; else return})\nconst takeLast = G(function*(n, xs) {\n  const list = []\n  let i = 0\n  for (const x of xs) list[i++ % n] = x\n  if (n > list.length) n = list.length\n  for (let j = 0; j < n; j++) yield list[(i + j) % n]\n})\nconst zip = G(function*(...xss) {\n  const its = xss.map(xs => xs[Symbol.iterator]())\n  for (;;) {\n    const rs = its.map(it => it.next())\n    if (rs.some(r => r.done)) return\n    yield rs.map(r => r.value)\n  }\n})\n\nfunction every(fn, xs) {for (const x of xs) if (!fn(x)) return false; return true}\nfunction some(fn, xs) {for (const x of xs) if (fn(x)) return true; return false}\nfunction find(fn, xs) {for (const x of xs) if (fn(x)) return x}\nfunction findLast(fn, xs) {let y; for (const x of xs) if (fn(x)) y = x; return y}\nfunction findIndex(fn, xs) {let i = 0; for (const x of xs) {if (fn(x)) return i; ++i} return -1}\nfunction findLastIndex(fn, xs) {let i = 0, j = -1; for (const x of xs) {if (fn(x)) j = i; ++i} return j}\nfunction indexOf(y, xs) {let i = 0; for (const x of xs) {if (x === y) return i; ++i} return -1}\nfunction lastIndexOf(y, xs) {let i = 0, j = -1; for (const x of xs) {if (x === y) j = i; ++i} return j}\nfunction includes(y, xs) {for (const x of xs) if (x === y) return true; return false}\nfunction reduce(a, fn, xs) {for (const x of xs) a = fn(a, x); return a}\nfunction inject(a, fn, xs) {for (const x of xs) fn(a, x); return a}\n\nfunction first(xs) {if (Array.isArray(xs)) return xs[0]; for (const x of xs) return x}\nconst head = first\nfunction last(xs) {if (Array.isArray(xs)) return xs[xs.length - 1]; let z; for (const x of xs) z = x; return z}\nfunction tail(xs) {return drop(1, xs)}\nfunction init(xs) {return dropLast(1, xs)}\n\nfunction count(xs) {if (Array.isArray(xs)) return xs.length; let i = 0; for (const x of xs) ++i; return i}\nfunction pick(i, xs) {if (Array.isArray(xs)) return xs[i]; for (const x of xs) if (i-- <= 0) return x}\n\nfunction sum(xs) {return reduce(0, (x, y) => x + Number(y), xs)}\nfunction product(xs) {return reduce(1, (x, y) => x * y, xs)}\nfunction max(xs) {return reduce(-Infinity, Math.max, xs)}\nfunction min(xs) {return reduce(Infinity, Math.min, xs)}\nfunction minMax(xs) {\n  let min = Infinity, max = -Infinity\n  for (const x of xs) {\n    const b = +x\n    if (b < min) min = b\n    if (b > max) max = b\n  }\n  return [min, max]\n}\n\nfunction groupBy(fn, unique, xs) {\n  if (!xs) [unique, xs] = [false, unique]\n  return inject(new Map, unique ? (m, x) => {\n    const k = fn(x), s = m.get(k)\n    if (s) s.add(x)\n    else m.set(k, new Set([x]))\n  } : (m, x) => {\n    const k = fn(x), l = m.get(k)\n    if (l) l.push(x)\n    else m.set(k, [x])\n  }, xs)\n}\nfunction keyBy(fn, xs) {\n  return inject(new Map, (m, x) => m.set(fn(x), x), xs)\n}\n\nconst unique = G(function*(xs) {\n  const used = new Set\n  for (const x of xs) {\n    if (!used.has(x)) {\n      yield x\n      used.add(x)\n    }\n  }\n})\n\nfunction toArray(xs) {return Array.from(xs)}\nconst array = toArray\nfunction toMap(xs) {return new Map(xs)}\nfunction toSet(xs) {return new Set(xs)}\nfunction toObject(xs, empty = false) {\n  const o = empty ? Object.create(null) : {}\n  for (const [k, v] of xs) {\n    o[k] = v\n  }\n  return o\n}\nconst intersperse = G(function*(sep, xs) {\n  let use = false\n  for (const x of xs) {\n    if (use) yield sep\n    yield x\n    use = true\n  }\n})\nfunction join(sep, xs) {\n  if (!xs) [sep, xs] = [\',\', sep]\n  let s = \'\'\n  if (sep) {\n    let use = false\n    for (const x of xs) {\n      if (use) s += sep\n      s += x\n      use = true\n    }\n  } else {\n    for (const x of xs) s += x\n  }\n  return s\n}\n\nconst slice = G(function*(xs, start = 0, end) {\n  if (Array.isArray(xs)) {\n    if (start < 0) start += array.length\n    if (end === undefined) end = array.length\n    else if (end < 0) end += array.length\n    for (let i = start; i < end; ++i) yield array[i]\n  } else if (end === undefined) {\n    yield* start < 0 ? takeLast(-start, xs) : drop(start, xs)\n  } else if (start >= 0) {\n    let i = 0\n    if (end === 0) return\n    else if (end > 0) {\n      for (const x of xs) {\n        if (i >= start) yield x\n        if (++i >= end) return\n      }\n    } else {\n      // yield* dropLast(-end, drop(start, xs))\n      const list = []\n      const n = -end\n      for (const x of xs) {\n        if (i >= start) {\n          const k = (i - start) % n\n          if (i - start >= n) yield list[k]\n          list[k] = x\n        }\n        ++i\n      }\n    }\n  } else {\n    // yield* dropLast(-end, takeLast(-start, xs))\n    const list = []\n    let n = -start\n    let i = 0\n    for (const x of xs) list[i++ % n] = x\n    if (n > list.length) n = list.length\n    for (let j = 0; j < n + end; j++) yield list[(i + j) % n]\n  }\n})\n\nclass Iter {\n  constructor(iter) {this.iter = iter}\n  [Symbol.iterator]() {return this.iter}\n  next() {return this.iter.next()}\n  toArray() {return Array.from(this.iter)}\n  array() {return Array.from(this.iter)}\n  toMap() {return new Map(this.iter)}\n  toSet() {return new Set(this.iter)}\n  toObject(empty = false) {return toObject(this.iter, empty)}\n  join(sep = \',\') {return join(sep, this.iter)}\n  intersperse(sep) {return intersperse(sep, this.iter)}\n\n  split(n = 2) {return split(n, this.iter)}\n  repeat(n) {return repeat(n, this.iter)}\n  cycle() {return cycle(this.iter)}\n  enumerate() {return enumerate(this.iter)}\n  map(fn) {return map(fn, this.iter)}\n  flatMap(fn) {return flatMap(fn, this.iter)}\n  tap(fn) {return tap(fn, this.iter)}\n  filter(fn) {return filter(fn, this.iter)}\n  reject(fn) {return reject(fn, this.iter)}\n  concat(...xss) {return concat(this.iter, ...xss)}\n  push(...xs) {return push(...xs, this.iter)}\n  unshift(...xs) {return unshift(...xs, this.iter)}\n  flatten() {return flatten(this.iter)}\n  chunksOf(n) {return chunksOf(n, this.iter)}\n  lookahead(n) {return lookahead(this.iter, n)}\n  subsequences(n) {return subsequences(this.iter, n)}\n  drop(n) {return drop(n, this.iter)}\n  dropWhile(fn) {return dropWhile(fn, this.iter)}\n  dropLast(n) {return dropLast(n, this.iter)}\n  take(n) {return take(n, this.iter)}\n  takeWhile(fn) {return takeWhile(fn, this.iter)}\n  takeLast(n) {return takeLast(n, this.iter)}\n  zip(...xss) {return zip(this.iter, ...xss)}\n\n  every(fn) {return every(fn, this.iter)}\n  some(fn) {return some(fn, this.iter)}\n  find(fn) {return find(fn, this.iter)}\n  findLast(fn) {return findLast(fn, this.iter)}\n  findIndex(fn) {return findIndex(fn, this.iter)}\n  findLastIndex(fn) {return findLastIndex(fn, this.iter)}\n  indexOf(x) {return indexOf(x, this.iter)}\n  lastIndexOf(x) {return lastIndexOf(x, this.iter)}\n  includes(x) {return includes(x, this.iter)}\n  reduce(a, fn) {return reduce(a, fn, this.iter)}\n  inject(a, fn) {return inject(a, fn, this.iter)}\n\n  first() {return first(this.iter)}\n  head() {return head(this.iter)}\n  last() {return last(this.iter)}\n  tail() {return tail(this.iter)}\n  init() {return init(this.iter)}\n  count() {return count(this.iter)}\n  pick(i) {return pick(i, this.iter)}\n\n  sum() {return sum(this.iter)}\n  product() {return product(this.iter)}\n  max() {return max(this.iter)}\n  min() {return min(this.iter)}\n  minMax() {return minMax(this.iter)}\n\n  groupBy(fn, unique = false) {return groupBy(fn, unique, this.iter)}\n  keyBy(fn) {return keyBy(fn, this.iter)}\n  unique() {return unique(this.iter)}\n\n  slice(start, end) {return slice(this.iter, start, end)}\n}\nclass SplitSource {\n  constructor(n, iter) {\n    this.iter = iter\n    this.derived = Array(n)\n    for (let i = this.derived.length; i--;) {\n      this.derived[i] = new SplitIter(this)\n    }\n  }\n  [Symbol.iterator]() {return this.derived[Symbol.iterator]()}\n  pull() {\n    const {done, value} = this.iter.next()\n    if (done) return\n    for (const b of this.derived) b.push(value)\n  }\n}\nclass SplitIter extends Iter {\n  constructor(source) {\n    super()\n    this.iter = this\n    this.buffer = []\n    this.source = source\n  }\n  [Symbol.iterator]() {return this}\n  push(v) {this.buffer.push(v)}\n  next() {\n    if (!this.buffer.length) this.source.pull()\n    return this.buffer.length ? {done: false, value: this.buffer.shift()} : {done: true}\n  }\n}\n\nObject.assign(module.exports = from, {\n  is, from, generator, empty,\n  range, irange,\n  replicate, forever, iterate,\n  entries, keys, values,\n  toArray, array, toMap, toSet, toObject,\n  intersperse, join,\n\n  split, repeat, cycle, enumerate,\n  map, flatMap, filter, reject,\n  concat, push, unshift, flatten,\n  chunksOf, lookahead, subsequences,\n  drop, dropWhile, dropLast,\n  take, takeWhile, takeLast,\n  zip,\n  every, some,\n  find, findLast, findIndex, findLastIndex, indexOf, lastIndexOf, includes,\n  reduce, inject,\n  first, head, last, tail, init,\n  count, pick,\n  sum, product, min, max, minMax,\n  groupBy, keyBy, unique,\n  slice,\n})\n".to_owned(),
                                    suffix: "}()".to_owned(),
                                    original: None,
                                },
                                deps: {
                                    let mut deps = HashMap::new();
                                    deps.insert("./math".to_owned(), Resolved::Normal(
                                        Path::new("examples/es6-everywhere-simple/math.js").to_owned(),
                                    ));
                                    deps.insert("itt".to_owned(), Resolved::Normal(
                                        Path::new("examples/es6-everywhere-simple/node_modules/itt/index.js").to_owned(),
                                    ));
                                    deps
                                },
                            },
                        );
                    }
                    modules
                },
                entry_point: Path::new("examples/es6-everywhere-simple/index.js"),
                map_output: &SourceMapOutput::Inline,
            };

            let mut out = Vec::new();
            b.iter(|| {
                out.clear();
                writer.write_map_to(&mut out).unwrap();
            });
            b.bytes = out.len() as u64;
        }
    }
}
