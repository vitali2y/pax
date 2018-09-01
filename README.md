# Pax

The [fastest](#is-it-fast) JavaScript bundler in the galaxy. Fully supports ECMAScript module syntax (`import`/`export`) in addition to CommonJS `require(<string>)`.

- [Why do I need it?](#why-do-i-need-it)
- [How do I get it?](#how-do-i-get-it)
- [How do I use it?](#how-do-i-use-it)
- [Does it do source maps?](#does-it-do-source-maps)
- [Modules?](#modules)
- [What are the options?](#what-are-the-options)
- [Is it fast?](#is-it-fast)

# Why do I need it?

Because your bundler is **too slow**.

You know the feeling. You make that tweak, hit <kbd>⌘S</kbd> <kbd>⌘Tab</kbd> <kbd>⌘R</kbd>, and… **nothing changes**. You get the old version. You beat the bundler. You wait a few seconds, hit <kbd>⌘R</kbd> again, and your changes finally show up. But it’s too late—**you’ve lost momentum.** It’s the wrong shade of pink. You spelled “menu” with a z. The bug still happens sometimes.

Rinse. Repeat. Ten cycles later, things are looking good. It’s time to `git commit`. But you spent **more time waiting than working**. And it’s your bundler’s fault.

Pax is a bundler. But you’ll never beat it. Why?

- It’s parallelized. It makes the most of your cores.
- It’s minimal. It isn’t bogged down by features you don’t need.
- It knows exactly enough about JavaScript to handle dependency resolution. It doesn’t even bother parsing most of your source code.

Don’t waste time waiting for your bundler to do its thing. Use Pax while you’re developing, and **iterate to your heart’s content**. Use your super-cool, magical, slow-as-molasses bundler for releases, when you don’t care how long it takes to run.

# How do I get it?

```sh
> cargo install pax
```

If you don’t have `cargo`, install it with [https://rustup.rs](https://rustup.rs/).

# How do I use it?

```js
// index.js:
const itt = require('itt')
const math = require('./math')
console.log(itt.range(10).map(math.square).join(' '))

// math.js:
exports.square = x => x * x
```

```sh
> px index.js bundle.js
```

Slap on a `<script src=bundle.js>`, and you’re ready to go. Pass `-w` to rebuild whenever you change a file.

```sh
> px -w index.js bundle.js
 ready bundle.js in 1 ms
update bundle.js in 1 ms
...
```

# Does it do source maps?

Of course!

```sh
# bundle.js and bundle.js.map
> px index.js bundle.js

# bundle.js with inline map
> px --map-inline index.js bundle.js

# bundle.js with no source map
> px index.js >bundle.js
# or
> px --no-map index.js bundle.js
```

# Modules?

That’s technically not a question. But yes.

```js
// index.mjs
import itt from 'itt'
import { square, cube } from './math'

console.log(itt.range(10).map(square).join(' '))
console.log(itt.range(10).map(cube).join(' '))

// math.mjs
export const square = x => x * x, cube = x => x * x * x
```

```
> px -e index.mjs bundle.js
```

If you need your modules to be in `.js` files for some reason, use `-E` (`--es-syntax-everywhere`) instead of `-e` (`--es-syntax`).

# What are the options?

```
> px --help
pax v0.3.0

Usage:
    px [options] <input> [output]
    px [-h | --help]

Options:
    -i, --input <input>
        Use <input> as the main module.

    -o, --output <output>
        Write bundle to <output> and source map to <output>.map.
        Default: '-' for stdout.

    -m, --map <map>
        Output source map to <map>.

    -I, --map-inline
        Output source map inline as data: URI.

    -M, --no-map
        Suppress source map output when it would normally be implied.

    -w, --watch
        Watch for changes to <input> and its dependencies.

    -W, --quiet-watch
        Don't emit a bell character for errors that occur while watching.
        Implies --watch.

    -e, --es-syntax
        Support .mjs files with ECMAScript module syntax:

            import itt from 'itt'
            export const greeting = 'Hello, world!'

        Instead of CommonJS require syntax:

            const itt = require('itt')
            exports.greeting = 'Hello, world!'

        .mjs (ESM) files can import .js (CJS) files, in which case the
        namespace object has a single `default` binding which reflects the
        value of `module.exports`. CJS files can require ESM files, in which
        case the resultant object is the namespace object.

    -E, --es-syntax-everywhere
        Implies --es-syntax. Allow ECMAScript module syntax in .js files.
        CJS-style `require()` calls are also allowed.

    -x, --external <module1,module2,...>
        Don't resolve or include modules named <module1>, <module2>, etc.;
        leave them as require('<module>') references in the bundle. Specifying
        a path instead of a module name does nothing.

    --external-core
        Ignore references to node.js core modules like 'events' and leave them
        as require('<module>') references in the bundle.

    -h, --help
        Print this message.

    -v, --version
        Print version information.
```

# Is it fast?

Umm…

Yes.

```sh
> time browserify index.js >browserify.js
real    0m0.225s
user    0m0.197s
sys     0m0.031s
> time node fuse-box.js
real    0m0.373s
user    0m0.324s
sys     0m0.051s
> time px index.js >bundle.js
real    0m0.010s
user    0m0.005s
sys     0m0.006s

# on a larger project
> time browserify src/main.js >browserify.js
real    0m2.385s
user    0m2.459s
sys     0m0.416s
> time px src/main.js >bundle.js
real    0m0.037s
user    0m0.071s
sys     0m0.019s

# want source maps?
> time browserify -d src/main.js -o bundle.js
real    0m3.142s
user    0m3.060s
sys     0m0.483s
> time px src/main.js bundle.js
real    0m0.046s
user    0m0.077s
sys     0m0.026s

# realtime!
> px -w examples/simple bundle.js
 ready bundle.js in 2 ms
update bundle.js in 2 ms
update bundle.js in 2 ms
update bundle.js in 1 ms
update bundle.js in 2 ms
update bundle.js in 1 ms
update bundle.js in 3 ms
^C
```
