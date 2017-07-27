# Parcel Redux

Because [Parcel](https://github.com/nathan/parcel#readme) wasn't already [fast enough](#is-it-fast).

Fully supports ECMAScript module syntax (`import`/`export`) in addition to CommonJS `require(<string>)`.

[How do I get it?](#how-do-i-get-it) · [How do I use it?](#how-do-i-use-it) · [Does it do source maps?](#does-it-do-source-maps) · [Modules?](#modules) · [What are the options?](#what-are-the-options) · [Is it fast?](#is-it-fast)

# How do I get it?

```sh
> cargo install parcel
```

If you don't have `cargo`, install it with [https://rustup.rs](https://rustup.rs/).

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
> parcel index.js parcel.js
```

And then `node parcel.js` or `<script src=parcel.js>` to your heart's content.

# Does it do source maps?

Of course!

```sh
# parcel.js and parcel.js.map
> parcel index.js parcel.js

# parcel.js with inline map
> parcel --map-inline index.js parcel.js

# parcel.js with no source map
> parcel index.js >parcel.js
# or
> parcel --no-map index.js parcel.js
```

# Modules?

That's technically not a question. But yes.

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
> parcel -e index.mjs parcel.js
```

If you need your modules to be in `.js` files for some reason, use `-E` (`--es-syntax-everywhere`) instead of `-e` (`--es-syntax`).

# What are the options?

```
> parcel --help
parcel v0.0.1

Usage:
    parcel [options] <input> [output]
    parcel [-h | --help]

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

    -h, --help
        Print this message.
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
> time prcl index.js >parcel.js
real    0m0.077s
user    0m0.059s
sys     0m0.017s
> time parcel-redux index.js >parcel.js
real    0m0.010s
user    0m0.005s
sys     0m0.006s

# on a larger project
> time browserify src/api-download.js >browserify.js
real    0m2.385s
user    0m2.459s
sys     0m0.416s
> time prcl src/api-download.js >parcel.js
real    0m0.204s
user    0m0.187s
sys     0m0.083s
> time parcel-redux src/api-download.js >parcel.js
real    0m0.037s
user    0m0.071s
sys     0m0.019s

# want source maps?
> time browserify -d src/api-download.js -o bundle.js
real    0m3.142s
user    0m3.060s
sys     0m0.483s
> time prcl src/api-download.js parcel.js
real    0m0.315s
user    0m0.281s
sys     0m0.100s
> time parcel-redux src/api-download.js parcel.js
real    0m0.046s
user    0m0.077s
sys     0m0.026s

# realtime!
> parcel-redux -w examples/simple bundle.js
ready in 2 ms
generate bundle.js in 2 ms
generate bundle.js in 2 ms
generate bundle.js in 1 ms
generate bundle.js in 2 ms
generate bundle.js in 1 ms
generate bundle.js in 3 ms
^C
```
