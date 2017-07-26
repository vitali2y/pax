# parcel redux

Because [parcel](https://github.com/nathan/parcel#readme) wasn't already fast enough.

Fully supports ECMAScript module syntax (`import`/`export`) in addition to CommonJS `require(<string>)`.

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
> parcel -w examples/simple bundle.js
ready
generate bundle.js in 2 ms
generate bundle.js in 2 ms
generate bundle.js in 1 ms
generate bundle.js in 2 ms
generate bundle.js in 1 ms
generate bundle.js in 3 ms
^C
```
