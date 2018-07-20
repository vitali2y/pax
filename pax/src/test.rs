#![allow(unused_imports)]

#[cfg(feature = "bench")]
extern crate test;

use std::path::Path;
use super::*;

cfg_if! {
    if #[cfg(feature = "bench")] {
        #[bench]
        fn bench_cjs_simple(b: &mut test::Bencher) {
            let entry_point = Path::new("examples/simple/index.js");
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
    }
}
