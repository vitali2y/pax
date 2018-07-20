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
    }
}
