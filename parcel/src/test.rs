extern crate test;

use std::path::Path;
use super::*;

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
