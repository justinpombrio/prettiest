use prettiest::oracle::verify_with_oracle;
use prettiest::{pretty_print, Doc, Overflow, PrettyResult, Width};

#[track_caller]
pub fn assert_pretty(doc: &Doc, width: Width, expected: &str) {
    assert_generic(
        doc,
        width,
        Some(PrettyResult::Valid {
            lines: expected.split("\n").map(|s| s.to_owned()).collect(),
            overflow: 0,
        }),
    );
}

#[track_caller]
pub fn assert_pretty_multiline(doc: &Doc, width: Width, expected: &str) {
    assert_pretty(doc, width, &expected[1..]);
}

#[track_caller]
pub fn assert_ugly(doc: &Doc, width: Width, expected: &str, expected_overflow: Overflow) {
    assert_generic(
        doc,
        width,
        Some(PrettyResult::Valid {
            lines: expected.split("\n").map(|s| s.to_owned()).collect(),
            overflow: expected_overflow,
        }),
    );
}

#[track_caller]
pub fn assert_ugly_multiline(doc: &Doc, width: Width, expected: &str, expected_overflow: Overflow) {
    assert_ugly(doc, width, &expected[1..], expected_overflow);
}

#[track_caller]
pub fn assert_invalid(doc: &Doc, width: Width) {
    assert_generic(doc, width, Some(PrettyResult::Invalid));
}

#[allow(unused)]
#[track_caller]
pub fn assert_unknown(doc: &Doc, width: Width) {
    assert_generic(doc, width, None)
}

#[track_caller]
fn assert_generic(doc: &Doc, width: Width, test_case: Option<PrettyResult>) {
    if let Some(test_case) = test_case {
        // 1. Compare what the test case says against the oracle.
        match verify_with_oracle(doc, width, &test_case) {
            Ok(()) => (),
            Err(oracle) => {
                compare_results(doc, width, ("ORACLE", &oracle), ("TEST CASE", &test_case))
            }
        };
        // 2. Compare what the implementation says against the test case.
        let implementation = pretty_print(doc, width);
        compare_results(
            doc,
            width,
            ("TEST CASE", &test_case),
            ("IMPLEMENTATION", &implementation),
        );
    } else {
        // 1. There is no expected result, so compare what the implementation says against the
        //    oracle directly.
        let implementation = pretty_print(doc, width);
        match verify_with_oracle(doc, width, &implementation) {
            Ok(()) => (),
            Err(oracle) => compare_results(
                doc,
                width,
                ("ORACLE", &oracle),
                ("IMPLEMENTATION", &implementation),
            ),
        }
    }
}

/*
#[track_caller]
fn assert_generic(doc: &Doc, width: Width, test_case: Option<PrettyResult>) {
    let oracle = oracular_pretty_print(doc, width);
    if let Some(test_case) = test_case {
        compare_results(doc, width, ("ORACLE", &oracle), ("TEST CASE", &test_case));
        let implementation = pretty_print(doc, width);
        compare_results(
            doc,
            width,
            ("TEST CASE", &test_case),
            ("IMPLEMENTATION", &implementation),
        );
    } else {
        let implementation = pretty_print(doc, width);
        compare_results(
            doc,
            width,
            ("ORACLE", &oracle),
            ("IMPLEMENTATION", &implementation),
        );
    }
}
*/

fn render_result(result: &PrettyResult) -> String {
    match result {
        PrettyResult::Invalid => "[invalid]".to_owned(),
        PrettyResult::Valid { lines, overflow } => {
            let mut output = lines.join("\n");
            if *overflow > 0 {
                output.push_str(&format!("\n[overflow {}]", overflow));
            }
            output
        }
    }
}

#[track_caller]
fn compare_results(
    doc: &Doc,
    width: Width,
    expected: (&str, &PrettyResult),
    actual: (&str, &PrettyResult),
) {
    let expected_as_str = render_result(expected.1);
    let actual_as_str = render_result(actual.1);
    if expected_as_str != actual_as_str {
        eprintln!("DOC: {}", doc);
        eprintln!("WIDTH: {}", width);
        eprintln!("{} SAYS:\n{}", expected.0, expected_as_str);
        eprintln!("BUT {} SAYS:\n{}", actual.0, actual_as_str);
        eprintln!("=================\n");
        assert_eq!(expected_as_str, actual_as_str);
    }
}
