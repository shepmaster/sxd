#[macro_export]
macro_rules! assert_error {
    ($e:expr, $p:pat) => {
        assert!(
            matches!($e, Err($p)),
            "Expected {}, but got {:?}",
            stringify!($p),
            $e
        )
    };
}
