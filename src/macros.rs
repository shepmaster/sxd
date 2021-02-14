#[macro_export]
macro_rules! abandon {
    ($e:expr) => {
        match $e {
            Ok(v) => v,
            Err(e) => return Err(e),
        }
    };
}

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
