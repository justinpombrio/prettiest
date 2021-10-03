#[cfg(feature = "logging")]
mod logging {
    use std::sync::atomic::{AtomicUsize, Ordering};

    const TAB_SIZE: usize = 2;

    #[doc(hidden)]
    pub static LOGGING_DEPTH: AtomicUsize = AtomicUsize::new(0);

    #[doc(hidden)]
    pub struct LoggingGuard;

    impl LoggingGuard {
        #[doc(hidden)]
        pub fn new() -> LoggingGuard {
            LOGGING_DEPTH.fetch_add(TAB_SIZE, Ordering::SeqCst);
            LoggingGuard
        }
    }

    impl Drop for LoggingGuard {
        fn drop(&mut self) {
            LOGGING_DEPTH.fetch_sub(TAB_SIZE, Ordering::SeqCst);
        }
    }

    #[macro_export]
    macro_rules! log {
        ($msg:literal, $($args:expr),*) => {
            let depth = $crate::util::LOGGING_DEPTH.load(std::sync::atomic::Ordering::SeqCst);
            eprintln!(concat!("{:indent$}", $msg), "", $($args),*, indent = depth);
        };
    }

    #[macro_export]
    macro_rules! log_span {
        () => {
            let _guard = $crate::util::LoggingGuard::new();
        };
    }
}

#[cfg(not(feature = "logging"))]
mod logging {
    #[macro_export]
    macro_rules! log {
        ($msg:literal, $($args:expr),*) => {};
    }

    #[macro_export]
    macro_rules! log_span {
        () => {};
    }
}

pub use logging::*;
