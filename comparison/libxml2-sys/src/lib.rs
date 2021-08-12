#![feature(c_variadic)]

use std::{
    convert::TryInto,
    ffi::CStr,
    fmt,
    os::raw::{c_char, c_void},
    ptr,
    sync::Once,
};

mod ffi {
    #![allow(non_upper_case_globals)]
    #![allow(non_camel_case_types)]
    #![allow(non_snake_case)]
    #![allow(dead_code)]

    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
}

pub fn parse(s: &str) -> Result<String> {
    one_time_setup();
    Document::parse(s).map(|d| d.to_string())
}

fn one_time_setup() {
    unsafe extern "C" fn no_op_error_handler(_ctx: *mut c_void, _msg: *const c_char, _args: ...) {}

    static START: Once = Once::new();

    START.call_once(|| unsafe {
        ffi::xmlInitParser();
        ffi::initGenericErrorDefaultFunc(&mut Some(no_op_error_handler))
    });
}

struct Document(*mut ffi::_xmlDoc);

impl Document {
    fn parse(s: &str) -> Result<Document> {
        if s.contains('\0') {
            return Err(Error(
                "libxml2 stops processing at embedded NULs; treating this as a failure".into(),
            ));
        }

        unsafe {
            let buf = s.as_ptr() as *const c_char;
            let buf_len = s.len().try_into().expect("Can't fit size");
            let url = ptr::null();
            let encoding = ptr::null();
            let options = ffi::xmlParserOption_XML_PARSE_PEDANTIC as _;

            let doc_ptr = ffi::xmlReadMemory(buf, buf_len, url, encoding, options);
            if doc_ptr.is_null() {
                Err(Error::expect())
            } else {
                Ok(Document(doc_ptr))
            }
        }
    }
}

impl fmt::Display for Document {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            let mut out_str = ptr::null_mut();
            let mut out_len = 0;

            ffi::xmlDocDumpMemory(self.0, &mut out_str, &mut out_len);
            if out_str.is_null() {
                let e = Error::expect();
                writeln!(f, "Unable to display document: {}", e)
            } else {
                let r = CStr::from_ptr(out_str as *mut c_char)
                    .to_string_lossy()
                    .fmt(f);
                if let Some(free) = ffi::xmlFree {
                    free(out_str as _);
                }
                r
            }
        }
    }
}

impl Drop for Document {
    fn drop(&mut self) {
        unsafe {
            ffi::xmlFreeDoc(self.0);
        }
    }
}

#[derive(Debug)]
pub struct Error(String);

impl Error {
    fn check() -> Result {
        unsafe {
            match ffi::xmlGetLastError().as_ref() {
                Some(err) => Err(Self(
                    CStr::from_ptr(err.message).to_string_lossy().into_owned(),
                )),
                None => Ok(()),
            }
        }
    }

    fn expect() -> Self {
        match Self::check() {
            Ok(()) => Self(String::from("Some error occurred but I don't know what")),
            Err(e) => e,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl std::error::Error for Error {}

type Result<T = (), E = Error> = std::result::Result<T, E>;
