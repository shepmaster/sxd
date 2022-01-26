#![deny(rust_2018_idioms)]

use std::{
    convert::TryInto,
    ffi::CStr,
    fmt,
    os::raw::{c_char, c_void},
    ptr,
};

mod ffi {
    #![allow(non_upper_case_globals)]
    #![allow(non_camel_case_types)]
    #![allow(non_snake_case)]
    #![allow(dead_code)]
    #![allow(clippy::all)]

    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
}

pub fn parse(s: &str) -> Result<String> {
    Document::parse(s).map(|d| d.to_string())
}

unsafe extern "C" fn global_error_handler(ctx: *mut c_void, error: *mut ffi::xmlError) {
    let ctx = ctx as *mut ffi::xmlParserCtxt;

    if let (Some(ctx), Some(error)) = (ctx.as_ref(), error.as_ref()) {
        if let Err(error) = Error::from_libxml2(error) {
            let errors = (*ctx)._private as *mut Errors;
            let errors = &mut *errors;
            errors.push(error);
        }
    }
}

struct Document(*mut ffi::_xmlDoc);

impl Document {
    fn parse(s: &str) -> Result<Document> {
        if s.contains('\0') {
            return Err(Error::high_level(
                "libxml2 stops processing at embedded NULs; treating this as a failure",
            ));
        }

        unsafe {
            let mut errors = Errors::new();
            let ctx = ffi::xmlNewParserCtxt();

            (*ctx)._private = &mut errors as *mut _ as *mut c_void;
            ffi::xmlSetStructuredErrorFunc(ctx as _, Some(global_error_handler));

            let buf = s.as_ptr() as *const c_char;
            let buf_len = s.len().try_into().expect("Can't fit size");
            let url = ptr::null();
            let encoding = ptr::null();
            let options = ffi::xmlParserOption_XML_PARSE_PEDANTIC as _;

            let doc_ptr = ffi::xmlCtxtReadMemory(ctx, buf, buf_len, url, encoding, options);

            // NB: memory leak here? ("However the parsed document in ctxt->myDoc is not freed.")
            ffi::xmlFreeParserCtxt(ctx);

            Error::from_multiple(errors)?;

            if doc_ptr.is_null() {
                Err(Error::high_level("NULL document without an error"))
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
                writeln!(f, "Unable to display document")
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

type Errors = Vec<Error>;

#[derive(Debug)]
#[allow(dead_code)]
pub enum Error {
    HighLevel {
        message: String,
    },

    LibXml2 {
        domain: i32,
        code: i32,
        message: String,
    },

    Multiple(Errors),
}

impl Error {
    fn high_level(message: impl Into<String>) -> Self {
        Self::HighLevel {
            message: message.into(),
        }
    }

    fn from_libxml2(error: &ffi::xmlError) -> Result<(), Self> {
        match error.code as _ {
            ffi::xmlParserErrors_XML_ERR_OK => Ok(()),

            _ => {
                let message =
                    unsafe { CStr::from_ptr(error.message).to_string_lossy().into_owned() };

                Err(Self::LibXml2 {
                    domain: error.domain,
                    code: error.code,
                    message,
                })
            }
        }
    }

    fn from_multiple(errors: Vec<Self>) -> Result<()> {
        if errors.is_empty() {
            Ok(())
        } else {
            Err(Self::Multiple(errors))
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Error::*;

        match self {
            HighLevel { message } => message.fmt(f),

            LibXml2 {
                domain,
                code,
                message,
            } => write!(f, "{message} ({domain}/{code})"),

            Multiple(errors) => {
                for e in errors {
                    e.fmt(f)?;
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for Error {}

type Result<T = (), E = Error> = std::result::Result<T, E>;
