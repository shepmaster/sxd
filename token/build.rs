#![deny(rust_2018_idioms)]

use quote::{format_ident, quote};
use std::{
    env,
    fs::File,
    io::{BufWriter, Write},
    path::PathBuf,
};

type Error = Box<dyn std::error::Error>;
type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Copy, Clone)]
struct TokenDef {
    doc: &'static str,
    name: &'static str,
    has_data: bool,
}

impl TokenDef {
    fn name_ident(&self) -> proc_macro2::Ident {
        format_ident!("{}", self.name)
    }
}

const TOKEN_DEFS: &[TokenDef] = &[
    TokenDef {
        doc: r#"`<?xml version="1.9"`"#,
        name: "DeclarationStart",
        has_data: true,
    },
    TokenDef {
        doc: r#"encoding="foo""#,
        name: "DeclarationEncoding",
        has_data: true,
    },
    TokenDef {
        doc: r#"standalone="no""#,
        name: "DeclarationStandalone",
        has_data: true,
    },
    TokenDef {
        doc: r#"`?>`"#,
        name: "DeclarationClose",
        has_data: false,
    },
    TokenDef {
        doc: r#"`<foo`"#,
        name: "ElementOpenStart",
        has_data: true,
    },
    TokenDef {
        doc: r#"
```xml
<alpha:beta sigma:gamma="value" />
      ^^^^^
```
"#,
        name: "ElementOpenStartSuffix",
        has_data: true,
    },
    TokenDef {
        doc: r#"
```xml
<alpha:beta sigma:gamma="value" />
           ^
```
"#,
        name: "ElementOpenStartComplete",
        has_data: false,
    },
    TokenDef {
        doc: r#"`>`"#,
        name: "ElementOpenEnd",
        has_data: false,
    },
    TokenDef {
        doc: r#"`/>`"#,
        name: "ElementSelfClose",
        has_data: false,
    },
    TokenDef {
        doc: r#"`</foo`"#,
        name: "ElementClose",
        has_data: true,
    },
    TokenDef {
        doc: r#"
```xml
</alpha:beta>
       ^^^^^
```
"#,
        name: "ElementCloseSuffix",
        has_data: true,
    },
    TokenDef {
        doc: r#"
```xml
</alpha:beta>
            ^
```
"#,
        name: "ElementCloseComplete",
        has_data: false,
    },
    TokenDef {
        doc: r#"`foo`"#,
        name: "AttributeStart",
        has_data: true,
    },
    TokenDef {
        doc: r#"
```xml
<alpha:beta sigma:gamma="value" />
                 ^^^^^
```
"#,
        name: "AttributeStartSuffix",
        has_data: true,
    },
    TokenDef {
        doc: r#"
```xml
<alpha:beta sigma:gamma="value" />
                       ^
```
"#,
        name: "AttributeStartComplete",
        has_data: false,
    },
    TokenDef {
        doc: r#"`="bar`"#,
        name: "AttributeValueLiteral",
        has_data: true,
    },
    TokenDef {
        doc: r#""#,
        name: "AttributeValueReferenceNamed",
        has_data: true,
    },
    TokenDef {
        doc: r#""#,
        name: "AttributeValueReferenceDecimal",
        has_data: true,
    },
    TokenDef {
        doc: r#""#,
        name: "AttributeValueReferenceHex",
        has_data: true,
    },
    TokenDef {
        doc: r#""#,
        name: "AttributeValueEnd",
        has_data: false,
    },
    TokenDef {
        doc: r#"`hello world`"#,
        name: "CharData",
        has_data: true,
    },
    TokenDef {
        doc: r#"`<![CDATA[hello world]]>`"#,
        name: "CData",
        has_data: true,
    },
    TokenDef {
        doc: r#"&lt;"#,
        name: "ReferenceNamed",
        has_data: true,
    },
    TokenDef {
        doc: r#"&#4242;"#,
        name: "ReferenceDecimal",
        has_data: true,
    },
    TokenDef {
        doc: r#"&#xABCD;"#,
        name: "ReferenceHex",
        has_data: true,
    },
    TokenDef {
        doc: r#"`<?a`"#,
        name: "ProcessingInstructionStart",
        has_data: true,
    },
    TokenDef {
        doc: r#"`b`"#,
        name: "ProcessingInstructionValue",
        has_data: true,
    },
    TokenDef {
        doc: r#"`?>`"#,
        name: "ProcessingInstructionEnd",
        has_data: false,
    },
    TokenDef {
        doc: r#"`<!--a-->`"#,
        name: "Comment",
        has_data: true,
    },
];

fn main() -> Result<()> {
    println!("cargo:rerun-if-changed=build.rs");

    let out_dir: PathBuf = env::var_os("OUT_DIR").ok_or("OUT_DIR is not set")?.into();
    let dest_path = out_dir.join("generated_token.rs");

    let mut f = File::create(dest_path)?;
    {
        let mut f = BufWriter::new(&mut f);

        writeln!(f, "{}", generate_token_kind())?;
        writeln!(f, "{}", generate_token())?;
        writeln!(f, "{}", generate_token_inherent())?;
        writeln!(f, "{}", generate_token_partialeq())?;
        writeln!(f, "{}", generate_uniform_kind())?;

        f.flush()?;
    }
    f.sync_all()?;

    Ok(())
}

fn defs_with_data() -> impl Iterator<Item = &'static TokenDef> {
    TOKEN_DEFS.iter().filter(|d| d.has_data)
}

fn generate_token_kind() -> proc_macro2::TokenStream {
    let associated_types = defs_with_data().map(|&d| {
        let name = d.name_ident();

        quote! { type #name }
    });

    quote! {
        pub trait TokenKind {
            #(#associated_types;)*
        }
    }
}

fn generate_token() -> proc_macro2::TokenStream {
    let variants = TOKEN_DEFS.iter().map(|&d| {
        let TokenDef { doc, has_data, .. } = d;
        let name = d.name_ident();
        let field = if has_data {
            quote! { (K::#name) }
        } else {
            quote! {}
        };

        quote! {
            #[doc = #doc]
            #name #field
        }
    });

    quote! {
        #[derive(Debug, Copy, Clone, Eq)]
        pub enum Token<K>
        where
            K: TokenKind,
        {
            #(#variants,)*
        }
    }
}

fn generate_token_inherent() -> proc_macro2::TokenStream {
    let k1_requirements = defs_with_data().map(|&d| {
        let name = d.name_ident();

        quote! { #name = T }
    });
    let k2_requirements = defs_with_data().map(|&d| {
        let name = d.name_ident();

        quote! { #name = U }
    });

    let arms = TOKEN_DEFS.iter().map(|&d| {
        let TokenDef { has_data, .. } = d;
        let name = d.name_ident();

        if has_data {
            quote! { Token::#name(v) => Token::#name(f(v)) }
        } else {
            quote! { Token::#name => Token::#name }
        }
    });

    quote! {
        impl<T, K1> Token<K1>
        where
            K1: TokenKind<#(#k1_requirements,)*>,
        {
            #[inline]
            pub fn map<U, K2>(self, f: impl FnOnce(T) -> U) -> Token<K2>
            where
                K2: TokenKind<#(#k2_requirements,)*>,
            {
                match self {
                    #(#arms,)*
                }
            }
        }
    }
}

fn generate_token_partialeq() -> proc_macro2::TokenStream {
    let bounds = defs_with_data().map(|&d| {
        let name = d.name_ident();

        quote! {
            K1::#name: PartialEq<K2::#name>
        }
    });

    let arms = TOKEN_DEFS.iter().map(|&d| {
        let TokenDef { has_data, .. } = d;
        let name = d.name_ident();

        if has_data {
            quote! { (Token::#name(l), Token::#name(r)) => l == r }
        } else {
            quote! { (Token::#name, Token::#name) => true }
        }
    });

    quote! {
        impl<K1, K2> PartialEq<Token<K2>> for Token<K1>
        where
            K1: TokenKind,
            K2: TokenKind,
            #(#bounds,)*
        {
            #[inline]
            fn eq(&self, other: &Token<K2>) -> bool {
                match (self, other) {
                    #(#arms,)*
                    _ => false,
                }
            }
        }
    }
}

fn generate_uniform_kind() -> proc_macro2::TokenStream {
    let types = defs_with_data().map(|&d| {
        let name = d.name_ident();

        quote! { type #name = T }
    });

    quote! {
        #[derive(Debug, Copy, Clone)]
        pub struct UniformKind<T>(core::marker::PhantomData<T>);

        impl<T> TokenKind for UniformKind<T> {
            #(#types;)*
        }
    }
}
