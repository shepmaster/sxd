use darling::{
    ast,
    util::{Flag, Override},
    FromDeriveInput, FromField, FromVariant,
};
use heck::ToKebabCase;
use quote::{format_ident, quote};
use syn::parse_macro_input;

#[derive(FromDeriveInput)]
#[darling(attributes(map))]
struct TypeConfig {
    ident: syn::Ident,
    data: ast::Data<VariantConfig, FieldConfigRaw>,
    element_name: Option<String>,
}

#[derive(FromVariant)]
#[darling(attributes(map))]
struct VariantConfig {
    ident: syn::Ident,
    fields: ast::Fields<VariantFieldConfig>,
}

#[derive(FromField)]
struct VariantFieldConfig {
    ty: syn::Type,
}

#[derive(FromField)]
#[darling(attributes(map))]
struct FieldConfigRaw {
    ident: Option<syn::Ident>,
    ty: syn::Type,
    attribute: Flag,
    content: Flag,
    rename: Option<String>,
    // Also add `default_value` that doesn't require a closure / fn?
    default: Option<Override<syn::Expr>>,
}

struct FieldConfig {
    ident: syn::Ident,
    ty: syn::Type,
    style: FieldStyle,
    default: Option<Override<syn::Expr>>,
}

enum FieldStyle {
    Attribute { name: String },
    Child,
}

#[proc_macro_derive(Mapper, attributes(map))]
pub fn derive_mapper(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(item as syn::DeriveInput);

    derive_mapper_core(input)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

fn derive_mapper_core(input: syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let type_config = TypeConfig::from_derive_input(&input)?;

    let crate_name = format_ident!("crate");
    let type_name = &type_config.ident;
    let builder_name = format_ident!("{}Builder", type_name);

    // DOC: kebab case default
    let element_name = type_config
        .element_name
        .clone()
        .unwrap_or_else(|| type_config.ident.to_string().to_kebab_case());

    match type_config.data {
        ast::Data::Enum(vs) => derive_mapper_core_enum(vs, crate_name, type_name, builder_name),
        ast::Data::Struct(fs) => {
            derive_mapper_core_struct(fs, crate_name, type_name, builder_name, element_name)
        }
    }
}

fn derive_mapper_core_enum(
    vs: Vec<VariantConfig>,
    crate_name: proc_macro2::Ident,
    type_name: &proc_macro2::Ident,
    builder_name: proc_macro2::Ident,
) -> syn::Result<proc_macro2::TokenStream> {
    let variant_attempts = vs.iter().map(|v| {
        let ident = &v.ident;

        assert_eq!(v.fields.len(), 1);
        let f = &v.fields.fields[0];
        let ty = &f.ty;

        quote! {
            let mut builder = <<#ty as FromContent>::Builder>::default();
            // TODO: would there ever be multiple calls to element here?
            // TODO: test with a Vec
            if Accumulated::Yes == builder.element(name, token_source)? {
                self.0 = builder.complete()?.map(#type_name::#ident);
                return Ok(Accumulated::Yes);
            }
        }
    });

    Ok(quote! {
        #[automatically_derived]
        impl #crate_name::FromContent for #type_name {
            type Builder = #builder_name;
        }

        #[derive(Default)]
        struct #builder_name(Option<#type_name>);

        #[automatically_derived]
        impl #crate_name::FromContentAccumulator for #builder_name {
            fn element(
                &mut self,
                name: QName<&str>,
                token_source: &mut dyn Peekable,
            ) -> Result<Accumulated> {
                #(#variant_attempts)*

                Ok(Accumulated::No)
            }
        }

        #[automatically_derived]
        impl #crate_name::FromContentBuilder for #builder_name {
            type Output = #type_name;

            fn complete(self) -> Result<Option<Self::Output>> {
                Ok(self.0)
            }
        }

    })
}

fn derive_mapper_core_struct(
    fs: ast::Fields<FieldConfigRaw>,
    crate_name: proc_macro2::Ident,
    type_name: &proc_macro2::Ident,
    builder_name: proc_macro2::Ident,
    element_name: String,
) -> syn::Result<proc_macro2::TokenStream> {
    let is_unit = fs.is_empty();
    let is_named = fs.iter().any(|f| f.ident.is_some());

    let fs = fs
        .into_iter()
        .enumerate()
        .map(|(i, f)| {
            let FieldConfigRaw {
                ident,
                ty,
                attribute,
                content,
                rename,
                default,
            } = f;

            // TODO warn on `rename` for content

            let style = match (attribute.is_present(), content.is_present()) {
                (true, false) => {
                    let name = rename
                        .or_else(|| ident.clone().map(|i| i.to_string().to_kebab_case()))
                        .expect("Must have a name for an attribute");
                    FieldStyle::Attribute { name }
                }

                (false, true) => FieldStyle::Child,

                (true, true) => panic!("Must specify only one child or attribute"),
                (false, false) => panic!("Must specify child or attribute"),
            };

            let ident = ident.unwrap_or_else(|| format_ident!("field{}", i));

            FieldConfig {
                ident,
                ty,
                style,
                default,
            }
        })
        .collect::<Vec<_>>();

    let mut attrs = Vec::with_capacity(fs.len());
    let mut children = Vec::with_capacity(fs.len());

    for f in &fs {
        if matches!(f.style, FieldStyle::Attribute { .. }) {
            attrs.push(f);
        } else {
            children.push(f)
        }
    }

    let attr_inits = attrs.iter().map(|f| {
        let ident = &f.ident;
        let ty = &f.ty;

        quote! {
            let mut #ident = <Option<<#ty as #crate_name::FromAttribute>::Builder>>::None
        }
    });

    let attr_match_arms = attrs.iter().map(|f| {
        let ident = &f.ident;
        // TODO: Need to fail if there's no `rename` selected for tuple struct fields
        let attribute_name = match &f.style {
            FieldStyle::Attribute { name } => name,
            _ => unreachable!("Fix the code to remove this arm"),
        };

        quote! {
            #attribute_name => #ident.get_or_insert_with(::core::default::Default::default)
        }
    });

    // TODO finalize earlier to avoid parsing the children on error?
    let attr_finalizers = attrs.iter().map(|f| {
        let ident = &f.ident;
        let when_missing = missing_expr(f.default.as_ref(), "The attribute X was missing");

        quote! { let #ident = #ident.map(#crate_name::FromAttributeBuilder::complete) #when_missing }
    });

    let child_inits = children.iter().map(|f| {
        let ident = &f.ident;
        let ty = &f.ty;

        quote! { let mut #ident = <<#ty as #crate_name::FromContent>::Builder>::default() }
    });

    let child_count = children.len();

    let child_list_inits = children.iter().map(|f| &f.ident);

    let child_finalizers = children.iter().map(|f| {
        let ident = &f.ident;
        let when_missing = missing_expr(f.default.as_ref(), "The child X was missing");

        quote! { let #ident = #crate_name::FromContentBuilder::complete(#ident)? #when_missing }
    });

    // TODO: Should `element_name` be an interned string?

    let struct_body = if is_unit {
        quote! { #type_name }
    } else if is_named {
        let field_names = fs.iter().map(|f| &f.ident);
        quote! { #type_name { #(#field_names,)* } }
    } else {
        let field_names = fs.iter().map(|f| &f.ident);
        quote! { #type_name(#(#field_names,)*) }
    };

    Ok(quote! {
        #[automatically_derived]
        impl #crate_name::FromContent for #type_name {
            type Builder = #builder_name;
        }

        #[derive(Default)]
        struct #builder_name(Option<#type_name>);

        #[automatically_derived]
        impl #crate_name::FromContentAccumulator for #builder_name {
            fn element(&mut self, name: QName<&str>, token_source: &mut dyn Peekable) -> Result<Accumulated> {
                if name.local_part != #element_name {
                    return Ok(Accumulated::No);
                }

                #(#attr_inits;)*
                let mut current_attr: Option<&mut dyn FromAttributeAccumulator> = None;

                // TODO add the runtime helper / re-export module for `Token`

                let might_contain_children = loop {
                    match token_source.expect_str()? {
                        Token::AttributeStart(t) => {
                            let selected: &mut dyn #crate_name::FromAttributeAccumulator = match t {
                                #(#attr_match_arms,)*
                                // TODO: optional ignore unknown attrs
                                // TODO: optional put unknown elsewhere (e.g. hashmap)

                                _ => Err(format!("Unknown attribute '{}'", t))?,
                            };

                            // TODO: error when attribute restarted? Or trust the validator?
                            current_attr = Some(selected);
                        }
                        Token::AttributeStartComplete => { /* no-op; name fused */ }

                        Token::AttributeValueLiteral(t) => {
                            current_attr.as_mut().expect("No attribute").literal(t.unify());
                        }
                        Token::AttributeValueReferenceNamed(t) => {
                            current_attr.as_mut().expect("No attribute").reference_named(t);
                        }
                        Token::AttributeValueReferenceDecimal(t) => {
                            current_attr.as_mut().expect("No attribute").reference_decimal(t);
                        }
                        Token::AttributeValueReferenceHex(t) => {
                            current_attr.as_mut().expect("No attribute").reference_hex(t);
                        }

                        Token::AttributeValueEnd => {
                            current_attr = None;
                        }

                        Token::ElementOpenEnd => {
                            break true;
                        }

                        Token::ElementSelfClose => {
                            break false;
                        }

                        o => Err(format!(
                            "Unexpected token {:?} when doing attributes",
                            o.variant()
                        ))?,
                    }
                };

                #(#child_inits;)*

                if might_contain_children {
                    let children: [&mut dyn #crate_name::FromContentAccumulator; #child_count] = [
                        #(&mut #child_list_inits,)*
                    ];
                    let mut children = children.into_iter();

                    let mut maybe_child = children.next();
                    let mut maybe_content = token_source.next_content()?;

                    while let (Some(child), Some(content)) = (maybe_child, maybe_content) {
                        use #crate_name::Content;

                        match content {
                            Content::CharData(s) => {
                                let mut s = s.map(|x| token_source.exchange_junk(x));
                                if Accumulated::Yes == child.char_data(s)? {
                                    maybe_child = Some(child);
                                    maybe_content = token_source.next_content()?;
                                } else if s.unify().trim().is_empty() {  // TODO: real whitespace check
                                    // TODO: handle incomplete text here
                                    maybe_child = Some(child);
                                    maybe_content = token_source.next_content()?;
                                } else {
                                    maybe_child = children.next();
                                    maybe_content = Some(content);
                                }
                            }

                            Content::ElementOpenStart(name) => {
                                if Accumulated::Yes == child.element(name.as_deref(), token_source)? {
                                    maybe_child = Some(child);
                                    maybe_content = token_source.next_content()?;
                                } else {
                                    maybe_child = children.next();
                                    maybe_content = Some(Content::ElementOpenStart(name));
                                }
                            }
                        }
                    }

                    token_source.expect_element_close()?;
                }

                #(#attr_finalizers;)*
                #(#child_finalizers;)*

                self.0 = Some(#struct_body);
                return Ok(Accumulated::Yes);
            }
        }

        #[automatically_derived]
        impl #crate_name::FromContentBuilder for #builder_name {
            type Output = #type_name;

            fn complete(self) -> Result<Option<Self::Output>> {
                Ok(self.0)
            }
        }
    })
}

fn missing_expr(def: Option<&Override<syn::Expr>>, error: &str) -> proc_macro2::TokenStream {
    match def {
        Some(Override::Explicit(def_expr)) => quote! { .unwrap_or_else(#def_expr) },

        Some(Override::Inherit) => quote! { .unwrap_or_else(::core::default::Default::default) },

        None => quote! { .expect(#error) },
    }
}
