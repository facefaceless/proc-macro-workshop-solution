use std::collections::HashSet;

use proc_macro::TokenStream;

use quote::quote;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, parse_quote, parse_str, Attribute, DataStruct, DeriveInput, Error, Expr,
    Fields, FieldsNamed, GenericParam, Generics, Ident, Lit, Token,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    // eprintln!("{:#?}", input);
    // TokenStream::new()
    match expand(&input) {
        Ok(stream) => stream,
        Err(e) => e.to_compile_error().into(),
    }
}

fn expand(input: &DeriveInput) -> syn::Result<TokenStream> {
    let struct_ident = &input.ident;
    let struct_name = struct_ident.to_string();

    let field_info = get_field_info(&input)?;

    let field_idents: Vec<_> = field_info.iter().map(|f| f.ident).collect();
    let field_names: Vec<_> = field_info.iter().map(|f| f.ident.to_string()).collect();
    let field_fmt: Vec<_> = field_info
        .iter()
        .map(|field| match &field.fmt {
            Some(fmt) => fmt,
            None => "{:?}",
        })
        .collect();

    // handle generic
    let generic_param_idents: HashSet<_> = input
        .generics
        .params
        .iter()
        .filter_map(|param| match param {
            GenericParam::Type(ty) => Some(&ty.ident),
            _ => None,
        })
        .collect();
    let mut visitor = TypeVisitor {
        be_aware: generic_param_idents,
        found: HashSet::new(),
    };
    visitor.visit_derive_input(input);
    let user_specified = get_bounds_from_attr(&input.attrs)?;
    let modified_generics =
        add_debug_bound(input.generics.clone(), &visitor.found, user_specified)?;
    let (impl_generics, ty_generics, where_clause) = modified_generics.split_for_impl();
    let output = quote! {
        impl #impl_generics core::fmt::Debug for #struct_ident #ty_generics #where_clause {
            fn fmt(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                fmt.debug_struct(#struct_name)
                    #(.field(#field_names, &format_args!(#field_fmt, self.#field_idents)))*
                    .finish()
            }
        }
    };

    Ok(output.into())
}

fn add_debug_bound(
    mut generics: Generics,
    extra_impl_debug: &HashSet<&syn::TypePath>,
    user_specified: Vec<String>,
) -> syn::Result<Generics> {
    let clause = generics.make_where_clause();
    if user_specified.is_empty() {
        for type_path in extra_impl_debug {
            clause
                .predicates
                .push(parse_quote!(#type_path: core::fmt::Debug));
        }
    } else {
        for bound in user_specified {
            clause.predicates.push(parse_str(&bound)?);
        }
    }
    Ok(generics)
}

struct FieldInfo<'a> {
    ident: &'a Ident,
    fmt: Option<String>,
}

// struct Info<'a> {
//     field_info: Vec<FieldInfo<'a>>,
//     ignore_params: HashSet<&'a Ident>,
//     should_impl_debug: HashSet<&'a Punctuated<PathSegment, Token![::]>>,
//     // should_impl_debug: HashSet<&'a Ident>,
// }

fn get_field_info(input: &DeriveInput) -> syn::Result<Vec<FieldInfo>> {
    match &input.data {
        syn::Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { named, .. }),
            ..
        }) => {
            let mut field_info = vec![];
            for field in named.iter() {
                let ident = field.ident.as_ref().unwrap();
                let fmt = get_field_fmt(&field.attrs);
                field_info.push(FieldInfo { ident, fmt });
            }
            Ok(field_info)
        }
        _ => Err(Error::new_spanned(
            input,
            "only named field struct is supported",
        )),
    }
}

fn get_field_fmt(attrs: &Vec<Attribute>) -> Option<String> {
    for attr in attrs.iter() {
        if let syn::Meta::NameValue(named_val) = &attr.meta {
            if let Some(seg) = named_val.path.segments.first() {
                if seg.ident == "debug" {
                    if let Expr::Lit(literal) = &named_val.value {
                        if let Lit::Str(str) = &literal.lit {
                            return Some(str.value());
                        }
                    }
                }
            }
        }
    }
    None
}

use syn::visit::{self, Visit};

struct TypeVisitor<'a> {
    be_aware: HashSet<&'a Ident>,
    found: HashSet<&'a syn::TypePath>,
}

impl<'ast> Visit<'ast> for TypeVisitor<'ast> {
    fn visit_type_path(&mut self, i: &'ast syn::TypePath) {
        let segments = &i.path.segments;
        if matches!(segments.last(), Some(seg) if seg.ident == "PhantomData") {
            return;
        }
        if segments.len() > 1 && self.be_aware.contains(&segments[0].ident) {
            self.found.insert(i);
            visit::visit_type_path(self, i);
        } else if segments.len() == 1 {
            if self.be_aware.contains(&segments[0].ident) {
                self.found.insert(i);
            } else {
                visit::visit_type_path(self, i);
            }
        } else {
            visit::visit_type_path(self, i);
        }
    }
}

fn get_bounds_from_attr(attrs: &Vec<Attribute>) -> syn::Result<Vec<String>> {
    let mut user_specified_bounds = vec![];
    for attr in attrs {
        if let syn::Meta::List(list) = &attr.meta {
            if list.path.is_ident("debug") {
                let bounds =
                    list.parse_args_with(Punctuated::<Expr, Token![,]>::parse_separated_nonempty)?;
                let err = |bound: &Expr| {
                    Err(syn::Error::new(
                        bound.span(),
                        "expect bound = \"trait bound\"",
                    ))
                };
                for bound in bounds.iter() {
                    match bound {
                        Expr::Assign(assign) => {
                            match assign.left.as_ref() {
                                Expr::Path(ident) if ident.path.is_ident("bound") => {}
                                _ => return err(bound),
                            }
                            match assign.right.as_ref() {
                                Expr::Lit(lit) => match &lit.lit {
                                    Lit::Str(str) => user_specified_bounds.push(str.value()),
                                    _ => return err(bound),
                                },
                                _ => return err(bound),
                            }
                        }
                        _ => return err(bound),
                    }
                }
            }
        }
    }
    Ok(user_specified_bounds)
}
