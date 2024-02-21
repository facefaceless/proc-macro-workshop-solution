use proc_macro::{Span, TokenStream, TokenTree};
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, DataStruct, DeriveInput, Fields, FieldsNamed, Ident, Path,
    TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match expand(&input) {
        Ok(output) => output,
        Err(e) => e.to_compile_error().into(),
    }
}

fn expand(input: &DeriveInput) -> syn::Result<TokenStream> {
    let struct_name = &input.ident;
    let builder_name = format!("{}Builder", struct_name);
    let builder_ident = Ident::new(&builder_name, input.span());
    // eprintln!("{:#?}", input.data);
    let fields_info = get_field_info(input)?;

    let builder_fields: Vec<_> = fields_info
        .iter()
        .map(|(ident, ty, .., gen_each)| match gen_each {
            Some(_) => quote!(#ident:#ty,),
            None => quote!(#ident:core::option::Option<#ty>,),
        })
        .collect();

    let builder_fields_init: Vec<_> = fields_info
        .iter()
        .map(|(ident, .., gen_each)| match gen_each {
            Some(_) => quote!(#ident:std::vec::Vec::new(),),
            None => quote!(#ident:core::option::Option::None,),
        })
        .collect();

    let builder_methods: Vec<_> = fields_info
        .iter()
        .map(|(ident, ty, _, gen_each)| match gen_each {
            Some((fun_name, elem_ty)) => {
                let fun_name_ident = Ident::new(fun_name, Span::call_site().into());
                if ident == &fun_name {
                    quote! {
                        pub fn #fun_name_ident(&mut self, #fun_name_ident:#elem_ty)->&mut Self{
                            self.#ident.push(#fun_name_ident);
                            self
                        }
                    }
                } else {
                    quote! {
                        pub fn #fun_name_ident(&mut self, #fun_name_ident:#elem_ty)->&mut Self{
                            self.#ident.push(#fun_name_ident);
                            self
                        }

                        pub fn #ident(&mut self, #ident:#ty)->&mut Self{
                            self.#ident = #ident;
                            self
                        }
                    }
                }
            }
            None => {
                quote! {
                    pub fn #ident(&mut self, #ident:#ty)->&mut Self{
                        self.#ident = core::option::Option::Some(#ident);
                        self
                    }
                }
            }
        })
        .collect();

    let struct_init = {
        let init: Vec<_> = fields_info
            .iter()
            .map(|(ident, ..)| quote!(#ident,))
            .collect();
        quote!(#struct_name { #(#init)* })
    };

    let builder_build_method = {
        let fields_check: Vec<_> = fields_info
            .iter()
            .map(|(ident, _ty, allow_uninit, gen_each)| match allow_uninit {
                true => quote!(let #ident = self.#ident.clone();),
                false => match gen_each {
                    Some(_) => quote!(let #ident = self.#ident.clone();),
                    None => {
                        let err_msg = format!("no {}", ident);
                        quote! {
                            let #ident = match &self.#ident {
                                Some(#ident) => #ident.clone(),
                                None => return core::result::Result::Err(#err_msg.to_owned().into())
                            };
                        }
                    }
                },
            })
            .collect();
        quote! {
            pub fn build(&mut self) -> core::result::Result<#struct_name, std::boxed::Box<dyn std::error::Error>> {
                #(#fields_check)*
                core::result::Result::Ok(#struct_init)
            }
        }
    };

    let output = quote! {
        pub struct #builder_ident {
            #(#builder_fields)*
        }
        impl #struct_name {
            pub fn builder() -> #builder_ident {
                #builder_ident { #(#builder_fields_init)* }
            }
        }
        impl #builder_ident {
            #(#builder_methods)*
            #builder_build_method
        }
    };
    // eprintln!("{}", output);
    Ok(output.into())
}

fn get_field_info(
    input: &DeriveInput,
) -> syn::Result<Vec<(&syn::Ident, &syn::Type, bool, Option<(String, &syn::Type)>)>> {
    match &input.data {
        syn::Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { named, .. }),
            ..
        }) => {
            let mut info = vec![];
            for field in named.iter() {
                // eprintln!("{:#?}", field.attrs);
                // check attributes
                let mut gen_each = None;
                let mut attr_token_stream = field.attrs.iter();
                while let Some(attr) = attr_token_stream.next() {
                    if let syn::Meta::List(list) = &attr.meta {
                        if let Some(seg) = list.path.segments.last() {
                            if seg.ident == "builder" {
                                let mut tokens =
                                    Into::<TokenStream>::into(list.tokens.clone()).into_iter();
                                // match "each"
                                match tokens.next() {
                                    Some(TokenTree::Ident(ident))
                                        if ident.to_string() == "each" => {}
                                    Some(_) => {
                                        return Err(syn::Error::new_spanned(
                                            list,
                                            "expected `builder(each = \"...\")`",
                                        ));
                                    }
                                    None => {
                                        return Err(syn::Error::new_spanned(
                                            &list.tokens,
                                            "no token found, expect an \"each\"",
                                        ))
                                    }
                                }
                                // match "="
                                match tokens.next() {
                                    Some(TokenTree::Punct(punct)) if punct.as_char() == '=' => {}
                                    Some(token) => {
                                        return Err(syn::Error::new(
                                            token.span().into(),
                                            "expect a '='",
                                        ))
                                    }
                                    None => {
                                        return Err(syn::Error::new_spanned(
                                            &list.tokens,
                                            "no token found, expect a '='",
                                        ))
                                    }
                                }
                                match tokens.next() {
                                    Some(TokenTree::Literal(literal))
                                        if literal.to_string().starts_with('"') =>
                                    {
                                        let name_of_fun_each =
                                            literal.to_string().trim_matches('"').to_owned();
                                        let vec_elem_ty = match &field.ty {
                                            syn::Type::Path(TypePath {
                                                path: Path { segments, .. },
                                                ..
                                            }) => match segments.last() {
                                                Some(seg) if seg.ident == "Vec" => {
                                                    match &seg.arguments {
                                                        syn::PathArguments::AngleBracketed(
                                                            args,
                                                        ) => match args.args.first() {
                                                            Some(syn::GenericArgument::Type(
                                                                inner_ty,
                                                            )) => inner_ty,
                                                            _ => todo!(),
                                                        },
                                                        _ => todo!(),
                                                    }
                                                }
                                                _ => todo!(),
                                            },
                                            _ => todo!(),
                                        };
                                        gen_each = Some((name_of_fun_each, vec_elem_ty));
                                    }
                                    Some(token) => {
                                        return Err(syn::Error::new(
                                            token.span().into(),
                                            "expect a string literal",
                                        ))
                                    }
                                    None => {
                                        return Err(syn::Error::new_spanned(
                                            &list.tokens,
                                            "no token found, expect a string literal",
                                        ))
                                    }
                                };
                                if let Some(token) = tokens.next() {
                                    return Err(syn::Error::new(token.span().into(), "expect end"));
                                }
                            }
                        }
                    }
                }
                let mut ty = None;
                let allow_uninit = match &field.ty {
                    syn::Type::Path(TypePath {
                        path: Path { segments, .. },
                        ..
                    }) => match segments.last() {
                        Some(seg) if seg.ident == "Option" => {
                            if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                                if let Some(syn::GenericArgument::Type(inner_ty)) =
                                    args.args.first()
                                {
                                    ty = Some(inner_ty);
                                }
                            }
                            true
                        }
                        _ => false,
                    },
                    _ => false,
                };
                info.push((
                    field.ident.as_ref().unwrap(),
                    ty.unwrap_or(&field.ty),
                    allow_uninit,
                    gen_each,
                ));
            }
            Ok(info)
        }
        _ => Err(syn::Error::new(
            input.span(),
            "only named field struct is supported",
        )),
    }
}
