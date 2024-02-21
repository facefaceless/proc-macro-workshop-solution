use proc_macro::TokenStream;
use quote::{format_ident, quote, quote_spanned, ToTokens};
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, Expr, Fields, FieldsNamed, Item, ItemEnum,
    Lit, Meta, Token,
};

#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let item = parse_macro_input!(input as Item);
    match expand_bitfield(item) {
        Ok(output) => output,
        Err(err) => err.to_compile_error().into(),
    }
}

struct SpecifierArg {
    ident: syn::Ident,
    n_lit: syn::LitInt,
}

impl syn::parse::Parse for SpecifierArg {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident = input.parse()?;
        input.parse::<Token![,]>()?;
        let n_lit = input.parse()?;
        Ok(SpecifierArg { ident, n_lit })
    }
}

#[proc_macro]
pub fn impl_specifier(input: TokenStream) -> TokenStream {
    let SpecifierArg { ident, n_lit } = parse_macro_input!(input as SpecifierArg);
    let n = match n_lit.base10_parse::<u8>() {
        Ok(n) => n,
        Err(err) => return err.to_compile_error().into(),
    };
    let param_ty = match n {
        1..=8 => quote!(::core::primitive::u8),
        9..=16 => quote!(::core::primitive::u16),
        17..=32 => quote!(::core::primitive::u32),
        33..=64 => quote!(::core::primitive::u64),
        _ => {
            return syn::Error::new_spanned(n_lit, "only integers from 1 to 64 are allowed")
                .to_compile_error()
                .into()
        }
    };
    let output = quote! {
        impl Specifier for #ident {
            type ParamType = #param_ty;
            const BITS: usize = #n_lit;
            unsafe fn read<const BIT_OFFSET: usize>(arr_ptr: *const ::core::primitive::u8) -> Self::ParamType {
                read::<BIT_OFFSET, { Self::BITS }>(arr_ptr) as Self::ParamType
            }
            unsafe fn write<const BIT_OFFSET: usize>(arr_ptr: *mut u8, val: Self::ParamType) {
                write::<BIT_OFFSET, { Self::BITS }>(arr_ptr, val.into());
            }
        }
    };
    output.into()
}

fn expand_bitfield(item: Item) -> syn::Result<TokenStream> {
    let mut item_struct = match item {
        Item::Struct(item) => item,
        item => return Err(syn::Error::new_spanned(item, "only work with struct")),
    };
    if let Fields::Unit = item_struct.fields {
        return Err(syn::Error::new_spanned(item_struct, "no field found"));
    }
    // total size in bit
    let mut size_in_bit = quote!(0usize);
    // fields' offset in bit
    let mut field_offsets = vec![];
    // name of getter method
    let mut getter_idents = vec![];
    // name of setter method
    let mut setter_idents = vec![];
    let mut field_types = vec![];
    // compile-time check for attr `#[bits = 1]`
    let mut bits_attr_assert = quote!();
    for (idx, field) in item_struct.fields.iter().enumerate() {
        // getter and setter names
        let ident_name = field
            .ident
            .as_ref()
            .map_or_else(|| idx.to_string(), |ident| ident.to_string());
        getter_idents.push(format_ident!("get_{}", ident_name));
        setter_idents.push(format_ident!("set_{}", ident_name));
        field_types.push(&field.ty);
        let ty = &field.ty;
        field_offsets.push(size_in_bit.clone());
        size_in_bit.extend(quote!(+ <#ty as bitfield::Specifier>::BITS));
        // check bits attr
        for attr in &field.attrs {
            if let Meta::NameValue(named_val) = &attr.meta {
                if named_val.path.is_ident("bits") {
                    if let Expr::Lit(lit) = &named_val.value {
                        if let Lit::Int(i) = &lit.lit {
                            let i: usize = i.base10_parse()?;
                            let field_ty = &field.ty;
                            let assert = quote_spanned! {named_val.value.span()=>
                                let _: [(); #i] = [(); <#field_ty as bitfield::Specifier>::BITS];
                            };
                            bits_attr_assert.extend(assert);
                            continue;
                        }
                    }
                    // if not a integer literal, report it
                    return Err(syn::Error::new_spanned(
                        &named_val.value,
                        "integer literal is expected",
                    ));
                }
            }
        }
    }

    let assertion = quote! {
        // introduce "anonymous module", avoid conflicts in user's module
        const _: () = {
            trait Bind {
                type BindType;
            }
            impl Bind for [(); 0] {
                type BindType = bitfield::checks::ZeroMod8;
            }
            impl Bind for [(); 1] {
                type BindType = bitfield::checks::OneMod8;
            }
            impl Bind for [(); 2] {
                type BindType = bitfield::checks::TwoMod8;
            }
            impl Bind for [(); 3] {
                type BindType = bitfield::checks::ThreeMod8;
            }
            impl Bind for [(); 4] {
                type BindType = bitfield::checks::FourMod8;
            }
            impl Bind for [(); 5] {
                type BindType = bitfield::checks::FiveMod8;
            }
            impl Bind for [(); 6] {
                type BindType = bitfield::checks::SixMod8;
            }
            impl Bind for [(); 7] {
                type BindType = bitfield::checks::SevenMod8;
            }
            struct Assert
            where
                <[(); (#size_in_bit) % 8] as Bind>::BindType: bitfield::checks::TotalSizeIsMultipleOfEightBits;

            #bits_attr_assert
        };
    };
    let struct_ident = &item_struct.ident;
    let impl_struct = quote! {
        impl #struct_ident {
            fn new() -> Self {
                Self { data: core::default::Default::default() }
            }

            #(
                fn #getter_idents(&self) -> <#field_types as bitfield::Specifier>::ParamType {
                    unsafe { <#field_types as bitfield::Specifier>::read::<{ #field_offsets }>(self.data.as_ptr()) }
                }
            )*

            #(
                fn #setter_idents(&mut self, val: <#field_types as bitfield::Specifier>::ParamType) {
                    unsafe { <#field_types as bitfield::Specifier>::write::<{ #field_offsets }>(self.data.as_mut_ptr(), val.into()); }
                }
            )*
        }
    };
    // replace fields with byte array
    let data_field: FieldsNamed = parse_quote!({ data: [u8; (#size_in_bit) / 8] });
    item_struct.fields = Fields::Named(data_field);
    let mut output = item_struct.into_token_stream();
    output.extend(assertion);
    output.extend(impl_struct);
    Ok(output.into())
}

#[proc_macro_derive(BitfieldSpecifier)]
pub fn specifier_derive(input: TokenStream) -> TokenStream {
    let item_enum = parse_macro_input!(input as ItemEnum);
    expand_specifier_derive(item_enum).unwrap_or_else(|err| err.to_compile_error().into())
}

fn expand_specifier_derive(item_enum: ItemEnum) -> syn::Result<TokenStream> {
    #[allow(unused)]
    let assertions = check_variants(&item_enum)?;
    let enum_ident = item_enum.ident;
    let bits = item_enum.variants.len().checked_ilog2().unwrap() as usize;
    let impl_specifier = quote! {
        impl bitfield::Specifier for #enum_ident {
            type ParamType = Self;

            const BITS: usize = #bits;

            unsafe fn read<const BIT_OFFSET: usize>(arr_ptr: *const u8) -> Self::ParamType {
                let num = bitfield::read::<BIT_OFFSET, { Self::BITS }>(arr_ptr);
                let mut ret = ::core::mem::MaybeUninit::<Self::ParamType>::uninit();
                ret.as_mut_ptr()
                    .cast::<bitfield::utype!(::core::mem::size_of::<Self::ParamType>())>()
                    .write(num as _);
                ret.assume_init()
            }

            unsafe fn write<const BIT_OFFSET: usize>(arr_ptr: *mut u8, val: Self::ParamType) {
                bitfield::write::<BIT_OFFSET, { Self::BITS }>(arr_ptr, val as isize as u64);
            }
        }
    };
    let output = quote! {
        #assertions
        #impl_specifier
    };
    Ok(output.into())
}

/// Returns check assertion if no other error found;
fn check_variants(item_enum: &ItemEnum) -> syn::Result<proc_macro2::TokenStream> {
    let mut assertions = quote!();
    let variants = &item_enum.variants;
    if !variants.len().is_power_of_two() {
        return Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "BitfieldSpecifier expected a number of variants which is a power of 2",
        ));
    }
    let enum_ident = &item_enum.ident;
    for variant in variants {
        // check enum type
        match variant.fields {
            Fields::Unit => {}
            _ => {
                return Err(syn::Error::new_spanned(
                    item_enum,
                    "unit-only enum is required",
                ));
            }
        }
        // check discriminant in range
        let assert_struct_ident = format_ident!("Assert{}", variant.ident);
        let variant_ident = &variant.ident;
        let is_less = quote! {
            ((#enum_ident::#variant_ident as u64) < (1 << <#enum_ident as bitfield::Specifier>::BITS))
        };
        let assert = quote_spanned! {variant.span()=>
            struct #assert_struct_ident where
            <[(); #is_less as usize] as Bind>::BindType: bitfield::checks::DiscriminantInRange;
        };
        assertions.extend(assert);
    }
    let assertions = quote! {
        // do checking in anonymous module
        const _: () = {
            trait Bind {
                type BindType;
            }
            impl Bind for [(); 0] {
                type BindType = bitfield::checks::False;
            }
            impl Bind for [(); 1] {
                type BindType = bitfield::checks::True;
            }
            #assertions
        };
    };
    Ok(assertions)
}
