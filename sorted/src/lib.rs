use proc_macro::{Span, TokenStream};

use quote::ToTokens;
use syn::{parse_macro_input, spanned::Spanned, visit_mut::VisitMut, Item, ItemEnum, ItemFn, Meta};

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let item = parse_macro_input!(input as Item);
    match sorted_impl(&item) {
        Ok(output) => output,
        Err(e) => {
            let mut e = e.to_compile_error();
            e.extend(item.to_token_stream());
            e.into()
        }
    }
}

fn sorted_impl(item: &Item) -> syn::Result<TokenStream> {
    match item {
        Item::Enum(item_enum) => check_enum(item_enum),
        _ => Err(syn::Error::new(
            Span::call_site().into(),
            "expected enum or match expression",
        )),
    }
}

fn check_enum(item_enum: &ItemEnum) -> syn::Result<TokenStream> {
    let ident_and_span: Vec<_> = item_enum
        .variants
        .iter()
        .map(|variant| (variant.ident.to_string(), variant.span()))
        .collect();
    for (idx, two_fields) in ident_and_span.windows(2).enumerate() {
        let (ident0, _) = &two_fields[0];
        let (ident1, span1) = &two_fields[1];
        if ident0 > ident1 {
            let mut ident = "";
            for i in 0..=idx {
                if &ident_and_span[i].0 > ident1 {
                    ident = &ident_and_span[i].0;
                    break;
                }
            }
            return Err(syn::Error::new(
                *span1,
                format!("{} should sort before {}", ident1, ident),
            ));
        }
    }
    Ok(item_enum.to_token_stream().into())
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let item_fn = parse_macro_input!(input as ItemFn);
    check_and_mutate_fn(item_fn)
}

fn check_and_mutate_fn(mut item_fn: ItemFn) -> TokenStream {
    let mut visitor = MatchVisitor::default();
    visitor.visit_item_fn_mut(&mut item_fn);
    match visitor.error {
        Some(err) => {
            let mut err = err.to_compile_error();
            // attach the modified fn without attr #[sorted]
            err.extend(item_fn.to_token_stream());
            err.into()
        }
        None => item_fn.to_token_stream().into(),
    }
}

#[derive(Default)]
struct MatchVisitor {
    error: Option<syn::Error>,
}

#[inline]
fn path_string(path: &syn::Path) -> String {
    path.segments
        .iter()
        .map(|seg| seg.ident.to_string())
        .collect::<Vec<_>>()
        .join("::")
}

impl VisitMut for MatchVisitor {
    fn visit_expr_match_mut(&mut self, i: &mut syn::ExprMatch) {
        if self.error.is_some() {
            return;
        }
        let mut sorted_attr_idx = None;
        for (idx, attr) in i.attrs.iter().enumerate() {
            if let Meta::Path(path) = &attr.meta {
                let seg = &path.segments;
                if seg.len() == 1 && seg[0].ident == "sorted" {
                    sorted_attr_idx = Some(idx);
                    break;
                }
            }
        }
        // found attr #[sorted]
        if let Some(idx) = sorted_attr_idx {
            // remove attr #[sorted]
            i.attrs.remove(idx);
            // check arm order
            let mut all_names_and_spans: Vec<(String, &dyn ToTokens)> = vec![];
            for (idx, arm) in i.arms.iter().enumerate() {
                match &arm.pat {
                    syn::Pat::Ident(pat_ident) => {
                        all_names_and_spans.push((pat_ident.ident.to_string(), &pat_ident.ident));
                    }
                    syn::Pat::Path(expr_path) => {
                        let name = path_string(&expr_path.path);
                        all_names_and_spans.push((name, &expr_path.path));
                    }
                    syn::Pat::Struct(pat_struct) => {
                        let name = path_string(&pat_struct.path);
                        all_names_and_spans.push((name, &pat_struct.path));
                    }
                    syn::Pat::TupleStruct(pat_tuple_struct) => {
                        let name = path_string(&pat_tuple_struct.path);
                        all_names_and_spans.push((name, &pat_tuple_struct.path));
                    }
                    syn::Pat::Wild(pat_wild) => {
                        if idx != i.arms.len() - 1 {
                            self.error = Some(syn::Error::new(
                                pat_wild.span(),
                                "`_` pattern should be placed at the last",
                            ));
                            return;
                        }
                    }
                    arm => {
                        self.error = Some(syn::Error::new(arm.span(), "unsupported by #[sorted]"));
                        return;
                    }
                };
            }
            let sorted = {
                let mut tmp = all_names_and_spans.iter().collect::<Vec<_>>();
                tmp.sort_by(|a, b| a.0.cmp(&b.0));
                tmp
            };
            for (sorted_ref, not_sorted) in sorted.into_iter().zip(all_names_and_spans.iter()) {
                if sorted_ref.0 != not_sorted.0 {
                    self.error = Some(syn::Error::new_spanned(
                        sorted_ref.1,
                        format!("{} should sort before {}", sorted_ref.0, not_sorted.0),
                    ));
                    return;
                }
            }
        }
        syn::visit_mut::visit_expr_match_mut(self, i);
    }
}
