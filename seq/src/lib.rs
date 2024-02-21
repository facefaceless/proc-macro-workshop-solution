use std::ops::Range;

use proc_macro::TokenStream;

use proc_macro2::{Delimiter, Group, Literal, TokenTree};
use quote::{format_ident, TokenStreamExt};
use syn::{
    braced,
    buffer::{Cursor, TokenBuffer},
    parse::{Parse, ParseStream},
    Ident, LitInt, Token,
};
struct Seq {
    ident: Ident,
    start: isize,
    end: isize,
    block: proc_macro2::TokenStream,
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;
        input.parse::<Token![in]>()?;
        let start = input.parse::<LitInt>()?.base10_parse()?;
        input.parse::<Token![..]>()?;
        let end = match input.parse::<Token![=]>() {
            Ok(_) => {
                let end: isize = input.parse::<LitInt>()?.base10_parse()?;
                end + 1
            }
            Err(_) => input.parse::<LitInt>()?.base10_parse()?,
        };

        let content;
        braced!(content in input);
        let block = content.parse()?;
        Ok(Seq {
            ident,
            start,
            end,
            block,
        })
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    match syn::parse(input) {
        Ok(seq) => expand(seq).into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn expand(seq: Seq) -> TokenStream {
    let Seq {
        ident,
        start,
        end,
        block,
    } = seq;

    // let start: isize = start.base10_parse().unwrap();
    // let end: isize = end.base10_parse().unwrap();
    let mut output = proc_macro2::TokenStream::new();

    let buffer = TokenBuffer::new2(block);

    for n in start..end {
        let (stream, detect_state) = trans_once(buffer.begin(), &ident.to_string(), n, start..end);
        match detect_state {
            BOTH_DETECTED => panic!("can't expand in both ways"),
            PATTERN_DETECTED => {
                output.extend(stream);
                break;
            }
            _ => output.extend(stream),
        }
    }

    output.into()
    // TokenStream::new()
}

type DetectState = u8;

const NONE_DETECTED: u8 = 0;
const PATTERN_DETECTED: u8 = 1;
const IDENT_VAR_DETECTED: u8 = 2;
const BOTH_DETECTED: u8 = 3;

fn trans_once(
    begin: Cursor,
    ident_var: &str,
    replace_n: isize,
    replace_range: Range<isize>,
) -> (proc_macro2::TokenStream, DetectState) {
    let mut ret = proc_macro2::TokenStream::new();
    let mut cursor = begin;
    let mut detect_state = NONE_DETECTED;
    while let Some((token_tree, cursor1)) = cursor.token_tree() {
        match token_tree {
            TokenTree::Group(group) => {
                let grouped_buffer = TokenBuffer::new2(group.stream());
                let (stream, state) = trans_once(
                    grouped_buffer.begin(),
                    ident_var,
                    replace_n,
                    replace_range.clone(),
                );
                detect_state |= state;

                ret.append(Group::new(group.delimiter(), stream));
                cursor = cursor1;
            }
            TokenTree::Ident(ident) => match ident == ident_var {
                true => {
                    detect_state |= IDENT_VAR_DETECTED;
                    ret.append(Literal::isize_unsuffixed(replace_n));
                    cursor = cursor1;
                }
                false => match trans_tilde_ident(cursor1, ident_var, replace_n) {
                    Some((replaced, cursor2)) => {
                        detect_state |= IDENT_VAR_DETECTED;
                        let new_ident = format_ident!("{}{}", ident, replaced);
                        ret.append(new_ident);
                        cursor = cursor2;
                    }
                    None => {
                        ret.append(ident);
                        cursor = cursor1;
                    }
                },
            },
            TokenTree::Punct(punct) if punct.as_char() == '#' => {
                if let Some((cursor2_0, _, cursor2_1)) = cursor1.group(Delimiter::Parenthesis) {
                    if let Some((punct, cursor3)) = cursor2_1.punct() {
                        if punct.as_char() == '*' {
                            detect_state |= PATTERN_DETECTED;
                            for n in replace_range.clone() {
                                let (stream, _) =
                                    trans_once(cursor2_0, ident_var, n, replace_range.clone());
                                ret.extend(stream);
                            }
                            cursor = cursor3;
                            continue;
                        }
                    }
                }
                ret.append(punct);
                cursor = cursor1;
            }
            tree => {
                ret.append(tree);
                cursor = cursor1;
            }
        }
    }
    (ret, detect_state)
}

// tilde_ident -> "~" ident_var ( tilde_suffix )?
fn trans_tilde_ident<'a>(
    cursor: Cursor<'a>,
    ident_var: &str,
    replace_with: isize,
) -> Option<(String, Cursor<'a>)> {
    cursor
        .punct()
        .map_or(None, |(punct, cursor1)| match punct.as_char() == '~' {
            true => cursor1
                .ident()
                .map_or(None, |(ident, cursor2)| match ident == ident_var {
                    true => {
                        let mut replaced = replace_with.to_string();
                        match trans_tilde_suffix(cursor2) {
                            Some((ident, cursor3)) => {
                                replaced.push_str(&ident.to_string());
                                Some((replaced, cursor3))
                            }
                            None => Some((replaced, cursor2)),
                        }
                    }
                    false => None,
                }),
            false => None,
        })
}

// tilde_suffix -> ( "~" ident )?
fn trans_tilde_suffix<'a>(cursor: Cursor<'a>) -> Option<(Ident, Cursor<'a>)> {
    cursor
        .punct()
        .map_or(None, |(punct, cursor1)| match punct.as_char() == '~' {
            true => match cursor1.ident() {
                Some((ident, cursor2)) => Some((ident, cursor2)),
                None => None,
            },
            false => None,
        })
}
