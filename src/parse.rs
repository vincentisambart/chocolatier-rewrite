use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;

#[derive(Debug, PartialEq, Eq)]
pub struct ObjCMethodParam {
    pub ident: Option<syn::Ident>,
    pub colon_token: syn::Token![:],
    pub expr: syn::Expr,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ObjCMethodParams {
    Without(syn::Ident),
    With(Vec<ObjCMethodParam>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ObjCMacroReceiver {
    SelfValue(syn::Token![self]),
    Class(syn::Ident),
    SelfClass(syn::Token![self], Box<ObjCMethodCall>),
    SelfAlloc(syn::Token![self], Box<ObjCMethodCall>),
    SelfClassAlloc(syn::Token![self], Box<ObjCMethodCall>),
    ClassAlloc(syn::Ident, Box<ObjCMethodCall>),
}

impl Parse for ObjCMacroReceiver {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::Token![self]) {
            Ok(Self::SelfValue(input.parse()?))
        } else if lookahead.peek(syn::Ident) {
            let ident: syn::Ident = input.parse()?;
            match ident.to_string().chars().next() {
                Some(first) if first.is_ascii_uppercase() => {}
                _ => {
                    return Err(syn::Error::new_spanned(
                        ident,
                        "receiver expected to be `self` or a class name",
                    ))
                }
            }
            Ok(Self::Class(ident))
        } else if lookahead.peek(syn::token::Bracket) {
            let method_call: ObjCMethodCall = input.parse()?;
            if !matches!(&method_call.params, ObjCMethodParams::Without(sel) if sel == "alloc" || sel == "class")
            {
                let ident = match method_call.params {
                    ObjCMethodParams::Without(ident) => ident,
                    ObjCMethodParams::With(vec) => vec[0].ident.as_ref().unwrap().clone(),
                };
                return Err(syn::Error::new_spanned(
                    ident,
                    "the only nested calls allowed are calls to `alloc` or `class`",
                ));
            }
            let receiver = match &method_call.receiver {
                ObjCMacroReceiver::SelfValue(self_token) => match &method_call.params {
                    ObjCMethodParams::Without(sel) if sel == "alloc" => {
                        ObjCMacroReceiver::SelfAlloc(*self_token, Box::new(method_call))
                    }
                    ObjCMethodParams::Without(sel) if sel == "class" => {
                        ObjCMacroReceiver::SelfClass(*self_token, Box::new(method_call))
                    }
                    _ => unreachable!(),
                },
                ObjCMacroReceiver::Class(class_ident) => match &method_call.params {
                    ObjCMethodParams::Without(sel) if sel == "alloc" => {
                        ObjCMacroReceiver::ClassAlloc(class_ident.clone(), Box::new(method_call))
                    }
                    ObjCMethodParams::Without(sel) if sel == "class" => {
                        // `[[[NSString class] alloc] init]` is equivalent to `[[NSString alloc] init]`
                        ObjCMacroReceiver::Class(class_ident.clone())
                    }
                    _ => unreachable!(),
                },
                ObjCMacroReceiver::SelfClass(self_token, _) => match &method_call.params {
                    ObjCMethodParams::Without(sel) if sel == "alloc" => {
                        ObjCMacroReceiver::SelfClassAlloc(*self_token, Box::new(method_call))
                    }
                    ObjCMethodParams::Without(sel) if sel == "class" => {
                        return Err(syn::Error::new(
                            method_call.span(),
                            "nested calls to class do not do anything useful",
                        ))
                    }
                    _ => unreachable!(),
                },
                _ => {
                    return Err(syn::Error::new(
                        method_call.span(),
                        "nested calls to alloc are not allowed",
                    ))
                }
            };
            Ok(receiver)
        } else {
            Err(lookahead.error())
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ObjCMethodCall {
    pub bracket_token: syn::token::Bracket,
    pub receiver: ObjCMacroReceiver,
    pub params: ObjCMethodParams,
}

impl ObjCMethodCall {
    pub fn selector(&self) -> String {
        match &self.params {
            ObjCMethodParams::Without(ident) => ident.to_string(),
            ObjCMethodParams::With(params) => {
                let mut sel = String::new();
                for param in params {
                    if let Some(ident) = &param.ident {
                        sel.push_str(&ident.to_string());
                    }
                    sel.push_str(":");
                }
                sel
            }
        }
    }
}

impl Spanned for ObjCMethodCall {
    fn span(&self) -> proc_macro2::Span {
        // Could probably do better but we only display the starting position anyway so that will do for the time being.
        self.bracket_token.span
    }
}

impl Parse for ObjCMethodCall {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let content;
        let bracket_token = syn::bracketed!(content in input);
        let receiver: ObjCMacroReceiver = content.parse()?;
        let method_name_start: syn::Ident = content.parse()?;
        let params = if content.peek(syn::Token![:]) {
            let mut v: Vec<ObjCMethodParam> = Vec::new();

            let colon_token = content.parse()?;
            let expr = content.parse()?;
            v.push(ObjCMethodParam {
                ident: Some(method_name_start),
                colon_token,
                expr,
            });

            loop {
                if content.is_empty() {
                    break;
                }

                let ident: Option<syn::Ident> = if content.peek(syn::Ident) {
                    Some(content.parse()?)
                } else {
                    None
                };
                let colon_token = content.parse()?;
                let expr = content.parse()?;
                v.push(ObjCMethodParam {
                    ident,
                    colon_token,
                    expr,
                });
            }

            ObjCMethodParams::With(v)
        } else {
            ObjCMethodParams::Without(method_name_start)
        };
        Ok(Self {
            bracket_token,
            receiver,
            params,
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ObjCPropertyGet {
    pub receiver: ObjCMacroReceiver,
    pub dot_token: syn::Token![.],
    pub property_name: syn::Ident,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ObjCPropertySet {
    pub receiver: ObjCMacroReceiver,
    pub dot_token: syn::Token![.],
    pub property_name: syn::Ident,
    pub eq_token: syn::Token![=],
    pub val_expr: syn::Expr,
}

// Valid uses of the objc!() macro:
// - objc!([self myInstanceMethod:val1 param2:val2])
// - objc!([self innerMethod:val1] myMethod:val2 param2:val3])
// - objc!(myFunc(val1, val2))
// - objc!(self.myInstanceProperty)
// - objc!(Self.myClassProperty)
// - objc!(self.myInstanceProperty = 1)
#[allow(clippy::large_enum_variant)]
#[derive(Debug)]
pub enum ObjCExpr {
    MethodCall(ObjCMethodCall),
    PropertyGet(ObjCPropertyGet),
    PropertySet(ObjCPropertySet),
}

impl ObjCExpr {
    pub fn receiver(&self) -> &ObjCMacroReceiver {
        match self {
            Self::MethodCall(call) => &call.receiver,
            Self::PropertyGet(get) => &get.receiver,
            Self::PropertySet(set) => &set.receiver,
        }
    }

    pub fn call_span(&self) -> proc_macro2::Span {
        match self {
            Self::MethodCall(call) => call.span(),
            Self::PropertyGet(get) => get.property_name.span(),
            Self::PropertySet(set) => set.property_name.span(),
        }
    }
}

impl Parse for ObjCExpr {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        if input.peek(syn::token::Bracket) {
            return Ok(Self::MethodCall(input.parse()?));
        }

        let receiver: ObjCMacroReceiver = input.parse()?;

        if !input.peek(syn::Token![.]) {
            let err_msg = "method or property name expected";
            let span = match receiver {
                ObjCMacroReceiver::SelfValue(token) => token.span,
                ObjCMacroReceiver::Class(ident) => ident.span(),
                ObjCMacroReceiver::SelfAlloc(_, call)
                | ObjCMacroReceiver::ClassAlloc(_, call)
                | ObjCMacroReceiver::SelfClass(_, call)
                | ObjCMacroReceiver::SelfClassAlloc(_, call) => call.span(),
            };
            return Err(syn::Error::new(span, err_msg));
        }

        let dot_token: syn::Token![.] = input.parse()?;
        let property_name: syn::Ident = input.parse()?;
        if !input.peek(syn::Token![=]) {
            let get = ObjCPropertyGet {
                receiver,
                dot_token,
                property_name,
            };
            return Ok(Self::PropertyGet(get));
        }

        let eq_token: syn::Token![=] = input.parse()?;
        let val_expr: syn::Expr = input.parse()?;
        let set = ObjCPropertySet {
            receiver,
            dot_token,
            property_name,
            eq_token,
            val_expr,
        };
        Ok(Self::PropertySet(set))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_objc_expr() {
        let no_param_call: ObjCExpr = syn::parse_quote!([self myMethod]);
        assert!(matches!(
            &no_param_call,
            ObjCExpr::MethodCall(ObjCMethodCall {
                bracket_token: _,
                receiver: ObjCMacroReceiver::SelfValue(_),
                params: ObjCMethodParams::Without(method_name),
            }) if method_name == "myMethod"
        ));

        let nested_call_multiparams: ObjCExpr =
            syn::parse_quote!([[self alloc] myMethod:1+2 otherParam:3+4]);
        match &nested_call_multiparams {
            ObjCExpr::MethodCall(ObjCMethodCall {
                bracket_token: _,
                receiver: ObjCMacroReceiver::SelfAlloc(_, _),
                params: ObjCMethodParams::With(params),
            }) => match params.as_slice() {
                [param1, param2]
                    if param1.ident.as_ref().unwrap() == "myMethod"
                        && param2.ident.as_ref().unwrap() == "otherParam" => {}
                _ => panic!("unexpected expr {:?}", nested_call_multiparams),
            },
            _ => panic!("unexpected expr {:?}", nested_call_multiparams),
        }

        let no_method_name: syn::Result<ObjCExpr> = syn::parse_str("self");
        assert!(no_method_name.is_err());

        let self_class_alloc_call: ObjCExpr = syn::parse_quote!([[[self class] alloc] init]);
        match self_class_alloc_call {
            ObjCExpr::MethodCall(call) => {
                assert!(matches!(
                    call.receiver,
                    ObjCMacroReceiver::SelfClassAlloc(_, _)
                ));
                assert!(matches!(call.params, ObjCMethodParams::Without(ident) if ident == "init"));
            }
            _ => panic!("unexpected expr {:?}", self_class_alloc_call),
        }
    }
}
