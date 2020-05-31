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
    MethodCall(Box<ObjCMethodCall>),
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
            Ok(Self::MethodCall(input.parse()?))
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
}

impl Parse for ObjCExpr {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let receiver: ObjCMacroReceiver = input.parse()?;

        if !input.peek(syn::Token![.]) {
            let err_msg = "method or property name expected";
            return match receiver {
                ObjCMacroReceiver::SelfValue(token) => Err(syn::Error::new_spanned(token, err_msg)),
                ObjCMacroReceiver::Class(token) => Err(syn::Error::new_spanned(token, err_msg)),
                ObjCMacroReceiver::MethodCall(call) => Ok(Self::MethodCall(*call)),
            };
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
                receiver: ObjCMacroReceiver::MethodCall(receiver),
                params: ObjCMethodParams::With(params),
            }) => {
                assert!(matches!(receiver.as_ref(), ObjCMethodCall {
                    bracket_token: _,
                    receiver: ObjCMacroReceiver::SelfValue(_),
                    params: ObjCMethodParams::Without(method_name),
                } if method_name == "alloc"));
                match params.as_slice() {
                    [param1, param2]
                        if param1.ident.as_ref().unwrap() == "myMethod"
                            && param2.ident.as_ref().unwrap() == "otherParam" => {}
                    _ => panic!("unexpected expr {:?}", nested_call_multiparams),
                }
            }
            _ => panic!("unexpected expr {:?}", nested_call_multiparams),
        }

        let no_method_name: syn::Result<ObjCExpr> = syn::parse_str("self");
        assert!(no_method_name.is_err());
    }
}
