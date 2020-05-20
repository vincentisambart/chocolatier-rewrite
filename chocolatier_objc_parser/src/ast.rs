use crate::clang::{self, CursorKind, TypeKind};
use crate::xcode::{self, Target};
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};

// TODO:
// - real ObjC type encoding of C blocks - clang impl: ASTContext::getObjCEncodingForBlock
// - (static) variable value

#[derive(Clone, Debug, PartialEq)]
pub enum Origin {
    ObjCCore,
    System,
    Framework(String),
    Library(String),
}

impl Origin {
    fn from_path(path: &str) -> Option<Self> {
        use once_cell::sync::Lazy;
        use regex::Regex;

        static FRAMEWORK_PATH_RE: Lazy<Regex> =
            Lazy::new(|| Regex::new(r"/([^./]+)\.framework/Headers/.+.h\z").unwrap());

        if let Some(caps) = FRAMEWORK_PATH_RE.captures(path) {
            let framework = caps.get(1).unwrap().as_str().to_owned();
            return Some(Self::Framework(framework));
        }

        if path.contains("/usr/include/") {
            static LIBRARY_PATH_RE: Lazy<Regex> =
                Lazy::new(|| Regex::new(r"/usr/include/([^./]+)/.+.h\z").unwrap());
            if let Some(caps) = LIBRARY_PATH_RE.captures(path) {
                let library = caps.get(1).unwrap().as_str();
                if library == "objc" {
                    return Some(Self::ObjCCore);
                } else if library == "sys" || library == "i386" {
                    return Some(Self::System);
                } else {
                    return Some(Self::Library(library.to_owned()));
                }
            } else {
                return Some(Self::System);
            }
        }

        None
    }

    fn from_cursor(cursor: &clang::Cursor<'_>) -> Option<Origin> {
        // As we preprocess the file in advance using the system compiler, we have to use presumed_location().
        let path = cursor.location()?.presumed_location().file;
        Self::from_path(&path)
    }
}

const MAX_INDENT_LEVEL: usize = 10;
fn show_type(desc: &str, clang_ty: &clang::Type<'_>, indent_level: usize) {
    let indent = (0..indent_level)
        .map(|_| "   ")
        .collect::<Vec<&str>>()
        .concat();

    if indent_level >= MAX_INDENT_LEVEL {
        println!("{}{}: {:?}", indent, desc, clang_ty);
        return;
    }

    println!("{}{}: {:?}", indent, desc, clang_ty);

    println!(
        "{}{} const qualified: {:?}",
        indent,
        desc,
        clang_ty.is_const_qualified()
    );

    if let Some(argument_types) = clang_ty.argument_types() {
        if argument_types.len() > 0 {
            println!("{}{} arguments types:", indent, desc);
            for (i, arg_type) in argument_types.enumerate() {
                let arg_desc = format!("{} argument type {}", desc, i);
                show_type(&arg_desc, &arg_type, indent_level);
            }
        }
    }

    if let Some(base_type) = clang_ty.objc_object_base_type() {
        println!("{}{} ObjC base type types: {:?}", indent, desc, base_type);
    }

    let canonical_type = clang_ty.canonical_type();
    if &canonical_type != clang_ty {
        println!("{}{} canonical type: {:?}", indent, desc, canonical_type);
    }

    if let Some(nullability) = clang_ty.nullability() {
        println!("{}{} nullability: {:?}", indent, desc, nullability);
    }

    if let Some(class_type) = clang_ty.class_type() {
        println!("{}{} class type: {:?}", indent, desc, class_type);
    }

    let objc_protocol_decls = clang_ty.objc_protocol_decls();
    if objc_protocol_decls.len() > 0 {
        println!(
            "{}{} objc protocol declarations: {:?}",
            indent,
            desc,
            objc_protocol_decls.collect::<Vec<_>>()
        );
    }
    let objc_type_arg_types = clang_ty.objc_type_arg_types();
    if objc_type_arg_types.len() > 0 {
        println!(
            "{}{} objc type arguments: {:?}",
            indent,
            desc,
            objc_type_arg_types.collect::<Vec<_>>()
        );
    }

    if let Some(pointee_type) = clang_ty.pointee_type() {
        let pointee_desc = format!("{} pointee", desc);
        show_type(&pointee_desc, &pointee_type, indent_level);
    }

    if let Some(elaborated_type) = clang_ty.named_type() {
        let pointee_desc = format!("{} elaborated type", desc);
        show_type(&pointee_desc, &elaborated_type, indent_level);
    }

    if let Some(modified_type) = clang_ty.modified_type() {
        let modified_desc = format!("{} modified", desc);
        show_type(&modified_desc, &modified_type, indent_level);
    }

    if let Some(decl) = clang_ty.declaration() {
        println!("{}{} decl: {:?}", indent, desc, decl);
    }

    if let Some(alignment) = clang_ty.align_of() {
        println!("{}{} alignment: {:?}", indent, desc, alignment);
    }

    if let Some(size) = clang_ty.size_of() {
        println!("{}{} size: {:?}", indent, desc, size);
    }

    if let Some(template_arguments) = clang_ty.template_arguments() {
        println!(
            "{}{} template arguments: {:?}",
            indent,
            desc,
            template_arguments.collect::<Vec<_>>()
        );
    }

    if let Some(result_type) = clang_ty.result_type() {
        let result_type_desc = format!("{} result type", desc);
        show_type(&result_type_desc, &result_type, indent_level);
    }

    if let Some(fields) = clang_ty.fields() {
        println!("{}{} fields:", indent, desc);
        for field in fields {
            show_tree(&field, indent_level + 1);
        }
    }
}

fn show_tree(cursor: &clang::Cursor<'_>, indent_level: usize) {
    let indent = (0..indent_level)
        .map(|_| "   ")
        .collect::<Vec<&str>>()
        .concat();

    if indent_level >= MAX_INDENT_LEVEL {
        println!("{} {:?}", indent, cursor);
        return;
    }

    if let Some(name) = cursor.spelling() {
        println!("{}*** kind: {:?} - {} ***", indent, cursor.kind(), name);
    } else {
        println!("{}*** kind: {:?} ***", indent, cursor.kind());
    }
    if cursor.display_name() != cursor.spelling() {
        if let Some(display_name) = cursor.display_name() {
            println!("{}display name: {:?}", indent, display_name);
        }
    }

    if [
        CursorKind::ObjCInstanceMethodDecl,
        CursorKind::ObjCClassMethodDecl,
        CursorKind::ObjCPropertyDecl,
    ]
    .contains(&cursor.kind())
    {
        println!("{}objc optional: {:?}", indent, cursor.is_objc_optional());
    }

    if let Some(location) = cursor.location() {
        println!("{}location: {:?}", indent, location);
    }

    if let Some(extent) = cursor.extent() {
        println!("{}extent: {:?}", indent, extent);

        if cursor.kind() == CursorKind::UnexposedAttr {
            println!("{}tokens: {:?}", indent, extent.tokenize());
        }
    }

    if let Some(usr) = cursor.usr() {
        println!("{}usr: {}", indent, usr);
    }

    if let Some(arguments) = cursor.arguments() {
        if arguments.len() > 0 {
            println!("{}arguments ({}):", indent, arguments.len());
            for arg in arguments {
                show_tree(&arg, indent_level + 1);
            }
        }
    }

    let availability = cursor.availability();
    if availability != clang::Availability::Available {
        println!("{}availability: {:?}", indent, availability);
    }

    let canonical_cursor = cursor.canonical_cursor();
    if &canonical_cursor != cursor {
        println!("{}canonical cursor: {:?}", indent, canonical_cursor);
    }

    if let Some(definition) = cursor.definition() {
        println!("{}definition: {:?}", indent, definition);
    }

    if let Some(template) = cursor.template() {
        println!("{}template: {:?}", indent, template);
    }

    if let Some(template_kind) = cursor.template_kind() {
        println!("{}template kind: {:?}", indent, template_kind);
    }

    if let Some(clang_ty) = cursor.type_() {
        show_type("type", &clang_ty, indent_level);
    }

    if let Some(enum_underlying_type) = cursor.enum_decl_int_type() {
        show_type("enum underlying type", &enum_underlying_type, indent_level);
    }

    if let Some(typedef_underlying_type) = cursor.typedef_decl_underlying_type() {
        show_type(
            "typedef underlying type",
            &typedef_underlying_type,
            indent_level,
        );
    }

    if let Some(result_type) = cursor.result_type() {
        show_type("result type", &result_type, indent_level);
    }

    if let Some(mangling) = cursor.mangling() {
        println!("{}mangling: {:?}", indent, mangling);
    }

    if let Some(ib_outlet_collection_type) = cursor.ib_outlet_collection_type() {
        println!(
            "{}ib_outlet_collection_type: {:?}",
            indent, ib_outlet_collection_type
        );
    }

    if let Some(objc_type_encoding) = cursor.objc_type_encoding() {
        println!("{}objc type encoding: {:?}", indent, objc_type_encoding);
    }

    if let Some(objc_selector_index) = cursor.objc_selector_index() {
        println!("{}objc selector index: {:?}", indent, objc_selector_index);
    }

    if let Some(objc_attributes) = cursor.objc_attributes() {
        println!("{}objc attributes: {:?}", indent, objc_attributes);
    }

    // Seems to crash...
    // if let Some(objc_receiver_type) = cursor.get_objc_receiver_type() {
    //     println!("{}objc receiver type: {:?}", indent, objc_receiver_type);
    // }

    if let Some(platform_availability) = cursor.platform_availability() {
        println!(
            "{}platform availability: {:?}",
            indent, platform_availability
        );
    }

    if let Some(reference) = cursor.referenced() {
        println!("{}reference: {:?}", indent, reference);
    }

    if let Some(storage_class) = cursor.storage_class() {
        println!("{}storage class: {:?}", indent, storage_class);
    }

    if let Some(linkage) = cursor.linkage() {
        println!("{}linkage: {:?}", indent, linkage);
    }

    if let Some(offset_of_field) = cursor.offset_of_field() {
        println!("{}offset of field: {:?}", indent, offset_of_field);
    }

    // if let Some(semantic_parent) = cursor.semantic_parent() {
    //     println!("{}semantic parent: {:?}", indent, semantic_parent);
    // }

    // if let Some(lexical_parent) = cursor.get_lexical_parent() {
    //     println!("{}lexical parent: {:?}", indent, lexical_parent);
    // }

    let children = cursor.children();
    if !children.is_empty() {
        println!("{}children:", indent);
        for child in children {
            show_tree(&child, indent_level + 1);
        }
    }
}

pub use clang::PlatformAvailability;
pub use clang::Version;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EnumExtensib {
    Open,
    Closed,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Attr {
    FlagEnum,
    EnumExtensib(EnumExtensib),
    Unavailable,
    Deprecated,
    PlatformAvailability(Vec<PlatformAvailability>),
    Noescape,
    NSReturnsRetained,
    NSReturnsNotRetained,
    NSReturnsAutoreleased,
    NSConsumesSelf,
    NSConsumed,
    CFReturnsRetained,
    CFReturnsNotRetained,
    CFConsumed,
    SwiftName(String),
    ObjCRuntimeName(String),
    ObjCBridge { ty_name: String, is_mutable: bool },
    ObjCDesignatedInitializer,
    NSObject,
}

// Gets the next token, expecting there is one.
// Ugly but it seems to work – but only because we preprocess the code in advance.
fn next_token(token: clang::Token<'_>) -> clang::Token<'_> {
    let extent = token.extent();
    let end = extent.end();
    let range = end.range_to(end);
    range.tokenize().into_iter().next().unwrap()
}

fn extract_attr_val_token<'a>(attr_name: &str, tokens: &[clang::Token<'a>]) -> clang::Token<'a> {
    // We expect to have 4 tokens here: <attr_name> ( name )
    if tokens.len() == 1 {
        // When the attribute was put on a typedef, the attribute is also present at the definition,
        // but for some reason, at the definition, its extent only covers the first token.
        // So try to directly get the 2 tokens after the one we just got.
        let second_token = next_token(tokens[0]);
        next_token(second_token)
    } else {
        assert!(
            tokens.len() == 4,
            "unexpected tokens for {} attribute: {:?}",
            attr_name,
            tokens
        );
        tokens[2]
    }
}

fn extract_attr_ident_value(attr_name: &str, tokens: &[clang::Token<'_>]) -> String {
    let val_token = extract_attr_val_token(attr_name, tokens);
    assert!(
        val_token.kind() == clang::TokenKind::Identifier,
        "unexpected value token for {} attribute: {:?}",
        attr_name,
        val_token
    );
    val_token.spelling()
}

fn extract_attr_lit_value(attr_name: &str, tokens: &[clang::Token<'_>]) -> String {
    let val_token = extract_attr_val_token(attr_name, tokens);
    assert!(
        val_token.kind() == clang::TokenKind::Literal,
        "unexpected value token for {} attribute: {:?}",
        attr_name,
        val_token
    );
    // spelling() returns an escaped string, with '"' at the start and end.
    let mut spelling = val_token.spelling();
    if let Some(start_pos) = spelling.find(|c| c != '"') {
        spelling.drain(..start_pos);
    }
    if let Some(end_pos) = spelling.rfind(|c| c != '"') {
        spelling.drain((end_pos + 1)..);
    }
    assert!(
        !spelling.contains('\\'),
        "need to properly unescape {:?}",
        spelling
    );
    spelling
}

fn ensure_attr_no_value(attr_name: &str, tokens: &[clang::Token<'_>]) {
    assert!(
        tokens.len() == 1,
        "unexpected tokens for {} attribute: {:?}",
        attr_name,
        tokens
    );
}

impl Attr {
    fn from_decl(decl: &clang::Cursor<'_>) -> Vec<Self> {
        let mut attrs: Vec<_> = decl
            .children()
            .iter()
            .filter_map(|child| match child.kind() {
                CursorKind::NSReturnsRetained => Some(Self::NSReturnsRetained),
                CursorKind::NSReturnsNotRetained => Some(Self::NSReturnsNotRetained),
                CursorKind::NSReturnsAutoreleased => Some(Self::NSReturnsAutoreleased),
                CursorKind::NSConsumesSelf => Some(Self::NSConsumesSelf),
                CursorKind::NSConsumed => Some(Self::NSConsumed),
                CursorKind::FlagEnum => Some(Self::FlagEnum),
                CursorKind::ObjCNSObject => Some(Self::NSObject),
                CursorKind::ObjCDesignatedInitializer => Some(Self::ObjCDesignatedInitializer),
                CursorKind::UnexposedAttr => {
                    let extent = match child.extent() {
                        Some(extent) => extent,
                        None => return None,
                    };
                    let tokens = extent.tokenize();
                    let spelling = tokens[0].spelling();
                    let attr_name = spelling.trim_matches('_');
                    match attr_name {
                        "noescape" => {
                            ensure_attr_no_value(attr_name, &tokens);
                            Some(Self::Noescape)
                        }
                        "cf_returns_retained" => {
                            ensure_attr_no_value(attr_name, &tokens);
                            Some(Self::CFReturnsRetained)
                        }
                        "cf_returns_not_retained" => {
                            ensure_attr_no_value(attr_name, &tokens);
                            Some(Self::CFReturnsNotRetained)
                        }
                        "cf_consumed" => {
                            ensure_attr_no_value(attr_name, &tokens);
                            Some(Self::CFConsumed)
                        }
                        "unavailable" => {
                            assert!(
                                tokens.len() == 1 || tokens.len() == 4,
                                "unexpected tokens for \"{}\": {:?}",
                                attr_name,
                                tokens
                            );
                            // There can be a message but for the time being just ignore it.
                            Some(Self::Unavailable)
                        }
                        "swift_name" => {
                            let name = extract_attr_lit_value(attr_name, &tokens);
                            Some(Self::SwiftName(name))
                        }
                        "objc_runtime_name" => {
                            let name = extract_attr_lit_value(attr_name, &tokens);
                            Some(Self::ObjCRuntimeName(name))
                        }
                        "enum_extensibility" => {
                            let kind = extract_attr_ident_value(attr_name, &tokens);
                            match kind.as_str() {
                                "open" => Some(Self::EnumExtensib(EnumExtensib::Open)),
                                "closed" => Some(Self::EnumExtensib(EnumExtensib::Closed)),
                                _ => {
                                    panic!("unexpected tokens for \"{}\": {:?}", attr_name, tokens)
                                }
                            }
                        }
                        "objc_bridge" | "objc_bridge_mutable" => {
                            let ty_name = extract_attr_ident_value(attr_name, &tokens);
                            if attr_name == "objc_bridge_mutable" {
                                Some(Self::ObjCBridge {
                                    ty_name,
                                    is_mutable: true,
                                })
                            } else {
                                Some(Self::ObjCBridge {
                                    ty_name,
                                    is_mutable: false,
                                })
                            }
                        }
                        _ => None,
                    }
                }
                _ => None,
            })
            .collect();

        if decl.availability() == clang::Availability::Deprecated {
            attrs.push(Self::Deprecated);
        }
        if let Some(availability) = decl.platform_availability() {
            attrs.push(Self::PlatformAvailability(availability));
        }

        // ReturnsNotRetained and ReturnsAutoreleased take priority over ReturnsRetained.
        // Removing ReturnsRetained when it's not active makes processing easier afterwards.
        if attrs
            .iter()
            .any(|attr| [Attr::NSReturnsNotRetained, Attr::NSReturnsAutoreleased].contains(attr))
        {
            attrs.retain(|attr| attr != &Attr::NSReturnsRetained);
        }

        attrs
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Nullability {
    NonNull,
    Nullable,
}

impl From<clang::Nullability> for Option<Nullability> {
    fn from(nul: clang::Nullability) -> Self {
        match nul {
            clang::Nullability::NonNull => Some(Nullability::NonNull),
            clang::Nullability::Nullable => Some(Nullability::Nullable),
            clang::Nullability::Unspecified => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct IdObjPtr {
    pub protocols: Vec<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SomeInstanceObjPtr {
    pub interface: String,
    pub protocols: Vec<String>,
    pub type_args: Vec<ObjCTypeArg>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ObjPtrKind {
    Class,
    Id(IdObjPtr),
    SomeInstance(SomeInstanceObjPtr),
    Block(CallableDesc),
    TypeParam(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ObjPtr {
    pub kind: ObjPtrKind,
    // You can specify nullability when referencing a typedef for a pointer type.
    pub nullability: Option<Nullability>,
}

impl ObjPtr {
    fn with_nullability(self, nullability: Option<Nullability>) -> Self {
        Self {
            kind: self.kind,
            nullability,
        }
    }
}

fn parm_decl_children<'a>(cursor: &clang::Cursor<'a>) -> impl Iterator<Item = clang::Cursor<'a>> {
    cursor
        .children()
        .into_iter()
        .filter(|child| child.kind() == CursorKind::ParmDecl)
}

#[derive(Clone, Debug, PartialEq)]
pub struct Field {
    pub name: Option<String>,
    pub ty: AttributedType,
    pub bit_offset: usize,
}

impl Field {
    fn from_cursor(cursor: &clang::Cursor<'_>, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(cursor.kind(), CursorKind::FieldDecl);
        let name = cursor.spelling();
        let ty = AttributedType::from_clang_ty(
            &cursor.type_().unwrap(),
            &mut parm_decl_children(cursor),
            unnamed_tag_ids,
        );
        let bit_offset = cursor.offset_of_field().unwrap();
        Self {
            name,
            ty,
            bit_offset,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TagId {
    Named(String),
    Unnamed(u32),
}

impl TagId {
    fn from_cursor(decl: &clang::Cursor<'_>, unnamed_tag_ids: &TagIdMap) -> Self {
        assert!([
            CursorKind::StructDecl,
            CursorKind::UnionDecl,
            CursorKind::EnumDecl,
        ]
        .contains(&decl.kind()));

        if let Some(name) = decl.spelling() {
            Self::Named(name)
        } else {
            Self::Unnamed(
                *unnamed_tag_ids
                    .get(&decl.usr().unwrap())
                    .expect("all unnamed tag should have an id assigned"),
            )
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum TagKind {
    Union,
    Struct,
    Enum,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TagRef {
    pub id: TagId,
    pub kind: TagKind,
    pub attrs: Vec<Attr>,
}

impl TagRef {
    fn from_clang_ty(clang_ty: &clang::Type<'_>, unnamed_tag_ids: &TagIdMap) -> Self {
        let decl = clang_ty.declaration().unwrap();

        let kind = match decl.kind() {
            CursorKind::StructDecl => TagKind::Struct,
            CursorKind::UnionDecl => TagKind::Union,
            CursorKind::EnumDecl => TagKind::Enum,
            unexpected_kind => panic!(
                "Unexpected kind for tag declaration {:?}: {:?}",
                unexpected_kind, clang_ty
            ),
        };

        let id = TagId::from_cursor(&decl, unnamed_tag_ids);
        let attrs = Attr::from_decl(&decl);

        Self { id, kind, attrs }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Param {
    pub name: Option<String>,
    pub ty: AttributedType,
    pub attrs: Vec<Attr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CallableDesc {
    pub result: Box<AttributedType>,
    pub params: Option<Vec<Param>>,
    pub is_variadic: bool,
    pub attrs: Vec<Attr>,
}

impl CallableDesc {
    fn from_clang_ty<'a>(
        clang_ty: &clang::Type<'_>,
        base_parm_decls: &mut impl Iterator<Item = clang::Cursor<'a>>,
        unnamed_tag_ids: &TagIdMap,
    ) -> Self {
        if clang_ty.kind() == TypeKind::Attributed {
            let mut callable = Self::from_clang_ty(
                &clang_ty.modified_type().unwrap(),
                base_parm_decls,
                unnamed_tag_ids,
            );
            let spelling = clang_ty.spelling();
            // Yes, that is very ugly but I could not find a better way to get that attribute
            // when it's applied to a C function.
            if spelling.contains("__attribute__((ns_returns_retained))") {
                callable.attrs.push(Attr::NSReturnsRetained);
            } else {
                panic!("Have to check what this Attributed is for - {:?}", clang_ty);
            }
            return callable;
        }

        // result must be processed before parameters due to the order the entities are in base_parm_decls.
        let result = Box::new(AttributedType::from_clang_ty(
            &clang_ty.result_type().unwrap(),
            base_parm_decls,
            unnamed_tag_ids,
        ));

        let params = match clang_ty.kind() {
            TypeKind::FunctionNoProto => None,
            TypeKind::FunctionProto => Some({
                clang_ty
                    .argument_types()
                    .unwrap()
                    .map(|_| {
                        // Not using the value of argument types, only their count.
                        // The param decl taken from the cursor seems to always have better information.
                        let parm_decl = base_parm_decls.next().unwrap();
                        let ty = AttributedType::from_clang_ty(
                            &parm_decl.type_().unwrap(),
                            &mut parm_decl_children(&parm_decl),
                            unnamed_tag_ids,
                        );
                        let attrs = Attr::from_decl(&parm_decl);
                        Param {
                            name: parm_decl.spelling(),
                            ty,
                            attrs,
                        }
                    })
                    .collect()
            }),
            unexpected_kind => panic!(
                "Unexpected kind for function declaration {:?}: {:?}",
                unexpected_kind, clang_ty
            ),
        };

        Self {
            result,
            params,
            is_variadic: clang_ty.is_variadic_function(),
            attrs: Vec::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedefRef {
    pub name: String,
    pub nullability: Option<Nullability>,
}

impl TypedefRef {
    fn from_clang_ty(clang_ty: &clang::Type<'_>) -> Self {
        assert_eq!(clang_ty.kind(), TypeKind::Typedef);
        let name = clang_ty.spelling();
        let nullability = clang_ty.nullability().and_then(Into::into);
        Self { name, nullability }
    }

    fn with_nullability(self, nullability: Option<Nullability>) -> Self {
        Self {
            name: self.name,
            nullability,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Pointer {
    pub pointee: Box<AttributedType>,
    pub nullability: Option<Nullability>,
}

impl Pointer {
    fn with_nullability(self, nullability: Option<Nullability>) -> Self {
        Self {
            pointee: self.pointee,
            nullability,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Array {
    pub size: Option<usize>,
    pub element: Box<AttributedType>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum SignedOrNotInt {
    Signed(i64),
    Unsigned(u64),
}

impl SignedOrNotInt {
    pub fn is_zero(&self) -> bool {
        match self {
            SignedOrNotInt::Signed(i) => *i == 0,
            SignedOrNotInt::Unsigned(u) => *u == 0,
        }
    }
}

impl std::fmt::Display for SignedOrNotInt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SignedOrNotInt::Signed(i) => i.fmt(f),
            SignedOrNotInt::Unsigned(u) => u.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumValue {
    pub name: String,
    pub value: SignedOrNotInt,
    pub attrs: Vec<Attr>,
}

fn type_signedness(clang_ty: &clang::Type<'_>) -> Option<Signedness> {
    match clang_ty.kind() {
        TypeKind::SChar
        | TypeKind::CharS
        | TypeKind::Short
        | TypeKind::Int
        | TypeKind::Long
        | TypeKind::LongLong
        | TypeKind::Float
        | TypeKind::Double
        | TypeKind::LongDouble => Some(Signedness::Signed),
        TypeKind::UChar
        | TypeKind::CharU
        | TypeKind::UShort
        | TypeKind::UInt
        | TypeKind::ULong
        | TypeKind::ULongLong => Some(Signedness::Unsigned),
        TypeKind::Attributed => type_signedness(&clang_ty.modified_type().unwrap()),
        TypeKind::Typedef => type_signedness(
            &clang_ty
                .declaration()
                .unwrap()
                .typedef_decl_underlying_type()
                .unwrap(),
        ),
        TypeKind::Elaborated => type_signedness(&clang_ty.named_type().unwrap()),
        _ => None,
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ObjCTypeArg {
    ObjPtr(ObjPtr),
    Typedef(TypedefRef),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Signedness {
    Signed,
    Unsigned,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum NumKind {
    SChar,
    UChar,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    ULong,
    LongLong,
    ULongLong,
    Half,
    Float,
    Double,
    LongDouble,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Unsupported {
    Vector,
    Unexposed,
    Complex,
    ExtVector,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Void,
    Bool,
    Num(NumKind),
    Tag(TagRef),
    Typedef(TypedefRef),
    Pointer(Pointer),
    Function(CallableDesc),
    ObjPtr(ObjPtr),
    /// `SEL` in Objective-C. A special type of (non-object) pointer.
    ObjCSel(Option<Nullability>),
    Array(Array),
    Unsupported(Unsupported),
}

impl Type {
    fn from_clang_ty<'a>(
        clang_ty: &clang::Type<'_>,
        base_parm_decls: &mut impl Iterator<Item = clang::Cursor<'a>>,
        unnamed_tag_ids: &TagIdMap,
    ) -> Self {
        match clang_ty.kind() {
            TypeKind::Void => Self::Void,
            // SChar is "signed char", CharS is "char" when it is signed by default.
            TypeKind::SChar | TypeKind::CharS => Self::Num(NumKind::SChar),
            // UChar is "unsigned char", CharU is "char" when it is unsigned by default.
            TypeKind::UChar | TypeKind::CharU => Self::Num(NumKind::UChar),
            TypeKind::Short => Self::Num(NumKind::Short),
            TypeKind::UShort => Self::Num(NumKind::UShort),
            TypeKind::Int => Self::Num(NumKind::Int),
            TypeKind::UInt => Self::Num(NumKind::UInt),
            TypeKind::Long => Self::Num(NumKind::Long),
            TypeKind::ULong => Self::Num(NumKind::ULong),
            TypeKind::LongLong => Self::Num(NumKind::LongLong),
            TypeKind::ULongLong => Self::Num(NumKind::ULongLong),
            TypeKind::Half => Self::Num(NumKind::Half),
            TypeKind::Float => Self::Num(NumKind::Float),
            TypeKind::Double => Self::Num(NumKind::Double),
            TypeKind::LongDouble => Self::Num(NumKind::LongDouble),
            TypeKind::Pointer => {
                let pointee_type = clang_ty.pointee_type().unwrap();
                Self::Pointer(Pointer {
                    pointee: Box::new(AttributedType::from_clang_ty(
                        &pointee_type,
                        base_parm_decls,
                        unnamed_tag_ids,
                    )),
                    nullability: None,
                })
            }
            TypeKind::ObjCObjectPointer => {
                let pointee_type = {
                    let mut pointee = clang_ty.pointee_type().unwrap();
                    loop {
                        match pointee.kind() {
                            // TODO: Check what's the difference between ObjCObject and ObjCInterface
                            TypeKind::ObjCObject | TypeKind::ObjCInterface => break pointee,
                            // Attributed is for example when __kindof is used.
                            TypeKind::Attributed => {
                                pointee = pointee.modified_type().unwrap();
                            }
                            unexpected_kind => {
                                panic!("unexpected pointee kind {:?}", unexpected_kind)
                            }
                        }
                    }
                };
                let protocols = pointee_type
                    .objc_protocol_decls()
                    .map(|decl| {
                        assert_eq!(decl.kind(), CursorKind::ObjCProtocolDecl);
                        decl.spelling().unwrap()
                    })
                    .collect();

                let type_args: Vec<ObjCTypeArg> = pointee_type
                    .objc_type_arg_types()
                    .map(|arg| {
                        let attributed = AttributedType::from_clang_ty(
                            &arg,
                            &mut Vec::new().into_iter(),
                            unnamed_tag_ids,
                        );
                        match attributed.ty {
                            Self::ObjPtr(ptr) => ObjCTypeArg::ObjPtr(ptr),
                            Self::Typedef(typedef) => ObjCTypeArg::Typedef(typedef),
                            unexpected => {
                                panic!("Type arguments not expected to be {:?}", unexpected)
                            }
                        }
                    })
                    .collect();

                let base_type = pointee_type.objc_object_base_type().unwrap();

                if base_type.kind() == TypeKind::ObjCId {
                    assert!(type_args.is_empty());
                    assert_eq!(base_type.spelling(), "id");
                    Self::ObjPtr(ObjPtr {
                        kind: ObjPtrKind::Id(IdObjPtr { protocols }),
                        nullability: None,
                    })
                } else {
                    Self::ObjPtr(ObjPtr {
                        kind: ObjPtrKind::SomeInstance(SomeInstanceObjPtr {
                            interface: base_type.spelling(),
                            protocols,
                            type_args,
                        }),
                        nullability: None,
                    })
                }
            }
            TypeKind::Attributed => {
                show_type("type", clang_ty, 0);
                panic!("TypeKind::Attributed should have been handled by AttributedType");
            }
            TypeKind::ObjCTypeParam => Self::ObjPtr(ObjPtr {
                kind: ObjPtrKind::TypeParam(clang_ty.spelling()),
                nullability: None,
            }),
            TypeKind::ObjCId => Self::ObjPtr(ObjPtr {
                kind: ObjPtrKind::Id(IdObjPtr {
                    protocols: Vec::new(),
                }),
                nullability: None,
            }),
            TypeKind::ObjCSel => Self::ObjCSel(None),
            TypeKind::ObjCClass => Self::ObjPtr(ObjPtr {
                kind: ObjPtrKind::Class,
                nullability: None,
            }),
            TypeKind::Typedef => {
                let decl = clang_ty.declaration().unwrap();
                if decl.kind() == CursorKind::TemplateTypeParameter {
                    Self::ObjPtr(ObjPtr {
                        kind: ObjPtrKind::TypeParam(clang_ty.spelling()),
                        nullability: None,
                    })
                } else {
                    Self::Typedef(TypedefRef::from_clang_ty(&clang_ty))
                }
            }
            TypeKind::Elaborated => Self::from_clang_ty(
                &clang_ty.named_type().unwrap(),
                base_parm_decls,
                unnamed_tag_ids,
            ),
            TypeKind::Record | TypeKind::Enum => {
                Self::Tag(TagRef::from_clang_ty(&clang_ty, unnamed_tag_ids))
            }
            TypeKind::FunctionNoProto | TypeKind::FunctionProto => Self::Function(
                CallableDesc::from_clang_ty(&clang_ty, base_parm_decls, unnamed_tag_ids),
            ),
            TypeKind::ConstantArray => Self::Array(Array {
                size: Some(clang_ty.num_elements().unwrap()),
                element: Box::new(AttributedType::from_clang_ty(
                    &clang_ty.element_type().unwrap(),
                    base_parm_decls,
                    unnamed_tag_ids,
                )),
            }),
            TypeKind::IncompleteArray => Self::Array(Array {
                size: None,
                element: Box::new(AttributedType::from_clang_ty(
                    &clang_ty.element_type().unwrap(),
                    base_parm_decls,
                    unnamed_tag_ids,
                )),
            }),
            TypeKind::BlockPointer => Self::ObjPtr(ObjPtr {
                kind: ObjPtrKind::Block(CallableDesc::from_clang_ty(
                    &clang_ty.pointee_type().unwrap(),
                    base_parm_decls,
                    unnamed_tag_ids,
                )),
                nullability: None,
            }),
            TypeKind::Bool => Self::Bool,
            TypeKind::Vector => Self::Unsupported(Unsupported::Vector),
            TypeKind::Unexposed => Self::Unsupported(Unsupported::Unexposed),
            TypeKind::Complex => Self::Unsupported(Unsupported::Complex),
            TypeKind::ExtVector => Self::Unsupported(Unsupported::ExtVector),
            unknown_kind => panic!("Unhandled type kind {:?}: {:?}", unknown_kind, clang_ty),
        }
    }

    fn with_nullability(self, nullability: Option<Nullability>) -> Self {
        match self {
            Self::Pointer(ptr) => Self::Pointer(ptr.with_nullability(nullability)),
            Self::ObjPtr(ptr) => Self::ObjPtr(ptr.with_nullability(nullability)),
            Self::ObjCSel(_) => Self::ObjCSel(nullability),
            Self::Typedef(typedef) => Self::Typedef(typedef.with_nullability(nullability)),
            _ => self,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct AttributedType {
    pub ty: Type,
    pub is_const_qualified: bool,
    // An undeclared struct, or void, won't have any alignment or size.
    pub alignment: Option<usize>,
    pub size: Option<usize>,
}

impl AttributedType {
    fn from_clang_ty<'a>(
        clang_ty: &clang::Type<'_>,
        base_parm_decls: &mut impl Iterator<Item = clang::Cursor<'a>>,
        unnamed_tag_ids: &TagIdMap,
    ) -> Self {
        if clang_ty.kind() == TypeKind::Attributed {
            let modified = Self::from_clang_ty(
                &clang_ty.modified_type().unwrap(),
                base_parm_decls,
                unnamed_tag_ids,
            );
            let mut attributed = if let Some(nullability) = clang_ty.nullability() {
                // Note that even if Into::into returns None we probably want to override the existing one with None.
                let nullability = nullability.into();
                modified.with_nullability(nullability)
            } else {
                modified
            };
            attributed.is_const_qualified |= clang_ty.is_const_qualified();
            attributed
        } else {
            let ty = Type::from_clang_ty(clang_ty, base_parm_decls, unnamed_tag_ids);
            let is_const_qualified = clang_ty.is_const_qualified();
            let alignment = clang_ty.align_of();
            let size = clang_ty.size_of();
            Self {
                ty,
                is_const_qualified,
                alignment,
                size,
            }
        }
    }

    fn with_nullability(self, nullability: Option<Nullability>) -> Self {
        Self {
            ty: self.ty.with_nullability(nullability),
            is_const_qualified: self.is_const_qualified,
            alignment: self.alignment,
            size: self.size,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum FileKind {
    None,
    Main,
    Some(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Location {
    file_kind: FileKind,
    line: u32,
    column: u32,
}

impl Location {
    fn from_cursor(cursor: &clang::Cursor<'_>) -> Self {
        let source_location = cursor.location().unwrap();
        // let location = source_location.get_file_location();
        let location = source_location.spelling_location();

        let file_kind = if let Some(file) = location.file {
            // For some reason, source_location.is_in_main_file() doesn't seem to work properly so do it ourselves.
            let tu_file = cursor
                .tu()
                .cursor()
                .extent()
                .unwrap()
                .start()
                .file_location()
                .file
                .unwrap();
            if tu_file == file {
                FileKind::Main
            } else {
                FileKind::Some(file.file_name())
            }
        } else {
            FileKind::None
        };

        let line = location.line;
        let column = location.column;

        Self {
            file_kind,
            line,
            column,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ObjCParam {
    pub name: String,
    pub ty: AttributedType,
    pub attrs: Vec<Attr>,
}

impl ObjCParam {
    fn from_cursor(decl: &clang::Cursor<'_>, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(decl.kind(), CursorKind::ParmDecl);
        let name = decl.spelling().unwrap();
        let ty = AttributedType::from_clang_ty(
            &decl.type_().unwrap(),
            &mut parm_decl_children(decl),
            unnamed_tag_ids,
        );
        let attrs = Attr::from_decl(&decl);
        Self { name, ty, attrs }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ObjCMethodKind {
    Class,
    Instance,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ObjCMethod {
    pub name: String,
    pub kind: ObjCMethodKind,
    pub params: Vec<ObjCParam>,
    pub result: AttributedType,
    pub attrs: Vec<Attr>,
    pub type_encoding: String,
}

impl ObjCMethod {
    fn from_cursor(cursor: &clang::Cursor<'_>, unnamed_tag_ids: &TagIdMap) -> Self {
        let kind = match cursor.kind() {
            CursorKind::ObjCClassMethodDecl => ObjCMethodKind::Class,
            CursorKind::ObjCInstanceMethodDecl => ObjCMethodKind::Instance,
            _ => panic!("cursor should be either a class or instance method"),
        };

        let params = cursor
            .arguments()
            .unwrap()
            .map(|arg| ObjCParam::from_cursor(&arg, unnamed_tag_ids))
            .collect();

        let result = AttributedType::from_clang_ty(
            &cursor.result_type().unwrap(),
            &mut parm_decl_children(cursor),
            unnamed_tag_ids,
        );

        let attrs = Attr::from_decl(cursor);

        let type_encoding = cursor.objc_type_encoding().unwrap();

        Self {
            name: cursor.spelling().unwrap(),
            kind,
            params,
            result,
            attrs,
            type_encoding,
        }
    }
}

fn is_generated_from_property(method_cursor: &clang::Cursor<'_>) -> bool {
    assert!([
        CursorKind::ObjCInstanceMethodDecl,
        CursorKind::ObjCClassMethodDecl,
    ]
    .contains(&method_cursor.kind()));
    let parent = method_cursor.semantic_parent().unwrap();
    assert!([
        CursorKind::ObjCInterfaceDecl,
        CursorKind::ObjCProtocolDecl,
        CursorKind::ObjCCategoryDecl,
    ]
    .contains(&parent.kind()));
    let method_location = method_cursor.location().unwrap();
    parent
        .children()
        .into_iter()
        .filter(|sibling| sibling.kind() == CursorKind::ObjCPropertyDecl)
        .any(|sibling| sibling.location().unwrap() == method_location)
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum PropOwnership {
    Strong,
    Weak,
    Copy,
    Assign,
    Retain,
    UnsafeUnretained,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Property {
    pub name: String,
    pub value: AttributedType,
    pub is_atomic: bool,
    pub is_writable: bool,
    pub is_class: bool,
    pub ownership: PropOwnership,
    pub custom_getter_sel: Option<String>,
    pub custom_setter_sel: Option<String>,
    pub attrs: Vec<Attr>,
}

impl Property {
    fn from_cursor(cursor: &clang::Cursor<'_>, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(cursor.kind(), CursorKind::ObjCPropertyDecl);

        let name = cursor.spelling().unwrap();
        let value = AttributedType::from_clang_ty(
            &cursor.type_().unwrap(),
            &mut parm_decl_children(&cursor),
            unnamed_tag_ids,
        );

        use clang::ObjCAttributes;
        let objc_attrs = cursor
            .objc_attributes()
            .unwrap_or_else(ObjCAttributes::empty);
        let is_atomic = {
            if objc_attrs.contains(ObjCAttributes::ATOMIC) {
                assert!(!objc_attrs.contains(ObjCAttributes::NONATOMIC));
                true
            } else {
                !objc_attrs.contains(ObjCAttributes::NONATOMIC)
            }
        };
        let is_writable = objc_attrs.contains(ObjCAttributes::READWRITE);
        if is_writable {
            assert!(!objc_attrs.contains(ObjCAttributes::READONLY));
        }
        let is_class = objc_attrs.contains(ObjCAttributes::CLASS);
        let ownership = {
            if objc_attrs.contains(ObjCAttributes::STRONG) {
                assert!(!objc_attrs.contains(ObjCAttributes::WEAK));
                assert!(!objc_attrs.contains(ObjCAttributes::COPY));
                assert!(!objc_attrs.contains(ObjCAttributes::ASSIGN));
                assert!(!objc_attrs.contains(ObjCAttributes::RETAIN));
                assert!(!objc_attrs.contains(ObjCAttributes::UNSAFE_UNRETAINED));
                PropOwnership::Strong
            } else if objc_attrs.contains(ObjCAttributes::WEAK) {
                assert!(!objc_attrs.contains(ObjCAttributes::COPY));
                assert!(!objc_attrs.contains(ObjCAttributes::ASSIGN));
                assert!(!objc_attrs.contains(ObjCAttributes::RETAIN));
                assert!(!objc_attrs.contains(ObjCAttributes::UNSAFE_UNRETAINED));
                PropOwnership::Weak
            } else if objc_attrs.contains(ObjCAttributes::COPY) {
                assert!(!objc_attrs.contains(ObjCAttributes::ASSIGN));
                assert!(!objc_attrs.contains(ObjCAttributes::RETAIN));
                assert!(!objc_attrs.contains(ObjCAttributes::UNSAFE_UNRETAINED));
                PropOwnership::Copy
            } else if objc_attrs.contains(ObjCAttributes::ASSIGN) {
                assert!(!objc_attrs.contains(ObjCAttributes::RETAIN));
                assert!(!objc_attrs.contains(ObjCAttributes::UNSAFE_UNRETAINED));
                PropOwnership::Assign
            } else if objc_attrs.contains(ObjCAttributes::RETAIN) {
                assert!(!objc_attrs.contains(ObjCAttributes::UNSAFE_UNRETAINED));
                PropOwnership::Retain
            } else if objc_attrs.contains(ObjCAttributes::UNSAFE_UNRETAINED) {
                PropOwnership::UnsafeUnretained
            } else {
                PropOwnership::Strong
            }
        };
        let custom_getter_sel;
        let custom_setter_sel;
        if objc_attrs.contains(ObjCAttributes::GETTER)
            || objc_attrs.contains(ObjCAttributes::SETTER)
        {
            let parent = cursor.semantic_parent().unwrap();
            let property_location = cursor.location().unwrap();
            let methods_at_same_location: Vec<clang::Cursor<'_>> = parent
                .children()
                .into_iter()
                .filter(|sibling| {
                    [
                        CursorKind::ObjCInstanceMethodDecl,
                        CursorKind::ObjCClassMethodDecl,
                    ]
                    .contains(&sibling.kind())
                        && sibling.location().unwrap() == property_location
                })
                .collect();
            if objc_attrs.contains(ObjCAttributes::GETTER) {
                custom_getter_sel = Some(
                    methods_at_same_location
                        .iter()
                        .find(|method| method.arguments().unwrap().len() == 0)
                        .unwrap()
                        .spelling()
                        .unwrap(),
                );
            } else {
                custom_getter_sel = None;
            }
            if objc_attrs.contains(ObjCAttributes::SETTER) {
                custom_setter_sel = Some(
                    methods_at_same_location
                        .iter()
                        .find(|method| method.arguments().unwrap().len() > 0)
                        .unwrap()
                        .spelling()
                        .unwrap(),
                );
            } else {
                custom_setter_sel = None;
            }
        } else {
            custom_getter_sel = None;
            custom_setter_sel = None;
        };

        let attrs = Attr::from_decl(&cursor);

        Self {
            name,
            value,
            is_atomic,
            is_writable,
            is_class,
            ownership,
            custom_getter_sel,
            custom_setter_sel,
            attrs,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct InterfaceDef {
    pub name: String,
    pub superclass: Option<String>,
    pub adopted_protocols: Vec<String>,
    pub template_params: Vec<String>,
    pub methods: Vec<ObjCMethod>,
    pub properties: Vec<Property>,
    pub origin: Option<Origin>,
}

impl InterfaceDef {
    fn from_cursor(cursor: &clang::Cursor<'_>, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(cursor.kind(), CursorKind::ObjCInterfaceDecl);
        let name = cursor.spelling().unwrap();
        let children = cursor.children();

        let superclass = children
            .iter()
            .find(|child| child.kind() == CursorKind::ObjCSuperClassRef)
            .map(|child| child.spelling().unwrap());
        let adopted_protocols = children
            .iter()
            .filter(|child| child.kind() == CursorKind::ObjCProtocolRef)
            .map(|child| child.spelling().unwrap())
            .collect();
        let template_params = children
            .iter()
            .filter(|child| child.kind() == CursorKind::TemplateTypeParameter)
            .map(|child| child.spelling().unwrap())
            .collect();
        let methods = children
            .iter()
            .filter(|child| {
                [
                    CursorKind::ObjCInstanceMethodDecl,
                    CursorKind::ObjCClassMethodDecl,
                ]
                .contains(&child.kind())
            })
            // Methods generated from property don't have children with the type info we want
            // so for the time being just ignore them.
            .filter(|method| !is_generated_from_property(method))
            .map(|method| ObjCMethod::from_cursor(method, unnamed_tag_ids))
            .collect();
        let properties = children
            .iter()
            .filter(|child| child.kind() == CursorKind::ObjCPropertyDecl)
            .map(|prop| Property::from_cursor(prop, unnamed_tag_ids))
            .collect();
        let origin = Origin::from_cursor(cursor);

        Self {
            name,
            superclass,
            adopted_protocols,
            template_params,
            methods,
            properties,
            origin,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CategoryDef {
    pub name: Option<String>,
    pub class: String,
    pub adopted_protocols: Vec<String>,
    pub methods: Vec<ObjCMethod>,
    pub properties: Vec<Property>,
    pub origin: Option<Origin>,
}

impl CategoryDef {
    fn from_cursor(cursor: &clang::Cursor<'_>, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(cursor.kind(), CursorKind::ObjCCategoryDecl);
        let children = cursor.children();

        let name = cursor.spelling();

        let class = children
            .iter()
            .find(|child| child.kind() == CursorKind::ObjCClassRef)
            .unwrap()
            .spelling()
            .unwrap();

        let adopted_protocols = children
            .iter()
            .filter(|child| child.kind() == CursorKind::ObjCProtocolRef)
            .map(|child| child.spelling().unwrap())
            .collect();

        let methods = children
            .iter()
            .filter(|child| {
                [
                    CursorKind::ObjCInstanceMethodDecl,
                    CursorKind::ObjCClassMethodDecl,
                ]
                .contains(&child.kind())
            })
            // Methods generated from property don't have children with the type info we want
            // so for the time being just ignore them.
            .filter(|method| !is_generated_from_property(method))
            .map(|method| ObjCMethod::from_cursor(method, unnamed_tag_ids))
            .collect();

        let properties = children
            .iter()
            .filter(|child| child.kind() == CursorKind::ObjCPropertyDecl)
            .map(|prop| Property::from_cursor(prop, unnamed_tag_ids))
            .collect();

        let origin = Origin::from_cursor(cursor);

        Self {
            name,
            class,
            adopted_protocols,
            methods,
            properties,
            origin,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ProtocolMethod {
    pub method: ObjCMethod,
    pub is_optional: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ProtocolProperty {
    pub property: Property,
    pub is_optional: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ProtocolDef {
    pub name: String,
    pub inherited_protocols: Vec<String>,
    pub methods: Vec<ProtocolMethod>,
    pub properties: Vec<ProtocolProperty>,
    pub origin: Option<Origin>,
}

impl ProtocolDef {
    fn from_cursor(cursor: &clang::Cursor<'_>, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(cursor.kind(), CursorKind::ObjCProtocolDecl);
        let children = cursor.children();

        let inherited_protocols = children
            .iter()
            .filter(|child| child.kind() == CursorKind::ObjCProtocolRef)
            .map(|child| child.spelling().unwrap())
            .collect();

        let methods = children
            .iter()
            .filter(|child| {
                [
                    CursorKind::ObjCInstanceMethodDecl,
                    CursorKind::ObjCClassMethodDecl,
                ]
                .contains(&child.kind())
            })
            // Methods generated from property don't have children with the type info we want
            // so for the time being just ignore them.
            .filter(|child| !is_generated_from_property(child))
            .map(|child| ProtocolMethod {
                method: ObjCMethod::from_cursor(child, unnamed_tag_ids),
                is_optional: child.is_objc_optional(),
            })
            .collect();

        let properties = children
            .iter()
            .filter(|child| child.kind() == CursorKind::ObjCPropertyDecl)
            .map(|child| ProtocolProperty {
                property: Property::from_cursor(child, unnamed_tag_ids),
                is_optional: child.is_objc_optional(),
            })
            .collect();

        let origin = Origin::from_cursor(cursor);

        Self {
            name: cursor.spelling().unwrap(),
            inherited_protocols,
            methods,
            properties,
            origin,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedefDecl {
    pub name: String,
    pub underlying: AttributedType,
    pub origin: Option<Origin>,
}

impl TypedefDecl {
    fn from_cursor(decl: &clang::Cursor<'_>, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(decl.kind(), CursorKind::TypedefDecl);

        let name = decl.spelling().unwrap();
        let underlying = AttributedType::from_clang_ty(
            &decl.typedef_decl_underlying_type().unwrap(),
            &mut parm_decl_children(&decl),
            unnamed_tag_ids,
        );
        let origin = Origin::from_cursor(decl);

        Self {
            name,
            underlying,
            origin,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum RecordKind {
    Union,
    Struct,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RecordDef {
    pub id: TagId,
    pub kind: RecordKind,
    pub fields: Vec<Field>,
    pub origin: Option<Origin>,
}

impl RecordDef {
    fn from_cursor(decl: &clang::Cursor<'_>, unnamed_tag_ids: &TagIdMap) -> Self {
        let id = TagId::from_cursor(decl, unnamed_tag_ids);

        let kind = match decl.kind() {
            CursorKind::StructDecl => RecordKind::Struct,
            CursorKind::UnionDecl => RecordKind::Union,
            unexpected => panic!("Record declaration is not expected to be {:?}", unexpected),
        };

        assert!(
            decl.definition().is_some(),
            "An record definition should have fields definition"
        );
        let fields = decl
            .type_()
            .unwrap()
            .fields()
            .unwrap()
            .iter()
            .map(|field| Field::from_cursor(field, unnamed_tag_ids))
            .collect();

        let origin = Origin::from_cursor(decl);

        Self {
            id,
            kind,
            fields,
            origin,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FuncDecl {
    pub name: String,
    pub desc: CallableDesc,
    pub linkage: Linkage,
    pub origin: Option<Origin>,
}

impl FuncDecl {
    fn from_cursor(decl: &clang::Cursor<'_>, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(decl.kind(), CursorKind::FunctionDecl);
        let clang_ty = decl.type_().unwrap();

        let name = decl.spelling().unwrap();
        let desc =
            CallableDesc::from_clang_ty(&clang_ty, &mut parm_decl_children(&decl), unnamed_tag_ids);
        let linkage = Linkage::from_cursor(decl);
        let origin = Origin::from_cursor(decl);

        Self {
            name,
            desc,
            linkage,
            origin,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumDef {
    pub id: TagId,
    pub underlying: AttributedType,
    pub values: Vec<EnumValue>,
    pub origin: Option<Origin>,
}

impl EnumDef {
    fn from_cursor(decl: &clang::Cursor<'_>, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(decl.kind(), CursorKind::EnumDecl);

        let id = TagId::from_cursor(decl, unnamed_tag_ids);

        let underlying = AttributedType::from_clang_ty(
            &decl.enum_decl_int_type().unwrap(),
            &mut parm_decl_children(&decl),
            unnamed_tag_ids,
        );

        assert!(decl.definition().is_some());
        let signedness = type_signedness(&decl.enum_decl_int_type().unwrap())
            .expect("The underlying type of an enum should have a signedness");
        let values = decl
            .children()
            .into_iter()
            .filter(|child| child.kind() == CursorKind::EnumConstantDecl)
            .map(|decl| {
                let values = decl.enum_constant_value().unwrap();

                EnumValue {
                    name: decl.spelling().unwrap(),
                    value: match signedness {
                        Signedness::Signed => SignedOrNotInt::Signed(values.0),
                        Signedness::Unsigned => SignedOrNotInt::Unsigned(values.1),
                    },
                    attrs: Attr::from_decl(&decl),
                }
            })
            .collect();
        let origin = Origin::from_cursor(decl);

        Self {
            id,
            underlying,
            values,
            origin,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Linkage {
    Internal,
    External,
}

impl Linkage {
    fn from_cursor(decl: &clang::Cursor<'_>) -> Self {
        use clang::LinkageKind;
        let clang_linkage = decl
            .linkage()
            .expect("expecting a cursor with a linkage kind");

        match clang_linkage {
            LinkageKind::UniqueExternal => panic!("not expecting unique external linkage in ObjC"),
            LinkageKind::Internal => Self::Internal,
            LinkageKind::External => Self::External,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDecl {
    pub name: String,
    pub ty: AttributedType,
    pub linkage: Linkage,
    pub origin: Option<Origin>,
}

impl VarDecl {
    fn from_cursor(decl: &clang::Cursor<'_>, unnamed_tag_ids: &TagIdMap) -> Self {
        assert_eq!(decl.kind(), CursorKind::VarDecl);
        let name = decl.spelling().unwrap();
        let ty = AttributedType::from_clang_ty(
            &decl.type_().unwrap(),
            &mut parm_decl_children(decl),
            unnamed_tag_ids,
        );
        let linkage = Linkage::from_cursor(decl);
        let origin = Origin::from_cursor(decl);
        Self {
            name,
            ty,
            linkage,
            origin,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    ProtocolDef(ProtocolDef),
    InterfaceDef(InterfaceDef),
    CategoryDef(CategoryDef),
    RecordDef(RecordDef),
    EnumDef(EnumDef),
    TypedefDecl(TypedefDecl),
    FuncDecl(FuncDecl),
    VarDecl(VarDecl),
}

#[derive(Clone, Debug, PartialEq)]
pub struct AttributedItem {
    pub item: Item,
    pub attrs: Vec<Attr>,
}

#[derive(Debug)]
pub enum ParseError {
    ClangError(clang::ClangError),
    CompilationError(String),
}

impl From<clang::ClangError> for ParseError {
    fn from(err: clang::ClangError) -> Self {
        Self::ClangError(err)
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::ClangError(err) => write!(f, "{}", err),
            ParseError::CompilationError(s) => f.write_str(&s),
        }
    }
}

type TagIdMap = HashMap<String, u32>;

/// Prints the full clang AST.
///
/// Should only be used for debugging purpose. Definitions with cycles can end up in a stack overflow.
pub fn print_full_clang_ast(target: Target, source: &str) {
    // Preprocess before to get more easily interesting tokens coming from #defines.
    let source: &str = &xcode::preprocess_objc(source, target);

    // The documentation says that files specified as unsaved must exist so create a dummy temporary empty file
    let file = tempfile::NamedTempFile::new().unwrap();
    let index = clang::Index::new(false, true);
    let (args, options) = parser_configuration(target);

    let tu = index
        .parse(
            &args,
            file.path(),
            &[clang::UnsavedFile::new(file.path(), source)],
            options,
        )
        .expect("source should build cleanly");
    show_tree(&tu.cursor(), 0);
}

fn parser_configuration(target: Target) -> (Vec<Cow<'static, str>>, clang::TuOptions) {
    use clang::TuOptions;
    (
        xcode::clang_options(target),
        TuOptions::SKIP_FUNCTION_BODIES
            | TuOptions::INCLUDE_ATTRIBUTED_TYPES // Needed to get nullability
            | TuOptions::VISIT_IMPLICIT_ATTRIBUTES, // TODO: Check if needed
    )
}

pub fn ast_from_str(target: Target, source: &str) -> Result<Vec<AttributedItem>, ParseError> {
    // Preprocess before to get more easily interesting tokens coming from #defines.
    let source: &str = &xcode::preprocess_objc(source, target);

    // The documentation says that files specified as unsaved must exist so create a dummy temporary empty file
    let file = tempfile::NamedTempFile::new().unwrap();
    let index = clang::Index::new(false, true);
    let (args, options) = parser_configuration(target);

    let tu = index.parse(
        &args,
        file.path(),
        &[clang::UnsavedFile::new(file.path(), source)],
        options,
    )?;

    // The parser will try to parse as much as possible, even with errors.
    // In that case, we still want fail because some information will be missing anyway.
    let diagnostics = tu.diagnostics();
    let mut errors = diagnostics.filter(|diagnostic| {
        let severity = diagnostic.severity();
        [
            clang::DiagnosticSeverity::Error,
            clang::DiagnosticSeverity::Fatal,
        ]
        .contains(&severity)
    });
    if let Some(error) = errors.next() {
        return Err(ParseError::CompilationError(error.spelling()));
    }

    let mut items: Vec<AttributedItem> = Vec::new();

    let tu_cursor = tu.cursor();

    let mut next_unique_id: u32 = 1;
    let mut unnamed_tag_ids = TagIdMap::new();
    // Make unique identifiers for tags (struct/union/enum) that have no name.
    // Not using USRs as I'm not sure they are supposed to be stable between different clang versions.
    // Also not using locations as due to the the preprocessor they might not be unique.
    tu_cursor.visit_children(|cursor, _| {
        match cursor.kind() {
            CursorKind::StructDecl | CursorKind::UnionDecl | CursorKind::EnumDecl
                if cursor.spelling().is_none() => {}
            _ => return clang::ChildVisitResult::Recurse,
        }

        if cursor.definition().unwrap() != cursor {
            return clang::ChildVisitResult::Recurse;
        }

        let usr = cursor.usr().unwrap();
        if unnamed_tag_ids.contains_key(&usr) {
            return clang::ChildVisitResult::Recurse;
        }
        unnamed_tag_ids.insert(usr, next_unique_id);

        next_unique_id += 1;

        clang::ChildVisitResult::Recurse
    });
    let unnamed_tag_ids = unnamed_tag_ids;

    let mut visited: HashSet<String> = HashSet::new();
    tu_cursor.visit_children(|cursor, _| {
        let usr = cursor.usr();
        if let Some(usr) = cursor.usr() {
            if visited.contains(&usr) {
                return clang::ChildVisitResult::Recurse;
            }
        }
        let item = match cursor.kind() {
            CursorKind::ObjCInterfaceDecl => Some(Item::InterfaceDef(InterfaceDef::from_cursor(
                &cursor,
                &unnamed_tag_ids,
            ))),
            CursorKind::ObjCProtocolDecl => Some(Item::ProtocolDef(ProtocolDef::from_cursor(
                &cursor,
                &unnamed_tag_ids,
            ))),
            CursorKind::ObjCCategoryDecl => Some(Item::CategoryDef(CategoryDef::from_cursor(
                &cursor,
                &unnamed_tag_ids,
            ))),
            CursorKind::TypedefDecl => Some(Item::TypedefDecl(TypedefDecl::from_cursor(
                &cursor,
                &unnamed_tag_ids,
            ))),
            CursorKind::FunctionDecl => Some(Item::FuncDecl(FuncDecl::from_cursor(
                &cursor,
                &unnamed_tag_ids,
            ))),
            CursorKind::StructDecl | CursorKind::UnionDecl => {
                // We only care about definition of structs or unions, not their declaration.
                // (details of unnamed ones are included directly in the types that include them)
                if let Some(def) = cursor.definition() {
                    if def == cursor {
                        Some(Item::RecordDef(RecordDef::from_cursor(
                            &cursor,
                            &unnamed_tag_ids,
                        )))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            CursorKind::EnumDecl => {
                // We only care about definition of enums, not their declaration.
                // But contrarily to struct and enums, we do care about unnamed ones as they are used to declare constants.
                if let Some(def) = cursor.definition() {
                    if def == cursor {
                        Some(Item::EnumDef(EnumDef::from_cursor(
                            &cursor,
                            &unnamed_tag_ids,
                        )))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            CursorKind::VarDecl => Some(Item::VarDecl(VarDecl::from_cursor(
                &cursor,
                &unnamed_tag_ids,
            ))),
            _ => None,
        };

        if let Some(item) = item {
            let attrs = Attr::from_decl(&cursor);
            let attributed = AttributedItem { item, attrs };
            items.push(attributed);
            visited.insert(usr.unwrap());
        }

        clang::ChildVisitResult::Recurse
    });
    Ok(items)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_guess_origin() {
        assert_eq!(Origin::from_path(""), None);
        assert_eq!(
            Origin::from_path("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/System/Library/Frameworks/Foundation.framework/Headers/NSValue.h"),
            Some(Origin::Framework("Foundation".to_owned())),
        );
        assert_eq!(
            Origin::from_path("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/System/Library/Frameworks/Metal.framework/Headers/MTLCaptureManager.h"),
            Some(Origin::Framework("Metal".to_owned())),
        );
        assert_eq!(
            Origin::from_path("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/usr/include/objc/NSObject.h"),
            Some(Origin::ObjCCore),
        );
        assert_eq!(
            Origin::from_path("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/usr/include/dispatch/object.h"),
            Some(Origin::Library("dispatch".to_owned())),
        );
        assert_eq!(
            Origin::from_path("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.13.sdk/usr/include/dispatch/queue.h"),
            Some(Origin::Library("dispatch".to_owned())),
        );
        assert_eq!(
            Origin::from_path("/usr/include/i386/_types.h"),
            Some(Origin::System)
        );
        assert_eq!(
            Origin::from_path("/usr/include/_ctype.h"),
            Some(Origin::System)
        );
        assert_eq!(
            Origin::from_path("/usr/include/sys/_types/_wchar_t.h"),
            Some(Origin::System)
        );
        assert_eq!(
            Origin::from_path("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/System/Library/Frameworks/IOKit.framework/Headers/graphics/IOGraphicsTypes.h"),
            Some(Origin::Framework("IOKit".to_owned())),
        );

        let source = r#"
            # 1 "/System/Library/Frameworks/Foundation.framework/Headers/Foundation.h" 1 3
            typedef int I;
        "#;

        let expected_items = vec![AttributedItem {
            item: Item::TypedefDecl(TypedefDecl {
                name: "I".to_string(),
                underlying: AttributedType {
                    ty: Type::Num(NumKind::Int),
                    is_const_qualified: false,
                    alignment: Some(4),
                    size: Some(4),
                },
                origin: Some(Origin::Framework("Foundation".to_string())),
            }),
            attrs: vec![],
        }];

        let parsed_items = ast_from_str(Target::MacOsX86_64, source).unwrap();
        assert_eq!(parsed_items, expected_items);
    }

    #[test]
    fn test_parameter_adopting_protocols() {
        let source = "
            @protocol P1, P2;
            @class B;
            @interface A
            - (void)foo:(id<P1, P2>)x;
            + (void)bar:(B<P2>* _Nonnull)y;
            - (id<P1, P2>)foobar:(B<P2>*)z;
            @end
        ";

        let expected_items = vec![AttributedItem {
            item: Item::InterfaceDef(InterfaceDef {
                name: "A".to_string(),
                superclass: None,
                adopted_protocols: vec![],
                template_params: vec![],
                methods: vec![
                    ObjCMethod {
                        name: "foo:".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![ObjCParam {
                            name: "x".to_string(),
                            ty: AttributedType {
                                ty: Type::ObjPtr(ObjPtr {
                                    kind: ObjPtrKind::Id(IdObjPtr {
                                        protocols: vec!["P1".to_string(), "P2".to_string()],
                                    }),
                                    nullability: None,
                                }),
                                is_const_qualified: false,
                                alignment: Some(8),
                                size: Some(8),
                            },
                            attrs: vec![],
                        }],
                        result: AttributedType {
                            ty: Type::Void,
                            is_const_qualified: false,
                            alignment: None,
                            size: None,
                        },
                        attrs: vec![],
                        type_encoding: "v24@0:8@16".to_string(),
                    },
                    ObjCMethod {
                        name: "bar:".to_string(),
                        kind: ObjCMethodKind::Class,
                        params: vec![ObjCParam {
                            name: "y".to_string(),
                            ty: AttributedType {
                                ty: Type::ObjPtr(ObjPtr {
                                    kind: ObjPtrKind::SomeInstance(SomeInstanceObjPtr {
                                        interface: "B".to_string(),
                                        protocols: vec!["P2".to_string()],
                                        type_args: vec![],
                                    }),
                                    nullability: Some(Nullability::NonNull),
                                }),
                                is_const_qualified: false,
                                alignment: Some(8),
                                size: Some(8),
                            },
                            attrs: vec![],
                        }],
                        result: AttributedType {
                            ty: Type::Void,
                            is_const_qualified: false,
                            alignment: None,
                            size: None,
                        },
                        attrs: vec![],
                        type_encoding: "v24@0:8@16".to_string(),
                    },
                    ObjCMethod {
                        name: "foobar:".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![ObjCParam {
                            name: "z".to_string(),
                            ty: AttributedType {
                                ty: Type::ObjPtr(ObjPtr {
                                    kind: ObjPtrKind::SomeInstance(SomeInstanceObjPtr {
                                        interface: "B".to_string(),
                                        protocols: vec!["P2".to_string()],
                                        type_args: vec![],
                                    }),
                                    nullability: None,
                                }),
                                is_const_qualified: false,
                                alignment: Some(8),
                                size: Some(8),
                            },
                            attrs: vec![],
                        }],
                        result: AttributedType {
                            ty: Type::ObjPtr(ObjPtr {
                                kind: ObjPtrKind::Id(IdObjPtr {
                                    protocols: vec!["P1".to_string(), "P2".to_string()],
                                }),
                                nullability: None,
                            }),
                            is_const_qualified: false,
                            alignment: Some(8),
                            size: Some(8),
                        },
                        attrs: vec![],
                        type_encoding: "@24@0:8@16".to_string(),
                    },
                ],
                properties: vec![],
                origin: None,
            }),
            attrs: vec![],
        }];

        let parsed_items = ast_from_str(Target::MacOsX86_64, source).unwrap();
        assert_eq!(parsed_items, expected_items);
    }

    #[test]
    fn test_superclass() {
        let source = "
            @interface A
            @end
            @interface B: A
            @end
        ";

        let expected_items = vec![
            AttributedItem {
                item: Item::InterfaceDef(InterfaceDef {
                    name: "A".to_string(),
                    superclass: None,
                    adopted_protocols: vec![],
                    template_params: vec![],
                    methods: vec![],
                    properties: vec![],
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::InterfaceDef(InterfaceDef {
                    name: "B".to_string(),
                    superclass: Some("A".to_string()),
                    adopted_protocols: vec![],
                    template_params: vec![],
                    methods: vec![],
                    properties: vec![],
                    origin: None,
                }),
                attrs: vec![],
            },
        ];

        let parsed_items = ast_from_str(Target::MacOsX86_64, source).unwrap();
        assert_eq!(parsed_items, expected_items);
    }

    #[test]
    fn test_template_params() {
        let source = "
            @interface A
            @end
            @interface B<X, Y, Z>: A
            - (X _Nonnull)x;
            @end
        ";

        let expected_items = vec![
            AttributedItem {
                item: Item::InterfaceDef(InterfaceDef {
                    name: "A".to_string(),
                    superclass: None,
                    adopted_protocols: vec![],
                    template_params: vec![],
                    methods: vec![],
                    properties: vec![],
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::InterfaceDef(InterfaceDef {
                    name: "B".to_string(),
                    superclass: Some("A".to_string()),
                    adopted_protocols: vec![],
                    template_params: vec!["X".to_string(), "Y".to_string(), "Z".to_string()],
                    methods: vec![ObjCMethod {
                        name: "x".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: AttributedType {
                            ty: Type::ObjPtr(ObjPtr {
                                kind: ObjPtrKind::TypeParam("X".to_string()),
                                nullability: Some(Nullability::NonNull),
                            }),
                            is_const_qualified: false,
                            alignment: Some(8),
                            size: Some(8),
                        },
                        attrs: vec![],
                        type_encoding: "@16@0:8".to_string(),
                    }],
                    properties: vec![],
                    origin: None,
                }),
                attrs: vec![],
            },
        ];

        let parsed_items = ast_from_str(Target::MacOsX86_64, source).unwrap();
        assert_eq!(parsed_items, expected_items);
    }

    #[test]
    fn test_protocol() {
        let source = "
            @protocol B
            @end
            @protocol C, D;
            @protocol A
            - (void)x;
            @optional
            + (void)y;
            - (int)z;
            @end
        ";

        let expected_items = vec![
            AttributedItem {
                item: Item::ProtocolDef(ProtocolDef {
                    name: "B".to_string(),
                    inherited_protocols: vec![],
                    methods: vec![],
                    properties: vec![],
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::ProtocolDef(ProtocolDef {
                    name: "A".to_string(),
                    inherited_protocols: vec![],
                    methods: vec![
                        ProtocolMethod {
                            method: ObjCMethod {
                                name: "x".to_string(),
                                kind: ObjCMethodKind::Instance,
                                params: vec![],
                                result: AttributedType {
                                    ty: Type::Void,
                                    is_const_qualified: false,
                                    alignment: None,
                                    size: None,
                                },
                                attrs: vec![],
                                type_encoding: "v16@0:8".to_string(),
                            },
                            is_optional: false,
                        },
                        ProtocolMethod {
                            method: ObjCMethod {
                                name: "y".to_string(),
                                kind: ObjCMethodKind::Class,
                                params: vec![],
                                result: AttributedType {
                                    ty: Type::Void,
                                    is_const_qualified: false,
                                    alignment: None,
                                    size: None,
                                },
                                attrs: vec![],
                                type_encoding: "v16@0:8".to_string(),
                            },
                            is_optional: true,
                        },
                        ProtocolMethod {
                            method: ObjCMethod {
                                name: "z".to_string(),
                                kind: ObjCMethodKind::Instance,
                                params: vec![],
                                result: AttributedType {
                                    ty: Type::Num(NumKind::Int),
                                    is_const_qualified: false,
                                    alignment: Some(4),
                                    size: Some(4),
                                },
                                attrs: vec![],
                                type_encoding: "i16@0:8".to_string(),
                            },
                            is_optional: true,
                        },
                    ],
                    properties: vec![],
                    origin: None,
                }),
                attrs: vec![],
            },
        ];

        let parsed_items = ast_from_str(Target::MacOsX86_64, source).unwrap();
        assert_eq!(parsed_items, expected_items);
    }

    #[test]
    fn test_category() {
        let source = "
            @protocol P;
            @interface A
            @end
            @interface A (Categ) <P>
            - (void)foo;
            @end
            @interface A ()
            - (void)firstUnnamedCategoryMethod;
            @end
            @interface A ()
            - (void)secondUnnamedCategoryMethod;
            @property int propertyOnUnnamedCategory;
            @end
        ";

        let expected_items = vec![
            AttributedItem {
                item: Item::InterfaceDef(InterfaceDef {
                    name: "A".to_string(),
                    superclass: None,
                    adopted_protocols: vec![],
                    template_params: vec![],
                    methods: vec![],
                    properties: vec![],
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::CategoryDef(CategoryDef {
                    name: Some("Categ".to_string()),
                    class: "A".to_string(),
                    adopted_protocols: vec!["P".to_string()],
                    methods: vec![ObjCMethod {
                        name: "foo".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: AttributedType {
                            ty: Type::Void,
                            is_const_qualified: false,
                            alignment: None,
                            size: None,
                        },
                        attrs: vec![],
                        type_encoding: "v16@0:8".to_string(),
                    }],
                    properties: vec![],
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::CategoryDef(CategoryDef {
                    name: None,
                    class: "A".to_string(),
                    adopted_protocols: vec![],
                    methods: vec![ObjCMethod {
                        name: "firstUnnamedCategoryMethod".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: AttributedType {
                            ty: Type::Void,
                            is_const_qualified: false,
                            alignment: None,
                            size: None,
                        },
                        attrs: vec![],
                        type_encoding: "v16@0:8".to_string(),
                    }],
                    properties: vec![],
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::CategoryDef(CategoryDef {
                    name: None,
                    class: "A".to_string(),
                    adopted_protocols: vec![],
                    methods: vec![ObjCMethod {
                        name: "secondUnnamedCategoryMethod".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: AttributedType {
                            ty: Type::Void,
                            is_const_qualified: false,
                            alignment: None,
                            size: None,
                        },
                        attrs: vec![],
                        type_encoding: "v16@0:8".to_string(),
                    }],
                    properties: vec![Property {
                        name: "propertyOnUnnamedCategory".to_string(),
                        value: AttributedType {
                            ty: Type::Num(NumKind::Int),
                            is_const_qualified: false,
                            alignment: Some(4),
                            size: Some(4),
                        },
                        is_atomic: true,
                        is_writable: false,
                        is_class: false,
                        ownership: PropOwnership::Strong,
                        custom_getter_sel: None,
                        custom_setter_sel: None,
                        attrs: vec![],
                    }],
                    origin: None,
                }),
                attrs: vec![],
            },
        ];

        let parsed_items = ast_from_str(Target::MacOsX86_64, source).unwrap();
        assert_eq!(parsed_items, expected_items);
    }

    #[test]
    fn test_id() {
        let source = "
            @protocol P;
            @interface A
            - (id)x;
            - (id<P>)y;
            - (id<P> _Nonnull)z;
            @end
        ";

        let expected_items = vec![AttributedItem {
            item: Item::InterfaceDef(InterfaceDef {
                name: "A".to_string(),
                superclass: None,
                adopted_protocols: vec![],
                template_params: vec![],
                methods: vec![
                    ObjCMethod {
                        name: "x".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: AttributedType {
                            ty: Type::ObjPtr(ObjPtr {
                                kind: ObjPtrKind::Id(IdObjPtr { protocols: vec![] }),
                                nullability: None,
                            }),
                            is_const_qualified: false,
                            alignment: Some(8),
                            size: Some(8),
                        },
                        attrs: vec![],
                        type_encoding: "@16@0:8".to_string(),
                    },
                    ObjCMethod {
                        name: "y".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: AttributedType {
                            ty: Type::ObjPtr(ObjPtr {
                                kind: ObjPtrKind::Id(IdObjPtr {
                                    protocols: vec!["P".to_string()],
                                }),
                                nullability: None,
                            }),
                            is_const_qualified: false,
                            alignment: Some(8),
                            size: Some(8),
                        },
                        attrs: vec![],
                        type_encoding: "@16@0:8".to_string(),
                    },
                    ObjCMethod {
                        name: "z".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: AttributedType {
                            ty: Type::ObjPtr(ObjPtr {
                                kind: ObjPtrKind::Id(IdObjPtr {
                                    protocols: vec!["P".to_string()],
                                }),
                                nullability: Some(Nullability::NonNull),
                            }),
                            is_const_qualified: false,
                            alignment: Some(8),
                            size: Some(8),
                        },
                        attrs: vec![],
                        type_encoding: "@16@0:8".to_string(),
                    },
                ],
                properties: vec![],
                origin: None,
            }),
            attrs: vec![],
        }];

        let parsed_items = ast_from_str(Target::MacOsX86_64, source).unwrap();
        assert_eq!(parsed_items, expected_items);
    }

    #[test]
    fn test_typedef() {
        let source = "
            typedef int I;
            @interface A
            - (I)foo;
            @end
        ";

        let expected_items = vec![
            AttributedItem {
                item: Item::TypedefDecl(TypedefDecl {
                    name: "I".to_string(),
                    underlying: AttributedType {
                        ty: Type::Num(NumKind::Int),
                        is_const_qualified: false,
                        alignment: Some(4),
                        size: Some(4),
                    },
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::InterfaceDef(InterfaceDef {
                    name: "A".to_string(),
                    superclass: None,
                    adopted_protocols: vec![],
                    template_params: vec![],
                    methods: vec![ObjCMethod {
                        name: "foo".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: AttributedType {
                            ty: Type::Typedef(TypedefRef {
                                name: "I".to_string(),
                                nullability: None,
                            }),
                            is_const_qualified: false,
                            alignment: Some(4),
                            size: Some(4),
                        },
                        attrs: vec![],
                        type_encoding: "i16@0:8".to_string(),
                    }],
                    properties: vec![],
                    origin: None,
                }),
                attrs: vec![],
            },
        ];

        let parsed_items = ast_from_str(Target::MacOsX86_64, source).unwrap();
        assert_eq!(parsed_items, expected_items);
    }

    #[test]
    fn test_instancetype() {
        let source = "
            @interface A
            - (_Nonnull instancetype)foo;
            @end
        ";

        let expected_items = vec![AttributedItem {
            item: Item::InterfaceDef(InterfaceDef {
                name: "A".to_string(),
                superclass: None,
                adopted_protocols: vec![],
                template_params: vec![],
                methods: vec![ObjCMethod {
                    name: "foo".to_string(),
                    kind: ObjCMethodKind::Instance,
                    params: vec![],
                    result: AttributedType {
                        ty: Type::Typedef(TypedefRef {
                            name: "instancetype".to_string(),
                            nullability: Some(Nullability::NonNull),
                        }),
                        is_const_qualified: false,
                        alignment: Some(8),
                        size: Some(8),
                    },
                    attrs: vec![],
                    type_encoding: "@16@0:8".to_string(),
                }],
                properties: vec![],
                origin: None,
            }),
            attrs: vec![],
        }];

        let parsed_items = ast_from_str(Target::MacOsX86_64, source).unwrap();
        assert_eq!(parsed_items, expected_items);
    }

    #[test]
    fn test_struct() {
        let source = "
            typedef struct S { int x; } T;
            @interface A
            - (struct S)standardStruct;
            - (struct { float f; union { int i; double d; }; })inlineUnnamedStruct;
            - (T *)pointerToStructTypedef;
            - (struct Undeclared *)pointerToUndeclaredStruct;
            - (struct DeclaredAfterwards *)pointerToStructDeclaredAfterwards;
            @end
            struct DeclaredAfterwards { char c; };
        ";

        let expected_items = vec![
            AttributedItem {
                item: Item::RecordDef(RecordDef {
                    id: TagId::Named("S".to_string()),
                    fields: vec![Field {
                        name: Some("x".to_string()),
                        ty: AttributedType {
                            ty: Type::Num(NumKind::Int),
                            is_const_qualified: false,
                            alignment: Some(4),
                            size: Some(4),
                        },
                        bit_offset: 0,
                    }],
                    kind: RecordKind::Struct,
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::TypedefDecl(TypedefDecl {
                    name: "T".to_string(),
                    underlying: AttributedType {
                        ty: Type::Tag(TagRef {
                            id: TagId::Named("S".to_string()),
                            kind: TagKind::Struct,
                            attrs: vec![],
                        }),
                        is_const_qualified: false,
                        alignment: Some(4),
                        size: Some(4),
                    },
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::InterfaceDef(InterfaceDef {
                    name: "A".to_string(),
                    superclass: None,
                    adopted_protocols: vec![],
                    template_params: vec![],
                    methods: vec![
                        ObjCMethod {
                            name: "standardStruct".to_string(),
                            kind: ObjCMethodKind::Instance,
                            params: vec![],
                            result: AttributedType {
                                ty: Type::Tag(TagRef {
                                    id: TagId::Named("S".to_string()),
                                    kind: TagKind::Struct,
                                    attrs: vec![],
                                }),
                                is_const_qualified: false,
                                alignment: Some(4),
                                size: Some(4),
                            },
                            attrs: vec![],
                            type_encoding: "{S=i}16@0:8".to_string(),
                        },
                        ObjCMethod {
                            name: "inlineUnnamedStruct".to_string(),
                            kind: ObjCMethodKind::Instance,
                            params: vec![],
                            result: AttributedType {
                                ty: Type::Tag(TagRef {
                                    id: TagId::Unnamed(1),
                                    kind: TagKind::Struct,
                                    attrs: vec![],
                                }),
                                is_const_qualified: false,
                                alignment: Some(8),
                                size: Some(16),
                            },
                            attrs: vec![],
                            type_encoding: "{?=f(?=id)}16@0:8".to_string(),
                        },
                        ObjCMethod {
                            name: "pointerToStructTypedef".to_string(),
                            kind: ObjCMethodKind::Instance,
                            params: vec![],
                            result: AttributedType {
                                ty: Type::Pointer(Pointer {
                                    pointee: Box::new(AttributedType {
                                        ty: Type::Typedef(TypedefRef {
                                            name: "T".to_string(),
                                            nullability: None,
                                        }),
                                        is_const_qualified: false,
                                        alignment: Some(4),
                                        size: Some(4),
                                    }),
                                    nullability: None,
                                }),
                                is_const_qualified: false,
                                alignment: Some(8),
                                size: Some(8),
                            },
                            attrs: vec![],
                            type_encoding: "^{S=i}16@0:8".to_string(),
                        },
                        ObjCMethod {
                            name: "pointerToUndeclaredStruct".to_string(),
                            kind: ObjCMethodKind::Instance,
                            params: vec![],
                            result: AttributedType {
                                ty: Type::Pointer(Pointer {
                                    pointee: Box::new(AttributedType {
                                        ty: Type::Tag(TagRef {
                                            id: TagId::Named("Undeclared".to_string()),
                                            kind: TagKind::Struct,
                                            attrs: vec![],
                                        }),
                                        is_const_qualified: false,
                                        alignment: None,
                                        size: None,
                                    }),
                                    nullability: None,
                                }),
                                is_const_qualified: false,
                                alignment: Some(8),
                                size: Some(8),
                            },
                            attrs: vec![],
                            type_encoding: "^{Undeclared=}16@0:8".to_string(),
                        },
                        ObjCMethod {
                            name: "pointerToStructDeclaredAfterwards".to_string(),
                            kind: ObjCMethodKind::Instance,
                            params: vec![],
                            result: AttributedType {
                                ty: Type::Pointer(Pointer {
                                    pointee: Box::new(AttributedType {
                                        ty: Type::Tag(TagRef {
                                            id: TagId::Named("DeclaredAfterwards".to_string()),
                                            kind: TagKind::Struct,
                                            attrs: vec![],
                                        }),
                                        is_const_qualified: false,
                                        alignment: Some(1),
                                        size: Some(1),
                                    }),
                                    nullability: None,
                                }),
                                is_const_qualified: false,
                                alignment: Some(8),
                                size: Some(8),
                            },
                            attrs: vec![],
                            type_encoding: "^{DeclaredAfterwards=c}16@0:8".to_string(),
                        },
                    ],
                    properties: vec![],
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::RecordDef(RecordDef {
                    id: TagId::Unnamed(1),
                    kind: RecordKind::Struct,
                    fields: vec![
                        Field {
                            name: Some("f".to_string()),
                            ty: AttributedType {
                                ty: Type::Num(NumKind::Float),
                                is_const_qualified: false,
                                alignment: Some(4),
                                size: Some(4),
                            },
                            bit_offset: 0,
                        },
                        Field {
                            name: None,
                            ty: AttributedType {
                                ty: Type::Tag(TagRef {
                                    id: TagId::Unnamed(2),
                                    kind: TagKind::Union,
                                    attrs: vec![],
                                }),
                                is_const_qualified: false,
                                alignment: Some(8),
                                size: Some(8),
                            },
                            bit_offset: 64,
                        },
                    ],
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::RecordDef(RecordDef {
                    id: TagId::Unnamed(2),
                    kind: RecordKind::Union,
                    fields: vec![
                        Field {
                            name: Some("i".to_string()),
                            ty: AttributedType {
                                ty: Type::Num(NumKind::Int),
                                is_const_qualified: false,
                                alignment: Some(4),
                                size: Some(4),
                            },
                            bit_offset: 0,
                        },
                        Field {
                            name: Some("d".to_string()),
                            ty: AttributedType {
                                ty: Type::Num(NumKind::Double),
                                is_const_qualified: false,
                                alignment: Some(8),
                                size: Some(8),
                            },
                            bit_offset: 0,
                        },
                    ],
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::RecordDef(RecordDef {
                    id: TagId::Named("DeclaredAfterwards".to_string()),
                    fields: vec![Field {
                        name: Some("c".to_string()),
                        ty: AttributedType {
                            ty: Type::Num(NumKind::SChar),
                            is_const_qualified: false,
                            alignment: Some(1),
                            size: Some(1),
                        },
                        bit_offset: 0,
                    }],
                    kind: RecordKind::Struct,
                    origin: None,
                }),
                attrs: vec![],
            },
        ];

        let parsed_items = ast_from_str(Target::MacOsX86_64, source).unwrap();
        assert_eq!(parsed_items, expected_items);
    }

    #[test]
    fn test_function_pointers() {
        let source = "
            typedef void (*T)(int typedefParam);
            @interface A
            - (int(*)())returningFunctionPointerWithUndefinedParameters;
            - (int(*)(float, ...))returningFunctionPointerVariadic;
            - (int(*)(void))returningFunctionPointerWithNoParameters;
            - (T)returningFunctionPointerTypedef;
            - (char (*(*)(double innerParam))(float outerParam))returningFunctionPointerReturningFunctionPointer;
            - (A *(*)(short returnedFunctionParameter))takingTypedef:(T)typedefParam andFunctionPointersTakingFunctionPointers:(A *(*)(float someFloat, int (*functionPointerParam)(char someChar)))complicatedParam;
            @end
        ";

        // Moved it out of expected_items because in its original place,
        // rustfmt will refuse formatting due to lines too long.
        let complicated_param = ObjCParam {
            name: "complicatedParam".to_string(),
            ty: AttributedType {
                ty: Type::Pointer(Pointer {
                    pointee: Box::new(AttributedType {
                        ty: Type::Function(CallableDesc {
                            result: Box::new(AttributedType {
                                ty: Type::ObjPtr(ObjPtr {
                                    kind: ObjPtrKind::SomeInstance(SomeInstanceObjPtr {
                                        interface: "A".to_string(),
                                        protocols: vec![],
                                        type_args: vec![],
                                    }),
                                    nullability: None,
                                }),
                                is_const_qualified: false,
                                alignment: Some(8),
                                size: Some(8),
                            }),
                            params: Some(vec![
                                Param {
                                    name: Some("someFloat".to_string()),
                                    ty: AttributedType {
                                        ty: Type::Num(NumKind::Float),
                                        is_const_qualified: false,
                                        alignment: Some(4),
                                        size: Some(4),
                                    },
                                    attrs: vec![],
                                },
                                Param {
                                    name: Some("functionPointerParam".to_string()),
                                    ty: AttributedType {
                                        ty: Type::Pointer(Pointer {
                                            pointee: Box::new(AttributedType {
                                                ty: Type::Function(CallableDesc {
                                                    result: Box::new(AttributedType {
                                                        ty: Type::Num(NumKind::Int),
                                                        is_const_qualified: false,
                                                        alignment: Some(4),
                                                        size: Some(4),
                                                    }),
                                                    params: Some(vec![Param {
                                                        name: Some("someChar".to_string()),
                                                        ty: AttributedType {
                                                            ty: Type::Num(NumKind::SChar),
                                                            is_const_qualified: false,
                                                            alignment: Some(1),
                                                            size: Some(1),
                                                        },
                                                        attrs: vec![],
                                                    }]),
                                                    is_variadic: false,
                                                    attrs: vec![],
                                                }),
                                                is_const_qualified: false,
                                                alignment: Some(4),
                                                size: Some(1),
                                            }),
                                            nullability: None,
                                        }),
                                        is_const_qualified: false,
                                        alignment: Some(8),
                                        size: Some(8),
                                    },
                                    attrs: vec![],
                                },
                            ]),
                            is_variadic: false,
                            attrs: vec![],
                        }),
                        is_const_qualified: false,
                        alignment: Some(4),
                        size: Some(1),
                    }),
                    nullability: None,
                }),
                is_const_qualified: false,
                alignment: Some(8),
                size: Some(8),
            },
            attrs: vec![],
        };

        let expected_items = vec![
            AttributedItem {
                item: Item::TypedefDecl(TypedefDecl {
                    name: "T".to_string(),
                    underlying: AttributedType {
                        ty: Type::Pointer(Pointer {
                            pointee: Box::new(AttributedType {
                                ty: Type::Function(CallableDesc {
                                    result: Box::new(AttributedType {
                                        ty: Type::Void,
                                        is_const_qualified: false,
                                        alignment: None,
                                        size: None,
                                    }),
                                    params: Some(vec![Param {
                                        name: Some("typedefParam".to_string()),
                                        ty: AttributedType {
                                            ty: Type::Num(NumKind::Int),
                                            is_const_qualified: false,
                                            alignment: Some(4),
                                            size: Some(4),
                                        },
                                        attrs: vec![],
                                    }]),
                                    is_variadic: false,
                                    attrs: vec![],
                                }),
                                is_const_qualified: false,
                                alignment: Some(4),
                                size: Some(1),
                            }),
                            nullability: None,
                        }),
                        is_const_qualified: false,
                        alignment: Some(8),
                        size: Some(8),
                    },
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::InterfaceDef(InterfaceDef {
                    name: "A".to_string(),
                    superclass: None,
                    adopted_protocols: vec![],
                    template_params: vec![],
                    methods: vec![
                        // - (int(*)())returningFunctionPointerWithUndefinedParameters;
                        ObjCMethod {
                            name: "returningFunctionPointerWithUndefinedParameters".to_string(),
                            kind: ObjCMethodKind::Instance,
                            params: vec![],
                            result: AttributedType {
                                ty: Type::Pointer(Pointer {
                                    pointee: Box::new(AttributedType {
                                        ty: Type::Function(CallableDesc {
                                            result: Box::new(AttributedType {
                                                ty: Type::Num(NumKind::Int),
                                                is_const_qualified: false,
                                                alignment: Some(4),
                                                size: Some(4),
                                            }),
                                            params: None,
                                            is_variadic: true,
                                            attrs: vec![],
                                        }),
                                        is_const_qualified: false,
                                        alignment: Some(4),
                                        size: Some(1),
                                    }),
                                    nullability: None,
                                }),
                                is_const_qualified: false,
                                alignment: Some(8),
                                size: Some(8),
                            },
                            attrs: vec![],
                            type_encoding: "^?16@0:8".to_string(),
                        },
                        // - (int(*)(float, ...))returningFunctionPointerVariadic;
                        ObjCMethod {
                            name: "returningFunctionPointerVariadic".to_string(),
                            kind: ObjCMethodKind::Instance,
                            params: vec![],
                            result: AttributedType {
                                ty: Type::Pointer(Pointer {
                                    pointee: Box::new(AttributedType {
                                        ty: Type::Function(CallableDesc {
                                            result: Box::new(AttributedType {
                                                ty: Type::Num(NumKind::Int),
                                                is_const_qualified: false,
                                                alignment: Some(4),
                                                size: Some(4),
                                            }),
                                            params: Some(vec![Param {
                                                name: None,
                                                ty: AttributedType {
                                                    ty: Type::Num(NumKind::Float),
                                                    is_const_qualified: false,
                                                    alignment: Some(4),
                                                    size: Some(4),
                                                },
                                                attrs: vec![],
                                            }]),
                                            is_variadic: true,
                                            attrs: vec![],
                                        }),
                                        is_const_qualified: false,
                                        alignment: Some(4),
                                        size: Some(1),
                                    }),
                                    nullability: None,
                                }),
                                is_const_qualified: false,
                                alignment: Some(8),
                                size: Some(8),
                            },
                            attrs: vec![],
                            type_encoding: "^?16@0:8".to_string(),
                        },
                        // - (int(*)(void))returningFunctionPointerWithNoParameters;
                        ObjCMethod {
                            name: "returningFunctionPointerWithNoParameters".to_string(),
                            kind: ObjCMethodKind::Instance,
                            params: vec![],
                            result: AttributedType {
                                ty: Type::Pointer(Pointer {
                                    pointee: Box::new(AttributedType {
                                        ty: Type::Function(CallableDesc {
                                            result: Box::new(AttributedType {
                                                ty: Type::Num(NumKind::Int),
                                                is_const_qualified: false,
                                                alignment: Some(4),
                                                size: Some(4),
                                            }),
                                            params: Some(vec![]),
                                            is_variadic: false,
                                            attrs: vec![],
                                        }),
                                        is_const_qualified: false,
                                        alignment: Some(4),
                                        size: Some(1),
                                    }),
                                    nullability: None,
                                }),
                                is_const_qualified: false,
                                alignment: Some(8),
                                size: Some(8),
                            },
                            attrs: vec![],
                            type_encoding: "^?16@0:8".to_string(),
                        },
                        // - (T)returningFunctionPointerTypedef;
                        ObjCMethod {
                            name: "returningFunctionPointerTypedef".to_string(),
                            kind: ObjCMethodKind::Instance,
                            params: vec![],
                            result: AttributedType {
                                ty: Type::Typedef(TypedefRef {
                                    name: "T".to_string(),
                                    nullability: None,
                                }),
                                is_const_qualified: false,
                                alignment: Some(8),
                                size: Some(8),
                            },
                            attrs: vec![],
                            type_encoding: "^?16@0:8".to_string(),
                        },
                        // - (char (*(*)(double innerParam))(float outerParam))returningFunctionPointerReturningFunctionPointer;
                        ObjCMethod {
                            name: "returningFunctionPointerReturningFunctionPointer".to_string(),
                            kind: ObjCMethodKind::Instance,
                            params: vec![],
                            result: AttributedType {
                                ty: Type::Pointer(Pointer {
                                    pointee: Box::new(AttributedType {
                                        ty: Type::Function(CallableDesc {
                                            result: Box::new(AttributedType {
                                                ty: Type::Pointer(Pointer {
                                                    pointee: Box::new(AttributedType {
                                                        ty: Type::Function(CallableDesc {
                                                            result: Box::new(AttributedType {
                                                                ty: Type::Num(NumKind::SChar),
                                                                is_const_qualified: false,
                                                                alignment: Some(1),
                                                                size: Some(1),
                                                            }),
                                                            params: Some(vec![Param {
                                                                name: Some(
                                                                    "outerParam".to_string(),
                                                                ),
                                                                ty: AttributedType {
                                                                    ty: Type::Num(NumKind::Float),
                                                                    is_const_qualified: false,
                                                                    alignment: Some(4),
                                                                    size: Some(4),
                                                                },
                                                                attrs: vec![],
                                                            }]),
                                                            is_variadic: false,
                                                            attrs: vec![],
                                                        }),
                                                        is_const_qualified: false,
                                                        alignment: Some(4),
                                                        size: Some(1),
                                                    }),
                                                    nullability: None,
                                                }),
                                                is_const_qualified: false,
                                                alignment: Some(8),
                                                size: Some(8),
                                            }),
                                            params: Some(vec![Param {
                                                name: Some("innerParam".to_string()),
                                                ty: AttributedType {
                                                    ty: Type::Num(NumKind::Double),
                                                    is_const_qualified: false,
                                                    alignment: Some(8),
                                                    size: Some(8),
                                                },
                                                attrs: vec![],
                                            }]),
                                            is_variadic: false,
                                            attrs: vec![],
                                        }),
                                        is_const_qualified: false,
                                        alignment: Some(4),
                                        size: Some(1),
                                    }),
                                    nullability: None,
                                }),
                                is_const_qualified: false,
                                alignment: Some(8),
                                size: Some(8),
                            },
                            attrs: vec![],
                            type_encoding: "^?16@0:8".to_string(),
                        },
                        // - (A *(*)(short returnedFunctionParameter))takingTypedef:(T)typedefParam andFunctionPointersTakingFunctionPointers:(A *(*)(float someFloat, int (*functionPointerParam)(char someChar)))complicatedParam;
                        ObjCMethod {
                            name: "takingTypedef:andFunctionPointersTakingFunctionPointers:"
                                .to_string(),
                            kind: ObjCMethodKind::Instance,
                            params: vec![
                                ObjCParam {
                                    name: "typedefParam".to_string(),
                                    ty: AttributedType {
                                        ty: Type::Typedef(TypedefRef {
                                            name: "T".to_string(),
                                            nullability: None,
                                        }),
                                        is_const_qualified: false,
                                        alignment: Some(8),
                                        size: Some(8),
                                    },
                                    attrs: vec![],
                                },
                                complicated_param,
                            ],
                            result: AttributedType {
                                ty: Type::Pointer(Pointer {
                                    pointee: Box::new(AttributedType {
                                        ty: Type::Function(CallableDesc {
                                            result: Box::new(AttributedType {
                                                ty: Type::ObjPtr(ObjPtr {
                                                    kind: ObjPtrKind::SomeInstance(
                                                        SomeInstanceObjPtr {
                                                            interface: "A".to_string(),
                                                            protocols: vec![],
                                                            type_args: vec![],
                                                        },
                                                    ),
                                                    nullability: None,
                                                }),
                                                is_const_qualified: false,
                                                alignment: Some(8),
                                                size: Some(8),
                                            }),
                                            params: Some(vec![Param {
                                                name: Some("returnedFunctionParameter".to_string()),
                                                ty: AttributedType {
                                                    ty: Type::Num(NumKind::Short),
                                                    is_const_qualified: false,
                                                    alignment: Some(2),
                                                    size: Some(2),
                                                },
                                                attrs: vec![],
                                            }]),
                                            is_variadic: false,
                                            attrs: vec![],
                                        }),
                                        is_const_qualified: false,
                                        alignment: Some(4),
                                        size: Some(1),
                                    }),
                                    nullability: None,
                                }),
                                is_const_qualified: false,
                                alignment: Some(8),
                                size: Some(8),
                            },
                            attrs: vec![],
                            type_encoding: "^?32@0:8^?16^?24".to_string(),
                        },
                    ],
                    properties: vec![],
                    origin: None,
                }),
                attrs: vec![],
            },
        ];

        let parsed_items = ast_from_str(Target::MacOsX86_64, source).unwrap();
        assert_eq!(parsed_items, expected_items);
    }

    #[test]
    fn test_method_for_selector() {
        // Taken from system headers
        let source = "
            typedef struct objc_class *Class;
            struct objc_object {
                Class _Nonnull isa __attribute__((deprecated));
            };
            typedef struct objc_object *id;
            typedef struct objc_selector *SEL;
            typedef id _Nullable (*IMP)(id _Nonnull, SEL _Nonnull, ...);

            @protocol P
            - (IMP)methodForSelector:(SEL)aSelector;
            @end
        ";

        let expected_items = vec![
            AttributedItem {
                item: Item::TypedefDecl(TypedefDecl {
                    name: "Class".to_string(),
                    underlying: AttributedType {
                        ty: Type::Pointer(Pointer {
                            pointee: Box::new(AttributedType {
                                ty: Type::Tag(TagRef {
                                    id: TagId::Named("objc_class".to_string()),
                                    kind: TagKind::Struct,
                                    attrs: vec![],
                                }),
                                is_const_qualified: false,
                                alignment: None,
                                size: None,
                            }),
                            nullability: None,
                        }),
                        is_const_qualified: false,
                        alignment: Some(8),
                        size: Some(8),
                    },
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::RecordDef(RecordDef {
                    id: TagId::Named("objc_object".to_string()),
                    fields: vec![Field {
                        name: Some("isa".to_string()),
                        ty: AttributedType {
                            ty: Type::ObjPtr(ObjPtr {
                                kind: ObjPtrKind::Class,
                                nullability: Some(Nullability::NonNull),
                            }),
                            is_const_qualified: false,
                            alignment: Some(8),
                            size: Some(8),
                        },
                        bit_offset: 0,
                    }],
                    kind: RecordKind::Struct,
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::TypedefDecl(TypedefDecl {
                    name: "id".to_string(),
                    underlying: AttributedType {
                        ty: Type::Pointer(Pointer {
                            pointee: Box::new(AttributedType {
                                ty: Type::Tag(TagRef {
                                    id: TagId::Named("objc_object".to_string()),
                                    kind: TagKind::Struct,
                                    attrs: vec![],
                                }),
                                is_const_qualified: false,
                                alignment: Some(8),
                                size: Some(8),
                            }),
                            nullability: None,
                        }),
                        is_const_qualified: false,
                        alignment: Some(8),
                        size: Some(8),
                    },
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::TypedefDecl(TypedefDecl {
                    name: "SEL".to_string(),
                    underlying: AttributedType {
                        ty: Type::Pointer(Pointer {
                            pointee: Box::new(AttributedType {
                                ty: Type::Tag(TagRef {
                                    id: TagId::Named("objc_selector".to_string()),
                                    kind: TagKind::Struct,
                                    attrs: vec![],
                                }),
                                is_const_qualified: false,
                                alignment: None,
                                size: None,
                            }),
                            nullability: None,
                        }),
                        is_const_qualified: false,
                        alignment: Some(8),
                        size: Some(8),
                    },
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::TypedefDecl(TypedefDecl {
                    name: "IMP".to_string(),
                    underlying: AttributedType {
                        ty: Type::Pointer(Pointer {
                            pointee: Box::new(AttributedType {
                                ty: Type::Function(CallableDesc {
                                    result: Box::new(AttributedType {
                                        ty: Type::ObjPtr(ObjPtr {
                                            kind: ObjPtrKind::Id(IdObjPtr { protocols: vec![] }),
                                            nullability: Some(Nullability::Nullable),
                                        }),
                                        is_const_qualified: false,
                                        alignment: Some(8),
                                        size: Some(8),
                                    }),
                                    params: Some(vec![
                                        Param {
                                            name: None,
                                            ty: AttributedType {
                                                ty: Type::ObjPtr(ObjPtr {
                                                    kind: ObjPtrKind::Id(IdObjPtr {
                                                        protocols: vec![],
                                                    }),
                                                    nullability: Some(Nullability::NonNull),
                                                }),
                                                is_const_qualified: false,
                                                alignment: Some(8),
                                                size: Some(8),
                                            },
                                            attrs: vec![],
                                        },
                                        Param {
                                            name: None,
                                            ty: AttributedType {
                                                ty: Type::ObjCSel(Some(Nullability::NonNull)),
                                                is_const_qualified: false,
                                                alignment: Some(8),
                                                size: Some(8),
                                            },
                                            attrs: vec![],
                                        },
                                    ]),
                                    is_variadic: true,
                                    attrs: vec![],
                                }),
                                is_const_qualified: false,
                                alignment: Some(4),
                                size: Some(1),
                            }),
                            nullability: None,
                        }),
                        is_const_qualified: false,
                        alignment: Some(8),
                        size: Some(8),
                    },
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::ProtocolDef(ProtocolDef {
                    name: "P".to_string(),
                    inherited_protocols: vec![],
                    methods: vec![ProtocolMethod {
                        method: ObjCMethod {
                            name: "methodForSelector:".to_string(),
                            kind: ObjCMethodKind::Instance,
                            params: vec![ObjCParam {
                                name: "aSelector".to_string(),
                                ty: AttributedType {
                                    ty: Type::ObjCSel(None),
                                    is_const_qualified: false,
                                    alignment: Some(8),
                                    size: Some(8),
                                },
                                attrs: vec![],
                            }],
                            result: AttributedType {
                                ty: Type::Typedef(TypedefRef {
                                    name: "IMP".to_string(),
                                    nullability: None,
                                }),
                                is_const_qualified: false,
                                alignment: Some(8),
                                size: Some(8),
                            },
                            attrs: vec![],
                            type_encoding: "^?24@0:8:16".to_string(),
                        },
                        is_optional: false,
                    }],
                    properties: vec![],
                    origin: None,
                }),
                attrs: vec![],
            },
        ];

        let parsed_items = ast_from_str(Target::MacOsX86_64, source).unwrap();
        assert_eq!(parsed_items, expected_items);
    }

    #[test]
    fn test_nslog() {
        // NSLog seems to be already known by the compiler so handled a bit differently by it.
        let source = "
            @class NSString;
            extern void NSLog(NSString *format, ...);
        ";

        let expected_items = vec![AttributedItem {
            item: Item::FuncDecl(FuncDecl {
                name: "NSLog".to_string(),
                desc: CallableDesc {
                    result: Box::new(AttributedType {
                        ty: Type::Void,
                        is_const_qualified: false,
                        alignment: None,
                        size: None,
                    }),
                    params: Some(vec![Param {
                        name: Some("format".to_string()),
                        ty: AttributedType {
                            ty: Type::ObjPtr(ObjPtr {
                                kind: ObjPtrKind::SomeInstance(SomeInstanceObjPtr {
                                    interface: "NSString".to_string(),
                                    protocols: vec![],
                                    type_args: vec![],
                                }),
                                nullability: None,
                            }),
                            is_const_qualified: false,
                            alignment: Some(8),
                            size: Some(8),
                        },
                        attrs: vec![],
                    }]),
                    is_variadic: true,
                    attrs: vec![],
                },
                linkage: Linkage::External,
                origin: None,
            }),
            attrs: vec![],
        }];

        let parsed_items = ast_from_str(Target::MacOsX86_64, source).unwrap();
        assert_eq!(parsed_items, expected_items);
    }

    #[test]
    fn test_param_attributes() {
        // Note that noescape has 2 spelling: noescape and __noescape__.
        let source = "
            @interface I
            - (void)methodWithConsumedParam:(id) __attribute((ns_consumed)) consumedParam;
            - (void)methodWithNoescapeBlock:(void (__attribute__((noescape)) ^)(void))block;
            @end
            void function_with_consumed_param(__attribute((ns_consumed)) id consumedParam);
            void function_with_noescape_block(void (__attribute__((__noescape__)) ^)(void));
        ";

        let expected_items = vec![
            AttributedItem {
                item: Item::InterfaceDef(InterfaceDef {
                    name: "I".to_string(),
                    superclass: None,
                    adopted_protocols: vec![],
                    template_params: vec![],
                    methods: vec![
                        ObjCMethod {
                            name: "methodWithConsumedParam:".to_string(),
                            kind: ObjCMethodKind::Instance,
                            params: vec![ObjCParam {
                                name: "consumedParam".to_string(),
                                ty: AttributedType {
                                    ty: Type::ObjPtr(ObjPtr {
                                        kind: ObjPtrKind::Id(IdObjPtr { protocols: vec![] }),
                                        nullability: None,
                                    }),
                                    is_const_qualified: false,
                                    alignment: Some(8),
                                    size: Some(8),
                                },
                                attrs: vec![Attr::NSConsumed],
                            }],
                            result: AttributedType {
                                ty: Type::Void,
                                is_const_qualified: false,
                                alignment: None,
                                size: None,
                            },
                            attrs: vec![],
                            type_encoding: "v24@0:8@16".to_string(),
                        },
                        ObjCMethod {
                            name: "methodWithNoescapeBlock:".to_string(),
                            kind: ObjCMethodKind::Instance,
                            params: vec![ObjCParam {
                                name: "block".to_string(),
                                ty: AttributedType {
                                    ty: Type::ObjPtr(ObjPtr {
                                        kind: ObjPtrKind::Block(CallableDesc {
                                            result: Box::new(AttributedType {
                                                ty: Type::Void,
                                                is_const_qualified: false,
                                                alignment: None,
                                                size: None,
                                            }),
                                            params: Some(vec![]),
                                            is_variadic: false,
                                            attrs: vec![],
                                        }),
                                        nullability: None,
                                    }),
                                    is_const_qualified: false,
                                    alignment: Some(8),
                                    size: Some(8),
                                },
                                attrs: vec![Attr::Noescape],
                            }],
                            result: AttributedType {
                                ty: Type::Void,
                                is_const_qualified: false,
                                alignment: None,
                                size: None,
                            },
                            attrs: vec![],
                            type_encoding: "v24@0:8@?16".to_string(),
                        },
                    ],
                    properties: vec![],
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::FuncDecl(FuncDecl {
                    name: "function_with_consumed_param".to_string(),
                    desc: CallableDesc {
                        result: Box::new(AttributedType {
                            ty: Type::Void,
                            is_const_qualified: false,
                            alignment: None,
                            size: None,
                        }),
                        params: Some(vec![Param {
                            name: Some("consumedParam".to_string()),
                            ty: AttributedType {
                                ty: Type::ObjPtr(ObjPtr {
                                    kind: ObjPtrKind::Id(IdObjPtr { protocols: vec![] }),
                                    nullability: None,
                                }),
                                is_const_qualified: false,
                                alignment: Some(8),
                                size: Some(8),
                            },
                            attrs: vec![Attr::NSConsumed],
                        }]),
                        is_variadic: false,
                        attrs: vec![],
                    },
                    linkage: Linkage::External,
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::FuncDecl(FuncDecl {
                    name: "function_with_noescape_block".to_string(),
                    desc: CallableDesc {
                        result: Box::new(AttributedType {
                            ty: Type::Void,
                            is_const_qualified: false,
                            alignment: None,
                            size: None,
                        }),
                        params: Some(vec![Param {
                            name: None,
                            ty: AttributedType {
                                ty: Type::ObjPtr(ObjPtr {
                                    kind: ObjPtrKind::Block(CallableDesc {
                                        result: Box::new(AttributedType {
                                            ty: Type::Void,
                                            is_const_qualified: false,
                                            alignment: None,
                                            size: None,
                                        }),
                                        params: Some(vec![]),
                                        is_variadic: false,
                                        attrs: vec![],
                                    }),
                                    nullability: None,
                                }),
                                is_const_qualified: false,
                                alignment: Some(8),
                                size: Some(8),
                            },
                            attrs: vec![Attr::Noescape],
                        }]),
                        is_variadic: false,
                        attrs: vec![],
                    },
                    linkage: Linkage::External,
                    origin: None,
                }),
                attrs: vec![],
            },
        ];

        let parsed_items = ast_from_str(Target::MacOsX86_64, source).unwrap();
        assert_eq!(parsed_items, expected_items);
    }

    #[test]
    fn test_return_attributes() {
        let source = r#"
            @interface I
            - (id)methodWithRetainedReturn __attribute__((__ns_returns_retained__));
            - (void)unavailableMethod __attribute__((unavailable("not available in automatic reference counting mode")));
            @end
            __attribute__((ns_returns_retained)) id function_with_retained_return(void);
        "#;

        let expected_items = vec![
            AttributedItem {
                item: Item::InterfaceDef(InterfaceDef {
                    name: "I".to_string(),
                    superclass: None,
                    adopted_protocols: vec![],
                    template_params: vec![],
                    methods: vec![
                        ObjCMethod {
                            name: "methodWithRetainedReturn".to_string(),
                            kind: ObjCMethodKind::Instance,
                            params: vec![],
                            result: AttributedType {
                                ty: Type::ObjPtr(ObjPtr {
                                    kind: ObjPtrKind::Id(IdObjPtr { protocols: vec![] }),
                                    nullability: None,
                                }),
                                is_const_qualified: false,
                                alignment: Some(8),
                                size: Some(8),
                            },
                            attrs: vec![Attr::NSReturnsRetained],
                            type_encoding: "@16@0:8".to_string(),
                        },
                        ObjCMethod {
                            name: "unavailableMethod".to_string(),
                            kind: ObjCMethodKind::Instance,
                            params: vec![],
                            result: AttributedType {
                                ty: Type::Void,
                                is_const_qualified: false,
                                alignment: None,
                                size: None,
                            },
                            attrs: vec![Attr::Unavailable],
                            type_encoding: "v16@0:8".to_string(),
                        },
                    ],
                    properties: vec![],
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::FuncDecl(FuncDecl {
                    name: "function_with_retained_return".to_string(),
                    desc: CallableDesc {
                        result: Box::new(AttributedType {
                            ty: Type::ObjPtr(ObjPtr {
                                kind: ObjPtrKind::Id(IdObjPtr { protocols: vec![] }),
                                nullability: None,
                            }),
                            is_const_qualified: false,
                            alignment: Some(8),
                            size: Some(8),
                        }),
                        params: Some(vec![]),
                        is_variadic: false,
                        attrs: vec![Attr::NSReturnsRetained],
                    },
                    linkage: Linkage::External,
                    origin: None,
                }),
                attrs: vec![],
            },
        ];

        let parsed_items = ast_from_str(Target::MacOsX86_64, source).unwrap();
        assert_eq!(parsed_items, expected_items);
    }

    #[test]
    fn test_implicit_attributes() {
        // From the ARC rules, methods of the alloc, copy, init, mutableCopy, or new method family
        // (https://clang.llvm.org/docs/AutomaticReferenceCounting.html#method-families)
        // returns implicitly return a retained object.
        // clang kindly adds to those an implicit ReturnsRetained attribute.
        let source = r#"
            @interface I
            + (instancetype)alloc;
            - (instancetype)init;
            - (instancetype)initWithSomething:(int)something;
            - (instancetype)initAnything;
            - (instancetype)initReturningAutoreleased __attribute__((ns_returns_autoreleased));
            - (instancetype)initReturningNotRetained __attribute__((ns_returns_not_retained));
            + (instancetype)new;
            - (id)copy;
            - (id)mutableCopy;
            + (instancetype)normalClassMethod;
            @end
        "#;

        let expected_items = vec![AttributedItem {
            item: Item::InterfaceDef(InterfaceDef {
                name: "I".to_string(),
                superclass: None,
                adopted_protocols: vec![],
                template_params: vec![],
                methods: vec![
                    ObjCMethod {
                        name: "alloc".to_string(),
                        kind: ObjCMethodKind::Class,
                        params: vec![],
                        result: AttributedType {
                            ty: Type::Typedef(TypedefRef {
                                name: "instancetype".to_string(),
                                nullability: None,
                            }),
                            is_const_qualified: false,
                            alignment: Some(8),
                            size: Some(8),
                        },
                        attrs: vec![Attr::NSReturnsRetained],
                        type_encoding: "@16@0:8".to_string(),
                    },
                    ObjCMethod {
                        name: "init".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: AttributedType {
                            ty: Type::Typedef(TypedefRef {
                                name: "instancetype".to_string(),
                                nullability: None,
                            }),
                            is_const_qualified: false,
                            alignment: Some(8),
                            size: Some(8),
                        },
                        attrs: vec![Attr::NSConsumesSelf, Attr::NSReturnsRetained],
                        type_encoding: "@16@0:8".to_string(),
                    },
                    ObjCMethod {
                        name: "initWithSomething:".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![ObjCParam {
                            name: "something".to_string(),
                            ty: AttributedType {
                                ty: Type::Num(NumKind::Int),
                                is_const_qualified: false,
                                alignment: Some(4),
                                size: Some(4),
                            },
                            attrs: vec![],
                        }],
                        result: AttributedType {
                            ty: Type::Typedef(TypedefRef {
                                name: "instancetype".to_string(),
                                nullability: None,
                            }),
                            is_const_qualified: false,
                            alignment: Some(8),
                            size: Some(8),
                        },
                        attrs: vec![Attr::NSConsumesSelf, Attr::NSReturnsRetained],
                        type_encoding: "@20@0:8i16".to_string(),
                    },
                    ObjCMethod {
                        name: "initAnything".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: AttributedType {
                            ty: Type::Typedef(TypedefRef {
                                name: "instancetype".to_string(),
                                nullability: None,
                            }),
                            is_const_qualified: false,
                            alignment: Some(8),
                            size: Some(8),
                        },
                        attrs: vec![Attr::NSConsumesSelf, Attr::NSReturnsRetained],
                        type_encoding: "@16@0:8".to_string(),
                    },
                    ObjCMethod {
                        name: "initReturningAutoreleased".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: AttributedType {
                            ty: Type::Typedef(TypedefRef {
                                name: "instancetype".to_string(),
                                nullability: None,
                            }),
                            is_const_qualified: false,
                            alignment: Some(8),
                            size: Some(8),
                        },
                        attrs: vec![Attr::NSReturnsAutoreleased, Attr::NSConsumesSelf],
                        type_encoding: "@16@0:8".to_string(),
                    },
                    ObjCMethod {
                        name: "initReturningNotRetained".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: AttributedType {
                            ty: Type::Typedef(TypedefRef {
                                name: "instancetype".to_string(),
                                nullability: None,
                            }),
                            is_const_qualified: false,
                            alignment: Some(8),
                            size: Some(8),
                        },
                        attrs: vec![Attr::NSReturnsNotRetained, Attr::NSConsumesSelf],
                        type_encoding: "@16@0:8".to_string(),
                    },
                    ObjCMethod {
                        name: "new".to_string(),
                        kind: ObjCMethodKind::Class,
                        params: vec![],
                        result: AttributedType {
                            ty: Type::Typedef(TypedefRef {
                                name: "instancetype".to_string(),
                                nullability: None,
                            }),
                            is_const_qualified: false,
                            alignment: Some(8),
                            size: Some(8),
                        },
                        attrs: vec![Attr::NSReturnsRetained],
                        type_encoding: "@16@0:8".to_string(),
                    },
                    ObjCMethod {
                        name: "copy".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: AttributedType {
                            ty: Type::ObjPtr(ObjPtr {
                                kind: ObjPtrKind::Id(IdObjPtr { protocols: vec![] }),
                                nullability: None,
                            }),
                            is_const_qualified: false,
                            alignment: Some(8),
                            size: Some(8),
                        },
                        attrs: vec![Attr::NSReturnsRetained],
                        type_encoding: "@16@0:8".to_string(),
                    },
                    ObjCMethod {
                        name: "mutableCopy".to_string(),
                        kind: ObjCMethodKind::Instance,
                        params: vec![],
                        result: AttributedType {
                            ty: Type::ObjPtr(ObjPtr {
                                kind: ObjPtrKind::Id(IdObjPtr { protocols: vec![] }),
                                nullability: None,
                            }),
                            is_const_qualified: false,
                            alignment: Some(8),
                            size: Some(8),
                        },
                        attrs: vec![Attr::NSReturnsRetained],
                        type_encoding: "@16@0:8".to_string(),
                    },
                    ObjCMethod {
                        name: "normalClassMethod".to_string(),
                        kind: ObjCMethodKind::Class,
                        params: vec![],
                        result: AttributedType {
                            ty: Type::Typedef(TypedefRef {
                                name: "instancetype".to_string(),
                                nullability: None,
                            }),
                            is_const_qualified: false,
                            alignment: Some(8),
                            size: Some(8),
                        },
                        attrs: vec![],
                        type_encoding: "@16@0:8".to_string(),
                    },
                ],
                properties: vec![],
                origin: None,
            }),
            attrs: vec![],
        }];

        let parsed_items = ast_from_str(Target::MacOsX86_64, source).unwrap();
        assert_eq!(parsed_items, expected_items);
    }

    #[test]
    fn test_swift_name() {
        let source = r#"
            enum CIDataMatrixCodeECCVersion : long
            {
                CIDataMatrixCodeECCVersion000 __attribute__((swift_name("v000"))) = 0,
                CIDataMatrixCodeECCVersion050 __attribute__((swift_name("v050"))) = 50,
                CIDataMatrixCodeECCVersion080 __attribute__((swift_name("v080"))) = 80,
            } __attribute__((swift_name("CIDataMatrixCodeDescriptor.ECCVersion")));
        "#;

        let expected_items = vec![AttributedItem {
            item: Item::EnumDef(EnumDef {
                id: TagId::Named("CIDataMatrixCodeECCVersion".to_string()),
                underlying: AttributedType {
                    ty: Type::Num(NumKind::Long),
                    is_const_qualified: false,
                    alignment: Some(8),
                    size: Some(8),
                },
                values: vec![
                    EnumValue {
                        name: "CIDataMatrixCodeECCVersion000".to_string(),
                        value: SignedOrNotInt::Signed(0),
                        attrs: vec![Attr::SwiftName("v000".to_string())],
                    },
                    EnumValue {
                        name: "CIDataMatrixCodeECCVersion050".to_string(),
                        value: SignedOrNotInt::Signed(50),
                        attrs: vec![Attr::SwiftName("v050".to_string())],
                    },
                    EnumValue {
                        name: "CIDataMatrixCodeECCVersion080".to_string(),
                        value: SignedOrNotInt::Signed(80),
                        attrs: vec![Attr::SwiftName("v080".to_string())],
                    },
                ],
                origin: None,
            }),
            attrs: vec![Attr::SwiftName(
                "CIDataMatrixCodeDescriptor.ECCVersion".to_string(),
            )],
        }];

        let parsed_items = ast_from_str(Target::MacOsX86_64, source).unwrap();
        assert_eq!(parsed_items, expected_items);
    }

    #[test]
    fn test_enum_attrs() {
        // Attributes must work even even they are defined via preprocessor macros.
        // The way we get some attributes is very brittle.
        let source = r#"
            #define ATTR_START __attribute__((
            #define ATTR_END ))
            #define ATTR_VAL_PART1 flag_enum,enum_extensibility
            #define ATTR_VAL_PART2 (open)
            #define ATTRS ATTR_START ATTR_VAL_PART1 ATTR_VAL_PART2 ATTR_END
            typedef enum ATTRS MTLIndirectCommandType : unsigned long MTLIndirectCommandType;
            enum MTLIndirectCommandType: unsigned long {
                MTLIndirectCommandTypeDraw = (1 << 0),
                MTLIndirectCommandTypeDrawIndexed = (1 << 1),
                MTLIndirectCommandTypeDrawPatches __attribute__((availability(tvos,unavailable))) = (1 << 2),
                MTLIndirectCommandTypeDrawIndexedPatches __attribute__((availability(tvos,unavailable))) = (1 << 3),
            } __attribute__((availability(macos,introduced=10.14))) __attribute__((availability(ios,introduced=12.0)));
        "#;

        let expected_items = vec![
            AttributedItem {
                item: Item::TypedefDecl(TypedefDecl {
                    name: "MTLIndirectCommandType".to_string(),
                    underlying: AttributedType {
                        ty: Type::Tag(TagRef {
                            id: TagId::Named("MTLIndirectCommandType".to_string()),
                            kind: TagKind::Enum,
                            attrs: vec![
                                Attr::FlagEnum,
                                Attr::EnumExtensib(EnumExtensib::Open),
                                Attr::PlatformAvailability(vec![
                                    PlatformAvailability {
                                        platform: "ios".to_string(),
                                        introduced: Some(Version::Minor {
                                            major: 12,
                                            minor: 0,
                                        }),
                                        deprecated: None,
                                        obsoleted: None,
                                        unavailable: false,
                                        message: None,
                                    },
                                    PlatformAvailability {
                                        platform: "macos".to_string(),
                                        introduced: Some(Version::Minor {
                                            major: 10,
                                            minor: 14,
                                        }),
                                        deprecated: None,
                                        obsoleted: None,
                                        unavailable: false,
                                        message: None,
                                    },
                                ]),
                            ],
                        }),
                        is_const_qualified: false,
                        alignment: Some(8),
                        size: Some(8),
                    },
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::EnumDef(EnumDef {
                    id: TagId::Named("MTLIndirectCommandType".to_string()),
                    underlying: AttributedType {
                        ty: Type::Num(NumKind::ULong),
                        is_const_qualified: false,
                        alignment: Some(8),
                        size: Some(8),
                    },
                    values: vec![
                        EnumValue {
                            name: "MTLIndirectCommandTypeDraw".to_string(),
                            value: SignedOrNotInt::Unsigned(1),
                            attrs: vec![Attr::PlatformAvailability(vec![
                                PlatformAvailability {
                                    platform: "ios".to_string(),
                                    introduced: Some(Version::Minor {
                                        major: 12,
                                        minor: 0,
                                    }),
                                    deprecated: None,
                                    obsoleted: None,
                                    unavailable: false,
                                    message: None,
                                },
                                PlatformAvailability {
                                    platform: "macos".to_string(),
                                    introduced: Some(Version::Minor {
                                        major: 10,
                                        minor: 14,
                                    }),
                                    deprecated: None,
                                    obsoleted: None,
                                    unavailable: false,
                                    message: None,
                                },
                            ])],
                        },
                        EnumValue {
                            name: "MTLIndirectCommandTypeDrawIndexed".to_string(),
                            value: SignedOrNotInt::Unsigned(2),
                            attrs: vec![Attr::PlatformAvailability(vec![
                                PlatformAvailability {
                                    platform: "ios".to_string(),
                                    introduced: Some(Version::Minor {
                                        major: 12,
                                        minor: 0,
                                    }),
                                    deprecated: None,
                                    obsoleted: None,
                                    unavailable: false,
                                    message: None,
                                },
                                PlatformAvailability {
                                    platform: "macos".to_string(),
                                    introduced: Some(Version::Minor {
                                        major: 10,
                                        minor: 14,
                                    }),
                                    deprecated: None,
                                    obsoleted: None,
                                    unavailable: false,
                                    message: None,
                                },
                            ])],
                        },
                        EnumValue {
                            name: "MTLIndirectCommandTypeDrawPatches".to_string(),
                            value: SignedOrNotInt::Unsigned(4),
                            attrs: vec![Attr::PlatformAvailability(vec![PlatformAvailability {
                                platform: "tvos".to_string(),
                                introduced: None,
                                deprecated: None,
                                obsoleted: None,
                                unavailable: true,
                                message: None,
                            }])],
                        },
                        EnumValue {
                            name: "MTLIndirectCommandTypeDrawIndexedPatches".to_string(),
                            value: SignedOrNotInt::Unsigned(8),
                            attrs: vec![Attr::PlatformAvailability(vec![PlatformAvailability {
                                platform: "tvos".to_string(),
                                introduced: None,
                                deprecated: None,
                                obsoleted: None,
                                unavailable: true,
                                message: None,
                            }])],
                        },
                    ],
                    origin: None,
                }),
                attrs: vec![
                    Attr::FlagEnum,
                    Attr::EnumExtensib(EnumExtensib::Open),
                    Attr::PlatformAvailability(vec![
                        PlatformAvailability {
                            platform: "ios".to_string(),
                            introduced: Some(Version::Minor {
                                major: 12,
                                minor: 0,
                            }),
                            deprecated: None,
                            obsoleted: None,
                            unavailable: false,
                            message: None,
                        },
                        PlatformAvailability {
                            platform: "macos".to_string(),
                            introduced: Some(Version::Minor {
                                major: 10,
                                minor: 14,
                            }),
                            deprecated: None,
                            obsoleted: None,
                            unavailable: false,
                            message: None,
                        },
                    ]),
                ],
            },
        ];

        let parsed_items = ast_from_str(Target::MacOsX86_64, source).unwrap();
        assert_eq!(parsed_items, expected_items);
    }

    #[test]
    fn test_nsobject_attr() {
        let source = "
            typedef const struct __attribute__((objc_bridge(id))) opaqueCFSomething *CFSomethingRef;
            @interface I
            @property (nonatomic, readonly, nullable) __attribute__((NSObject)) CFSomethingRef prop;
            @end        
        ";

        let expected_items = vec![
            AttributedItem {
                item: Item::TypedefDecl(TypedefDecl {
                    name: "CFSomethingRef".to_string(),
                    underlying: AttributedType {
                        ty: Type::Pointer(Pointer {
                            pointee: Box::new(AttributedType {
                                ty: Type::Tag(TagRef {
                                    id: TagId::Named("opaqueCFSomething".to_string()),
                                    kind: TagKind::Struct,
                                    attrs: vec![Attr::ObjCBridge {
                                        ty_name: "id".to_string(),
                                        is_mutable: false,
                                    }],
                                }),
                                is_const_qualified: true,
                                alignment: None,
                                size: None,
                            }),
                            nullability: None,
                        }),
                        is_const_qualified: false,
                        alignment: Some(8),
                        size: Some(8),
                    },
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::InterfaceDef(InterfaceDef {
                    name: "I".to_string(),
                    superclass: None,
                    adopted_protocols: vec![],
                    template_params: vec![],
                    methods: vec![],
                    properties: vec![Property {
                        name: "prop".to_string(),
                        value: AttributedType {
                            ty: Type::Typedef(TypedefRef {
                                name: "CFSomethingRef".to_string(),
                                nullability: Some(Nullability::Nullable),
                            }),
                            is_const_qualified: false,
                            alignment: Some(8),
                            size: Some(8),
                        },
                        is_atomic: false,
                        is_writable: false,
                        is_class: false,
                        ownership: PropOwnership::Strong,
                        custom_getter_sel: None,
                        custom_setter_sel: None,
                        attrs: vec![Attr::NSObject],
                    }],
                    origin: None,
                }),
                attrs: vec![],
            },
        ];

        let parsed_items = ast_from_str(Target::MacOsX86_64, source).unwrap();
        assert_eq!(parsed_items, expected_items);
    }

    #[test]
    fn test_const() {
        let source = "
            void function_with_nullable_const_param(unsigned char const *_Nullable const);
            void function_with_nonnull_const_typedef(id _Nonnull const);
        ";

        let expected_items = vec![
            AttributedItem {
                item: Item::FuncDecl(FuncDecl {
                    name: "function_with_nullable_const_param".to_string(),
                    desc: CallableDesc {
                        result: Box::new(AttributedType {
                            ty: Type::Void,
                            is_const_qualified: false,
                            alignment: None,
                            size: None,
                        }),
                        params: Some(vec![Param {
                            name: None,
                            ty: AttributedType {
                                ty: Type::Pointer(Pointer {
                                    pointee: Box::new(AttributedType {
                                        ty: Type::Num(NumKind::UChar),
                                        is_const_qualified: true,
                                        alignment: Some(1),
                                        size: Some(1),
                                    }),
                                    nullability: Some(Nullability::Nullable),
                                }),
                                is_const_qualified: true,
                                alignment: Some(8),
                                size: Some(8),
                            },
                            attrs: vec![],
                        }]),
                        is_variadic: false,
                        attrs: vec![],
                    },
                    linkage: Linkage::External,
                    origin: None,
                }),
                attrs: vec![],
            },
            AttributedItem {
                item: Item::FuncDecl(FuncDecl {
                    name: "function_with_nonnull_const_typedef".to_string(),
                    desc: CallableDesc {
                        result: Box::new(AttributedType {
                            ty: Type::Void,
                            is_const_qualified: false,
                            alignment: None,
                            size: None,
                        }),
                        params: Some(vec![Param {
                            name: None,
                            ty: AttributedType {
                                ty: Type::ObjPtr(ObjPtr {
                                    kind: ObjPtrKind::Id(IdObjPtr { protocols: vec![] }),
                                    nullability: Some(Nullability::NonNull),
                                }),
                                is_const_qualified: true,
                                alignment: Some(8),
                                size: Some(8),
                            },
                            attrs: vec![],
                        }]),
                        is_variadic: false,
                        attrs: vec![],
                    },
                    linkage: Linkage::External,
                    origin: None,
                }),
                attrs: vec![],
            },
        ];

        let parsed_items = ast_from_str(Target::MacOsX86_64, source).unwrap();
        assert_eq!(parsed_items, expected_items);
    }
}
