// Heavily based on the clang crate, with naming closer to the original libclang.

use crate::xcode;
use bitflags::bitflags;
use std::convert::TryInto;
use std::ffi::{CStr, CString};
use std::marker::PhantomData;

fn ensure_libclang_loaded() {
    if clang_sys::is_loaded() {
        return;
    }

    use once_cell::sync::Lazy;
    use std::sync::Arc;
    static LIBCLANG: Lazy<Arc<clang_sys::SharedLibrary>> = Lazy::new(|| {
        // Force clang-sys to use the libclang from the active Xcode.
        // A libclang built by yourself might not be able to handle code preprocessed by Xcode's clang.
        std::env::set_var("LIBCLANG_PATH", xcode::libclang_path());
        clang_sys::load().expect("could to find libclang");
        let library = clang_sys::get_library().expect("could not find the library we just loaded");
        println!("loaded libclang from {}", library.path().display());
        library
    });

    clang_sys::set_library(Some(LIBCLANG.clone()));
}

#[derive(Debug)]
pub struct Index {
    ptr: clang_sys::CXIndex,
}

impl Index {
    fn from_ptr(ptr: clang_sys::CXIndex) -> Option<Self> {
        if ptr.is_null() {
            None
        } else {
            Some(Self { ptr })
        }
    }

    pub fn new(exclude: bool, diagnostics: bool) -> Self {
        ensure_libclang_loaded();
        let ptr = unsafe { clang_sys::clang_createIndex(exclude as _, diagnostics as _) };
        Self::from_ptr(ptr).unwrap()
    }

    pub fn parse<S: AsRef<str>>(
        &self,
        args: &[S],
        file_path: &std::path::Path,
        unsaved: &[UnsavedFile],
        options: TuOptions,
    ) -> Result<TranslationUnit<'_>, ClangError> {
        let c_file_path =
            CString::new(file_path.as_os_str().to_str().expect("invalid C string")).unwrap();

        let cstr_args: Vec<CString> = args
            .iter()
            .map(|arg| CString::new(arg.as_ref()).unwrap())
            .collect();
        let c_args: Vec<*const _> = cstr_args.iter().map(|cstr| cstr.as_ptr()).collect();

        let mut c_unsaved: Vec<clang_sys::CXUnsavedFile> = unsaved
            .iter()
            .map(|unsaved| clang_sys::CXUnsavedFile {
                Filename: unsaved.path.as_ptr(),
                Contents: unsaved.contents.as_ptr(),
                Length: unsaved.contents.as_bytes().len() as _,
            })
            .collect();

        let mut tu_ptr = std::ptr::null_mut();
        let err_code = unsafe {
            clang_sys::clang_parseTranslationUnit2(
                self.ptr,
                c_file_path.as_ptr(),
                c_args.as_ptr(),
                c_args.len() as _,
                c_unsaved.as_mut_ptr(),
                c_unsaved.len() as _,
                options.bits(),
                &mut tu_ptr,
            )
        };
        if let Some(error) = ClangError::from_err_code(err_code) {
            Err(error)
        } else {
            Ok(TranslationUnit::from_ptr(tu_ptr))
        }
    }
}

impl Drop for Index {
    fn drop(&mut self) {
        unsafe {
            clang_sys::clang_disposeIndex(self.ptr);
        }
    }
}

#[derive(Debug)]
pub struct UnsavedFile {
    path: CString,
    contents: CString,
}

impl UnsavedFile {
    pub fn new(path: &std::path::Path, contents: &str) -> Self {
        let path = CString::new(path.as_os_str().to_str().expect("invalid C string")).unwrap();
        let contents = CString::new(contents).unwrap();
        Self { path, contents }
    }
}

bitflags! {
    pub struct TuOptions: i32 {
        const CACHE_COMPLETION_RESULTS = clang_sys::CXTranslationUnit_CacheCompletionResults;
        const DETAILED_PREPROCESSING_RECORD = clang_sys::CXTranslationUnit_DetailedPreprocessingRecord;
        const BRIEFS_IN_COMPLETION_RESULTS = clang_sys::CXTranslationUnit_IncludeBriefCommentsInCodeCompletion;
        const INCOMPLETE = clang_sys::CXTranslationUnit_Incomplete;
        const SKIP_FUNCTION_BODIES = clang_sys::CXTranslationUnit_SkipFunctionBodies;
        const KEEP_GOING = clang_sys::CXTranslationUnit_KeepGoing;
        const SINGLE_FILE_PARSE = clang_sys::CXTranslationUnit_SingleFileParse;
        const LIMIT_SKIP_FUNCTION_BODIES_TO_PREAMBLE = clang_sys::CXTranslationUnit_LimitSkipFunctionBodiesToPreamble;
        const INCLUDE_ATTRIBUTED_TYPES = clang_sys::CXTranslationUnit_IncludeAttributedTypes;
        const VISIT_IMPLICIT_ATTRIBUTES = clang_sys::CXTranslationUnit_VisitImplicitAttributes;
    }
}

#[derive(Debug)]
pub struct TranslationUnit<'idx> {
    ptr: clang_sys::CXTranslationUnit,
    _marker: PhantomData<&'idx Index>,
}

impl<'idx> TranslationUnit<'idx> {
    fn from_ptr(ptr: clang_sys::CXTranslationUnit) -> TranslationUnit<'idx> {
        assert!(!ptr.is_null());
        Self {
            ptr,
            _marker: PhantomData,
        }
    }

    pub fn cursor(&self) -> Cursor<'_> {
        let raw = unsafe { clang_sys::clang_getTranslationUnitCursor(self.ptr) };
        Cursor::from_raw(raw, self).expect("invalid cursor")
    }

    pub fn diagnostics(&self) -> impl ExactSizeIterator<Item = Diagnostic<'_>> {
        let num = unsafe { clang_sys::clang_getNumDiagnostics(self.ptr) };
        PropertyIter::new(self, num, clang_sys::clang_getDiagnostic)
    }
}

impl Drop for TranslationUnit<'_> {
    fn drop(&mut self) {
        unsafe {
            clang_sys::clang_disposeTranslationUnit(self.ptr);
        }
    }
}

impl PartialEq for TranslationUnit<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ClangError {
    Failure,
    Crashed,
    InvalidArguments,
    ASTReadError,
}

impl std::fmt::Display for ClangError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = match self {
            ClangError::Failure => "libclang failed parsing",
            ClangError::Crashed => "libclang crashed",
            ClangError::InvalidArguments => "libclang was given invalid arguments",
            ClangError::ASTReadError => "libclang crashed",
        };
        f.write_str(message)?;
        Ok(())
    }
}

impl ClangError {
    pub fn from_err_code(err_code: clang_sys::CXErrorCode) -> Option<ClangError> {
        match err_code {
            clang_sys::CXError_Success => None,
            clang_sys::CXError_Crashed => Some(ClangError::Crashed),
            clang_sys::CXError_InvalidArguments => Some(ClangError::InvalidArguments),
            clang_sys::CXError_ASTReadError => Some(ClangError::ASTReadError),
            _ => Some(ClangError::Failure),
        }
    }
}

fn to_string(clang_str: clang_sys::CXString) -> Option<String> {
    if clang_str.data.is_null() {
        return None;
    }
    unsafe {
        let c = CStr::from_ptr(clang_sys::clang_getCString(clang_str));
        let owned = c.to_str().expect("invalid Rust string").to_string();
        Some(owned)
    }
}

fn into_string(clang_str: clang_sys::CXString) -> Option<String> {
    match to_string(clang_str) {
        Some(str) => {
            unsafe { clang_sys::clang_disposeString(clang_str) };
            Some(str)
        }
        None => None,
    }
}

fn none_if_empty(str: Option<String>) -> Option<String> {
    match str {
        Some(str) if !str.is_empty() => Some(str),
        _ => None,
    }
}

pub struct Cursor<'a> {
    raw: clang_sys::CXCursor,
    tu: &'a TranslationUnit<'a>,
}

impl<'a> Cursor<'a> {
    fn from_raw(raw: clang_sys::CXCursor, tu: &'a TranslationUnit<'a>) -> Option<Self> {
        unsafe {
            let null_cur = clang_sys::clang_getNullCursor();
            if clang_sys::clang_equalCursors(raw, null_cur) == 0
                && clang_sys::clang_isInvalid(raw.kind) == 0
            {
                Some(Self { raw, tu })
            } else {
                None
            }
        }
    }

    pub fn kind(&self) -> CursorKind {
        self.raw.kind.into()
    }

    pub fn tu(&self) -> &'a TranslationUnit<'a> {
        self.tu
    }

    pub fn availability(&self) -> Availability {
        unsafe { clang_sys::clang_getCursorAvailability(self.raw) }.into()
    }

    pub fn spelling(&self) -> Option<String> {
        none_if_empty(into_string(unsafe {
            clang_sys::clang_getCursorSpelling(self.raw)
        }))
    }

    pub fn display_name(&self) -> Option<String> {
        none_if_empty(into_string(unsafe {
            clang_sys::clang_getCursorDisplayName(self.raw)
        }))
    }

    pub fn mangling(&self) -> Option<String> {
        none_if_empty(into_string(unsafe {
            clang_sys::clang_Cursor_getMangling(self.raw)
        }))
    }

    pub fn objc_type_encoding(&self) -> Option<String> {
        none_if_empty(into_string(unsafe {
            clang_sys::clang_getDeclObjCTypeEncoding(self.raw)
        }))
    }

    pub fn usr(&self) -> Option<String> {
        none_if_empty(into_string(unsafe {
            clang_sys::clang_getCursorUSR(self.raw)
        }))
    }

    pub fn arguments(&self) -> Option<impl ExactSizeIterator<Item = Cursor<'_>>> {
        let num = unsafe { clang_sys::clang_Cursor_getNumArguments(self.raw) };
        if num < 0 {
            None
        } else {
            Some(PropertyIter::new(
                self,
                num as _,
                clang_sys::clang_Cursor_getArgument,
            ))
        }
    }

    pub fn is_objc_optional(&self) -> bool {
        unsafe { clang_sys::clang_Cursor_isObjCOptional(self.raw) != 0 }
    }

    pub fn is_declaration(&self) -> bool {
        unsafe { clang_sys::clang_isDeclaration(self.raw.kind) != 0 }
    }

    pub fn objc_selector_index(&self) -> Option<u32> {
        let index = unsafe { clang_sys::clang_Cursor_getObjCSelectorIndex(self.raw) };
        if index < 0 {
            None
        } else {
            Some(index as _)
        }
    }

    pub fn objc_attributes(&self) -> Option<ObjCAttributes> {
        let attributes = unsafe { clang_sys::clang_Cursor_getObjCPropertyAttributes(self.raw, 0) };
        if attributes != 0 {
            Some(ObjCAttributes::from_bits(attributes).expect("unexpected bit pattern"))
        } else {
            None
        }
    }

    pub fn location(&self) -> Option<SourceLocation<'a>> {
        SourceLocation::from_raw(
            unsafe { clang_sys::clang_getCursorLocation(self.raw) },
            self.tu,
        )
    }

    pub fn extent(&self) -> Option<SourceRange<'a>> {
        SourceRange::from_raw(
            unsafe { clang_sys::clang_getCursorExtent(self.raw) },
            self.tu,
        )
    }

    pub fn platform_availability(&self) -> Option<Vec<PlatformAvailability>> {
        if !self.is_declaration() {
            return None;
        }

        let len = unsafe {
            clang_sys::clang_getCursorPlatformAvailability(
                self.raw,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                0,
            )
        };

        if len == 0 {
            return None;
        }

        let mut raw_availabilities: Vec<clang_sys::CXPlatformAvailability> =
            vec![clang_sys::CXPlatformAvailability::default(); len as _];

        let new_len = unsafe {
            clang_sys::clang_getCursorPlatformAvailability(
                self.raw,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                raw_availabilities.as_mut_ptr(),
                raw_availabilities.len() as _,
            )
        };
        assert!(new_len >= len);
        if new_len > len {
            // Sometime clang underestimates the number of values so resize our vector and try again.
            raw_availabilities.resize_with(new_len as _, Default::default);

            let new_new_len = unsafe {
                clang_sys::clang_getCursorPlatformAvailability(
                    self.raw,
                    std::ptr::null_mut(),
                    std::ptr::null_mut(),
                    std::ptr::null_mut(),
                    std::ptr::null_mut(),
                    raw_availabilities.as_mut_ptr(),
                    raw_availabilities.len() as _,
                )
            };

            assert_eq!(new_len, new_new_len);
        }

        let availabilities = raw_availabilities
            .into_iter()
            .map(PlatformAvailability::from_raw)
            .collect();
        Some(availabilities)
    }

    pub fn storage_class(&self) -> Option<StorageClass> {
        StorageClass::from_raw(unsafe { clang_sys::clang_Cursor_getStorageClass(self.raw) })
    }

    pub fn linkage(&self) -> Option<LinkageKind> {
        LinkageKind::from_raw(unsafe { clang_sys::clang_getCursorLinkage(self.raw) })
    }

    pub fn offset_of_field(&self) -> Option<usize> {
        let offset = unsafe { clang_sys::clang_Cursor_getOffsetOfField(self.raw) };
        if offset < 0 {
            None
        } else {
            Some(offset as _)
        }
    }

    pub fn enum_constant_value(&self) -> Option<(i64, u64)> {
        if self.kind() == CursorKind::EnumConstantDecl {
            unsafe {
                let signed = clang_sys::clang_getEnumConstantDeclValue(self.raw);
                let unsigned = clang_sys::clang_getEnumConstantDeclUnsignedValue(self.raw);
                Some((signed, unsigned))
            }
        } else {
            None
        }
    }

    pub fn canonical_cursor(&self) -> Cursor<'a> {
        Cursor::from_raw(
            unsafe { clang_sys::clang_getCanonicalCursor(self.raw) },
            self.tu,
        )
        .expect("invalid cursor")
    }

    pub fn semantic_parent(&self) -> Option<Cursor<'a>> {
        Cursor::from_raw(
            unsafe { clang_sys::clang_getCursorSemanticParent(self.raw) },
            self.tu,
        )
    }

    pub fn lexical_parent(&self) -> Option<Cursor<'a>> {
        Cursor::from_raw(
            unsafe { clang_sys::clang_getCursorLexicalParent(self.raw) },
            self.tu,
        )
    }

    pub fn definition(&self) -> Option<Cursor<'a>> {
        Cursor::from_raw(
            unsafe { clang_sys::clang_getCursorDefinition(self.raw) },
            self.tu,
        )
    }

    pub fn referenced(&self) -> Option<Cursor<'a>> {
        Cursor::from_raw(
            unsafe { clang_sys::clang_getCursorReferenced(self.raw) },
            self.tu,
        )
    }

    pub fn template(&self) -> Option<Cursor<'a>> {
        Cursor::from_raw(
            unsafe { clang_sys::clang_getSpecializedCursorTemplate(self.raw) },
            self.tu,
        )
    }

    pub fn template_kind(&self) -> Option<CursorKind> {
        let kind: CursorKind = unsafe { clang_sys::clang_getTemplateCursorKind(self.raw) }.into();
        match kind {
            CursorKind::NoDeclFound => None,
            _ => Some(kind),
        }
    }

    pub fn type_(&self) -> Option<Type<'a>> {
        Type::from_raw(unsafe { clang_sys::clang_getCursorType(self.raw) }, self.tu)
    }

    pub fn enum_decl_int_type(&self) -> Option<Type<'a>> {
        Type::from_raw(
            unsafe { clang_sys::clang_getEnumDeclIntegerType(self.raw) },
            self.tu,
        )
    }

    pub fn typedef_decl_underlying_type(&self) -> Option<Type<'a>> {
        Type::from_raw(
            unsafe { clang_sys::clang_getTypedefDeclUnderlyingType(self.raw) },
            self.tu,
        )
    }

    pub fn result_type(&self) -> Option<Type<'a>> {
        Type::from_raw(
            unsafe { clang_sys::clang_getCursorResultType(self.raw) },
            self.tu,
        )
    }

    pub fn receiver_type(&self) -> Option<Type<'a>> {
        Type::from_raw(
            unsafe { clang_sys::clang_Cursor_getReceiverType(self.raw) },
            self.tu,
        )
    }

    pub fn ib_outlet_collection_type(&self) -> Option<Type<'a>> {
        Type::from_raw(
            unsafe { clang_sys::clang_getIBOutletCollectionType(self.raw) },
            self.tu,
        )
    }

    pub fn visit_children<F: FnMut(Cursor<'a>, Cursor<'a>) -> ChildVisitResult>(
        &self,
        f: F,
    ) -> bool {
        struct Data<'a, F: FnMut(Cursor<'a>, Cursor<'a>) -> ChildVisitResult> {
            f: F,
            tu: &'a TranslationUnit<'a>,
        }

        extern "C" fn visit<'a, F: FnMut(Cursor<'a>, Cursor<'a>) -> ChildVisitResult>(
            cursor: clang_sys::CXCursor,
            parent: clang_sys::CXCursor,
            data: clang_sys::CXClientData,
        ) -> clang_sys::CXChildVisitResult {
            unsafe {
                let data: &mut Data<'a, F> = &mut *(data as *mut Data<'a, F>);
                let cursor = Cursor::from_raw(cursor, data.tu).expect("invalid cursor");
                let parent = Cursor::from_raw(parent, data.tu).expect("invalid cursor");
                (data.f)(cursor, parent).into()
            }
        }

        let mut data = Data { f, tu: self.tu };
        unsafe {
            clang_sys::clang_visitChildren(self.raw, visit::<F>, &mut data as *mut Data<'a, F> as _)
                != 0
        }
    }

    pub fn children(&self) -> Vec<Cursor<'a>> {
        let mut children = Vec::new();
        self.visit_children(|cursor, _| {
            children.push(cursor);
            ChildVisitResult::Continue
        });
        children
    }
}

impl PartialEq for Cursor<'_> {
    fn eq(&self, other: &Self) -> bool {
        unsafe { clang_sys::clang_equalCursors(self.raw, other.raw) != 0 }
    }
}

impl std::fmt::Debug for Cursor<'_> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("Cursor")
            .field("kind", &self.kind())
            .field("display_name", &self.display_name())
            .field("location", &self.location())
            .finish()
    }
}

trait PropertyIterOwner<'a> {
    type Raw;
    fn raw(&self) -> Self::Raw;
    fn tu(&'a self) -> &'a TranslationUnit<'a>;
}

impl<'a> PropertyIterOwner<'a> for Cursor<'a> {
    type Raw = clang_sys::CXCursor;

    fn raw(&self) -> Self::Raw {
        self.raw
    }

    fn tu(&'a self) -> &'a TranslationUnit<'a> {
        self.tu
    }
}

impl<'a> PropertyIterOwner<'a> for Type<'a> {
    type Raw = clang_sys::CXType;

    fn raw(&self) -> Self::Raw {
        self.raw
    }

    fn tu(&'a self) -> &'a TranslationUnit<'a> {
        self.tu
    }
}

impl<'a> PropertyIterOwner<'a> for TranslationUnit<'a> {
    type Raw = clang_sys::CXTranslationUnit;

    fn raw(&self) -> Self::Raw {
        self.ptr
    }

    fn tu(&'a self) -> &'a TranslationUnit<'a> {
        self
    }
}

trait PropertyIterItem<'a> {
    type Raw;
    fn from_raw(raw: Self::Raw, tu: &'a TranslationUnit<'a>) -> Self;
}

impl<'a> PropertyIterItem<'a> for Cursor<'a> {
    type Raw = clang_sys::CXCursor;

    fn from_raw(raw: Self::Raw, tu: &'a TranslationUnit<'a>) -> Self {
        Self::from_raw(raw, tu).expect("invalid cursor")
    }
}

impl<'a> PropertyIterItem<'a> for Type<'a> {
    type Raw = clang_sys::CXType;

    fn from_raw(raw: Self::Raw, tu: &'a TranslationUnit<'a>) -> Self {
        Self::from_raw(raw, tu).expect("invalid type")
    }
}

impl<'a> PropertyIterItem<'a> for Diagnostic<'a> {
    type Raw = clang_sys::CXDiagnostic;

    fn from_raw(ptr: Self::Raw, tu: &'a TranslationUnit<'a>) -> Self {
        Self::from_ptr(ptr, tu).expect("invalid diagnostic")
    }
}

struct PropertyIter<'a, Owner, Item>
where
    Owner: PropertyIterOwner<'a>,
    Item: PropertyIterItem<'a>,
{
    owner: &'a Owner,
    len: u32,
    get_item: unsafe fn(Owner::Raw, u32) -> Item::Raw,
    index: u32,
}

impl<'a, Owner, Item> PropertyIter<'a, Owner, Item>
where
    Owner: PropertyIterOwner<'a>,
    Item: PropertyIterItem<'a>,
{
    fn new(owner: &'a Owner, len: u32, get_item: unsafe fn(Owner::Raw, u32) -> Item::Raw) -> Self {
        Self {
            owner,
            get_item,
            len,
            index: 0,
        }
    }
}

impl<'a, Owner, Item> Iterator for PropertyIter<'a, Owner, Item>
where
    Owner: PropertyIterOwner<'a>,
    Item: PropertyIterItem<'a>,
{
    type Item = Item;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index == self.len {
            None
        } else {
            let next = Item::from_raw(
                unsafe { (self.get_item)(self.owner.raw(), self.index) },
                self.owner.tu(),
            );
            self.index += 1;
            Some(next)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let left = self.len - self.index;
        (left as _, Some(left as _))
    }
}

impl<'a, Owner, Item> ExactSizeIterator for PropertyIter<'a, Owner, Item>
where
    Owner: PropertyIterOwner<'a>,
    Item: PropertyIterItem<'a>,
{
}

pub struct Type<'a> {
    raw: clang_sys::CXType,
    tu: &'a TranslationUnit<'a>,
}

impl<'a> Type<'a> {
    fn from_raw(raw: clang_sys::CXType, tu: &'a TranslationUnit<'a>) -> Option<Self> {
        if raw.kind == clang_sys::CXType_Invalid {
            None
        } else {
            Some(Self { raw, tu })
        }
    }

    pub fn kind(&self) -> TypeKind {
        self.raw.kind.into()
    }

    pub fn pointee_type(&self) -> Option<Type<'_>> {
        Type::from_raw(
            unsafe { clang_sys::clang_getPointeeType(self.raw) },
            self.tu,
        )
    }

    pub fn modified_type(&self) -> Option<Type<'a>> {
        Type::from_raw(
            unsafe { clang_sys::clang_Type_getModifiedType(self.raw) },
            self.tu,
        )
    }

    pub fn objc_object_base_type(&self) -> Option<Type<'a>> {
        Type::from_raw(
            unsafe { clang_sys::clang_Type_getObjCObjectBaseType(self.raw) },
            self.tu,
        )
    }

    pub fn result_type(&self) -> Option<Type<'a>> {
        Type::from_raw(unsafe { clang_sys::clang_getResultType(self.raw) }, self.tu)
    }

    pub fn named_type(&self) -> Option<Type<'a>> {
        Type::from_raw(
            unsafe { clang_sys::clang_Type_getNamedType(self.raw) },
            self.tu,
        )
    }

    pub fn class_type(&self) -> Option<Type<'a>> {
        Type::from_raw(
            unsafe { clang_sys::clang_Type_getClassType(self.raw) },
            self.tu,
        )
    }

    pub fn canonical_type(&self) -> Type<'a> {
        Type::from_raw(
            unsafe { clang_sys::clang_getCanonicalType(self.raw) },
            self.tu,
        )
        .expect("a type should have a canonical version")
    }

    pub fn element_type(&self) -> Option<Type<'a>> {
        Type::from_raw(
            unsafe { clang_sys::clang_getElementType(self.raw) },
            self.tu,
        )
    }

    pub fn argument_types(&self) -> Option<impl ExactSizeIterator<Item = Type<'_>>> {
        let num = unsafe { clang_sys::clang_getNumArgTypes(self.raw) };
        if num < 0 {
            None
        } else {
            Some(PropertyIter::new(
                self,
                num as _,
                clang_sys::clang_getArgType,
            ))
        }
    }

    pub fn template_arguments(&self) -> Option<impl ExactSizeIterator<Item = Type<'_>>> {
        let num = unsafe { clang_sys::clang_Type_getNumTemplateArguments(self.raw) };
        if num < 0 {
            None
        } else {
            Some(PropertyIter::new(
                self,
                num as _,
                clang_sys::clang_Type_getTemplateArgumentAsType,
            ))
        }
    }

    pub fn objc_type_arg_types(&self) -> impl ExactSizeIterator<Item = Type<'_>> {
        let num = unsafe { clang_sys::clang_Type_getNumObjCTypeArgs(self.raw) };
        PropertyIter::new(self, num, clang_sys::clang_Type_getObjCTypeArg)
    }

    pub fn objc_protocol_decls(&self) -> impl ExactSizeIterator<Item = Cursor<'_>> {
        let num = unsafe { clang_sys::clang_Type_getNumObjCProtocolRefs(self.raw) };
        PropertyIter::new(self, num, clang_sys::clang_Type_getObjCProtocolDecl)
    }

    pub fn spelling(&self) -> String {
        into_string(unsafe { clang_sys::clang_getTypeSpelling(self.raw) })
            .expect("invalid type spelling")
    }

    pub fn declaration(&self) -> Option<Cursor<'a>> {
        Cursor::from_raw(
            unsafe { clang_sys::clang_getTypeDeclaration(self.raw) },
            self.tu,
        )
    }

    pub fn is_variadic_function(&self) -> bool {
        unsafe { clang_sys::clang_isFunctionTypeVariadic(self.raw) != 0 }
    }

    pub fn is_const_qualified(&self) -> bool {
        unsafe { clang_sys::clang_isConstQualifiedType(self.raw) != 0 }
    }

    pub fn nullability(&self) -> Option<Nullability> {
        Nullability::from_raw(unsafe { clang_sys::clang_Type_getNullability(self.raw) })
    }

    pub fn align_of(&self) -> Option<usize> {
        let align_of = unsafe { clang_sys::clang_Type_getAlignOf(self.raw) };
        if align_of < 0 {
            None
        } else {
            Some(align_of as _)
        }
    }

    pub fn size_of(&self) -> Option<usize> {
        let size = unsafe { clang_sys::clang_Type_getSizeOf(self.raw) };
        if size < 0 {
            None
        } else {
            Some(size as _)
        }
    }

    pub fn offset_of(&self, field_name: &str) -> Option<usize> {
        let field_name = CString::new(field_name).unwrap();
        let offset = unsafe { clang_sys::clang_Type_getOffsetOf(self.raw, field_name.as_ptr()) };
        if offset < 0 {
            None
        } else {
            Some(offset as _)
        }
    }

    pub fn num_elements(&self) -> Option<usize> {
        let size = unsafe { clang_sys::clang_getNumElements(self.raw) };
        if size >= 0 {
            Some(size as _)
        } else {
            None
        }
    }

    pub fn visit_fields<F: FnMut(Cursor<'a>) -> VisitorResult>(&self, f: F) -> bool {
        struct Data<'a, F: FnMut(Cursor<'a>) -> VisitorResult> {
            f: F,
            tu: &'a TranslationUnit<'a>,
        }

        extern "C" fn visit<'a, F: FnMut(Cursor<'a>) -> VisitorResult>(
            cursor: clang_sys::CXCursor,
            data: clang_sys::CXClientData,
        ) -> clang_sys::CXChildVisitResult {
            unsafe {
                let data: &mut Data<'a, F> = &mut *(data as *mut Data<'a, F>);
                let cursor = Cursor::from_raw(cursor, data.tu).expect("invalid cursor");
                (data.f)(cursor).into()
            }
        }

        let mut data = Data { f, tu: self.tu };
        unsafe {
            clang_sys::clang_Type_visitFields(
                self.raw,
                visit::<F>,
                &mut data as *mut Data<'a, F> as _,
            ) != 0
        }
    }

    pub fn fields(&self) -> Option<Vec<Cursor<'a>>> {
        if self.kind() == TypeKind::Record {
            let mut fields = Vec::new();
            self.visit_fields(|cursor| {
                fields.push(cursor);
                VisitorResult::Continue
            });
            Some(fields)
        } else {
            None
        }
    }
}

impl PartialEq for Type<'_> {
    fn eq(&self, other: &Self) -> bool {
        unsafe { clang_sys::clang_equalTypes(self.raw, other.raw) != 0 }
    }
}

impl std::fmt::Debug for Type<'_> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("Type")
            .field("kind", &self.kind())
            .field("spelling", &self.spelling())
            .finish()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Nullability {
    NonNull,
    Nullable,
    Unspecified,
}

impl Nullability {
    fn from_raw(raw: clang_sys::CXTypeNullabilityKind) -> Option<Self> {
        use clang_sys::*;
        #[allow(non_upper_case_globals)]
        match raw {
            CXTypeNullability_NonNull => Some(Nullability::NonNull),
            CXTypeNullability_Nullable => Some(Nullability::Nullable),
            CXTypeNullability_Unspecified => Some(Nullability::Unspecified),
            _ => None,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CursorKind {
    UnexposedDecl,
    StructDecl,
    UnionDecl,
    ClassDecl,
    EnumDecl,
    FieldDecl,
    EnumConstantDecl,
    FunctionDecl,
    VarDecl,
    ParmDecl,
    ObjCInterfaceDecl,
    ObjCCategoryDecl,
    ObjCProtocolDecl,
    ObjCPropertyDecl,
    ObjCIvarDecl,
    ObjCInstanceMethodDecl,
    ObjCClassMethodDecl,
    ObjCImplementationDecl,
    ObjCCategoryImplDecl,
    TypedefDecl,
    CXXMethod,
    Namespace,
    LinkageSpec,
    Constructor,
    Destructor,
    ConversionFunction,
    TemplateTypeParameter,
    NonTypeTemplateParameter,
    TemplateTemplateParameter,
    FunctionTemplate,
    ClassTemplate,
    ClassTemplatePartialSpecialization,
    NamespaceAlias,
    UsingDirective,
    UsingDeclaration,
    TypeAliasDecl,
    ObjCSynthesizeDecl,
    ObjCDynamicDecl,
    CXXAccessSpecifier,
    ObjCSuperClassRef,
    ObjCProtocolRef,
    ObjCClassRef,
    TypeRef,
    CXXBaseSpecifier,
    TemplateRef,
    NamespaceRef,
    MemberRef,
    LabelRef,
    OverloadedDeclRef,
    VariableRef,
    InvalidFile,
    NoDeclFound,
    NotImplemented,
    InvalidCode,
    UnexposedExpr,
    DeclRefExpr,
    MemberRefExpr,
    CallExpr,
    ObjCMessageExpr,
    BlockExpr,
    IntegerLiteral,
    FloatingLiteral,
    ImaginaryLiteral,
    StringLiteral,
    CharacterLiteral,
    ParenExpr,
    UnaryOperator,
    ArraySubscriptExpr,
    BinaryOperator,
    CompoundAssignOperator,
    ConditionalOperator,
    CStyleCastExpr,
    CompoundLiteralExpr,
    InitListExpr,
    AddrLabelExpr,
    StmtExpr,
    GenericSelectionExpr,
    GNUNullExpr,
    CXXStaticCastExpr,
    CXXDynamicCastExpr,
    CXXReinterpretCastExpr,
    CXXConstCastExpr,
    CXXFunctionalCastExpr,
    CXXTypeidExpr,
    CXXBoolLiteralExpr,
    CXXNullPtrLiteralExpr,
    CXXThisExpr,
    CXXThrowExpr,
    CXXNewExpr,
    CXXDeleteExpr,
    UnaryExpr,
    ObjCStringLiteral,
    ObjCEncodeExpr,
    ObjCSelectorExpr,
    ObjCProtocolExpr,
    ObjCBridgedCastExpr,
    PackExpansionExpr,
    SizeOfPackExpr,
    LambdaExpr,
    ObjCBoolLiteralExpr,
    ObjCSelfExpr,
    OMPArraySectionExpr,
    ObjCAvailabilityCheckExpr,
    FixedPointLiteral,
    UnexposedStmt,
    LabelStmt,
    CompoundStmt,
    CaseStmt,
    DefaultStmt,
    IfStmt,
    SwitchStmt,
    WhileStmt,
    DoStmt,
    ForStmt,
    GotoStmt,
    IndirectGotoStmt,
    ContinueStmt,
    BreakStmt,
    ReturnStmt,
    AsmStmt,
    ObjCAtTryStmt,
    ObjCAtCatchStmt,
    ObjCAtFinallyStmt,
    ObjCAtThrowStmt,
    ObjCAtSynchronizedStmt,
    ObjCAutoreleasePoolStmt,
    ObjCForCollectionStmt,
    CXXCatchStmt,
    CXXTryStmt,
    CXXForRangeStmt,
    SEHTryStmt,
    SEHExceptStmt,
    SEHFinallyStmt,
    MSAsmStmt,
    NullStmt,
    DeclStmt,
    OMPParallelDirective,
    OMPSimdDirective,
    OMPForDirective,
    OMPSectionsDirective,
    OMPSectionDirective,
    OMPSingleDirective,
    OMPParallelForDirective,
    OMPParallelSectionsDirective,
    OMPTaskDirective,
    OMPMasterDirective,
    OMPCriticalDirective,
    OMPTaskyieldDirective,
    OMPBarrierDirective,
    OMPTaskwaitDirective,
    OMPFlushDirective,
    SEHLeaveStmt,
    OMPOrderedDirective,
    OMPAtomicDirective,
    OMPForSimdDirective,
    OMPParallelForSimdDirective,
    OMPTargetDirective,
    OMPTeamsDirective,
    OMPTaskgroupDirective,
    OMPCancellationPointDirective,
    OMPCancelDirective,
    OMPTargetDataDirective,
    OMPTaskLoopDirective,
    OMPTaskLoopSimdDirective,
    OMPDistributeDirective,
    OMPTargetEnterDataDirective,
    OMPTargetExitDataDirective,
    OMPTargetParallelDirective,
    OMPTargetParallelForDirective,
    OMPTargetUpdateDirective,
    OMPDistributeParallelForDirective,
    OMPDistributeParallelForSimdDirective,
    OMPDistributeSimdDirective,
    OMPTargetParallelForSimdDirective,
    OMPTargetSimdDirective,
    OMPTeamsDistributeDirective,
    OMPTeamsDistributeSimdDirective,
    OMPTeamsDistributeParallelForSimdDirective,
    OMPTeamsDistributeParallelForDirective,
    OMPTargetTeamsDirective,
    OMPTargetTeamsDistributeDirective,
    OMPTargetTeamsDistributeParallelForDirective,
    OMPTargetTeamsDistributeParallelForSimdDirective,
    OMPTargetTeamsDistributeSimdDirective,
    TranslationUnit,
    UnexposedAttr,
    IBActionAttr,
    IBOutletAttr,
    IBOutletCollectionAttr,
    CXXFinalAttr,
    CXXOverrideAttr,
    AnnotateAttr,
    AsmLabelAttr,
    PackedAttr,
    PureAttr,
    ConstAttr,
    NoDuplicateAttr,
    CUDAConstantAttr,
    CUDADeviceAttr,
    CUDAGlobalAttr,
    CUDAHostAttr,
    CUDASharedAttr,
    VisibilityAttr,
    DLLExport,
    DLLImport,
    NSReturnsRetained,
    NSReturnsNotRetained,
    NSReturnsAutoreleased,
    NSConsumesSelf,
    NSConsumed,
    ObjCException,
    ObjCNSObject,
    ObjCIndependentClass,
    ObjCPreciseLifetime,
    ObjCReturnsInnerPointer,
    ObjCRequiresSuper,
    ObjCRootClass,
    ObjCSubclassingRestricted,
    ObjCExplicitProtocolImpl,
    ObjCDesignatedInitializer,
    ObjCRuntimeVisible,
    ObjCBoxable,
    FlagEnum,
    ConvergentAttr,
    WarnUnusedAttr,
    WarnUnusedResultAttr,
    AlignedAttr,
    PreprocessingDirective,
    MacroDefinition,
    MacroExpansion,
    InclusionDirective,
    ModuleImportDecl,
    TypeAliasTemplateDecl,
    StaticAssert,
    FriendDecl,
    OverloadCandidate,
}

impl From<clang_sys::CXCursorKind> for CursorKind {
    fn from(kind: clang_sys::CXCursorKind) -> Self {
        use clang_sys::*;
        #[allow(non_upper_case_globals)]
        match kind {
            CXCursor_UnexposedDecl => CursorKind::UnexposedDecl,
            CXCursor_StructDecl => CursorKind::StructDecl,
            CXCursor_UnionDecl => CursorKind::UnionDecl,
            CXCursor_ClassDecl => CursorKind::ClassDecl,
            CXCursor_EnumDecl => CursorKind::EnumDecl,
            CXCursor_FieldDecl => CursorKind::FieldDecl,
            CXCursor_EnumConstantDecl => CursorKind::EnumConstantDecl,
            CXCursor_FunctionDecl => CursorKind::FunctionDecl,
            CXCursor_VarDecl => CursorKind::VarDecl,
            CXCursor_ParmDecl => CursorKind::ParmDecl,
            CXCursor_ObjCInterfaceDecl => CursorKind::ObjCInterfaceDecl,
            CXCursor_ObjCCategoryDecl => CursorKind::ObjCCategoryDecl,
            CXCursor_ObjCProtocolDecl => CursorKind::ObjCProtocolDecl,
            CXCursor_ObjCPropertyDecl => CursorKind::ObjCPropertyDecl,
            CXCursor_ObjCIvarDecl => CursorKind::ObjCIvarDecl,
            CXCursor_ObjCInstanceMethodDecl => CursorKind::ObjCInstanceMethodDecl,
            CXCursor_ObjCClassMethodDecl => CursorKind::ObjCClassMethodDecl,
            CXCursor_ObjCImplementationDecl => CursorKind::ObjCImplementationDecl,
            CXCursor_ObjCCategoryImplDecl => CursorKind::ObjCCategoryImplDecl,
            CXCursor_TypedefDecl => CursorKind::TypedefDecl,
            CXCursor_CXXMethod => CursorKind::CXXMethod,
            CXCursor_Namespace => CursorKind::Namespace,
            CXCursor_LinkageSpec => CursorKind::LinkageSpec,
            CXCursor_Constructor => CursorKind::Constructor,
            CXCursor_Destructor => CursorKind::Destructor,
            CXCursor_ConversionFunction => CursorKind::ConversionFunction,
            CXCursor_TemplateTypeParameter => CursorKind::TemplateTypeParameter,
            CXCursor_NonTypeTemplateParameter => CursorKind::NonTypeTemplateParameter,
            CXCursor_TemplateTemplateParameter => CursorKind::TemplateTemplateParameter,
            CXCursor_FunctionTemplate => CursorKind::FunctionTemplate,
            CXCursor_ClassTemplate => CursorKind::ClassTemplate,
            CXCursor_ClassTemplatePartialSpecialization => {
                CursorKind::ClassTemplatePartialSpecialization
            }
            CXCursor_NamespaceAlias => CursorKind::NamespaceAlias,
            CXCursor_UsingDirective => CursorKind::UsingDirective,
            CXCursor_UsingDeclaration => CursorKind::UsingDeclaration,
            CXCursor_TypeAliasDecl => CursorKind::TypeAliasDecl,
            CXCursor_ObjCSynthesizeDecl => CursorKind::ObjCSynthesizeDecl,
            CXCursor_ObjCDynamicDecl => CursorKind::ObjCDynamicDecl,
            CXCursor_CXXAccessSpecifier => CursorKind::CXXAccessSpecifier,
            CXCursor_ObjCSuperClassRef => CursorKind::ObjCSuperClassRef,
            CXCursor_ObjCProtocolRef => CursorKind::ObjCProtocolRef,
            CXCursor_ObjCClassRef => CursorKind::ObjCClassRef,
            CXCursor_TypeRef => CursorKind::TypeRef,
            CXCursor_CXXBaseSpecifier => CursorKind::CXXBaseSpecifier,
            CXCursor_TemplateRef => CursorKind::TemplateRef,
            CXCursor_NamespaceRef => CursorKind::NamespaceRef,
            CXCursor_MemberRef => CursorKind::MemberRef,
            CXCursor_LabelRef => CursorKind::LabelRef,
            CXCursor_OverloadedDeclRef => CursorKind::OverloadedDeclRef,
            CXCursor_VariableRef => CursorKind::VariableRef,
            CXCursor_InvalidFile => CursorKind::InvalidFile,
            CXCursor_NoDeclFound => CursorKind::NoDeclFound,
            CXCursor_NotImplemented => CursorKind::NotImplemented,
            CXCursor_InvalidCode => CursorKind::InvalidCode,
            CXCursor_UnexposedExpr => CursorKind::UnexposedExpr,
            CXCursor_DeclRefExpr => CursorKind::DeclRefExpr,
            CXCursor_MemberRefExpr => CursorKind::MemberRefExpr,
            CXCursor_CallExpr => CursorKind::CallExpr,
            CXCursor_ObjCMessageExpr => CursorKind::ObjCMessageExpr,
            CXCursor_BlockExpr => CursorKind::BlockExpr,
            CXCursor_IntegerLiteral => CursorKind::IntegerLiteral,
            CXCursor_FloatingLiteral => CursorKind::FloatingLiteral,
            CXCursor_ImaginaryLiteral => CursorKind::ImaginaryLiteral,
            CXCursor_StringLiteral => CursorKind::StringLiteral,
            CXCursor_CharacterLiteral => CursorKind::CharacterLiteral,
            CXCursor_ParenExpr => CursorKind::ParenExpr,
            CXCursor_UnaryOperator => CursorKind::UnaryOperator,
            CXCursor_ArraySubscriptExpr => CursorKind::ArraySubscriptExpr,
            CXCursor_BinaryOperator => CursorKind::BinaryOperator,
            CXCursor_CompoundAssignOperator => CursorKind::CompoundAssignOperator,
            CXCursor_ConditionalOperator => CursorKind::ConditionalOperator,
            CXCursor_CStyleCastExpr => CursorKind::CStyleCastExpr,
            CXCursor_CompoundLiteralExpr => CursorKind::CompoundLiteralExpr,
            CXCursor_InitListExpr => CursorKind::InitListExpr,
            CXCursor_AddrLabelExpr => CursorKind::AddrLabelExpr,
            CXCursor_StmtExpr => CursorKind::StmtExpr,
            CXCursor_GenericSelectionExpr => CursorKind::GenericSelectionExpr,
            CXCursor_GNUNullExpr => CursorKind::GNUNullExpr,
            CXCursor_CXXStaticCastExpr => CursorKind::CXXStaticCastExpr,
            CXCursor_CXXDynamicCastExpr => CursorKind::CXXDynamicCastExpr,
            CXCursor_CXXReinterpretCastExpr => CursorKind::CXXReinterpretCastExpr,
            CXCursor_CXXConstCastExpr => CursorKind::CXXConstCastExpr,
            CXCursor_CXXFunctionalCastExpr => CursorKind::CXXFunctionalCastExpr,
            CXCursor_CXXTypeidExpr => CursorKind::CXXTypeidExpr,
            CXCursor_CXXBoolLiteralExpr => CursorKind::CXXBoolLiteralExpr,
            CXCursor_CXXNullPtrLiteralExpr => CursorKind::CXXNullPtrLiteralExpr,
            CXCursor_CXXThisExpr => CursorKind::CXXThisExpr,
            CXCursor_CXXThrowExpr => CursorKind::CXXThrowExpr,
            CXCursor_CXXNewExpr => CursorKind::CXXNewExpr,
            CXCursor_CXXDeleteExpr => CursorKind::CXXDeleteExpr,
            CXCursor_UnaryExpr => CursorKind::UnaryExpr,
            CXCursor_ObjCStringLiteral => CursorKind::ObjCStringLiteral,
            CXCursor_ObjCEncodeExpr => CursorKind::ObjCEncodeExpr,
            CXCursor_ObjCSelectorExpr => CursorKind::ObjCSelectorExpr,
            CXCursor_ObjCProtocolExpr => CursorKind::ObjCProtocolExpr,
            CXCursor_ObjCBridgedCastExpr => CursorKind::ObjCBridgedCastExpr,
            CXCursor_PackExpansionExpr => CursorKind::PackExpansionExpr,
            CXCursor_SizeOfPackExpr => CursorKind::SizeOfPackExpr,
            CXCursor_LambdaExpr => CursorKind::LambdaExpr,
            CXCursor_ObjCBoolLiteralExpr => CursorKind::ObjCBoolLiteralExpr,
            CXCursor_ObjCSelfExpr => CursorKind::ObjCSelfExpr,
            CXCursor_OMPArraySectionExpr => CursorKind::OMPArraySectionExpr,
            CXCursor_ObjCAvailabilityCheckExpr => CursorKind::ObjCAvailabilityCheckExpr,
            CXCursor_FixedPointLiteral => CursorKind::FixedPointLiteral,
            CXCursor_UnexposedStmt => CursorKind::UnexposedStmt,
            CXCursor_LabelStmt => CursorKind::LabelStmt,
            CXCursor_CompoundStmt => CursorKind::CompoundStmt,
            CXCursor_CaseStmt => CursorKind::CaseStmt,
            CXCursor_DefaultStmt => CursorKind::DefaultStmt,
            CXCursor_IfStmt => CursorKind::IfStmt,
            CXCursor_SwitchStmt => CursorKind::SwitchStmt,
            CXCursor_WhileStmt => CursorKind::WhileStmt,
            CXCursor_DoStmt => CursorKind::DoStmt,
            CXCursor_ForStmt => CursorKind::ForStmt,
            CXCursor_GotoStmt => CursorKind::GotoStmt,
            CXCursor_IndirectGotoStmt => CursorKind::IndirectGotoStmt,
            CXCursor_ContinueStmt => CursorKind::ContinueStmt,
            CXCursor_BreakStmt => CursorKind::BreakStmt,
            CXCursor_ReturnStmt => CursorKind::ReturnStmt,
            CXCursor_AsmStmt => CursorKind::AsmStmt,
            CXCursor_ObjCAtTryStmt => CursorKind::ObjCAtTryStmt,
            CXCursor_ObjCAtCatchStmt => CursorKind::ObjCAtCatchStmt,
            CXCursor_ObjCAtFinallyStmt => CursorKind::ObjCAtFinallyStmt,
            CXCursor_ObjCAtThrowStmt => CursorKind::ObjCAtThrowStmt,
            CXCursor_ObjCAtSynchronizedStmt => CursorKind::ObjCAtSynchronizedStmt,
            CXCursor_ObjCAutoreleasePoolStmt => CursorKind::ObjCAutoreleasePoolStmt,
            CXCursor_ObjCForCollectionStmt => CursorKind::ObjCForCollectionStmt,
            CXCursor_CXXCatchStmt => CursorKind::CXXCatchStmt,
            CXCursor_CXXTryStmt => CursorKind::CXXTryStmt,
            CXCursor_CXXForRangeStmt => CursorKind::CXXForRangeStmt,
            CXCursor_SEHTryStmt => CursorKind::SEHTryStmt,
            CXCursor_SEHExceptStmt => CursorKind::SEHExceptStmt,
            CXCursor_SEHFinallyStmt => CursorKind::SEHFinallyStmt,
            CXCursor_MSAsmStmt => CursorKind::MSAsmStmt,
            CXCursor_NullStmt => CursorKind::NullStmt,
            CXCursor_DeclStmt => CursorKind::DeclStmt,
            CXCursor_OMPParallelDirective => CursorKind::OMPParallelDirective,
            CXCursor_OMPSimdDirective => CursorKind::OMPSimdDirective,
            CXCursor_OMPForDirective => CursorKind::OMPForDirective,
            CXCursor_OMPSectionsDirective => CursorKind::OMPSectionsDirective,
            CXCursor_OMPSectionDirective => CursorKind::OMPSectionDirective,
            CXCursor_OMPSingleDirective => CursorKind::OMPSingleDirective,
            CXCursor_OMPParallelForDirective => CursorKind::OMPParallelForDirective,
            CXCursor_OMPParallelSectionsDirective => CursorKind::OMPParallelSectionsDirective,
            CXCursor_OMPTaskDirective => CursorKind::OMPTaskDirective,
            CXCursor_OMPMasterDirective => CursorKind::OMPMasterDirective,
            CXCursor_OMPCriticalDirective => CursorKind::OMPCriticalDirective,
            CXCursor_OMPTaskyieldDirective => CursorKind::OMPTaskyieldDirective,
            CXCursor_OMPBarrierDirective => CursorKind::OMPBarrierDirective,
            CXCursor_OMPTaskwaitDirective => CursorKind::OMPTaskwaitDirective,
            CXCursor_OMPFlushDirective => CursorKind::OMPFlushDirective,
            CXCursor_SEHLeaveStmt => CursorKind::SEHLeaveStmt,
            CXCursor_OMPOrderedDirective => CursorKind::OMPOrderedDirective,
            CXCursor_OMPAtomicDirective => CursorKind::OMPAtomicDirective,
            CXCursor_OMPForSimdDirective => CursorKind::OMPForSimdDirective,
            CXCursor_OMPParallelForSimdDirective => CursorKind::OMPParallelForSimdDirective,
            CXCursor_OMPTargetDirective => CursorKind::OMPTargetDirective,
            CXCursor_OMPTeamsDirective => CursorKind::OMPTeamsDirective,
            CXCursor_OMPTaskgroupDirective => CursorKind::OMPTaskgroupDirective,
            CXCursor_OMPCancellationPointDirective => CursorKind::OMPCancellationPointDirective,
            CXCursor_OMPCancelDirective => CursorKind::OMPCancelDirective,
            CXCursor_OMPTargetDataDirective => CursorKind::OMPTargetDataDirective,
            CXCursor_OMPTaskLoopDirective => CursorKind::OMPTaskLoopDirective,
            CXCursor_OMPTaskLoopSimdDirective => CursorKind::OMPTaskLoopSimdDirective,
            CXCursor_OMPDistributeDirective => CursorKind::OMPDistributeDirective,
            CXCursor_OMPTargetEnterDataDirective => CursorKind::OMPTargetEnterDataDirective,
            CXCursor_OMPTargetExitDataDirective => CursorKind::OMPTargetExitDataDirective,
            CXCursor_OMPTargetParallelDirective => CursorKind::OMPTargetParallelDirective,
            CXCursor_OMPTargetParallelForDirective => CursorKind::OMPTargetParallelForDirective,
            CXCursor_OMPTargetUpdateDirective => CursorKind::OMPTargetUpdateDirective,
            CXCursor_OMPDistributeParallelForDirective => {
                CursorKind::OMPDistributeParallelForDirective
            }
            CXCursor_OMPDistributeParallelForSimdDirective => {
                CursorKind::OMPDistributeParallelForSimdDirective
            }
            CXCursor_OMPDistributeSimdDirective => CursorKind::OMPDistributeSimdDirective,
            CXCursor_OMPTargetParallelForSimdDirective => {
                CursorKind::OMPTargetParallelForSimdDirective
            }
            CXCursor_OMPTargetSimdDirective => CursorKind::OMPTargetSimdDirective,
            CXCursor_OMPTeamsDistributeDirective => CursorKind::OMPTeamsDistributeDirective,
            CXCursor_OMPTeamsDistributeSimdDirective => CursorKind::OMPTeamsDistributeSimdDirective,
            CXCursor_OMPTeamsDistributeParallelForSimdDirective => {
                CursorKind::OMPTeamsDistributeParallelForSimdDirective
            }
            CXCursor_OMPTeamsDistributeParallelForDirective => {
                CursorKind::OMPTeamsDistributeParallelForDirective
            }
            CXCursor_OMPTargetTeamsDirective => CursorKind::OMPTargetTeamsDirective,
            CXCursor_OMPTargetTeamsDistributeDirective => {
                CursorKind::OMPTargetTeamsDistributeDirective
            }
            CXCursor_OMPTargetTeamsDistributeParallelForDirective => {
                CursorKind::OMPTargetTeamsDistributeParallelForDirective
            }
            CXCursor_OMPTargetTeamsDistributeParallelForSimdDirective => {
                CursorKind::OMPTargetTeamsDistributeParallelForSimdDirective
            }
            CXCursor_OMPTargetTeamsDistributeSimdDirective => {
                CursorKind::OMPTargetTeamsDistributeSimdDirective
            }
            CXCursor_TranslationUnit => CursorKind::TranslationUnit,
            CXCursor_UnexposedAttr => CursorKind::UnexposedAttr,
            CXCursor_IBActionAttr => CursorKind::IBActionAttr,
            CXCursor_IBOutletAttr => CursorKind::IBOutletAttr,
            CXCursor_IBOutletCollectionAttr => CursorKind::IBOutletCollectionAttr,
            CXCursor_CXXFinalAttr => CursorKind::CXXFinalAttr,
            CXCursor_CXXOverrideAttr => CursorKind::CXXOverrideAttr,
            CXCursor_AnnotateAttr => CursorKind::AnnotateAttr,
            CXCursor_AsmLabelAttr => CursorKind::AsmLabelAttr,
            CXCursor_PackedAttr => CursorKind::PackedAttr,
            CXCursor_PureAttr => CursorKind::PureAttr,
            CXCursor_ConstAttr => CursorKind::ConstAttr,
            CXCursor_NoDuplicateAttr => CursorKind::NoDuplicateAttr,
            CXCursor_CUDAConstantAttr => CursorKind::CUDAConstantAttr,
            CXCursor_CUDADeviceAttr => CursorKind::CUDADeviceAttr,
            CXCursor_CUDAGlobalAttr => CursorKind::CUDAGlobalAttr,
            CXCursor_CUDAHostAttr => CursorKind::CUDAHostAttr,
            CXCursor_CUDASharedAttr => CursorKind::CUDASharedAttr,
            CXCursor_VisibilityAttr => CursorKind::VisibilityAttr,
            CXCursor_DLLExport => CursorKind::DLLExport,
            CXCursor_DLLImport => CursorKind::DLLImport,
            CXCursor_NSReturnsRetained => CursorKind::NSReturnsRetained,
            CXCursor_NSReturnsNotRetained => CursorKind::NSReturnsNotRetained,
            CXCursor_NSReturnsAutoreleased => CursorKind::NSReturnsAutoreleased,
            CXCursor_NSConsumesSelf => CursorKind::NSConsumesSelf,
            CXCursor_NSConsumed => CursorKind::NSConsumed,
            CXCursor_ObjCException => CursorKind::ObjCException,
            CXCursor_ObjCNSObject => CursorKind::ObjCNSObject,
            CXCursor_ObjCIndependentClass => CursorKind::ObjCIndependentClass,
            CXCursor_ObjCPreciseLifetime => CursorKind::ObjCPreciseLifetime,
            CXCursor_ObjCReturnsInnerPointer => CursorKind::ObjCReturnsInnerPointer,
            CXCursor_ObjCRequiresSuper => CursorKind::ObjCRequiresSuper,
            CXCursor_ObjCRootClass => CursorKind::ObjCRootClass,
            CXCursor_ObjCSubclassingRestricted => CursorKind::ObjCSubclassingRestricted,
            CXCursor_ObjCExplicitProtocolImpl => CursorKind::ObjCExplicitProtocolImpl,
            CXCursor_ObjCDesignatedInitializer => CursorKind::ObjCDesignatedInitializer,
            CXCursor_ObjCRuntimeVisible => CursorKind::ObjCRuntimeVisible,
            CXCursor_ObjCBoxable => CursorKind::ObjCBoxable,
            CXCursor_FlagEnum => CursorKind::FlagEnum,
            CXCursor_ConvergentAttr => CursorKind::ConvergentAttr,
            CXCursor_WarnUnusedAttr => CursorKind::WarnUnusedAttr,
            CXCursor_WarnUnusedResultAttr => CursorKind::WarnUnusedResultAttr,
            CXCursor_AlignedAttr => CursorKind::AlignedAttr,
            CXCursor_PreprocessingDirective => CursorKind::PreprocessingDirective,
            CXCursor_MacroDefinition => CursorKind::MacroDefinition,
            CXCursor_MacroExpansion => CursorKind::MacroExpansion,
            CXCursor_InclusionDirective => CursorKind::InclusionDirective,
            CXCursor_ModuleImportDecl => CursorKind::ModuleImportDecl,
            CXCursor_TypeAliasTemplateDecl => CursorKind::TypeAliasTemplateDecl,
            CXCursor_StaticAssert => CursorKind::StaticAssert,
            CXCursor_FriendDecl => CursorKind::FriendDecl,
            CXCursor_OverloadCandidate => CursorKind::OverloadCandidate,
            _ => panic!("unknown cursor kind {:?}", kind),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TypeKind {
    Unexposed,
    Void,
    Bool,
    CharU,
    UChar,
    Char16,
    Char32,
    UShort,
    UInt,
    ULong,
    ULongLong,
    UInt128,
    CharS,
    SChar,
    WChar,
    Short,
    Int,
    Long,
    LongLong,
    Int128,
    Float,
    Double,
    LongDouble,
    NullPtr,
    Overload,
    Dependent,
    ObjCId,
    ObjCClass,
    ObjCSel,
    Float128,
    Half,
    Float16,
    ShortAccum,
    Accum,
    LongAccum,
    UShortAccum,
    UAccum,
    ULongAccum,
    Complex,
    Pointer,
    BlockPointer,
    LValueReference,
    RValueReference,
    Record,
    Enum,
    Typedef,
    ObjCInterface,
    ObjCObjectPointer,
    FunctionNoProto,
    FunctionProto,
    ConstantArray,
    Vector,
    IncompleteArray,
    VariableArray,
    DependentSizedArray,
    MemberPointer,
    Auto,
    Elaborated,
    Pipe,
    ObjCObject,
    ObjCTypeParam,
    Attributed,
    ExtVector,
}

impl From<clang_sys::CXTypeKind> for TypeKind {
    fn from(kind: clang_sys::CXTypeKind) -> Self {
        use clang_sys::*;
        #[allow(non_upper_case_globals)]
        match kind {
            CXType_Unexposed => TypeKind::Unexposed,
            CXType_Void => TypeKind::Void,
            CXType_Bool => TypeKind::Bool,
            CXType_Char_U => TypeKind::CharU,
            CXType_UChar => TypeKind::UChar,
            CXType_Char16 => TypeKind::Char16,
            CXType_Char32 => TypeKind::Char32,
            CXType_UShort => TypeKind::UShort,
            CXType_UInt => TypeKind::UInt,
            CXType_ULong => TypeKind::ULong,
            CXType_ULongLong => TypeKind::ULongLong,
            CXType_UInt128 => TypeKind::UInt128,
            CXType_Char_S => TypeKind::CharS,
            CXType_SChar => TypeKind::SChar,
            CXType_WChar => TypeKind::WChar,
            CXType_Short => TypeKind::Short,
            CXType_Int => TypeKind::Int,
            CXType_Long => TypeKind::Long,
            CXType_LongLong => TypeKind::LongLong,
            CXType_Int128 => TypeKind::Int128,
            CXType_Float => TypeKind::Float,
            CXType_Double => TypeKind::Double,
            CXType_LongDouble => TypeKind::LongDouble,
            CXType_NullPtr => TypeKind::NullPtr,
            CXType_Overload => TypeKind::Overload,
            CXType_Dependent => TypeKind::Dependent,
            CXType_ObjCId => TypeKind::ObjCId,
            CXType_ObjCClass => TypeKind::ObjCClass,
            CXType_ObjCSel => TypeKind::ObjCSel,
            CXType_Float128 => TypeKind::Float128,
            CXType_Half => TypeKind::Half,
            CXType_Float16 => TypeKind::Float16,
            CXType_ShortAccum => TypeKind::ShortAccum,
            CXType_Accum => TypeKind::Accum,
            CXType_LongAccum => TypeKind::LongAccum,
            CXType_UShortAccum => TypeKind::UShortAccum,
            CXType_UAccum => TypeKind::UAccum,
            CXType_ULongAccum => TypeKind::ULongAccum,
            CXType_Complex => TypeKind::Complex,
            CXType_Pointer => TypeKind::Pointer,
            CXType_BlockPointer => TypeKind::BlockPointer,
            CXType_LValueReference => TypeKind::LValueReference,
            CXType_RValueReference => TypeKind::RValueReference,
            CXType_Record => TypeKind::Record,
            CXType_Enum => TypeKind::Enum,
            CXType_Typedef => TypeKind::Typedef,
            CXType_ObjCInterface => TypeKind::ObjCInterface,
            CXType_ObjCObjectPointer => TypeKind::ObjCObjectPointer,
            CXType_FunctionNoProto => TypeKind::FunctionNoProto,
            CXType_FunctionProto => TypeKind::FunctionProto,
            CXType_ConstantArray => TypeKind::ConstantArray,
            CXType_Vector => TypeKind::Vector,
            CXType_IncompleteArray => TypeKind::IncompleteArray,
            CXType_VariableArray => TypeKind::VariableArray,
            CXType_DependentSizedArray => TypeKind::DependentSizedArray,
            CXType_MemberPointer => TypeKind::MemberPointer,
            CXType_Auto => TypeKind::Auto,
            CXType_Elaborated => TypeKind::Elaborated,
            CXType_Pipe => TypeKind::Pipe,
            CXType_ObjCObject => TypeKind::ObjCObject,
            CXType_ObjCTypeParam => TypeKind::ObjCTypeParam,
            CXType_Attributed => TypeKind::Attributed,
            CXType_ExtVector => TypeKind::ExtVector,
            _ => panic!("unknown type kind {:?}", kind),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Availability {
    Available,
    Deprecated,
    NotAvailable,
    NotAccessible,
}

impl From<clang_sys::CXAvailabilityKind> for Availability {
    fn from(availability: clang_sys::CXAvailabilityKind) -> Self {
        use clang_sys::*;
        #[allow(non_upper_case_globals)]
        match availability {
            CXAvailability_Available => Availability::Available,
            CXAvailability_Deprecated => Availability::Deprecated,
            CXAvailability_NotAvailable => Availability::NotAvailable,
            CXAvailability_NotAccessible => Availability::NotAccessible,
            _ => panic!("invalid availability {:?}", availability),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ChildVisitResult {
    Break,
    Continue,
    Recurse,
}

impl From<ChildVisitResult> for clang_sys::CXChildVisitResult {
    fn from(result: ChildVisitResult) -> Self {
        use clang_sys::*;
        match result {
            ChildVisitResult::Break => CXChildVisit_Break,
            ChildVisitResult::Continue => CXChildVisit_Continue,
            ChildVisitResult::Recurse => CXChildVisit_Recurse,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum VisitorResult {
    Break,
    Continue,
}

impl From<VisitorResult> for clang_sys::CXVisitorResult {
    fn from(result: VisitorResult) -> Self {
        use clang_sys::*;
        match result {
            VisitorResult::Break => CXVisit_Break,
            VisitorResult::Continue => CXVisit_Continue,
        }
    }
}

bitflags! {
    pub struct ObjCAttributes: i32 {
        const READONLY = clang_sys::CXObjCPropertyAttr_readonly;
        const GETTER = clang_sys::CXObjCPropertyAttr_getter;
        const ASSIGN = clang_sys::CXObjCPropertyAttr_assign;
        const READWRITE = clang_sys::CXObjCPropertyAttr_readwrite;
        const RETAIN = clang_sys::CXObjCPropertyAttr_retain;
        const COPY = clang_sys::CXObjCPropertyAttr_copy;
        const NONATOMIC = clang_sys::CXObjCPropertyAttr_nonatomic;
        const SETTER = clang_sys::CXObjCPropertyAttr_setter;
        const ATOMIC = clang_sys::CXObjCPropertyAttr_atomic;
        const WEAK = clang_sys::CXObjCPropertyAttr_weak;
        const STRONG = clang_sys::CXObjCPropertyAttr_strong;
        const UNSAFE_UNRETAINED = clang_sys::CXObjCPropertyAttr_unsafe_unretained;
        const CLASS = clang_sys::CXObjCPropertyAttr_class;
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum StorageClass {
    Extern,
    Static,
    PrivateExtern,
    Auto,
    Register,
}

impl StorageClass {
    fn from_raw(storage_class: clang_sys::CX_StorageClass) -> Option<Self> {
        use clang_sys::*;
        #[allow(non_upper_case_globals)]
        match storage_class {
            CX_SC_Invalid | CX_SC_None => None,
            CX_SC_Extern => Some(StorageClass::Extern),
            CX_SC_Static => Some(StorageClass::Static),
            CX_SC_PrivateExtern => Some(StorageClass::PrivateExtern),
            CX_SC_Auto => Some(StorageClass::Auto),
            CX_SC_Register => Some(StorageClass::Register),
            _ => panic!("unknown storage class {:?}", storage_class),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LinkageKind {
    Internal,
    UniqueExternal,
    External,
}

impl LinkageKind {
    fn from_raw(raw: clang_sys::CXLinkageKind) -> Option<Self> {
        use clang_sys::*;
        #[allow(non_upper_case_globals)]
        match raw {
            CXLinkage_Invalid | CXLinkage_NoLinkage => None,
            CXLinkage_Internal => Some(Self::Internal),
            CXLinkage_UniqueExternal => Some(Self::UniqueExternal),
            CXLinkage_External => Some(Self::External),
            _ => panic!("invalid linkage kind {:?}", raw),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PlatformAvailability {
    pub platform: String,
    pub introduced: Option<Version>,
    pub deprecated: Option<Version>,
    pub obsoleted: Option<Version>,
    pub unavailable: bool,
    pub message: Option<String>,
}

impl PlatformAvailability {
    fn from_raw(mut raw: clang_sys::CXPlatformAvailability) -> Self {
        let availability = PlatformAvailability {
            platform: to_string(raw.Platform).expect("invalid string"),
            introduced: Version::from_raw(raw.Introduced),
            deprecated: Version::from_raw(raw.Deprecated),
            obsoleted: Version::from_raw(raw.Obsoleted),
            unavailable: raw.Unavailable != 0,
            message: none_if_empty(to_string(raw.Message)),
        };
        unsafe {
            clang_sys::clang_disposeCXPlatformAvailability(&mut raw);
        }
        availability
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Version {
    Major {
        major: u32,
    },
    Minor {
        major: u32,
        minor: u32,
    },
    Subminor {
        major: u32,
        minor: u32,
        subminor: u32,
    },
}

impl Version {
    fn from_raw(raw: clang_sys::CXVersion) -> Option<Self> {
        if raw.Major == -1 {
            assert_eq!(raw.Minor, -1);
            assert_eq!(raw.Subminor, -1);
            None
        } else if raw.Minor == -1 {
            assert_eq!(raw.Subminor, -1);
            Some(Version::Major {
                major: raw.Major.try_into().unwrap(),
            })
        } else if raw.Subminor == -1 {
            Some(Version::Minor {
                major: raw.Major.try_into().unwrap(),
                minor: raw.Minor.try_into().unwrap(),
            })
        } else {
            Some(Version::Subminor {
                major: raw.Major.try_into().unwrap(),
                minor: raw.Minor.try_into().unwrap(),
                subminor: raw.Subminor.try_into().unwrap(),
            })
        }
    }
}

#[derive(Copy, Clone)]
pub struct SourceLocation<'a> {
    raw: clang_sys::CXSourceLocation,
    tu: &'a TranslationUnit<'a>,
}

impl<'a> SourceLocation<'a> {
    fn from_raw(raw: clang_sys::CXSourceLocation, tu: &'a TranslationUnit<'a>) -> Option<Self> {
        unsafe {
            if clang_sys::clang_equalLocations(raw, clang_sys::clang_getNullLocation()) == 0 {
                let location = SourceLocation { raw, tu };
                let file_location = location.file_location();
                // For some reason clang_equalLocations sometimes does not seem to work properly so do it ourselves.
                if file_location.file.is_none()
                    && file_location.line == 0
                    && file_location.column == 0
                    && file_location.offset == 0
                {
                    None
                } else {
                    Some(location)
                }
            } else {
                None
            }
        }
    }

    pub fn file_location(&self) -> Location<'a> {
        let mut file: clang_sys::CXFile = std::ptr::null_mut();
        let mut line: u32 = 0;
        let mut column: u32 = 0;
        let mut offset: u32 = 0;
        unsafe {
            clang_sys::clang_getFileLocation(
                self.raw,
                &mut file,
                &mut line,
                &mut column,
                &mut offset,
            );
        }
        let file = File::from_ptr(file, self.tu);
        Location {
            file,
            line,
            column,
            offset,
        }
    }

    pub fn presumed_location(&self) -> PresumedLocation {
        let mut file: clang_sys::CXString = clang_sys::CXString::default();
        let mut line: u32 = 0;
        let mut column: u32 = 0;
        unsafe {
            clang_sys::clang_getPresumedLocation(self.raw, &mut file, &mut line, &mut column);
        }
        let file = into_string(file).expect("invalid file name");
        PresumedLocation { file, line, column }
    }

    pub fn spelling_location(&self) -> Location<'_> {
        let mut file: clang_sys::CXFile = std::ptr::null_mut();
        let mut line: u32 = 0;
        let mut column: u32 = 0;
        let mut offset: u32 = 0;
        unsafe {
            clang_sys::clang_getSpellingLocation(
                self.raw,
                &mut file,
                &mut line,
                &mut column,
                &mut offset,
            );
        }
        let file = File::from_ptr(file, self.tu);
        Location {
            file,
            line,
            column,
            offset,
        }
    }

    pub fn cursor(&self) -> Option<Cursor<'a>> {
        let raw = unsafe { clang_sys::clang_getCursor(self.tu.ptr, self.raw) };
        Cursor::from_raw(raw, self.tu)
    }

    pub fn range_to(&self, end: Self) -> SourceRange<'a> {
        assert_eq!(self.tu, end.tu);
        SourceRange::from_raw(
            unsafe { clang_sys::clang_getRange(self.raw, end.raw) },
            self.tu,
        )
        .expect("invalid range")
    }
}

impl PartialEq for SourceLocation<'_> {
    fn eq(&self, other: &Self) -> bool {
        unsafe { clang_sys::clang_equalLocations(self.raw, other.raw) != 0 }
    }
}

impl std::fmt::Debug for SourceLocation<'_> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let location = self.spelling_location();
        formatter
            .debug_struct("SourceLocation")
            .field("file", &location.file)
            .field("line", &location.line)
            .field("column", &location.column)
            .field("offset", &location.offset)
            .finish()
    }
}

pub struct SourceRange<'a> {
    raw: clang_sys::CXSourceRange,
    tu: &'a TranslationUnit<'a>,
}

impl<'a> SourceRange<'a> {
    fn from_raw(raw: clang_sys::CXSourceRange, tu: &'a TranslationUnit<'a>) -> Option<Self> {
        unsafe {
            if clang_sys::clang_Range_isNull(raw) == 0 {
                Some(SourceRange { raw, tu })
            } else {
                None
            }
        }
    }

    pub fn start(&self) -> SourceLocation<'a> {
        SourceLocation::from_raw(unsafe { clang_sys::clang_getRangeStart(self.raw) }, self.tu)
            .expect("unexpected null location")
    }

    pub fn end(&self) -> SourceLocation<'a> {
        SourceLocation::from_raw(unsafe { clang_sys::clang_getRangeEnd(self.raw) }, self.tu)
            .expect("unexpected null location")
    }

    pub fn tokenize(&self) -> Vec<Token<'a>> {
        unsafe {
            let mut raw_tokens_ptr: *mut clang_sys::CXToken = std::ptr::null_mut();
            let mut len: u32 = 0;
            clang_sys::clang_tokenize(self.tu.ptr, self.raw, &mut raw_tokens_ptr, &mut len);
            let raw_tokens = std::slice::from_raw_parts(raw_tokens_ptr, len as _);
            let tokens = raw_tokens
                .iter()
                .map(|t| Token::from_raw(*t, self.tu))
                .collect();
            clang_sys::clang_disposeTokens(self.tu.ptr, raw_tokens_ptr, len);
            tokens
        }
    }
}

impl std::cmp::PartialEq for SourceRange<'_> {
    fn eq(&self, other: &SourceRange<'_>) -> bool {
        unsafe { clang_sys::clang_equalRanges(self.raw, other.raw) != 0 }
    }
}

impl std::fmt::Debug for SourceRange<'_> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("SourceRange")
            .field("start", &self.start())
            .field("end", &self.end())
            .finish()
    }
}

pub struct Location<'a> {
    pub file: Option<File<'a>>,
    pub line: u32,
    pub column: u32,
    pub offset: u32,
}

pub struct PresumedLocation {
    pub file: String,
    pub line: u32,
    pub column: u32,
}

pub struct File<'a> {
    ptr: clang_sys::CXFile,
    tu: &'a TranslationUnit<'a>,
}

impl<'a> File<'a> {
    fn from_ptr(ptr: clang_sys::CXFile, tu: &'a TranslationUnit<'a>) -> Option<Self> {
        if ptr.is_null() {
            None
        } else {
            Some(File { ptr, tu })
        }
    }

    pub fn file_name(&self) -> String {
        into_string(unsafe { clang_sys::clang_getFileName(self.ptr) }).expect("invalid string")
    }

    pub fn id(&self) -> (u64, u64, u64) {
        unsafe {
            let mut id = clang_sys::CXFileUniqueID::default();
            clang_sys::clang_getFileUniqueID(self.ptr, &mut id);
            (id.data[0] as u64, id.data[1] as u64, id.data[2] as u64)
        }
    }

    pub fn location(&self, line: u32, column: u32) -> Option<SourceLocation<'a>> {
        SourceLocation::from_raw(
            unsafe { clang_sys::clang_getLocation(self.tu.ptr, self.ptr, line, column) },
            self.tu,
        )
    }

    pub fn location_for_offset(&self, offset: u32) -> Option<SourceLocation<'a>> {
        SourceLocation::from_raw(
            unsafe { clang_sys::clang_getLocationForOffset(self.tu.ptr, self.ptr, offset) },
            self.tu,
        )
    }
}

impl std::cmp::PartialEq for File<'_> {
    fn eq(&self, other: &File<'_>) -> bool {
        self.id() == other.id()
    }
}

impl std::fmt::Debug for File<'_> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("File")
            .field("file_name", &self.file_name())
            .finish()
    }
}

#[derive(Copy, Clone)]
pub struct Token<'a> {
    raw: clang_sys::CXToken,
    tu: &'a TranslationUnit<'a>,
}

impl<'a> Token<'a> {
    fn from_raw(raw: clang_sys::CXToken, tu: &'a TranslationUnit<'a>) -> Self {
        Token { raw, tu }
    }

    pub fn kind(&self) -> TokenKind {
        TokenKind::from_raw(unsafe { clang_sys::clang_getTokenKind(self.raw) })
    }

    pub fn spelling(&self) -> String {
        into_string(unsafe { clang_sys::clang_getTokenSpelling(self.tu.ptr, self.raw) })
            .expect("invalid string")
    }

    pub fn location(&self) -> SourceLocation<'_> {
        SourceLocation::from_raw(
            unsafe { clang_sys::clang_getTokenLocation(self.tu.ptr, self.raw) },
            self.tu,
        )
        .expect("invalid location")
    }

    pub fn extent(&self) -> SourceRange<'a> {
        SourceRange::from_raw(
            unsafe { clang_sys::clang_getTokenExtent(self.tu.ptr, self.raw) },
            self.tu,
        )
        .expect("invalid extent")
    }
}

impl std::fmt::Debug for Token<'_> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("Token")
            .field("kind", &self.kind())
            .field("spelling", &self.spelling())
            .field("extent", &self.extent())
            .finish()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Punctuation,
    Keyword,
    Identifier,
    Literal,
    Comment,
}

impl TokenKind {
    fn from_raw(raw: clang_sys::CXTokenKind) -> Self {
        use clang_sys::*;
        #[allow(non_upper_case_globals)]
        match raw {
            CXToken_Punctuation => TokenKind::Punctuation,
            CXToken_Keyword => TokenKind::Keyword,
            CXToken_Identifier => TokenKind::Identifier,
            CXToken_Literal => TokenKind::Literal,
            CXToken_Comment => TokenKind::Comment,
            _ => panic!("invalid token kind {:?}", raw),
        }
    }
}

pub struct Diagnostic<'a> {
    ptr: clang_sys::CXDiagnostic,
    tu: &'a TranslationUnit<'a>,
}

impl<'a> Diagnostic<'a> {
    fn from_ptr(ptr: clang_sys::CXDiagnostic, tu: &'a TranslationUnit<'a>) -> Option<Self> {
        if ptr.is_null() {
            None
        } else {
            Some(Diagnostic { ptr, tu })
        }
    }

    pub fn severity(&self) -> DiagnosticSeverity {
        DiagnosticSeverity::from_raw(unsafe { clang_sys::clang_getDiagnosticSeverity(self.ptr) })
    }

    pub fn spelling(&self) -> String {
        into_string(unsafe { clang_sys::clang_getDiagnosticSpelling(self.ptr) })
            .expect("invalid string")
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Ignored,
    Note,
    Warning,
    Error,
    Fatal,
}

impl DiagnosticSeverity {
    fn from_raw(raw: clang_sys::CXDiagnosticSeverity) -> Self {
        use clang_sys::*;
        #[allow(non_upper_case_globals)]
        match raw {
            CXDiagnostic_Ignored => DiagnosticSeverity::Ignored,
            CXDiagnostic_Note => DiagnosticSeverity::Note,
            CXDiagnostic_Warning => DiagnosticSeverity::Warning,
            CXDiagnostic_Error => DiagnosticSeverity::Error,
            CXDiagnostic_Fatal => DiagnosticSeverity::Fatal,
            _ => panic!("invalid severity {:?}", raw),
        }
    }
}
