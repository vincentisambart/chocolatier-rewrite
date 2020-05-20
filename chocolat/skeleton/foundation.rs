#[objc_enum]
pub struct NSStringEncoding;

#[objc_enum]
impl NSStringEncoding {
    pub const ASCII: Self = NSASCIIStringEncoding;
    pub const UTF8: Self = NSUTF8StringEncoding;
    // Testing a strange use of some use of enums akin to categorys in Cocoa.
    pub const PROPRIETARY: Self = NSProprietaryStringEncoding;
}

#[objc_interface]
pub trait NSStringInterface {
    fn to_string(&self) -> Result<String, std::str::Utf8Error> {
        let raw_self = self.as_raw().as_ptr();
        let cstr = unsafe {
            let bytes = objc!(self.UTF8String);
            ffi::CStr::from_ptr(bytes)
        };
        Ok(cstr.to_str()?.to_string())
    }

    fn char_at(&self, index: usize) -> u16 {
        objc!([self charAtIndex:index])
    }

    fn len(&self) -> usize {
        objc!(self.length)
    }
}

#[objc_interface]
impl NSString {
    pub fn new_with_str(text: &str) -> Self {
        let bytes = text.as_ptr();
        let len = text.len();
        let encoding = NSStringEncoding::UTF8;
        unsafe {
            Self::from_raw_unchecked(
                objc!([[Self alloc] initWithBytes:bytes length:len encoding:encoding]),
            )
        }
    }
}

#[objc_interface]
pub trait NSArrayInterface<T: ObjCPtr> {
    fn first(&self) -> Option<T> {
        unsafe { objc!(self.firstObject) }
    }
    fn last(&self) -> Option<T> {
        unsafe { objc!(self.lastObject) }
    }
    fn object_at(&self, index: usize) -> T {
        unsafe { objc![self objectAtIndex:index] }
    }
    fn count(&self) -> usize {
        unsafe { objc![self count] }
    }
    fn adding_object<U: Into<T> + ObjCPtr>(&self, object: &U) -> NSArray<T> {
        unsafe { objc![self arrayByAddingObject:object] }
    }
    fn enumerate_objects<F: FnMut(T, usize, &mut bool) + Clone>(&self, f: F) {
        // TODO: thread check should maybe be done at the runtime level if block not Send + Sync?
        #[cfg(debug_assertions)]
        let thread_id = thread::current().id();
        let wrapper = move |obj, idx, stop_ptr| {
            debug_assert!(thread::current().id() == thread_id);
            let mut stop: bool = false;
            block(objc, idx, stop);
            if stop {
                *stop_ptr = 1;
            }
        };
        unsafe { very_unsafe_objc![self enumerateObjectsUsingBlock:block] }
    }
}

#[objc_interface]
pub struct NSArray<T: ObjCPtr> {
    ptr: UntypedObjCPtr,
    _marker: std::marker::PhantomData<T>,
}

#[objc_interface]
impl<T: ObjCPtr> NSArray<T> {
    pub fn new() -> Self {
        unsafe { objc![Self new] }
    }
}

#[objc_interface]
pub trait NSMutableArrayInterface<T: ObjCPtr> {
    fn add_object<U: Into<T> + ObjCPtr>(&self, object: &U) {
        unsafe { objc![Self addObject:U] }
    }

    fn insert_object_at<U: Into<T> + ObjCPtr>(&self, object: &U, index: usize) {
        unsafe { objc![Self insertObject:object atIndex:index] }
    }
}

#[objc_interface]
pub struct NSMutableArray<T: ObjCPtr> {
    ptr: UntypedObjCPtr,
    _marker: std::marker::PhantomData<T>,
}

#[objc_interface]
impl<T: ObjCPtr> NSMutableArray<T> {
    pub fn new() -> Self {
        unsafe { objc!([Self new]) }
    }
}
