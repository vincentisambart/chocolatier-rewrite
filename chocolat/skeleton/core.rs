use std::ptr::NonNull;

#[repr(C)]
pub struct ObjCObject {
    _private: [u8; 0],
}

#[repr(C)]
pub struct ObjCClass {
    _private: [u8; 0],
}

// ARC runtime support - https://clang.llvm.org/docs/AutomaticReferenceCounting.html#runtime-support
#[link(name = "objc", kind = "dylib")]
extern "C" {
    // fn objc_autoreleasePoolPush() -> *const ffi::c_void;
    // fn objc_autoreleasePoolPop(pool: *const ffi::c_void);
    fn objc_release(value: *mut ObjCObject);
    fn objc_retain(value: *mut ObjCObject) -> *mut ObjCObject;
    pub fn objc_getClass(name: *const u8) -> *mut ObjCClass;
}

pub trait ObjCPtr: Sized {
    fn class() -> NonNull<ObjCClass>;
    unsafe fn from_raw_unchecked(ptr: NonNull<ObjCObject>) -> Self;
    fn as_raw(&self) -> NonNull<ObjCObject>;
}

#[repr(transparent)]
pub struct UntypedObjCPtr {
    raw: NonNull<ObjCObject>,
}

impl UntypedObjCPtr {
    pub unsafe fn from_raw_unchecked(raw: NonNull<ObjCObject>) -> Self {
        Self { raw }
    }

    pub fn as_raw(&self) -> NonNull<ObjCObject> {
        self.raw
    }
}

impl Drop for UntypedObjCPtr {
    fn drop(&mut self) {
        unsafe {
            objc_release(self.as_raw().as_ptr());
        }
    }
}

impl Clone for UntypedObjCPtr {
    fn clone(&self) -> Self {
        let raw_self = self.as_raw().as_ptr();
        let ptr_ret = unsafe { objc_retain(raw_self) };
        debug_assert_eq!(
            ptr_ret, raw_self,
            "objc_retain should not change the pointer, only increment the ref count"
        );
        let raw_ret = NonNull::new(ptr_ret).expect("objc_retain should not return a null pointer");
        unsafe { Self::from_raw_unchecked(raw_ret) }
    }
}

#[objc_protocol]
pub trait NSObjectProtocol {
    fn hash(&self) -> usize {
        unsafe { objc!(self.hash) }
    }
    fn is_equal(&self, obj: Option<&impl ObjCPtr>) -> bool {
        unsafe { objc!([self isEqual:obj]) }
    }
    fn description(&self) -> Option<NSString> {
        unsafe { objc!(self.description) }
    }
}

#[objc_interface]
pub trait NSObjectInterface {
    fn new() -> Self {
        objc!([self new])
    }
}

#[objc_interface]
pub struct NSObject;
