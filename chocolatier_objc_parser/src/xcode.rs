use once_cell::sync::Lazy;
use std::borrow::Cow;
use std::process::Command;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Sdk {
    MacOs,
    IOs,
    IOsSimulator,
    TvOs,
    TvOsSimulator,
    WatchOs,
    WatchOsSimulator,
}

impl Sdk {
    pub fn sdk_name(self) -> &'static str {
        match self {
            Self::MacOs => "macosx",
            Self::IOs => "iphoneos",
            Self::IOsSimulator => "iphonesimulator",
            Self::TvOs => "appletvos",
            Self::TvOsSimulator => "appletvsimulator",
            Self::WatchOs => "watchos",
            Self::WatchOsSimulator => "watchsimulator",
        }
    }
}

pub fn sdk_path(sdk: Sdk) -> String {
    let output = Command::new("xcrun")
        .args(&["--sdk", sdk.sdk_name(), "--show-sdk-path"])
        .output()
        .expect("xcrun command failed to start");
    assert!(output.status.success());
    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Target {
    MacOsX86_64,
    IOsArm64,
    IOsSimulatorX86_64,
}

impl Target {
    const MACOS_X86_64_TRIPLE: &'static str = "x86_64-apple-macos";
    const IOS_ARM64_TRIPLE: &'static str = "arm64-apple-ios";
    const IOS_SIMULATOR_X86_64_TRIPLE: &'static str = "x86_64-apple-ios-simulator";

    /// Triple passed to clang.
    pub fn triple(self) -> &'static str {
        match self {
            Self::MacOsX86_64 => Self::MACOS_X86_64_TRIPLE,
            Self::IOsArm64 => Self::IOS_ARM64_TRIPLE,
            Self::IOsSimulatorX86_64 => Self::IOS_SIMULATOR_X86_64_TRIPLE,
        }
    }

    fn sdk(self) -> Sdk {
        match self {
            Self::MacOsX86_64 => Sdk::MacOs,
            Self::IOsArm64 => Sdk::IOs,
            Self::IOsSimulatorX86_64 => Sdk::IOsSimulator,
        }
    }

    /// All variants of the enum, used by examples to specify the target.
    /// Use the same values as Target::triple().
    pub fn variants() -> Vec<&'static str> {
        [Self::MacOsX86_64, Self::IOsArm64, Self::IOsSimulatorX86_64]
            .iter()
            .map(|target| target.triple())
            .collect()
    }
}

impl std::str::FromStr for Target {
    type Err = &'static str;

    /// Expecting same values as the ones returned by Target::triple().
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            Self::MACOS_X86_64_TRIPLE => Ok(Self::MacOsX86_64),
            Self::IOS_ARM64_TRIPLE => Ok(Self::IOsArm64),
            Self::IOS_SIMULATOR_X86_64_TRIPLE => Ok(Self::IOsSimulatorX86_64),
            _ => Err("unknown target"),
        }
    }
}

pub fn libclang_path() -> std::path::PathBuf {
    // Instead of hard-coding it, using `xcodebuild -find-library libclang.dylib` might be better?
    let path = std::path::Path::new(DEVELOPER_DIR.as_str())
        .join("Toolchains/XcodeDefault.xctoolchain/usr/lib/libclang.dylib");
    assert!(
        path.exists(),
        "Could not find libclang at {}",
        path.to_string_lossy()
    );
    path
}

pub static DEVELOPER_DIR: Lazy<String> = Lazy::new(|| {
    let output = Command::new("xcode-select")
        .args(&["--print-path"])
        .output()
        .expect("xcode-select command failed to start");
    assert!(output.status.success());
    String::from_utf8_lossy(&output.stdout).trim().to_string()
});

pub fn clang_options(target: Target) -> Vec<Cow<'static, str>> {
    vec![
        // Our temporary files don't have an Objective-C extension so set the language explicitely
        // (for some reason -ObjC doesn't seem to work properly)
        Cow::Borrowed("-x"),
        Cow::Borrowed("objective-c"),
        // We want ARC (Automatic Reference Counting) ON.
        Cow::Borrowed("-fobjc-arc"),
        Cow::Borrowed("-isysroot"),
        Cow::Owned(sdk_path(target.sdk())),
        Cow::Borrowed("-target"),
        Cow::Borrowed(target.triple()),
    ]
}

pub fn preprocess_objc(source: &str, target: Target) -> String {
    use std::io::Write;
    use std::process::Stdio;

    let clang_opts = clang_options(target);
    // "-E" means only do preprocessing.
    let mut xcrun_args = vec!["clang", "-E"];
    xcrun_args.extend(clang_opts.iter().map(|opt| opt.as_ref()));
    xcrun_args.push("-"); // read from stdin
    let mut child = Command::new("xcrun")
        .args(&xcrun_args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("clang command failed to start");

    {
        let stdin = child.stdin.as_mut().expect("Failed to open stdin");
        stdin
            .write_all(source.as_bytes())
            .expect("Failed to write to stdin");
    }

    let output = child.wait_with_output().expect("Failed to read stdout");
    assert!(output.status.success());
    String::from_utf8_lossy(&output.stdout).to_string()
}
