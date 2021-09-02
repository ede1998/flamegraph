use std::path::PathBuf;

use anyhow::{anyhow, Context};
use cargo_metadata::{camino::Utf8PathBuf, Artifact, Message, MetadataCommand, Package};
use structopt::StructOpt;

use flamegraph::Workload;

#[derive(Debug, StructOpt)]
#[structopt(
    setting = structopt::clap::AppSettings::TrailingVarArg
)]
struct Opt {
    /// Build with the dev profile
    #[structopt(long = "dev")]
    dev: bool,

    /// package with the binary to run
    #[structopt(short = "p", long = "package")]
    package: Option<String>,

    /// Binary to run
    #[structopt(
        short = "b",
        long = "bin",
        conflicts_with = "bench",
        conflicts_with = "unit-test",
        conflicts_with = "example",
        conflicts_with = "test"
    )]
    bin: Option<String>,

    /// Example to run
    #[structopt(
        long = "example",
        conflicts_with = "bench",
        conflicts_with = "unit-test",
        conflicts_with = "bin",
        conflicts_with = "test"
    )]
    example: Option<String>,

    /// Test binary to run (currently profiles the test harness and all tests in the binary)
    #[structopt(
        long = "test",
        conflicts_with = "bench",
        conflicts_with = "unit-test",
        conflicts_with = "bin",
        conflicts_with = "example"
    )]
    test: Option<String>,

    /// Crate target to unit test, <unit-test> may be omitted if crate only has one target
    /// (currently profiles the test harness and all tests in the binary; test selection
    /// can be passed as trailing arguments after `--` as separator)
    #[structopt(
        long = "unit-test",
        conflicts_with = "bench",
        conflicts_with = "bin",
        conflicts_with = "test",
        conflicts_with = "example"
    )]
    unit_test: Option<Option<String>>,

    /// Benchmark to run
    #[structopt(
        long = "bench",
        conflicts_with = "bin",
        conflicts_with = "unit-test",
        conflicts_with = "example",
        conflicts_with = "test"
    )]
    bench: Option<String>,

    /// Path to Cargo.toml
    #[structopt(long = "manifest-path")]
    manifest_path: Option<PathBuf>,

    /// Build features to enable
    #[structopt(short = "f", long = "features")]
    features: Option<String>,

    /// Disable default features
    #[structopt(long = "no-default-features")]
    no_default_features: bool,

    #[structopt(flatten)]
    graph: flamegraph::Options,

    trailing_arguments: Vec<String>,
}

#[derive(Debug, StructOpt)]
#[structopt(
    name = "cargo-flamegraph",
    about = "A cargo subcommand for generating flamegraphs, using inferno"
)]
enum Opts {
    #[structopt(name = "flamegraph")]
    Flamegraph(Opt),
}

enum RequestedTarget {
    Bin(String),
    Example(String),
    UnitTest(Option<String>),
    Test(String),
    Bench(String),
    None,
}

impl RequestedTarget {
    fn as_category(&self) -> Category {
        match self {
            Self::Bin(_) => Category::Bin,
            Self::Example(_) => Category::Example,
            Self::UnitTest(_) => Category::UnitTest,
            Self::Test(_) => Category::Test,
            Self::Bench(_) => Category::Bench,
            Self::None => Category::Bin,
        }
    }
}

impl From<&Opt> for RequestedTarget {
    fn from(value: &Opt) -> Self {
        match value {
            Opt { bin: Some(t), .. } => Self::Bin(t.clone()),
            Opt {
                example: Some(t), ..
            } => Self::Example(t.clone()),
            Opt { test: Some(t), .. } => Self::Test(t.clone()),
            Opt { bench: Some(t), .. } => Self::Bench(t.clone()),
            Opt {
                unit_test: Some(t), ..
            } => Self::UnitTest(t.clone()),
            _ => Self::None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Category {
    Bin,
    Example,
    UnitTest,
    Test,
    Bench,
}

impl Category {
    fn allows_kind(&self, kinds: &[String]) -> bool {
        match self {
            Self::Bin => kinds.iter().any(|k| k == "bin"),
            Self::Example => kinds.iter().any(|k| k == "example"),
            Self::UnitTest => kinds.iter().any(|k| k == "bin" || k == "lib"),
            Self::Test => kinds.iter().any(|k| k == "test"),
            Self::Bench => kinds.iter().any(|k| k == "bench"),
        }
    }
}

fn build(target: &ValidTarget, opt: &Opt) -> anyhow::Result<Utf8PathBuf> {
    use std::process::{Command, Output, Stdio};
    let mut cmd = Command::new("cargo");

    match target.category {
        Category::Bin => {
            cmd.args(&["build", "--bin", &target.name]);
        }
        Category::Example => {
            cmd.args(&["build", "--example", &target.name]);
        }
        Category::Test => {
            cmd.args(&["build", "--test", &target.name]);
        }
        Category::UnitTest => {
            // `cargo test` is required because `cargo build` does not have flags to build
            // individual unit test targets. `cargo test` requires differentiating between lib and bin.
            match target.kind.iter().any(|k| k == "lib") {
                true => cmd.args(&["test", "--no-run", "--lib"]),
                false => cmd.args(&["test", "--no-run", "--bin", &target.name]),
            };
        }
        Category::Bench => {
            // This will build benchmarks with the `bench` profile. This is needed
            // because the `--profile` argument for `cargo build` is unstable.
            match opt.dev {
                true => cmd.args(&["build", "--bench", &target.name]),
                false => cmd.args(&["bench", "--no-run", &target.name]),
            };
        }
    }

    // do not use `--release` when we are building for `bench`
    if !opt.dev && target.category != Category::Bench {
        cmd.arg("--release");
    }

    cmd.args(&["--package", &target.package]);

    if let Some(ref manifest_path) = opt.manifest_path {
        cmd.arg("--manifest-path");
        cmd.arg(manifest_path);
    }

    if let Some(ref features) = opt.features {
        cmd.arg("--features");
        cmd.arg(features);
    }

    if opt.no_default_features {
        cmd.arg("--no-default-features");
    }

    cmd.arg("--message-format=json-render-diagnostics");

    if opt.graph.verbose {
        println!("build command: {:?}", cmd);
    }

    let Output { status, stdout, .. } = cmd
        .stderr(Stdio::inherit())
        .output()
        .context("failed to execute cargo build command")?;

    if !status.success() {
        return Err(anyhow!("cargo build failed"));
    }

    let artifacts: Vec<Artifact> = Message::parse_stream(&*stdout)
        .filter_map(|m| match m {
            Ok(Message::CompilerArtifact(artifact)) => Some(Ok(artifact)),
            Ok(_) => None,
            Err(e) => Some(Err(e).context("failed to parse cargo build output")),
        })
        .collect::<anyhow::Result<_, _>>()?;

    // `target.kind` is a `Vec`, but it always seems to contain exactly one element.
    let (debug_level, binary_path) = artifacts
        .iter()
        .find_map(|a| {
            a.executable
                .as_deref()
                .filter(|_| a.target.name == target.name && a.target.kind == target.kind)
                .map(|e| (a.profile.debuginfo, e))
        })
        .ok_or_else(|| {
            let targets: Vec<_> = artifacts
                .iter()
                .map(|a| (&a.target.kind, &a.target.name))
                .collect();
            anyhow!(
                "could not find desired target {:?} in the targets for this crate: {:?}",
                target,
                targets
            )
        })?;

    const NONE: u32 = 0;
    if !opt.dev && debug_level.unwrap_or(NONE) == NONE {
        let profile = match target.category {
            Category::Example | Category::Bin => "release",
            // tests use the bench profile in release mode.
            Category::UnitTest | Category::Test | Category::Bench => "bench",
        };

        eprintln!("\nWARNING: profiling without debuginfo. Enable symbol information by adding the following lines to Cargo.toml:\n");
        eprintln!("[profile.{}]", profile);
        eprintln!("debug = true\n");
        eprintln!("Or set this environment variable:\n");
        eprintln!("CARGO_PROFILE_{}_DEBUG=true\n", profile.to_uppercase());
    }

    Ok(binary_path.to_path_buf())
}

fn workload(opt: &Opt, binary_path: Utf8PathBuf) -> anyhow::Result<Vec<String>> {
    let mut command = Vec::with_capacity(1 + opt.trailing_arguments.len());
    command.push(binary_path.to_string());
    command.extend(opt.trailing_arguments.iter().cloned());
    Ok(command)
}

#[derive(Debug)]
struct ValidTarget {
    category: Category,
    package: String,
    name: String,
    kind: Vec<String>,
}

impl ValidTarget {
    /// Returns true if the package name matches or `package` is None.
    /// This allows ignoring the package if it's not given explicitly.
    fn is_in_package(&self, package: Option<&str>) -> bool {
        package.map_or(true, |p| p == self.package)
    }
}

impl std::fmt::Display for ValidTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "target {} in package {}", self.name, self.package)
    }
}

fn load_targets<F>(cat: Category, target_filter: F) -> anyhow::Result<Vec<ValidTarget>>
where
    F: FnMut(&ValidTarget) -> bool,
{
    Ok(MetadataCommand::new()
        .no_deps()
        .exec()
        .context("failed to access crate metadata")?
        .packages
        .into_iter()
        .flat_map(|p| {
            let Package { targets, name, .. } = p;
            targets.into_iter().map(move |t| ValidTarget {
                package: name.clone(),
                name: t.name,
                kind: t.kind,
                category: cat,
            })
        })
        .filter(target_filter)
        .collect())
}

fn find_unique_target(category: Category, pkg: Option<&str>) -> anyhow::Result<ValidTarget> {
    let mut targets = load_targets(category, |t| {
        category.allows_kind(&t.kind) && t.is_in_package(pkg)
    })?;

    match targets.as_slice() {
        [_] => Ok(targets.remove(0)),
        [] => Err(anyhow!(
            "crate has no automatically selectable target: try passing `--example <example>` \
                or similar to choose a binary"
        )),
        _ => Err(anyhow!(
            "several possible targets found: {:?}, please pass an explicit target.",
            targets
        )),
    }
}

fn verify_target(name: &str, cat: Category, pkg: Option<&str>) -> anyhow::Result<ValidTarget> {
    let mut targets: Vec<_> = load_targets(cat, |t| {
        t.is_in_package(pkg) && t.name == name && cat.allows_kind(&t.kind)
    })?;

    match targets.as_slice() {
        [_] => Ok(targets.remove(0)),
        [] => Err(anyhow!(
            "workspace does not contain target {} of category {:?}{}",
            name,
            cat,
            pkg.map(|p| format!(" in package {}", p))
                .unwrap_or_default()
        )),
        _ if pkg.is_none() => Err(anyhow!("several possible targets found: {:?}, please try to pass an explict package as well.", targets)),
        _ => Err(anyhow!("several possible targets found: {:?}, but package, target and target kind combination should be unique.", targets)),
    }
}

fn main() -> anyhow::Result<()> {
    let Opts::Flamegraph(opt) = Opts::from_args();

    let requested_target = RequestedTarget::from(&opt);
    let package = opt.package.as_deref();
    let target = match requested_target {
        RequestedTarget::None => find_unique_target(Category::Bin, package),
        RequestedTarget::UnitTest(None) => find_unique_target(Category::UnitTest, package),
        RequestedTarget::Bench(ref t)
        | RequestedTarget::Bin(ref t)
        | RequestedTarget::Example(ref t)
        | RequestedTarget::Test(ref t)
        | RequestedTarget::UnitTest(Some(ref t)) => {
            verify_target(t, requested_target.as_category(), package)
        }
    }?;
    let executable = build(&target, &opt)?;
    let workload = workload(&opt, executable)?;
    flamegraph::generate_flamegraph_for_workload(Workload::Command(workload), opt.graph)
}
