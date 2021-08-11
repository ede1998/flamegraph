use std::fs;
use std::path::{Path, PathBuf};

use cargo_metadata::Target;
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

    /// Output file, flamegraph.svg if not present
    #[structopt(
        parse(from_os_str),
        short = "o",
        long = "output"
    )]
    output: Option<PathBuf>,

    /// Build features to enable
    #[structopt(short = "f", long = "features")]
    features: Option<String>,

    /// Disable default features
    #[structopt(long = "no-default-features")]
    no_default_features: bool,

    /// Open the output .svg file with default program
    #[structopt(long = "open")]
    open: bool,

    /// Run with root privileges (using `sudo`)
    #[structopt(long = "root")]
    root: bool,

    /// Print extra output to help debug problems
    #[structopt(short = "v", long = "verbose")]
    verbose: bool,

    /// Sampling frequency
    #[structopt(short = "F", long = "freq")]
    frequency: Option<u32>,

    /// Custom command for invoking perf/dtrace
    #[structopt(
        short = "c",
        long = "cmd",
        conflicts_with = "freq"
    )]
    custom_cmd: Option<String>,

    /// Disable inlining for perf script because of performace issues
    #[structopt(long = "no-inline")]
    script_no_inline: bool,

    #[structopt(flatten)]
    flamegraph_options: flamegraph::FlamegraphOptions,

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

fn build(opt: &Opt) {
    let mut cmd = std::process::Command::new("cargo");

    // This will build benchmarks with the `bench` profile. This is needed
    // because the `--profile` argument for `cargo build` is unstable.
    if !opt.dev && opt.bench.is_some() {
        cmd.args(&["bench", "--no-run"]);
    } else {
        cmd.arg("build");
    }

    // do not use `--release` when we are building for `bench`
    if !opt.dev && opt.bench.is_none() {
        cmd.arg("--release");
    }

    if let Some(ref package) = opt.package {
        cmd.arg("--package");
        cmd.arg(package);
    }

    if let Some(ref bin) = opt.bin {
        cmd.arg("--bin");
        cmd.arg(bin);
    }

    if let Some(ref example) = opt.example {
        cmd.arg("--example");
        cmd.arg(example);
    }

    if let Some(ref test) = opt.test {
        cmd.arg("--test");
        cmd.arg(test);
    }

    if let Some(ref bench) = opt.bench {
        cmd.arg("--bench");
        cmd.arg(bench);
    }

    if opt.unit_test.is_some() {
        cmd.arg("--tests");
    }

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

    if opt.verbose {
        println!("build command: {:?}", cmd);
    }

    let mut child = cmd
        .spawn()
        .expect("failed to spawn cargo build command");

    let exit_status = child.wait().expect(
        "failed to wait for cargo build child to finish",
    );

    if !opt.dev {
        cmd.arg("--message-format=json");

        let output: Vec<u8> = cmd
            .output()
            .expect("failed to execute cargo build command")
            .stdout;

        let messages =
            cargo_metadata::Message::parse_stream(&*output);

        let mut has_debuginfo = false;

        let profile = if opt.bench.is_some() {
            "bench"
        } else {
            "release"
        };

        // get names of binaries in the workload
        let workload_filenames = workload(opt)
            .iter()
            .filter_map(|w| {
                PathBuf::from(w)
                    .file_name()?
                    .to_str()
                    .map(|filename| filename.to_owned())
            })
            .collect::<Vec<_>>();

        // This is an extremely coarse check to see
        // if any of our build artifacts have debuginfo
        // enabled.
        for message in messages {
            let artifact = if let Ok(
                cargo_metadata::Message::CompilerArtifact(
                    artifact,
                ),
            ) = message
            {
                artifact
            } else {
                continue;
            };

            // - Check that this is a binary we are interested in.
            //   This should start with the target name (binaries can have names in format
            //   `benchmark-deadbeef`)
            // - The iterator is reversed because the entities we are interested in are most likely
            //   to appear in the end of the list.
            if workload_filenames.iter().rev().any(|w| {
                w.starts_with(&artifact.target.name)
            }) && artifact.profile.debuginfo.unwrap_or(0)
                != 0
            {
                has_debuginfo = true;
            }
        }

        if !has_debuginfo {
            eprintln!(
                "\nWARNING: building without debuginfo. \
                 Enable symbol information by adding \
                 the following lines to Cargo.toml:\n"
            );
            eprintln!("[profile.{}]", profile);
            eprintln!("debug = true\n");
        }
    }

    if !exit_status.success() {
        eprintln!("cargo build failed: {:?}", child.stderr);
        std::process::exit(1);
    }
}

fn find_binary(ty: &str, path: &Path, bin: &str) -> String {
    // Ignorance-based error handling. We really do not care about any errors
    // popping up from the filesystem search here. Thus, we just bash them into
    // place using `Option`s monadic properties. Not pretty though.
    fs::read_dir(path)
        .ok()
        .and_then(|mut r| {
            r.find(|f| {
                if let Ok(f) = f {
                    let file_name = f.file_name();
                    let name = file_name.to_string_lossy();
                    name.starts_with(bin)
                        && !name.ends_with(".d")
                } else {
                    false
                }
            })
            .and_then(|r| r.ok())
        })
        .and_then(|f| {
            f.path()
                .file_name()
                .map(|n| n.to_string_lossy().to_string())
        })
        .unwrap_or_else(|| {
            eprintln!(
                "could not find desired target {} \
                 in the {} targets for this crate",
                bin, ty
            );
            std::process::exit(1);
        })
}

fn workload(opt: &Opt) -> Vec<String> {
    let mut metadata_cmd =
        cargo_metadata::MetadataCommand::new();
    metadata_cmd.no_deps();
    let metadata = metadata_cmd
        .exec()
        .expect("could not access crate metadata");

    let mut binary_path = metadata.target_directory;

    if let Ok(t) = std::env::var("CARGO_BUILD_TARGET") {
        binary_path.push(t);
    }

    if opt.dev {
        binary_path.push("debug");
    } else {
        binary_path.push("release");
    }

    if opt.example.is_some() {
        binary_path.push("examples");
    } else if opt.bench.is_some() || opt.unit_test.is_some()
    {
        binary_path.push("deps");
    }

    let all_targets: Vec<Target> = metadata
        .packages
        .into_iter()
        .flat_map(|p| p.targets)
        .collect();

    let targets: Vec<_> = all_targets
        .iter()
        .filter(|t| t.crate_types.contains(&"bin".into()))
        .map(|t| &t.name)
        .collect();

    if opt.unit_test.is_none() && targets.is_empty() {
        eprintln!("no Rust binary targets found");
        std::process::exit(1);
    }

    let target = if let Some(ref test) = opt.test {
        find_binary("test", &binary_path, test)
    } else if let Some(ref bench) = opt.bench {
        find_binary("bench", &binary_path, bench)
    } else if let Some(ref unit_test) = opt.unit_test {
        let unit_test = unit_test.as_ref().unwrap_or_else(|| match all_targets.as_slice() {
                [target] => &target.name,
                _ => {
                    let all_target_names: Vec<_> = all_targets.iter().map(|t| &t.name).collect();
                    eprintln!( "please specify unit test target explicitly, crate has multiple targets: {:?}", all_target_names);
                    std::process::exit(1);
                }
            });
        // minus in crate name becomes underscore in test executable name
        let unit_test = unit_test.replace('-', "_");
        find_binary("unit-test", &binary_path, &unit_test)
    } else if let Some(ref bin) =
        opt.bin.as_ref().or_else(|| opt.example.as_ref())
    {
        if targets.contains(bin) {
            bin.to_string()
        } else {
            eprintln!(
                "could not find desired target {} \
                 in the targets for this crate: {:?}",
                bin, targets
            );
            std::process::exit(1);
        }
    } else if targets.len() == 1 {
        targets[0].to_owned()
    } else {
        eprintln!(
            "several possible targets found: {:?}, \
             please pass `--bin <binary>` or `--example <example>` \
             to cargo flamegraph to choose one of them",
            targets
        );
        std::process::exit(1);
    };

    binary_path.push(target);

    let mut result = opt.trailing_arguments.clone();
    result.insert(0, binary_path.to_string_lossy().into());
    result
}

fn main() {
    let Opts::Flamegraph(mut opt) = Opts::from_args();

    build(&opt);

    let workload = workload(&opt);
    if opt.verbose {
        println!("workload: {:?}", workload);
    }

    let flamegraph_filename: PathBuf = opt
        .output
        .take()
        .unwrap_or_else(|| "flamegraph.svg".into());

    flamegraph::generate_flamegraph_for_workload(
        Workload::Command(workload),
        &flamegraph_filename,
        opt.root,
        opt.script_no_inline,
        opt.frequency,
        opt.custom_cmd,
        opt.flamegraph_options.into_inferno(),
        opt.verbose,
    );

    if opt.open {
        if let Err(e) = opener::open(&flamegraph_filename) {
            eprintln!(
                "Failed to open [{}]. Error: {}",
                flamegraph_filename.display(),
                e
            );
        }
    }
}
