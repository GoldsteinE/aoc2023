use std::{
    env,
    io::{self, Write as _},
    path::{Path, PathBuf},
    process::Stdio,
};

use crossterm::{cursor, style::Stylize as _};
use tokio::{process::Command, sync::mpsc};

#[derive(Debug, Clone, Copy)]
enum Part {
    Build,
    Demo1,
    Part1,
    Demo2,
    Part2,
}

#[derive(Debug, Clone, Copy)]
enum Status {
    Fail,
    Okay,
    Na,
}

#[derive(Debug, Clone, Copy)]
struct State {
    day: u8,
    idx: u8,
    part: Part,
    status: Status,
}

#[derive(Debug, Clone, Copy)]
struct Day {
    parts: [Status; 5],
}

fn compare_answers(expected: &str, found: &[u8]) -> bool {
    let found = match std::str::from_utf8(found) {
        Ok(found) => found,
        Err(_) => return false,
    };

    expected.trim() == found.trim()
}

async fn run_script(
    script: impl AsRef<Path>,
    dir_name: &Path,
    args: impl IntoIterator<Item = &str>,
    expected_output: Option<String>,
) -> Status {
    let script = script.as_ref();

    let res = Command::new(script)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .current_dir(dir_name)
        .args(args)
        .output()
        .await;

    match res {
        Ok(output) if output.status.success() => match expected_output {
            Some(expected) if !compare_answers(&expected, &output.stdout) => Status::Fail,
            _ => Status::Okay,
        },
        _ => Status::Fail,
    }
}

async fn run_part(dir_name: &Path, args: [&str; 2]) -> Status {
    let out_file = dir_name
        .join("out")
        .join(format!("{}{}.txt", args[0], args[1]));
    let out = match tokio::fs::read_to_string(out_file).await {
        Ok(out) => out,
        Err(_) => return Status::Fail,
    };

    run_script("scripts/run.sh", dir_name, args, Some(out)).await
}

#[rustfmt::skip]
async fn check_day(day: u8, idx: u8, events: mpsc::UnboundedSender<State>) {
    let dir_name = PathBuf::from(format!("day{:02}", day));

    let res = run_script("scripts/build.sh", &dir_name, [], None).await;
    events.send(State { day, idx, status: res, part: Part::Build }).ok();

    if !matches!(res, Status::Okay) {
        let res = Status::Na;
        for part in [Part::Demo1, Part::Demo2, Part::Part1, Part::Part2] {
            events.send(State { day, idx, status: res, part }).unwrap();
        }
        return;
    }

    tokio::join!(
        async {
            let status = run_part(&dir_name, ["demo", "1"]).await;
            events.send(State { day, idx, status, part: Part::Demo1 }).unwrap();
        },
        async {
            let status = run_part(&dir_name, ["demo", "2"]).await;
            events.send(State { day, idx, status, part: Part::Demo2 }).unwrap();
        },
        async {
            let status = run_part(&dir_name, ["part", "1"]).await;
            events.send(State { day, idx, status, part: Part::Part1 }).unwrap();
        },
        async {
            let status = run_part(&dir_name, ["part", "2"]).await;
            events.send(State { day, idx, status, part: Part::Part2 }).unwrap();
        },
    );
}

fn count_days() -> u8 {
    for x in 1..=25 {
        if !matches!(std::fs::metadata(format!("day{x:02}")), Ok(m) if m.is_dir()) {
            return x - 1;
        }
    }

    25
}

async fn do_main() -> Status {
    let days: Result<Vec<u8>, _> = env::args().skip(1).map(|arg| arg.parse()).collect();
    let mut days = match days {
        Ok(days) => days,
        Err(_) => {
            eprintln!("Usage: check.rs [day...]");
            return Status::Fail;
        }
    };

    if days.is_empty() {
        days = (1..=count_days()).collect();
    }

    println!(
        " {} | {} | {} | {} | {} | {}",
        "day".bold(),
        "builds".bold(),
        "demo 1".bold(),
        "part 1".bold(),
        "demo 2".bold(),
        "part 2".bold()
    );
    println!(" --- | ------ | ------ | ------ | ------ | ------");

    let interactive = crossterm::tty::IsTty::is_tty(&io::stdout());

    if interactive {
        for day in &days {
            let dayf = format!("{day:03}");
            println!(
                " {} |  {1}  |  {1}  |  {1}  |  {1}  |  {1}",
                dayf.bold(),
                "wait".yellow()
            );
        }
    }

    io::stdout().flush().ok();

    let (tx, mut rx) = mpsc::unbounded_channel();
    for (idx, &day) in days.iter().enumerate() {
        tokio::spawn(check_day(day, idx as u8, tx.clone()));
    }
    drop(tx); // unneeded extra receiver


    let mut overall_status = Status::Okay;
    if interactive {
        let (_col, last_row) = cursor::position().expect("failed to find cursor");
        crossterm::execute!(io::stdout(), cursor::Hide).ok();

        while let Some(update) = rx.recv().await {
            let row = last_row - u16::from(days.len() as u8 - update.idx);
            let col = match update.part {
                Part::Build => 8,
                Part::Demo1 => 17,
                Part::Part1 => 26,
                Part::Demo2 => 35,
                Part::Part2 => 44,
            };
            crossterm::execute!(io::stdout(), cursor::MoveTo(col, row))
                .expect("failed to move cursor");

            #[rustfmt::skip]
            #[allow(clippy::let_unit_value)]
            let () = match update.status {
                Status::Fail => {
                    overall_status = Status::Fail;
                    /* output */print!("{}", "fail".red());
                }
                Status::Okay => print!("{}", " ok ".green()),
                Status::Na   => print!("{}", " -- ".grey()),
            };

            io::stdout().flush().ok();
        }

        crossterm::execute!(std::io::stdout(), cursor::MoveTo(0, last_row), cursor::Show).ok();
    } else {
        let mut results = [None; 25];
        for day in days {
            results[usize::from(day) - 1] = Some(Day {
                parts: [Status::Na; 5],
            });
        }

        while let Some(update) = rx.recv().await {
            let part = match update.part {
                Part::Build => 0,
                Part::Demo1 => 1,
                Part::Part1 => 2,
                Part::Demo2 => 3,
                Part::Part2 => 4,
            };
            results[usize::from(update.day) - 1].as_mut().unwrap().parts[part] = update.status;
        }

        for (idx, day) in results.iter().enumerate() {
            if let Some(day) = day {
                print!(" {:03}", idx + 1);
                for part in day.parts {
                    #[rustfmt::skip]
                    #[allow(clippy::let_unit_value)]
                    let () = match part {
                        Status::Fail => {
                            overall_status = Status::Fail;
                            /* output */print!(" |  fail ");
                        },
                        Status::Okay => print!(" |   ok  "),
                        Status::Na =>   print!(" |   --  "),
                    };
                }
                println!();
            }
        }
    }

    overall_status
}

#[tokio::main]
async fn main() {
    let status = do_main().await;
    if !matches!(status, Status::Okay) {
        std::process::exit(1);
    }
}
