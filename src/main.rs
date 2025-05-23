use chrono::{
    DateTime, Datelike, FixedOffset, NaiveDate, Timelike, Weekday,
    format::{Parsed, parse_and_remainder, strftime::StrftimeItems},
};
use clap::{Parser, ValueEnum};
use std::{
    collections::HashMap,
    io::{BufRead, BufReader, BufWriter, stdout},
    ops::Range,
    process::{Command, Stdio},
};
use terminal::*;
use thiserror::Error;

mod terminal;

#[derive(Parser, Debug)]
#[command(
    version,
    about,
    after_help = "\
The COLOR argument can be default or a hex color (example: #ff0000).

The STYLE argument is a comma separated list of none, bold, fg=COLOR, and bg=COLOR (example: bold,fg=#ffffff,bg=#000000).
"
)]
struct Cli {
    /// Report format [aliases: h, y, yy, mm].
    #[arg(short, long, default_value_t = Output::Year, value_enum)]
    output: Output,

    /// Display small squares.
    #[arg(short = 'x', long)]
    small: bool,

    /// Display years with separated months.
    #[arg(short = 'm', long)]
    months: bool,

    /// Normalize commits per hour.
    #[arg(long)]
    normalize_hours: bool,

    /// Display numbers.
    #[arg(long)]
    numbers: bool,

    /// Display style of years.
    #[arg(
        long,
        value_name = "STYLE",
        default_value = "",
        value_parser = parse_style,
    )]
    year_style: Style,

    /// Display style of axis labels (name of days, name of months, hours).
    #[arg(
        long,
        value_name = "STYLE",
        default_value = "",
        value_parser = parse_style,
    )]
    label_style: Style,

    /// Display style of titles.
    #[arg(
        long,
        value_name = "STYLE",
        default_value = "bold",
        value_parser = parse_style,
    )]
    title_style: Style,

    /// Heatmap colors.
    #[arg(
        long,
        value_name = "COLOR|STYLE",
        value_delimiter = ';',
        num_args = 1..,
        default_value = "none;#9be9a8;#40c463;#30a14e;#216e39;#00441b",
        value_parser = parse_color_or_style,
    )]
    colors: Vec<ColorOrStyle>,

    #[arg(long, value_name = "NUMBER", default_value_t = 6)]
    step: u32,

    /// Use author date of commits ("%ad").
    #[arg(long, visible_alias = "ad")]
    author_date: bool,

    /// Group commits by author name ("%an").
    #[arg(long, visible_alias = "an")]
    author_name: bool,

    /// Group commits by author email ("%ae").
    #[arg(long, visible_alias = "ae")]
    author_email: bool,

    /// Shorthand for --author-name --author-email.
    #[arg(short = 'A', long = "ane")]
    author_name_and_email: bool,

    /// Use committer date of commits ("%cd").
    #[arg(long, visible_alias = "cd", conflicts_with = "author_date")]
    committer_date: bool,

    /// Group commits by committer name ("%cn").
    #[arg(long, visible_alias = "cn")]
    committer_name: bool,

    /// Group commits by committer email ("%ce").
    #[arg(long, visible_alias = "ce")]
    committer_email: bool,

    /// Shorthand for --committer-name --committer-email.
    #[arg(short = 'C', long = "cne")]
    committer_name_and_email: bool,

    /// Display Sunday as the first day of the week.
    #[arg(long)]
    sunday: bool,

    /// Display Monday as the first day of the week.
    #[arg(long, conflicts_with = "sunday")]
    monday: bool,

    #[arg(help_heading = "git log", short = 'n', long, value_name = "NUMBER")]
    max_count: Option<String>,

    #[arg(help_heading = "git log", long, value_name = "NUMBER")]
    skip: Option<String>,

    #[arg(
        help_heading = "git log",
        long,
        visible_alias = "after",
        value_name = "DATE"
    )]
    since: Option<String>,

    #[arg(
        help_heading = "git log",
        long,
        visible_alias = "before",
        value_name = "DATE"
    )]
    until: Option<String>,

    #[arg(
        help_heading = "git log",
        short,
        long,
        value_name = "PATTERN",
        value_delimiter = ' ',
        num_args = 1..,
    )]
    author: Vec<String>,

    #[arg(
        help_heading = "git log",
        short,
        long,
        value_name = "PATTERN",
        value_delimiter = ' ',
        num_args = 1..,
    )]
    committer: Vec<String>,

    #[arg(help_heading = "git log", long, value_name = "PATTERN")]
    grep: Option<String>,

    #[arg(help_heading = "git log", long)]
    invert_grep: bool,

    #[arg(help_heading = "git log", long)]
    merges: bool,

    #[arg(help_heading = "git log", long)]
    no_merges: bool,

    #[arg(help_heading = "git log", long)]
    first_parent: bool,

    #[arg(help_heading = "git log", long)]
    all: bool,

    #[arg(help_heading = "git log", value_name = "REVISION-RANGE")]
    revisions_or_paths: Vec<String>,

    #[arg(help_heading = "git log", value_name = "PATH", last = true)]
    paths: Vec<String>,
}

impl Cli {
    fn first_weekday(&self) -> FirstWeekday {
        if self.sunday {
            FirstWeekday::Sunday
        } else {
            FirstWeekday::Monday
        }
    }
}

#[derive(Clone, Debug, ValueEnum)]
enum Output {
    #[value(alias("h"))]
    Hour,
    #[value(alias("y"))]
    Year,
    #[value(alias("yy"))]
    YearByYear,
    #[value(alias("mm"))]
    MonthByMonth,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum ColorOrStyle {
    Color(BackgroundColor),
    Style(Style),
}

#[derive(Error, Debug)]
enum ParseColorError {
    #[error("expected default or hex color")]
    Invalid,
    #[error("non-hex digit found in string")]
    InvalidHex(#[from] std::num::ParseIntError),
    #[error("exactly 6 digits expected")]
    InvalidLength,
}

fn parse_color(arg: &str) -> Result<Color, ParseColorError> {
    if arg == "default" {
        Ok(Color::default())
    } else if let Some(s) = arg.strip_prefix('#') {
        if s.len() == 6 {
            Ok(Color::hex(u32::from_str_radix(s, 16)?))
        } else {
            Err(ParseColorError::InvalidLength)
        }
    } else {
        Err(ParseColorError::Invalid)
    }
}

#[derive(Error, Debug)]
enum ParseStyleError {
    #[error("invalid attribute: {0}")]
    InvalidAttribute(String),
    #[error("invalid color: {0}")]
    InvalidColor(#[from] ParseColorError),
}

fn parse_style(arg: &str) -> Result<Style, ParseStyleError> {
    let mut style = Style::default();

    for s in arg.split(',').filter(|s| !s.is_empty()) {
        if s == "none" {
            style = Style::default();
        } else if s == "bold" {
            style = style.with_bold(true);
        } else if let Some(s) = s.strip_prefix("fg=") {
            style = style.with_fg(parse_color(s)?);
        } else if let Some(s) = s.strip_prefix("bg=") {
            style = style.with_bg(parse_color(s)?);
        } else {
            return Err(ParseStyleError::InvalidAttribute(s.to_string()));
        }
    }

    Ok(style)
}

fn parse_color_or_style(arg: &str) -> Result<ColorOrStyle, ParseStyleError> {
    parse_color(arg)
        .map(BackgroundColor::from)
        .map(ColorOrStyle::Color)
        .or_else(|_| parse_style(arg).map(ColorOrStyle::Style))
}

#[derive(Clone, Copy, Debug)]
enum FirstWeekday {
    Monday,
    Sunday,
}

fn day_index(weekday: Weekday, first_weekday: FirstWeekday) -> usize {
    usize::try_from(
        match first_weekday {
            FirstWeekday::Monday => weekday.number_from_monday(),
            FirstWeekday::Sunday => weekday.number_from_sunday(),
        } - 1,
    )
    .unwrap()
}

struct HeatColors {
    styles: Box<[Style]>,
    step: u32,
}

impl HeatColors {
    fn new(colors: &[ColorOrStyle], step: u32) -> Self {
        assert!(!colors.is_empty());

        Self {
            styles: colors
                .iter()
                .map(|&x| match x {
                    ColorOrStyle::Color(color) => Style::default()
                        .with_bg(color)
                        .with_fg(color.contrast_text()),
                    ColorOrStyle::Style(style) => style,
                })
                .collect(),
            step,
        }
    }

    fn max(&self) -> u32 {
        u32::try_from(self.styles.len() - 1).unwrap() * self.step
    }

    fn get(&self, n: u32) -> Style {
        *self
            .styles
            .get(usize::try_from((n + self.step - 1) / self.step).unwrap())
            .unwrap_or_else(|| self.styles.last().unwrap())
    }
}

fn draw_squares<S, F>(surface: &mut S, height: usize, width: usize, f: F) -> usize
where
    S: Surface,
    F: Fn(usize, usize) -> Option<(Cell, Cell)>,
{
    for y in 0..height {
        for x in 0..width {
            if let Some((cell, cell2)) = f(y, x) {
                surface.put(y, x * 2, cell);
                surface.put(y, x * 2 + 1, cell2);
            }
        }
    }

    height
}

fn draw_half_squares<S, F>(
    surface: &mut S,
    background: BackgroundColor,
    height: usize,
    width: usize,
    f: F,
) -> usize
where
    S: Surface,
    F: Fn(usize, usize) -> Option<BackgroundColor>,
{
    const UPPER_HALF_BLOCK: char = '▀';
    const LOWER_HALF_BLOCK: char = '▄';

    for y in (0..height).step_by(2) {
        for x in 0..width {
            let (upper, lower) = match (f(y, x), f(y + 1, x)) {
                (Some(upper), Some(lower)) => (upper, lower),
                (Some(upper), None) => (upper, background),
                (None, Some(lower)) => (background, lower),
                (None, None) => continue,
            };

            let cell = if lower == upper {
                Cell::new(' ', Style::default().with_bg(lower))
            } else if let Ok(lower) = ForegroundColor::try_from(lower) {
                Cell::new(
                    LOWER_HALF_BLOCK,
                    Style::default().with_fg(lower).with_bg(upper),
                )
            } else if let Ok(upper) = ForegroundColor::try_from(upper) {
                Cell::new(
                    UPPER_HALF_BLOCK,
                    Style::default().with_fg(upper).with_bg(lower),
                )
            } else {
                unreachable!();
            };

            surface.put(y / 2, x, cell);
        }
    }

    (height + 1) / 2
}

fn draw_heat_grid<S, F>(
    surface: &mut S,
    height: usize,
    width: usize,
    small: bool,
    colors: &HeatColors,
    numbers: bool,
    f: F,
) -> usize
where
    S: Surface,
    F: Fn(usize, usize) -> Option<u32>,
{
    if small {
        fn get_square(n: Option<u32>, colors: &HeatColors) -> Option<BackgroundColor> {
            Some(colors.get(n?).bg())
        }

        draw_half_squares(surface, Color::default().into(), height, width, |y, x| {
            get_square(f(y, x), colors)
        })
    } else {
        fn get_square(n: Option<u32>, colors: &HeatColors, numbers: bool) -> Option<(Cell, Cell)> {
            let n = n?;
            let style = colors.get(n);

            Some(if n == 0 {
                (Cell::new(' ', style), Cell::new('.', style))
            } else if !numbers {
                (Cell::new(' ', style), Cell::new(' ', style))
            } else if n < 10 {
                (
                    Cell::new(' ', style),
                    Cell::new(char::from_digit(n, 10).unwrap(), style),
                )
            } else if n < 100 {
                (
                    Cell::new(char::from_digit(n / 10, 10).unwrap(), style),
                    Cell::new(char::from_digit(n % 10, 10).unwrap(), style),
                )
            } else {
                (Cell::new('+', style), Cell::new('+', style))
            })
        }

        draw_squares(surface, height, width, |y, x| {
            get_square(f(y, x), colors, numbers)
        })
    }
}

fn draw_hour_grid<S, F>(
    surface: &mut S,
    small: bool,
    relative: bool,
    numbers: bool,
    colors: &HeatColors,
    commits: F,
) -> usize
where
    S: Surface,
    F: Fn(usize, usize) -> u32,
{
    let height = 7;
    let width = 24;

    let max = (0..height)
        .map(|y| (0..width).map(|x| commits(y, x)).max().unwrap())
        .max()
        .unwrap();

    let relative = relative && max > colors.max();

    let normalize = |n: u32| {
        if relative {
            (n * colors.max() + max - 1) / max
        } else {
            n
        }
    };

    draw_heat_grid(
        surface,
        height,
        width,
        small,
        colors,
        !relative && numbers,
        |y, x| {
            if y >= height {
                None
            } else {
                Some(normalize(commits(y, x)))
            }
        },
    )
}

fn draw_year_grid<S, F>(
    surface: &mut S,
    year: i32,
    small: bool,
    numbers: bool,
    first_weekday: FirstWeekday,
    colors: &HeatColors,
    commits: F,
) -> usize
where
    S: Surface,
    F: Fn(usize) -> u32,
{
    let first_day_y = day_index(
        NaiveDate::from_yo_opt(year, 1).unwrap().weekday(),
        first_weekday,
    );

    let number_of_days =
        usize::try_from(NaiveDate::from_ymd_opt(year, 12, 31).unwrap().ordinal()).unwrap();

    let height = 7;
    let width = 53;

    let get_ordinal0 = |y: usize, x: usize| {
        if y >= height {
            None
        } else {
            match (x * height + y).checked_sub(first_day_y) {
                Some(i) if i < number_of_days => Some(i),
                _ => None,
            }
        }
    };

    draw_heat_grid(surface, height, width, small, colors, numbers, |y, x| {
        #[allow(clippy::redundant_closure)]
        get_ordinal0(y, x).map(|i| commits(i))
    })
}

fn last_day_of_month(year: i32, month: u32) -> Option<NaiveDate> {
    if month == 12 {
        NaiveDate::from_ymd_opt(year, 12, 31)
    } else {
        NaiveDate::from_ymd_opt(year, month + 1, 1)?.pred_opt()
    }
}

fn draw_month_grid<S, F>(
    surface: &mut S,
    year: i32,
    month: u32,
    small: bool,
    numbers: bool,
    first_weekday: FirstWeekday,
    colors: &HeatColors,
    commits: F,
) -> usize
where
    S: Surface,
    F: Fn(usize) -> u32,
{
    let first_day = NaiveDate::from_ymd_opt(year, month, 1).unwrap();
    let first_day_y = day_index(first_day.weekday(), first_weekday);
    let first_day_ordinal0 = usize::try_from(first_day.ordinal0()).unwrap();

    let last_day_ordinal =
        usize::try_from(last_day_of_month(year, month).unwrap().ordinal()).unwrap();

    let number_of_days = last_day_ordinal - first_day_ordinal0;

    let height = 7;
    let width = 6;

    let get_ordinal0 = |y: usize, x: usize| {
        if y >= height {
            None
        } else {
            match (x * height + y).checked_sub(first_day_y) {
                Some(i) if i < number_of_days => Some(first_day_ordinal0 + i),
                _ => None,
            }
        }
    };

    draw_heat_grid(surface, height, width, small, colors, numbers, |y, x| {
        #[allow(clippy::redundant_closure)]
        get_ordinal0(y, x).map(|i| commits(i))
    })
}

fn draw_month_line<S, F>(
    surface: &mut S,
    year: i32,
    month: u32,
    numbers: bool,
    colors: &HeatColors,
    commits: F,
) where
    S: Surface,
    F: Fn(usize) -> u32,
{
    let first_day = NaiveDate::from_ymd_opt(year, month, 1).unwrap();
    let first_day_ordinal0 = usize::try_from(first_day.ordinal0()).unwrap();

    let last_day_ordinal =
        usize::try_from(last_day_of_month(year, month).unwrap().ordinal()).unwrap();

    let number_of_days = last_day_ordinal - first_day_ordinal0;

    draw_heat_grid(
        surface,
        1,
        number_of_days,
        false,
        colors,
        numbers,
        |y, x| {
            if y == 0 && x < number_of_days {
                Some(commits(first_day_ordinal0 + x))
            } else {
                None
            }
        },
    );
}

type Hours = [u32; 24];

#[derive(Debug)]
struct CommitsPerYear {
    count: u32,
    commits_per_day: [u32; 366],
    commits_per_month: [u32; 12],
}

impl CommitsPerYear {
    const fn new() -> Self {
        Self {
            count: 0,
            commits_per_day: [0; 366],
            commits_per_month: [0; 12],
        }
    }

    fn is_empty(&self) -> bool {
        self.count == 0
    }

    fn is_month_empty(&self, month: u32) -> bool {
        self.commits_per_month[usize::try_from(month - 1u32).unwrap()] == 0
    }

    fn commits_per_day0(&self, ordinal0: usize) -> u32 {
        self.commits_per_day[ordinal0]
    }

    fn add_commit(&mut self, date: NaiveDate) {
        self.count += 1;
        self.commits_per_day[usize::try_from(date.ordinal0()).unwrap()] += 1;
        self.commits_per_month[usize::try_from(date.month() - 1).unwrap()] += 1;
    }
}

#[derive(Debug)]
struct Group {
    title: String,
    first_commit_year: i32,
    commits_per_year: Vec<CommitsPerYear>,
    commits_per_weekday: [Hours; 7],
}

impl Group {
    pub fn new(title: String) -> Self {
        Self {
            title,
            first_commit_year: 0,
            commits_per_year: Vec::new(),
            commits_per_weekday: [[0; 24]; 7],
        }
    }

    pub fn commits_by_year(&self, year: i32) -> &CommitsPerYear {
        static ZERO: CommitsPerYear = CommitsPerYear::new();

        match usize::try_from(year - self.first_commit_year) {
            Err(_) => &ZERO,
            Ok(i) => self.commits_per_year.get(i).unwrap_or(&ZERO),
        }
    }

    pub fn years(&self) -> Range<i32> {
        self.first_commit_year
            ..self.first_commit_year + i32::try_from(self.commits_per_year.len()).unwrap()
    }

    pub fn add_commit(&mut self, commit_date: DateTime<FixedOffset>, first_weekday: FirstWeekday) {
        let date = commit_date.date_naive();
        let time = commit_date.time();

        self.commits_per_weekday[day_index(date.weekday(), first_weekday)]
            [usize::try_from(time.hour()).unwrap()] += 1;

        if self.commits_per_year.is_empty() {
            self.first_commit_year = date.year();
        }

        let offset = date.year() - self.first_commit_year;
        let year_index = match offset.try_into() {
            Err(_) => {
                self.commits_per_year.splice(
                    0..0,
                    (0..usize::try_from(-offset).unwrap()).map(|_| CommitsPerYear::new()),
                );
                self.first_commit_year = date.year();
                0
            }
            Ok(index) => {
                if index >= self.commits_per_year.len() {
                    self.commits_per_year
                        .resize_with(index + 1, CommitsPerYear::new);
                }
                index
            }
        };

        self.commits_per_year[year_index].add_commit(date);
    }
}

fn draw_month_names<S>(
    surface: &mut S,
    year: i32,
    small: bool,
    months: bool,
    first_weekday: FirstWeekday,
    style: Style,
) where
    S: Surface,
{
    let jan1_y = day_index(
        NaiveDate::from_yo_opt(year, 1).unwrap().weekday(),
        first_weekday,
    );

    let items = StrftimeItems::new("%b").parse().unwrap();

    for month in 1..=12 {
        let first_day = NaiveDate::from_ymd_opt(year, month, 1).unwrap();

        let name = first_day
            .format_with_items(items.as_slice().iter())
            .to_string();

        let x = if months {
            usize::try_from(month - 1).unwrap() * (6 + 1)
                + if day_index(first_day.weekday(), first_weekday) == 0 {
                    0
                } else {
                    1
                }
        } else {
            (jan1_y + usize::try_from(first_day.ordinal0()).unwrap()) / 7
        };

        surface
            .translate(0, if small { x } else { x * 2 })
            .write(&name, style);
    }
}

fn draw_year_commits<S>(
    surface: &mut S,
    commits: &CommitsPerYear,
    year: i32,
    first_weekday: FirstWeekday,
    small: bool,
    numbers: bool,
    months: bool,
    colors: &HeatColors,
    weekdays_label: &SurfaceBuffer,
    weekdays_label_width: usize,
    months_label: &SurfaceBuffer,
) -> usize
where
    S: Surface,
{
    let mut x = 0;

    if !small {
        surface.translate(1, x).draw(weekdays_label);

        x += weekdays_label_width + 1;
    }

    surface.translate(0, x).draw(months_label);

    let height = if months {
        (1..=12)
            .map(|month| {
                let height = draw_month_grid(
                    &mut surface.translate(1, x),
                    year,
                    month,
                    small,
                    numbers,
                    first_weekday,
                    colors,
                    |i| commits.commits_per_day0(i),
                );

                x += if small { 6 + 1 } else { 12 + 2 };

                height
            })
            .max()
            .unwrap()
    } else {
        draw_year_grid(
            &mut surface.translate(1, x),
            year,
            small,
            numbers,
            first_weekday,
            colors,
            |i| commits.commits_per_day0(i),
        )
    };

    1 + height
}

fn build_git_command(cli: &Cli) -> Command {
    fn compose_name_and_email(
        prefix: &str,
        name_format: &str,
        email_format: &str,
        name: bool,
        email: bool,
    ) -> Option<String> {
        match (name, email) {
            (true, true) => Some(format!("{prefix}: {name_format} <{email_format}>")),
            (true, false) => Some(format!("{prefix}: {name_format}")),
            (false, true) => Some(format!("{prefix}: <{email_format}>")),
            (false, false) => None,
        }
    }

    let date_format = if cli.author_date { "%ad" } else { "%cd" };

    let title_format = match (
        compose_name_and_email(
            "Author",
            "%an",
            "%ae",
            cli.author_name || cli.author_name_and_email,
            cli.author_email || cli.author_name_and_email,
        ),
        compose_name_and_email(
            "Committer",
            "%cn",
            "%ce",
            cli.committer_name || cli.committer_name_and_email,
            cli.committer_email || cli.committer_name_and_email,
        ),
    ) {
        (Some(author), None) => author,
        (None, Some(committer)) => committer,
        (Some(author), Some(committer)) => format!("{author}, {committer}"),
        (None, None) => "".to_owned(),
    };

    let mut cmd = Command::new("git");

    cmd.args([
        "log",
        "--date=raw",
        &format!("--pretty=format:{date_format}{title_format}"),
    ]);

    let mut arg = |option: &str, value: &Option<String>| {
        if let Some(s) = value {
            cmd.args([option, s]);
        }
    };

    arg("--max-count", &cli.max_count);
    arg("--skip", &cli.skip);
    arg("--since", &cli.since);
    arg("--until", &cli.until);
    arg("--grep", &cli.grep);

    let mut arg = |option: &str, value: &Vec<String>| {
        for s in value {
            cmd.args([option, s]);
        }
    };

    arg("--author", &cli.author);
    arg("--committer", &cli.committer);

    let mut arg = |option: &str, value: bool| {
        if value {
            cmd.arg(option);
        }
    };

    arg("--invert-grep", cli.invert_grep);
    arg("--merges", cli.merges);
    arg("--no-merges", cli.no_merges);
    arg("--first-parent", cli.first_parent);
    arg("--all", cli.all);

    cmd.args(cli.revisions_or_paths.iter());

    if !cli.paths.is_empty() {
        cmd.arg("--");
        cmd.args(cli.paths.iter());
    }

    cmd
}

fn read_git(cli: &Cli) -> Vec<Group> {
    let mut child = build_git_command(cli)
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();

    let stdout = child.stdout.take().unwrap();

    let mut groups = Vec::<Group>::new();
    let mut by_title = HashMap::<String, usize>::new();

    let items = StrftimeItems::new("%s %z").parse().unwrap();

    for line in BufReader::new(stdout).lines() {
        let line = line.unwrap();

        let mut parsed = Parsed::new();
        let title = parse_and_remainder(&mut parsed, &line, items.as_slice().iter()).unwrap();
        let dt = parsed.to_datetime().unwrap();

        if let Some(&i) = by_title.get(title) {
            groups[i].add_commit(dt, cli.first_weekday());
        } else {
            let mut group = Group::new(title.to_string());
            group.add_commit(dt, cli.first_weekday());
            by_title.insert(title.to_string(), groups.len());
            groups.push(group);
        }
    }

    if !child.wait().unwrap().success() {
        std::process::exit(1);
    }

    groups
}

fn main() {
    let cli = Cli::parse();

    let colors = HeatColors::new(&cli.colors, cli.step);

    let groups = read_git(&cli);

    let weekdays_label = {
        let mut surface = SurfaceBuffer::new();

        let items = StrftimeItems::new("%a").parse().unwrap();

        for i in 1..=7 {
            let dt = NaiveDate::from_yo_opt(1970, i).unwrap();
            let y = day_index(dt.weekday(), cli.first_weekday());
            let name = dt.format_with_items(items.as_slice().iter()).to_string();

            surface.translate(y, 0).write(&name, cli.label_style);
        }

        surface
    };
    let weekdays_label_width = weekdays_label.width();

    let all_years = groups
        .iter()
        .map(|group| group.years())
        .reduce(|acc, x| acc.start.min(x.start)..acc.end.max(x.end))
        .unwrap_or(0..0);

    let months_label_cache = all_years
        .clone()
        .map(|year| {
            let mut surface = SurfaceBuffer::new();
            draw_month_names(
                &mut surface,
                year,
                cli.small,
                cli.months,
                cli.first_weekday(),
                cli.label_style,
            );
            surface
        })
        .collect::<Vec<_>>()
        .into_boxed_slice();

    let mut screen = SurfaceBuffer::new();

    match cli.output {
        Output::Hour => {
            let mut y = 0;

            for group in &groups {
                if !group.title.is_empty() {
                    screen.translate(y, 0).write(&group.title, cli.title_style);

                    y += 1;
                }

                let mut x = 0;

                if !cli.small {
                    screen.translate(y + 1, 0).draw(&weekdays_label);

                    x += weekdays_label_width + 1;

                    for am_pm in (0..24).step_by(12) {
                        for hour in 0..12 {
                            let text =
                                format!("{:2}", if am_pm == 12 && hour == 0 { 12 } else { hour });

                            screen
                                .translate(y, x + (am_pm + hour) * 2)
                                .write(&text, cli.label_style);
                        }
                    }

                    y += 1;
                }

                y += draw_hour_grid(
                    &mut screen.translate(y, x),
                    cli.small,
                    cli.normalize_hours,
                    cli.numbers,
                    &colors,
                    |day, hour| group.commits_per_weekday[day][hour],
                );

                y += 1;
            }
        }
        Output::Year => {
            let mut y = 0;

            for group in &groups {
                if !group.title.is_empty() {
                    screen.translate(y, 0).write(&group.title, cli.title_style);

                    y += 1;
                }

                for year in group.years() {
                    let commits = group.commits_by_year(year);

                    if commits.is_empty() {
                        continue;
                    }

                    screen
                        .translate(y, 0)
                        .write(&format!("{year}"), cli.year_style);

                    y += draw_year_commits(
                        &mut screen.translate(y, 8),
                        commits,
                        year,
                        cli.first_weekday(),
                        cli.small,
                        cli.numbers,
                        cli.months,
                        &colors,
                        &weekdays_label,
                        weekdays_label_width,
                        &months_label_cache[usize::try_from(year - all_years.start).unwrap()],
                    );

                    if !cli.small {
                        y += 1;
                    }
                }

                y += 1;
            }
        }
        Output::YearByYear => {
            let mut y = 0;

            for year in all_years.clone() {
                screen
                    .translate(y, 0)
                    .write(&format!("{year}"), cli.year_style);
                y += 1;

                for group in &groups {
                    let commits = group.commits_by_year(year);

                    if commits.is_empty() {
                        continue;
                    }

                    screen.translate(y, 8).write(&group.title, cli.title_style);
                    y += 1;

                    y += draw_year_commits(
                        &mut screen.translate(y, 8),
                        commits,
                        year,
                        cli.first_weekday(),
                        cli.small,
                        cli.numbers,
                        cli.months,
                        &colors,
                        &weekdays_label,
                        weekdays_label_width,
                        &months_label_cache[usize::try_from(year - all_years.start).unwrap()],
                    );

                    if !cli.small {
                        y += 1;
                    }
                }

                y += 1;
            }
        }
        Output::MonthByMonth => {
            let mut y = 0;

            // Rather simplistic and should be somewhere inside terminal.rs.
            let title_width = groups
                .iter()
                .map(|group| group.title.chars().count())
                .max()
                .unwrap_or(0);

            let items = StrftimeItems::new("%b").parse().unwrap();

            let month_names = (1..=12)
                .map(|month| {
                    NaiveDate::from_ymd_opt(1970, month, 1)
                        .unwrap()
                        .format_with_items(items.as_slice().iter())
                        .to_string()
                })
                .collect::<Box<[_]>>();

            for year in all_years.clone() {
                let mut year_empty = true;

                for month in 1..=12 {
                    let mut month_empty = true;

                    for group in &groups {
                        let commits = group.commits_by_year(year);

                        if commits.is_empty() || commits.is_month_empty(month) {
                            continue;
                        }

                        if year_empty {
                            year_empty = false;

                            screen
                                .translate(y, 0)
                                .write(&format!("{year}"), cli.year_style);
                        }

                        let mut x = 5;

                        if month_empty {
                            month_empty = false;

                            let i = usize::try_from(month - 1).unwrap();
                            screen
                                .translate(y, x)
                                .write(&month_names[i], cli.year_style);
                        }

                        x += 3 + 4;

                        screen.translate(y, x).write(&group.title, cli.title_style);

                        x += title_width + 1;

                        draw_month_line(
                            &mut screen.translate(y, x),
                            year,
                            month,
                            cli.numbers,
                            &colors,
                            |i| commits.commits_per_day0(i),
                        );

                        y += 1;
                    }

                    if !month_empty {
                        y += 1;
                    }
                }
            }
        }
    }

    screen.write_ansi(BufWriter::new(stdout().lock())).unwrap();

    std::process::exit(0);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, PartialEq)]
    struct CellSurface {
        y: usize,
        x: usize,
        cell: Cell,
    }

    impl Surface for Option<CellSurface> {
        fn put(&mut self, y: usize, x: usize, cell: Cell) {
            assert!(self.is_none());
            *self = Some(CellSurface { y, x, cell });
        }
    }

    #[test]
    fn test_parse_color() {
        assert_eq!(
            parse_color("").unwrap_err().to_string(),
            "expected default or hex color"
        );
        assert_eq!(
            parse_color("#12345x").unwrap_err().to_string(),
            "non-hex digit found in string"
        );
        assert_eq!(
            parse_color("#1234567").unwrap_err().to_string(),
            "exactly 6 digits expected"
        );
        assert_eq!(parse_color("default").unwrap(), Color::default());
        assert_eq!(parse_color("#123456").unwrap(), Color::hex(0x123456));
    }

    #[test]
    fn test_parse_style() {
        let black = Color::hex(0x000000);
        let white = Color::hex(0xffffff);
        assert_eq!(parse_style("").unwrap(), Style::default());
        assert_eq!(parse_style(",,,").unwrap(), Style::default());
        assert_eq!(
            parse_style(" ").unwrap_err().to_string(),
            "invalid attribute:  "
        );
        assert_eq!(
            parse_style("x=y").unwrap_err().to_string(),
            "invalid attribute: x=y"
        );
        assert_eq!(parse_style("none").unwrap(), Style::default());
        assert_eq!(
            parse_style("bold").unwrap(),
            Style::default().with_bold(true)
        );
        assert_eq!(parse_style("bold,none").unwrap(), Style::default());
        assert_eq!(
            parse_style("fg=").unwrap_err().to_string(),
            "invalid color: expected default or hex color"
        );
        assert_eq!(
            parse_style("fg=#ffffff").unwrap(),
            Style::default().with_fg(white)
        );
        assert_eq!(
            parse_style("bg=").unwrap_err().to_string(),
            "invalid color: expected default or hex color"
        );
        assert_eq!(
            parse_style("bg=#000000").unwrap(),
            Style::default().with_bg(black)
        );
        assert_eq!(
            parse_style("fg=#ffffff,bg=#000000,bold").unwrap(),
            Style::default()
                .with_fg(white)
                .with_bg(black)
                .with_bold(true)
        );
    }

    #[test]
    fn test_parse_color_or_style() {
        assert_eq!(
            parse_color_or_style("").unwrap(),
            ColorOrStyle::Style(Style::default())
        );
        assert_eq!(
            parse_color_or_style("bold").unwrap(),
            ColorOrStyle::Style(Style::default().with_bold(true))
        );
        assert_eq!(
            parse_color_or_style("#ff0000").unwrap(),
            ColorOrStyle::Color(Color::hex(0xff0000).into())
        );
        assert_eq!(
            parse_color_or_style("x").unwrap_err().to_string(),
            "invalid attribute: x"
        );
    }

    #[test]
    fn test_heat_colors() {
        let black = Color::hex(0x000000);
        let white = Color::hex(0xffffff);
        assert_eq!(
            HeatColors::new(&[ColorOrStyle::Color(black.into())], 1).get(0),
            Style::default().with_bg(black).with_fg(white),
        );

        let a = Style::default().with_fg(Color::hex(1));
        let b = Style::default().with_fg(Color::hex(2));
        let c = Style::default().with_fg(Color::hex(3));
        assert!(a != b);
        assert!(a != c);
        assert!(b != c);
        let colors = HeatColors::new(
            &[
                ColorOrStyle::Style(a),
                ColorOrStyle::Style(b),
                ColorOrStyle::Style(c),
            ],
            3,
        );
        assert_eq!(colors.get(0), a);
        assert_eq!(colors.get(1), b);
        assert_eq!(colors.get(2), b);
        assert_eq!(colors.get(3), b);
        assert_eq!(colors.get(4), c);
        assert_eq!(colors.get(5), c);
        assert_eq!(colors.get(6), c);
        assert_eq!(colors.get(1000), c);
        assert_eq!(colors.max(), 6);
    }

    #[test]
    fn test_draw_half_squares() {
        let bgs = [
            BackgroundColor::new(Color::default()),
            BackgroundColor::new(Color::hex(0x000000)),
            BackgroundColor::new(Color::hex(0xffffff)),
        ];

        let squares = [
            None,
            Some(BackgroundColor::new(Color::default())),
            Some(BackgroundColor::new(Color::hex(0x000000))),
            Some(BackgroundColor::new(Color::hex(0xffffff))),
        ];

        for bg in bgs {
            for upper in squares {
                for lower in squares {
                    let mut surface = None::<CellSurface>;

                    draw_half_squares(&mut surface, bg, 2, 1, |y, x| match (y, x) {
                        (0, 0) => upper,
                        (1, 0) => lower,
                        _ => unreachable!(),
                    });

                    if upper.is_none() && lower.is_none() {
                        assert_eq!(surface, None);
                    } else {
                        let CellSurface { y, x, cell } = surface.unwrap();
                        let style = cell.style();

                        let upper = upper.unwrap_or(bg);
                        let lower = lower.unwrap_or(bg);

                        assert_eq!((y, x), (0, 0));

                        match cell.text() {
                            ' ' => {
                                assert_eq!(upper, style.bg());
                                assert_eq!(lower, style.bg());
                            }
                            '▀' => {
                                assert_eq!(upper.try_into(), Ok(style.fg()));
                                assert_eq!(lower, style.bg());
                                assert!(lower != upper);
                                assert!(style.bg().is_default());
                            }
                            '▄' => {
                                assert_eq!(upper, style.bg());
                                assert_eq!(lower.try_into(), Ok(style.fg()));
                                assert!(lower != upper);
                            }
                            _ => unreachable!(),
                        }
                    }
                }
            }
        }
    }
}
