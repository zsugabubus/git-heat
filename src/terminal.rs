use std::{io, ops::Deref};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Color {
    Default,
    TrueColor { r: u8, g: u8, b: u8 },
}

impl Color {
    #[must_use]
    pub const fn default() -> Self {
        Self::Default
    }

    #[must_use]
    pub const fn is_default(self) -> bool {
        matches!(self, Self::Default)
    }

    #[must_use]
    pub const fn rgb(r: u8, g: u8, b: u8) -> Self {
        Self::TrueColor { r, g, b }
    }

    #[must_use]
    pub const fn hex(x: u32) -> Self {
        let [_, r, g, b] = x.to_be_bytes();
        Self::rgb(r, g, b)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct BackgroundColor(Color);

impl BackgroundColor {
    pub const fn new(color: Color) -> Self {
        Self(color)
    }

    #[must_use]
    pub fn contrast_text(self) -> ForegroundColor {
        match *self {
            Color::Default => Color::Default,
            Color::TrueColor { r, g, b } => {
                use apca::*;

                fn f(x: u8) -> f32 {
                    f32::from(x) / 255.0f32
                }

                match lightness(Background((f(r), f(g), f(b)))) {
                    Lightness::Dark => Color::rgb(255, 255, 255),
                    Lightness::Light => Color::rgb(0, 0, 0),
                }
            }
        }
        .into()
    }
}

impl Deref for BackgroundColor {
    type Target = Color;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<Color> for BackgroundColor {
    fn from(value: Color) -> Self {
        Self::new(value)
    }
}

impl TryFrom<ForegroundColor> for BackgroundColor {
    type Error = ();

    fn try_from(value: ForegroundColor) -> Result<Self, Self::Error> {
        match *value {
            Color::Default => Err(()),
            x => Ok(x.into()),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ForegroundColor(Color);

impl ForegroundColor {
    pub const fn new(color: Color) -> Self {
        Self(color)
    }
}

impl Deref for ForegroundColor {
    type Target = Color;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<Color> for ForegroundColor {
    fn from(value: Color) -> Self {
        Self::new(value)
    }
}

impl TryFrom<BackgroundColor> for ForegroundColor {
    type Error = ();

    fn try_from(value: BackgroundColor) -> Result<Self, Self::Error> {
        match *value {
            Color::Default => Err(()),
            x => Ok(x.into()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Style {
    fg: ForegroundColor,
    bg: BackgroundColor,
    bold: bool,
}

impl Style {
    #[must_use]
    pub const fn default() -> Self {
        Self {
            fg: ForegroundColor::new(Color::default()),
            bg: BackgroundColor::new(Color::default()),
            bold: false,
        }
    }

    #[must_use]
    pub fn is_default(&self) -> bool {
        self == &Style::default()
    }

    #[must_use]
    pub fn with_fg<T: Into<ForegroundColor>>(mut self, color: T) -> Self {
        self.fg = color.into();
        self
    }

    #[must_use]
    pub fn fg(&self) -> ForegroundColor {
        self.fg
    }

    #[must_use]
    pub fn with_bg<T: Into<BackgroundColor>>(mut self, color: T) -> Self {
        self.bg = color.into();
        self
    }

    #[must_use]
    pub fn bg(&self) -> BackgroundColor {
        self.bg
    }

    #[must_use]
    pub fn with_bold(mut self, yes: bool) -> Self {
        self.bold = yes;
        self
    }

    #[must_use]
    pub fn bold(&self) -> bool {
        self.bold
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Cell {
    ch: char,
    style: Style,
}

impl Cell {
    const ERASED: Self = Self {
        ch: '\0',
        style: Style::default(),
    };

    #[must_use]
    pub const fn new(ch: char, style: Style) -> Self {
        Self { ch, style }
    }

    #[must_use]
    fn is_erased(&self) -> bool {
        self == &Self::ERASED
    }

    #[must_use]
    pub fn text(&self) -> char {
        self.ch
    }

    #[must_use]
    pub fn style(&self) -> &Style {
        &self.style
    }
}

pub trait Surface {
    fn put(&mut self, y: usize, x: usize, cell: Cell);

    fn write(&mut self, s: &str, style: Style) {
        for (x, c) in s.chars().enumerate() {
            self.put(0, x, Cell::new(c, style));
        }
    }

    #[must_use]
    fn translate(&mut self, y: usize, x: usize) -> Translate<Self> {
        Translate { target: self, y, x }
    }

    fn draw(&mut self, source: &SurfaceBuffer) {
        for (y, line) in source.lines.iter().enumerate() {
            for (x, cell) in line
                .cells
                .iter()
                .enumerate()
                .filter(|(_, cell)| !cell.is_erased())
            {
                self.put(y, x, *cell);
            }
        }
    }
}

#[derive(Debug, Clone)]
struct Line {
    cells: Vec<Cell>,
}

impl Line {
    #[must_use]
    fn new() -> Self {
        Self { cells: Vec::new() }
    }

    fn put(&mut self, x: usize, cell: Cell) {
        if x >= self.cells.len() {
            self.cells.resize(x + 1, Cell::ERASED);
        }
        self.cells[x] = cell;
    }

    #[must_use]
    fn width(&self) -> usize {
        self.cells.len()
    }
}

pub struct SurfaceBuffer {
    lines: Vec<Line>,
}

impl SurfaceBuffer {
    #[must_use]
    pub fn new() -> Self {
        Self { lines: Vec::new() }
    }

    #[must_use]
    pub fn height(&self) -> usize {
        self.lines.len()
    }

    #[must_use]
    pub fn width(&self) -> usize {
        self.lines
            .iter()
            .map(|line| line.width())
            .max()
            .unwrap_or(0)
    }

    pub fn put(&mut self, y: usize, x: usize, cell: Cell) {
        if y >= self.lines.len() {
            self.lines.resize_with(y + 1, Line::new);
        }
        self.lines[y].put(x, cell);
    }

    pub fn write_ansi<W: io::Write>(&self, mut writer: W) -> io::Result<()> {
        fn write_color<W: io::Write>(
            mut writer: W,
            color: Color,
            set_param: u8,
            reset_param: u8,
        ) -> io::Result<()> {
            match color {
                Color::Default => {
                    write!(writer, "\x1b[{}m", reset_param)
                }
                Color::TrueColor { r, g, b } => {
                    write!(writer, "\x1b[{};2;{};{};{}m", set_param, r, g, b)
                }
            }
        }

        for line in &self.lines {
            let mut pen = Style::default();

            for cell in &line.cells {
                let style = &cell.style;

                let c = if cell.ch == '\0' { ' ' } else { cell.ch };

                if pen.fg != style.fg && c != ' ' {
                    pen.fg = style.fg;
                    write_color(&mut writer, *pen.fg, 38, 39)?;
                }

                if pen.bg != style.bg {
                    pen.bg = style.bg;
                    write_color(&mut writer, *pen.bg, 48, 49)?;
                }

                if pen.bold != style.bold {
                    pen.bold = style.bold;
                    writer.write_all(if pen.bold { b"\x1b[1m" } else { b"\x1b[22m" })?;
                }

                writer.write_all(c.encode_utf8(&mut [0; 4]).as_bytes())?;
            }

            if !pen.is_default() {
                writer.write_all(b"\x1b[0m")?;
            }

            writer.write_all(b"\n")?;
        }

        Ok(())
    }

    #[cfg(test)]
    #[must_use]
    fn to_ansi_string(&self) -> String {
        let mut v = Vec::new();
        self.write_ansi(&mut v).unwrap();
        String::from_utf8(v).unwrap()
    }
}

impl Surface for SurfaceBuffer {
    fn put(&mut self, y: usize, x: usize, cell: Cell) {
        self.put(y, x, cell);
    }
}

#[derive(Debug)]
pub struct Translate<'a, S: ?Sized> {
    target: &'a mut S,
    y: usize,
    x: usize,
}

impl<'a, S> Surface for Translate<'a, S>
where
    S: Surface,
{
    fn put(&mut self, y: usize, x: usize, cell: Cell) {
        self.target.put(self.y + y, self.x + x, cell);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn contrast_text() {
        assert_eq!(
            BackgroundColor::new(Color::default()).contrast_text(),
            ForegroundColor::new(Color::default())
        );
        assert_eq!(
            BackgroundColor::new(Color::hex(0xffffff)).contrast_text(),
            ForegroundColor::new(Color::hex(0x000000))
        );
        assert_eq!(
            BackgroundColor::new(Color::hex(0x000000)).contrast_text(),
            ForegroundColor::new(Color::hex(0xffffff))
        );
        assert_eq!(
            BackgroundColor::new(Color::hex(0xa3a3a3)).contrast_text(),
            ForegroundColor::new(Color::hex(0xffffff))
        );
    }

    #[test]
    fn color() {
        assert!(ForegroundColor::try_from(BackgroundColor::new(Color::default())).is_err());
        assert!(BackgroundColor::try_from(ForegroundColor::new(Color::default())).is_err());

        let color = Color::hex(0xffffff);
        assert_eq!(
            ForegroundColor::try_from(BackgroundColor::new(color)),
            Ok(ForegroundColor::from(color))
        );
        assert_eq!(
            BackgroundColor::try_from(ForegroundColor::new(color)),
            Ok(BackgroundColor::from(color))
        );
    }

    #[test]
    fn style() {
        assert_eq!(Style::default().fg(), Color::default().into());
        assert_eq!(Style::default().bg(), Color::default().into());
        assert!(!Style::default().bold());

        let color = Color::hex(0xabcdef);
        assert_eq!(Style::default().with_fg(color).fg(), color.into());
        assert_eq!(Style::default().with_bg(color).bg(), color.into());
        assert!(Style::default().with_bold(true).bold());
    }

    #[test]
    fn translate() {
        let style = Style::default();
        let mut surface = SurfaceBuffer::new();
        surface.write("a", style);
        surface.translate(0, 1).write("b", style);
        surface.translate(1, 0).write("c", style);
        surface.translate(0, 1).translate(1, 0).write("d", style);
        assert_eq!(surface.to_ansi_string(), "ab\ncd\n");
    }

    #[test]
    fn write() {
        let default = Style::default();
        let black = Color::hex(0x000000);
        let white = Color::hex(0xffffff);

        assert_eq!(SurfaceBuffer::new().to_ansi_string(), "");

        let mut surface = SurfaceBuffer::new();
        surface.translate(0, 2).write("a", default);
        assert_eq!(surface.to_ansi_string(), "  a\n");

        let mut surface = SurfaceBuffer::new();
        surface.write(" a", Style::default().with_fg(black));
        assert_eq!(surface.to_ansi_string(), " \x1b[38;2;0;0;0ma\x1b[0m\n");

        let mut surface = SurfaceBuffer::new();
        surface.write(
            "a",
            Style::default()
                .with_fg(black)
                .with_bg(white)
                .with_bold(true),
        );
        surface.translate(0, 1).write("b", default);
        assert_eq!(
            surface.to_ansi_string(),
            "\x1b[38;2;0;0;0m\x1b[48;2;255;255;255m\x1b[1ma\x1b[39m\x1b[49m\x1b[22mb\n"
        );

        let mut surface = SurfaceBuffer::new();
        surface.write(
            "a",
            Style::default()
                .with_fg(black)
                .with_bg(white)
                .with_bold(true),
        );
        surface
            .translate(0, 1)
            .write("b", Style::default().with_fg(white).with_bg(black));
        assert_eq!(surface.to_ansi_string(), "\x1b[38;2;0;0;0m\x1b[48;2;255;255;255m\x1b[1ma\x1b[38;2;255;255;255m\x1b[48;2;0;0;0m\x1b[22mb\x1b[0m\n");
    }
}
