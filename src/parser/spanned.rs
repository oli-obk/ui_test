use std::num::NonZeroUsize;

use bstr::{ByteSlice, Utf8Error};

use crate::rustc_stderr::Span;

#[derive(Default, Debug, Clone, Copy)]
pub struct MaybeSpanned<T> {
    data: T,
    span: Option<Span>,
}

impl<T> std::ops::Deref for MaybeSpanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> MaybeSpanned<T> {
    /// Values from the `Config` struct don't have lines.
    pub fn new_config(data: T) -> Self {
        Self { data, span: None }
    }

    pub fn span(&self) -> Option<Span> {
        self.span
    }

    pub fn into_inner(self) -> T {
        self.data
    }
}

impl<T> From<Spanned<T>> for MaybeSpanned<T> {
    fn from(value: Spanned<T>) -> Self {
        Self {
            data: value.data,
            span: Some(value.span),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Spanned<T> {
    data: T,
    span: Span,
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<'a> Spanned<&'a str> {
    pub fn strip_prefix(&self, prefix: &str) -> Option<Self> {
        let data = self.data.strip_prefix(prefix)?;
        let mut span = self.span;
        span.column_start =
            NonZeroUsize::new(span.column_start.get() + prefix.chars().count()).unwrap();
        Some(Self { span, data })
    }

    pub fn strip_suffix(&self, suffix: &str) -> Option<Self> {
        let data = self.data.strip_suffix(suffix)?;
        let mut span = self.span;
        span.column_end =
            NonZeroUsize::new(span.column_end.get() - suffix.chars().count()).unwrap();
        Some(Self { span, data })
    }

    pub fn trim_start(&self) -> Self {
        let data = self.data.trim_start();
        let mut span = self.span;
        span.column_start = NonZeroUsize::new(
            span.column_start.get() + self.data.chars().count() - data.chars().count(),
        )
        .unwrap();
        Self { data, span }
    }

    pub fn trim_end(&self) -> Self {
        let data = self.data.trim_end();
        let mut span = self.span;
        span.column_end = NonZeroUsize::new(
            span.column_end.get() - (self.data.chars().count() - data.chars().count()),
        )
        .unwrap();
        Self { data, span }
    }

    pub fn trim(&self) -> Self {
        self.trim_start().trim_end()
    }

    pub fn split_at(&self, i: usize) -> (Self, Self) {
        let (a, b) = self.data.split_at(i);
        (
            Self {
                data: a,
                span: Span {
                    column_end: NonZeroUsize::new(self.span.column_start.get() + a.chars().count())
                        .unwrap(),
                    ..self.span
                },
            },
            Self {
                data: b,
                span: Span {
                    column_start: NonZeroUsize::new(
                        self.span.column_start.get() + a.chars().count(),
                    )
                    .unwrap(),
                    ..self.span
                },
            },
        )
    }

    pub fn split_once(&self, splitter: &str) -> Option<(Self, Self)> {
        let (a, b) = self.data.split_once(splitter)?;
        Some((
            Self {
                data: a,
                span: Span {
                    column_end: NonZeroUsize::new(self.span.column_start.get() + a.chars().count())
                        .unwrap(),
                    ..self.span
                },
            },
            Self {
                data: b,
                span: Span {
                    column_start: NonZeroUsize::new(
                        self.span.column_start.get() + a.chars().count() + splitter.chars().count(),
                    )
                    .unwrap(),
                    ..self.span
                },
            },
        ))
    }
}

impl<'a> Spanned<&'a [u8]> {
    pub fn strip_prefix(&self, prefix: &[u8]) -> Option<Self> {
        let data = self.data.strip_prefix(prefix)?;
        let mut span = self.span;
        span.column_start = NonZeroUsize::new(span.column_start.get() + prefix.len()).unwrap();
        Some(Self { span, data })
    }

    pub fn split_once_str(&self, splitter: &str) -> Option<(Self, Self)> {
        let (a, b) = self.data.split_once_str(splitter)?;
        Some((
            Self {
                data: a,
                span: Span {
                    column_end: NonZeroUsize::new(self.span.column_start.get() + a.len()).unwrap(),
                    ..self.span
                },
            },
            Self {
                data: b,
                span: Span {
                    column_start: NonZeroUsize::new(
                        self.span.column_start.get() + a.len() + splitter.len(),
                    )
                    .unwrap(),
                    ..self.span
                },
            },
        ))
    }

    pub fn to_str(self) -> Result<Spanned<&'a str>, Utf8Error> {
        Ok(Spanned {
            data: self.data.to_str()?,
            span: self.span,
        })
    }
}

impl<T> Spanned<T> {
    pub fn new(data: T, span: Span) -> Self {
        Self { data, span }
    }

    pub fn line(&self) -> NonZeroUsize {
        self.span.line_start
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            data: f(self.data),
            span: self.span,
        }
    }

    pub fn into_inner(self) -> T {
        self.data
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, Copy)]
pub struct OptWithLine<T>(Option<Spanned<T>>);

impl<T> std::ops::Deref for OptWithLine<T> {
    type Target = Option<Spanned<T>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> From<Option<Spanned<T>>> for OptWithLine<T> {
    fn from(value: Option<Spanned<T>>) -> Self {
        Self(value)
    }
}

impl<T> From<Spanned<T>> for OptWithLine<T> {
    fn from(value: Spanned<T>) -> Self {
        Self(Some(value))
    }
}

impl<T> Default for OptWithLine<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T> OptWithLine<T> {
    pub fn new(data: T, span: Span) -> Self {
        Self(Some(Spanned::new(data, span)))
    }

    /// Tries to set the value if not already set. Returns newly passed
    /// value in case there was already a value there.
    #[must_use]
    pub fn set(&mut self, data: T, span: Span) -> Option<Spanned<T>> {
        let new = Spanned::new(data, span);
        if self.0.is_some() {
            Some(new)
        } else {
            self.0 = Some(new);
            None
        }
    }

    #[must_use]
    pub fn into_inner(self) -> Option<Spanned<T>> {
        self.0
    }
}
