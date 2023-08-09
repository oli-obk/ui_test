use std::num::NonZeroUsize;

#[derive(Default, Debug, Clone, Copy)]
pub struct MaybeSpanned<T> {
    data: T,
    line: Option<NonZeroUsize>,
}

impl<T> std::ops::Deref for MaybeSpanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> MaybeSpanned<T> {
    pub fn new(data: T, line: NonZeroUsize) -> Self {
        Self {
            data,
            line: Some(line),
        }
    }

    /// Values from the `Config` struct don't have lines.
    pub fn new_config(data: T) -> Self {
        Self { data, line: None }
    }

    pub fn line(&self) -> Option<NonZeroUsize> {
        self.line
    }

    pub fn into_inner(self) -> T {
        self.data
    }
}

impl<T> From<Spanned<T>> for MaybeSpanned<T> {
    fn from(value: Spanned<T>) -> Self {
        Self {
            data: value.data,
            line: Some(value.line),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Spanned<T> {
    data: T,
    line: NonZeroUsize,
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> Spanned<T> {
    pub fn new(data: T, line: NonZeroUsize) -> Self {
        Self { data, line }
    }

    pub fn line(&self) -> NonZeroUsize {
        self.line
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            data: f(self.data),
            line: self.line,
        }
    }

    pub fn into_inner(self) -> T {
        self.data
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
    pub fn new(data: T, line: NonZeroUsize) -> Self {
        Self(Some(Spanned::new(data, line)))
    }

    /// Tries to set the value if not already set. Returns newly passed
    /// value in case there was already a value there.
    #[must_use]
    pub fn set(&mut self, data: T, line: NonZeroUsize) -> Option<Spanned<T>> {
        let new = Spanned::new(data, line);
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
