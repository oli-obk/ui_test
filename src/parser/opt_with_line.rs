use std::num::NonZeroUsize;

#[derive(Default, Debug, Clone, Copy)]
pub struct MaybeWithLine<T> {
    data: T,
    line: Option<NonZeroUsize>,
}

impl<T> std::ops::Deref for MaybeWithLine<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> MaybeWithLine<T> {
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

impl<T> From<WithLine<T>> for MaybeWithLine<T> {
    fn from(value: WithLine<T>) -> Self {
        Self {
            data: value.data,
            line: Some(value.line),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct WithLine<T> {
    data: T,
    line: NonZeroUsize,
}

impl<T> std::ops::Deref for WithLine<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> WithLine<T> {
    pub fn new(data: T, line: NonZeroUsize) -> Self {
        Self { data, line }
    }

    pub fn line(&self) -> NonZeroUsize {
        self.line
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> WithLine<U> {
        WithLine {
            data: f(self.data),
            line: self.line,
        }
    }

    pub fn into_inner(self) -> T {
        self.data
    }
}

#[derive(Debug, Clone, Copy)]
pub struct OptWithLine<T>(Option<WithLine<T>>);

impl<T> std::ops::Deref for OptWithLine<T> {
    type Target = Option<WithLine<T>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> From<Option<WithLine<T>>> for OptWithLine<T> {
    fn from(value: Option<WithLine<T>>) -> Self {
        Self(value)
    }
}

impl<T> From<WithLine<T>> for OptWithLine<T> {
    fn from(value: WithLine<T>) -> Self {
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
        Self(Some(WithLine::new(data, line)))
    }

    /// Tries to set the value if not already set. Returns newly passed
    /// value in case there was already a value there.
    #[must_use]
    pub fn set(&mut self, data: T, line: NonZeroUsize) -> Option<WithLine<T>> {
        let new = WithLine::new(data, line);
        if self.0.is_some() {
            Some(new)
        } else {
            self.0 = Some(new);
            None
        }
    }

    #[must_use]
    pub fn into_inner(self) -> Option<WithLine<T>> {
        self.0
    }
}
