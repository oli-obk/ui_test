#[derive(Default, Debug, Clone, Copy)]
pub struct WithLine<T> {
    data: T,
    line: usize,
}

impl<T> std::ops::Deref for WithLine<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> WithLine<T> {
    pub fn new(data: T, line: usize) -> Self {
        Self { data, line }
    }
    pub fn line(&self) -> usize {
        self.line
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> WithLine<U> {
        WithLine {
            data: f(self.data),
            line: self.line,
        }
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
    pub fn new(data: T, line: usize) -> Self {
        Self(Some(WithLine::new(data, line)))
    }

    /// Tries to set the value if not already set. Returns newly passed
    /// value in case there was already a value there.
    #[must_use]
    pub fn set(&mut self, data: T, line: usize) -> Option<WithLine<T>> {
        let new = WithLine::new(data, line);
        if self.0.is_some() {
            Some(new)
        } else {
            self.0 = Some(new);
            None
        }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> OptWithLine<U> {
        OptWithLine(self.0.map(|wl| wl.map(f)))
    }

    pub fn or(self, other: impl Into<Self>) -> Self {
        Self(self.0.or(other.into().into_inner()))
    }

    pub fn unwrap_or(self, other: impl Into<WithLine<T>>) -> WithLine<T> {
        self.0.unwrap_or_else(|| other.into())
    }

    #[must_use]
    pub fn into_inner(self) -> Option<WithLine<T>> {
        self.0
    }
}
