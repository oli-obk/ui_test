#[derive(Default, Debug, Clone)]
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
}

#[derive(Debug, Clone)]
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

impl<T> Default for OptWithLine<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T> OptWithLine<T> {
    pub fn new(data: T, line: usize) -> Self {
        Self(Some(WithLine::new(data, line)))
    }
    #[must_use]
    pub fn set(&mut self, data: T, line: usize) -> Option<WithLine<T>> {
        let old = self.0.take();
        self.0 = Some(WithLine::new(data, line));
        old
    }
}
