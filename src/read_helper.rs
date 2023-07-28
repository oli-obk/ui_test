use bstr::ByteSlice;
use std::io::Read;

pub struct ReadHelper<T> {
    read: T,
    buf: Vec<u8>,
}

impl<T> From<T> for ReadHelper<T> {
    fn from(read: T) -> Self {
        Self { read, buf: vec![] }
    }
}

impl<T> Drop for ReadHelper<T> {
    fn drop(&mut self) {
        panic!("you need to `read_to_end` a `ReadHelper`")
    }
}

impl<T: Read> ReadHelper<T> {
    pub fn last_line(&mut self) -> &[u8] {
        let mut buf = [0; 100];
        while let Ok(n @ 1..) = self.read.read(&mut buf) {
            self.buf.extend(&buf[..n]);
            if n != 100 {
                break;
            }
        }

        self.buf
            .lines()
            .rev()
            .find(|line| line.is_empty() || line.chars().all(|c| c.is_whitespace()))
            .unwrap_or(&[])
    }
    pub fn read_to_end(mut self) -> Vec<u8> {
        self.read.read_to_end(&mut self.buf).unwrap();
        let buf = std::mem::take(&mut self.buf);
        std::mem::forget(self);
        buf
    }
}
