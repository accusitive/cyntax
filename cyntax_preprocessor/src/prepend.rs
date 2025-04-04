use std::{collections::VecDeque, fmt::Debug, iter::Peekable};

#[derive(Debug)]
pub struct PrependingPeekableIterator<I: Iterator + Debug> {
    pub queue: VecDeque<I::Item>,
    inner:     Peekable<I>,
}
impl<I: Iterator + Debug> Iterator for PrependingPeekableIterator<I>
where I::Item: Debug
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(item) = self.queue.pop_front() {
            return Some(item);
        } else {
            self.inner.next()
        }
    }
}
impl<I: Iterator + Debug> PrependingPeekableIterator<I> {
    pub fn new(i: I) -> Self {
        Self { queue: VecDeque::new(), inner: i.peekable() }
    }
    pub fn peek(&mut self) -> Option<&I::Item> {
        if let Some(front) = self.queue.front() {
            return Some(front);
        } else {
            self.inner.peek()
        }
    }
    pub fn prepend_extend<J: Iterator<Item = I::Item>>(&mut self, mut iter: J)
    where J::Item: Debug {
        let mut index = 0;
        while let Some(item) = iter.next() {
            self.queue.insert(index, item);
            index += 1;
        }
    }
}
