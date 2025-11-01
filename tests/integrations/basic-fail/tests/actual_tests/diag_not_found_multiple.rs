pub fn add(left: usize, right: usize) -> usize {
    left + right
}

fn main() {
    // NOTE: add this so that we don't need `check-pass` (which seems to remove the note about
    // missed diagnostics) but still get more than 1 missed diagnostics
    add("42", 3);
    //~^ E0308

    // NOTE: apparently the "there were n missed diagnostics" message only fires when there are
    // multiple missed diagnostics on the same line. We couldn't use `add("42", "3")` because that
    // results in _one_ "arguments to this function are incorrect" diagnostic, not two separate ones.
    #[rustfmt::skip]
    {
        add("42", 3); add("42", 3);
    };
}
