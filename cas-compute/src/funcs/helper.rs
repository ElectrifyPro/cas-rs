#![cfg(feature = "numerical")]

/// Private helper function used by the `builtin` macro to advances an argument iterator and return
/// the next value, while also counting the number of arguments.
pub(crate) fn next_arg(
    args: &mut dyn Iterator<Item = crate::numerical::value::Value>,
    arg_count: &mut usize,
) -> Option<crate::numerical::value::Value> {
    if let Some(arg) = args.next() {
        *arg_count += 1;
        Some(arg)
    } else {
        None
    }
}

/// Private helper function used the `builtin` macro to advance an argument iterator to the end,
/// counting the remaining arguments. The total number of arguments counted is returned.
pub(crate) fn count_all_args(
    args: &mut dyn Iterator<Item = crate::numerical::value::Value>,
    arg_count: &mut usize,
) -> usize {
    while args.next().is_some() {
        *arg_count += 1;
    }

    *arg_count
}
