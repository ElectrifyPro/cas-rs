# Contribution guidelines

Thank you for considering contributing to `cas-rs`! There's a couple of ways you
can contribute, described below. I greatly appreciate any and all contributions
to `cas-rs`!

If you have any questions about contributing outside of what is covered here,
feel free to ask a question using
[GitHub discussions](https://github.com/ElectrifyPro/cas-rs/discussions).

## Reporting issues, requesting features

Use the [GitHub issue tracker](https://github.com/ElectrifyPro/cas-rs/issues).

When reporting an issue, please include as much detail as possible, including a
_**minimal example** that reproduces the issue_. A minimal example is the
smallest possible version of your code / configuration that still reproduces the
issue you're experiencing. Including one makes it significantly easier for
others to narrow down where in code the issue might be.

If you're requesting a feature, please include a detailed description of the
feature you have in mind, and a rationale for why you want or need it.

## General questions or help

Use [GitHub discussions](https://github.com/ElectrifyPro/cas-rs/discussions) for
general questions about `cas-rs` or contributing.

## Contributing code

Open a [pull request](https://github.com/ElectrifyPro/cas-rs/pulls) with your
changes.

You may want to discuss your proposed changes in an
[issue](#reporting-issues-requesting-features) before opening a pull request. If
you're working on a large change, it's a good idea to get feedback on your
approach before you spend a lot of time on it.

When contributing new features, you should include appropriate tests and
documentation for your changes. Tests will help make sure that your new feature
is working as intended, and also ensure that future changes from other
developers won't break your feature. Documentation is intended to help users of
the library understand how to use your new feature.

Tests should cover all new functionality (and probably pass), and documentation
should be clear and concise. If you made particular design decisions that you
think others should be aware of, please include those in documentation as well.
If a bug is fixed, include tests that reproduces the bug and now pass after your
fix is applied.

### Branching

There are two main
[branches](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-branches)
in `cas-rs`:

- `main`: This is the stable branch. Anyone should be able to clone this branch
  and build a fully functional version of the library. You will most likely base
  your work on this branch.
- `dev`: This is the development branch. This is where new features are being
  actively developed. Currently, this branch is significantly different from
  `main`.

Create a separate branch for your work based on the `main` branch so that you
won't come into conflict if `main` changes while you're working. When you're
ready to submit your changes, open a pull request against the `main` branch. Set
the base branch to `main`, and the compare branch to your feature branch,
indicating that you want to merge your changes into `main`.

### Code style

Currently, there is no strict style guide, though this may change in the future.
You should try to match the existing style of the code you're working on, and
default to using Rust style conventions.
