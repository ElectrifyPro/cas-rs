//! # Features
//!
//! - `mysql`: Derives [`mysql_common`] traits for various types provided by this crate.

#![cfg(feature = "numerical")]

pub mod builtin;
pub mod fmt;
pub mod func;
pub mod trig_mode;
pub mod value;
