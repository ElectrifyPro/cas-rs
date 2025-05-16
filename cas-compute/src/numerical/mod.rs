//! Helpers for numerical evaluation of CalcScript expressions.
//!
//! If you are looking to evaluate CalcScript expressions, see the related
//! [`cas-vm`](https://crates.io/crates/cas-vm) crate instead. Since version `cas-compute` 0.2.0,
//! this crate no longer provides this functionality.
//!
//! # Usage
//!
//! This crate contains tools to help you evaluate CalcScript expressions. It is meant to be used
//! in conjunction with [`cas-vm`], a virtual machine that executes CalcScript code, to tweak
//! evaluation and extract results.
//!
//! When interacting with [`cas-vm`], you'll mostly come into contact with the [`Value`] type,
//! which packages any value representable in the CalcScript language, including integers, floats,
//! complex numbers, lists, functions, and more. [`Value`] contains a number of convenience methods
//! to manipulate and convert values between types, as well as formatting them for display.
//!
//! [`cas-vm`]: https://crates.io/crates/cas-vm

#![cfg(feature = "numerical")]

pub mod builtin;
pub mod fmt;
pub mod func;
pub mod trig_mode;
pub mod value;

pub use value::Value;
