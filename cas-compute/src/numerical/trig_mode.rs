#[cfg(feature = "mysql")]
use mysql_common::prelude::FromValue;

#[cfg(feature = "serde")]
use serde_repr::{Deserialize_repr, Serialize_repr};

/// The trigonometric mode of a context. This will affect the evaluation of input to trigonometric
/// functions, and output from trigonometric functions.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
#[cfg_attr(feature = "mysql", derive(FromValue))]
#[cfg_attr(feature = "mysql", mysql(is_integer))]
#[cfg_attr(feature = "serde", derive(Serialize_repr, Deserialize_repr))]
#[repr(u8)]
pub enum TrigMode {
    /// Use radians.
    #[default]
    Radians,

    /// Use degrees.
    Degrees,
}
