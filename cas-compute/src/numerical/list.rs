#[cfg(feature = "sync")]
use std::{sync::{Arc, Mutex}};

#[cfg(not(feature = "sync"))]
use std::{cell::RefCell, rc::Rc};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use super::Value;

/// A list of values.
///
/// In `cas-rs`, a list is a reference to a vector of values. This is done to allow efficient
/// cloning of lists, as well as mutation of lists in-place. References are passed around
/// by default, which can result in somewhat confusing behavior, for example:
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct List {
    /// The values in the list.
    #[cfg(feature = "sync")]
    pub values: Arc<Mutex<Vec<Value>>>,

    /// The values in the list.
    #[cfg(not(feature = "sync"))]
    pub values: Rc<RefCell<Vec<Value>>>,
}

impl List {
    /// Returns true if the two lists point to the same inner vector of values.
    pub fn ptr_eq(&self, other: &Self) -> bool {
        #[cfg(feature = "sync")]
        {
            Arc::ptr_eq(&self.values, &other.values)
        }

        #[cfg(not(feature = "sync"))]
        {
            Rc::ptr_eq(&self.values, &other.values)
        }
    }

    /// Returns a raw pointer to the inner vector of values.
    pub(crate) fn data_ptr(&self) -> *const Vec<Value> {
        #[cfg(feature = "sync")]
        {
            let values = self.values.lock().unwrap();
            let reference = &*values;
            reference as *const _
        }

        #[cfg(not(feature = "sync"))]
        {
            self.values.as_ptr()
        }
    }

    /// Returns `true` if the list is empty.
    pub fn is_empty(&self) -> bool {
        #[cfg(feature = "sync")]
        {
            let values = self.values.lock().unwrap();
            values.is_empty()
        }

        #[cfg(not(feature = "sync"))]
        {
            let values = self.values.borrow();
            values.is_empty()
        }
    }

    /// Borrows the inner vector of values.
    #[cfg(feature = "sync")]
    pub fn borrow(&self) -> std::sync::MutexGuard<'_, Vec<Value>> {
        self.values.lock().unwrap()
    }

    /// Mutably borrows the inner vector of values.
    ///
    /// This method is identical to [`List::borrow`] but is provided for parity when the `sync`
    /// feature is disabled.
    #[cfg(feature = "sync")]
    pub fn borrow_mut(&self) -> std::sync::MutexGuard<'_, Vec<Value>> {
        self.values.lock().unwrap()
    }

    /// Borrows the inner vector of values.
    #[cfg(not(feature = "sync"))]
    pub fn borrow(&self) -> std::cell::Ref<'_, Vec<Value>> {
        self.values.borrow()
    }

    /// Mutably borrows the inner vector of values.
    #[cfg(not(feature = "sync"))]
    pub fn borrow_mut(&self) -> std::cell::RefMut<'_, Vec<Value>> {
        self.values.borrow_mut()
    }
}

impl PartialEq for List {
    fn eq(&self, other: &Self) -> bool {
        #[cfg(feature = "sync")]
        {
            let self_values = self.values.lock().unwrap();
            let other_values = other.values.lock().unwrap();
            *self_values == *other_values
        }

        #[cfg(not(feature = "sync"))]
        {
            self.values == other.values
        }
    }
}

impl From<Vec<Value>> for List {
    fn from(values: Vec<Value>) -> Self {
        #[cfg(feature = "sync")]
        {
            List {
                values: Arc::new(Mutex::new(values)),
            }
        }

        #[cfg(not(feature = "sync"))]
        {
            List {
                values: Rc::new(RefCell::new(values)),
            }
        }
    }
}
