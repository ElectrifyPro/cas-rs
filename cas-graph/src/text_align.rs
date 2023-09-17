use cairo::{Context, Error};

/// A trait to add the `show_text_align` to the [`Context`] type.
pub trait ShowTextAlign {
    /// Shows the given text at the given `(x, y)` position, with the given alignment.
    ///
    /// Draw text to the given context with the given alignment point.
    ///
    /// By default, text is rendered with the bottom left corner of the text at the given `(x, y)`
    /// point. This function allows you to specify an alignment point, which is a pair of `(x, y)`
    /// values, each between `0.0` and `1.0`, indicating the horizontal and vertical alignment of the
    /// text, respectively.
    ///
    /// For example, `(0.0, 0.0)` will align the bottom left corner of the text to the given `(x, y)`
    /// point, `(0.5, 0.5)` will center the text, `(1.0, 1.0)` will align the top right corner of the
    /// text to the given `(x, y)` point, etc.
    fn show_text_align(
        &self,
        text: &str,
        point: (f64, f64),
        align: (f64, f64),
    ) -> Result<(), Error>;
}

impl ShowTextAlign for Context {
    fn show_text_align(
        &self,
        text: &str,
        (x, y): (f64, f64),
        align: (f64, f64),
    ) -> Result<(), Error> {
        let extents = self.text_extents(text)?;
        let x = x - extents.width() * align.0;
        let y = y + extents.height() * align.1;
        self.move_to(x, y);
        self.show_text(text)?;
        Ok(())
    }
}
