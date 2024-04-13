use cas_parser::parser::ast::{
    expr::Expr,
    index::Index,
    literal::Literal,
};
use crate::eval_break;
use crate::numerical::{
    ctxt::Ctxt,
    error::{
        kind::{
            IndexOutOfBounds,
            IndexOutOfRange,
            InvalidIndexTarget,
            InvalidIndexType,
            UndefinedVariable,
            UnsupportedIndexTarget,
        },
        Error,
    },
    eval::Eval,
    value::Value,
};

/// Check if the index is valid, returning the symbol the list is binded to along with the index
/// into the list.
pub fn check_index<'a>(
    expr: &Index,
    ctxt: &'a mut Ctxt,
) -> Result<(&'a mut Vec<Value>, usize), Error> {
    // TODO: currently, we only support the syntax: `list[0] = 5`
    // so sensible things like `[0, 1, 2][0]` will not work

    // first, ensure the target is a symbol with a list value
    // don't borrow the value yet, since we also need to compute the index first; just take the
    // symbol name
    let symbol = {
        if let Expr::Literal(Literal::Symbol(sym)) = &*expr.target {
            let value = ctxt.get_var_ref(&sym.name)
                .ok_or_else(|| {
                    Error::new(vec![expr.target.span()], UndefinedVariable {
                        name: sym.name.clone(),
                    })
                })?;

            if let Value::List(_) = value {
                &sym.name
            } else {
                return Err(Error::new(vec![expr.target.span()], InvalidIndexTarget {
                    expr_type: value.typename(),
                }));
            }
        } else {
            return Err(Error::new(vec![expr.target.span()], UnsupportedIndexTarget));
        }
    };
    let index = match expr.index.eval(ctxt)?.coerce_number() {
        Value::Integer(index) => index,
        other => return Err(Error::new(
            vec![expr.target.span(), expr.index.span()],
            InvalidIndexType {
                expr_type: other.typename(),
            },
        )),
    };

    let index = index.to_usize().ok_or_else(|| {
        Error::new(vec![expr.target.span(), expr.index.span()], IndexOutOfRange)
    })?;

    // now that we know the index is valid, init the borrow
    let Value::List(ref mut list) = ctxt.get_var_mut(symbol).unwrap() else {
        unreachable!()
    };

    let list_len = list.borrow().len();
    if index >= list_len {
        return Err(Error::new(
            vec![expr.target.span(), expr.index.span()],
            IndexOutOfBounds {
                len: list_len,
                index,
            },
        ));
    }

    todo!()
    // Ok((list, index))
}

impl Eval for Index {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        let (list, index) = check_index(self, ctxt)?;
        Ok(list[index].clone())
    }
}
