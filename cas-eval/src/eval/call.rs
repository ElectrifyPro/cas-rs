use cas_parser::parser::{assign::Param, call::Call};
use crate::{
    builtins::{self, Builtin},
    consts::float,
    ctxt::{MAX_RECURSION_DEPTH, Ctxt, Func},
    error::{
        kind::{
            InvalidDerivativeArguments,
            MissingArgument,
            NonNumericDerivative,
            StackOverflow,
            TooManyArguments,
            UndefinedFunction,
        },
        Error,
    },
    eval::Eval,
    funcs::choose,
    value::Value,
};
use rug::{ops::Pow, Float};
use std::ops::Range;

/// A trait to help with computing derivatives of functions.
trait Derv {
    /// The call site of the function.
    fn call_site(&self) -> Vec<Range<usize>>;

    /// The number of derivatives to compute.
    fn derivatives(&self) -> u8;

    /// Evaluates the function at the given value.
    fn eval(&self, ctxt: &mut Ctxt, float: Float) -> Result<Value, Error>;

    // /// Evaluates the derivative of the function at the given value.
    // fn estimate_derivative(&self, ctxt: &Ctxt, float: Float) -> Result<Float, Error> {
    //     if self.derivatives() == 0 {
    //         self.eval(ctxt, float)
    //     } else {
    //         if self.num_args() != 1 {
    //             return Err(Error::new(self.call_site(), InvalidDerivativeArguments {
    //                 name: self.name().into(),
    //             }));
    //         }
    //         compute_derivative(self, ctxt, float)
    //     }
    // }
}

/// For builtin functions.
impl Derv for (&Call, &dyn Builtin) {
    fn call_site(&self) -> Vec<Range<usize>> {
        self.0.outer_span().to_vec()
    }

    fn derivatives(&self) -> u8 {
        self.0.derivatives
    }

    fn eval(&self, ctxt: &mut Ctxt, float: Float) -> Result<Value, Error> {
        self.1.eval(ctxt, &[Value::Number(float)])
            .map_err(|err| err.into_error(&self.0))
    }
}

/// For user-defined functions.
impl Derv for (&Call, &Func) {
    fn call_site(&self) -> Vec<Range<usize>> {
        self.0.outer_span().to_vec()
    }

    fn derivatives(&self) -> u8 {
        self.0.derivatives
    }

    fn eval(&self, ctxt: &mut Ctxt, float: Float) -> Result<Value, Error> {
        let symbol = &self.1.header.params[0].symbol().name;
        ctxt.add_var(symbol, Value::Number(float));
        self.1.body.eval(ctxt)
    }
}

/// Computes the numerical derivative of an expression, using the higher-order differentiation
/// method found
/// [here](https://en.wikipedia.org/wiki/Numerical_differentiation#Higher_derivatives).
fn compute_derivative(derv: &dyn Derv, ctxt: &mut Ctxt, initial: Value) -> Result<Value, Error> {
    let mut sum_left = float(0);
    let mut sum_right = float(0);
    let step = float(1e-32);

    let get_real = |value: Value| -> Result<Float, Error> {
        match value.coerce_real() {
            Value::Number(n) => Ok(n),
            value => Err(Error::new(derv.call_site(), NonNumericDerivative {
                expr_type: value.typename(),
            })),
        }
    };

    let initial = get_real(initial)?;
    let derivatives = derv.derivatives();

    for k in 0..=derivatives {
        let a = float(-1).pow(k + derivatives);
        let b = choose(derivatives.into(), k.into());

        // TODO: eval will do unnecessary typechecking on builtin functions
        let c = get_real(derv.eval(ctxt, &initial + float(k * &step))?)?;
        let d = get_real(derv.eval(ctxt, &initial - float(k * &step))?)?;

        sum_left += float(&a * &b) * c;
        sum_right += a * b * d;
    }

    let result_left = sum_left / float(&step).pow(derivatives);
    let result_right = sum_right / float(-step).pow(derivatives);
    Ok(Value::Number((result_left + result_right) / float(2)))
}

impl Eval for Call {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        // try using a builtin function
        if let Some(builtin) = builtins::get_builtin(&self.name.name) {
            let mut args = self.args.iter()
                .map(|arg| arg.eval(ctxt))
                .collect::<Result<Vec<_>, _>>()?;
            return if self.derivatives == 0 {
                builtin.eval(ctxt, &args)
                    .map_err(|err| err.into_error(self))
            } else {
                if builtin.num_args() != 1 {
                    return Err(Error::new(self.outer_span().to_vec(), InvalidDerivativeArguments {
                        name: self.name.name.clone(),
                    }));
                }
                compute_derivative(&(self, builtin.as_ref()), ctxt, args.swap_remove(0))
            };
        }

        let func = ctxt.get_func(&self.name.name)
            .ok_or_else(|| Error::new(vec![self.name.span.clone()], UndefinedFunction {
                name: self.name.name.clone(),
                suggestions: ctxt.get_similar_funcs(&self.name.name)
                    .into_iter()
                    .map(|func| func.header.name.name.clone())
                    .collect(),
            }))?;
        let mut ctxt = ctxt.clone();
        if func.recursive {
            ctxt.stack_depth += 1;
        }

        if ctxt.stack_depth > MAX_RECURSION_DEPTH {
            // TODO
            ctxt.max_depth_reached = true;

            // we do not include the call span
            //
            // this is because this call span is the span of the function call within the *function
            // definition*, not the user's input
            //
            // this error will be propogated up to the top level of the call stack, where `self` is
            // now representing the function call of the user's input
            return Err(Error::new(vec![], StackOverflow));
        }

        let mut index = 0;
        let mut args = self.args.iter();
        let mut params = func.header.params.iter();
        loop {
            match (args.next(), params.next()) {
                // a positional argument
                // evaluate it and add it to the context for use in the function body
                (Some(arg), Some(param)) => {
                    let value = arg.eval(&mut ctxt)?;
                    ctxt.add_var(&param.symbol().name, value);
                },

                // too many arguments were given
                (Some(_), None) => return Err(Error::new(self.outer_span().to_vec(), TooManyArguments {
                    name: self.name.name.clone(),
                    expected: func.header.params.len(),
                    given: self.args.len(),
                })),

                // no argument was given for this parameter
                // use the default value if there is one
                (None, Some(param)) => {
                    // if there is no default, that's an error
                    let value = match param {
                        Param::Symbol(_) => return Err(Error::new(
                            self.outer_span().to_vec(),
                            MissingArgument {
                                name: self.name.name.clone(),
                                index,
                                expected: func.header.params.len(),
                                given: self.args.len(),
                            },
                        )),
                        Param::Default(_, expr) => expr.eval(&mut ctxt),
                    }?;
                    ctxt.add_var(&param.symbol().name, value);
                },

                // begin evaluation
                (None, None) => break,
            }

            index += 1;
        }

        let result = if self.derivatives == 0 {
            func.body.eval(&mut ctxt)
        } else {
            if func.header.params.len() != 1 {
                return Err(Error::new(self.outer_span().to_vec(), InvalidDerivativeArguments {
                    name: self.name.name.clone(),
                }));
            }
            let initial = ctxt.get_var(&func.header.params[0].symbol().name).unwrap();
            compute_derivative(&(self, func), &mut ctxt, initial)
        };
        if func.recursive {
            ctxt.stack_depth -= 1;
        }

        if result.is_err() && ctxt.max_depth_reached {
            // we are now at the top level of a stack overflow error, so we can insert the span of
            // the user's input
            result.map_err(|mut err| {
                err.spans.extend(self.outer_span().to_vec());
                err
            })
        } else {
            result
        }
    }
}
