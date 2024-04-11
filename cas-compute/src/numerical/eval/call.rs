use cas_parser::parser::ast::{assign::Param, call::Call};
use crate::eval_break;
use crate::funcs::combinatoric::Ncr;
use crate::numerical::{
    ctxt::{MAX_RECURSION_DEPTH, Ctxt, Func, UserFunc},
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
    value::Value,
};
use crate::primitive::float;
use rug::{ops::Pow, Float};

/// Computes the numerical derivative of an expression, using the higher-order differentiation
/// method found
/// [here](https://en.wikipedia.org/wiki/Numerical_differentiation#Higher_derivatives).
fn compute_derivative(
    call: &Call,
    implementation: &Func,
    ctxt: &mut Ctxt,
    initial: Value,
) -> Result<Value, Error> {
    let mut sum_left = float(0);
    let mut sum_right = float(0);
    let step = float(1e-32);

    let get_real = |value: Value| -> Result<Float, Error> {
        match value.coerce_float() {
            Value::Float(n) => Ok(n),
            value => Err(Error::new(
                call.outer_span().to_vec(),
                NonNumericDerivative {
                    expr_type: value.typename(),
                },
            )),
        }
    };

    let eval = |ctxt: &mut Ctxt, location: Float| -> Result<Value, Error> {
        match implementation {
            Func::Builtin(builtin) => {
                builtin.eval(ctxt, &mut Some(Value::Float(location)).into_iter())
                    .map_err(|err| err.into_error(call))
            },
            Func::UserFunc(UserFunc { header, body, .. }) => {
                let symbol = &header.params[0].symbol().name;
                ctxt.add_var(symbol, Value::Float(location));
                body.eval(ctxt)
            },
        }
    };

    let initial = get_real(initial)?;
    let derivatives = call.derivatives;

    for k in 0..=derivatives {
        // synonym for a = (-1)^(k + derivatives) to avoid overflow errors
        let a = if k % 2 == derivatives % 2 {
            1
        } else {
            -1
        };
        let b = Ncr::eval_static(derivatives.into(), k.into());

        // TODO: eval will do unnecessary typechecking on builtin functions
        let c = get_real(eval(ctxt, &initial + float(k * &step))?)?;
        let d = get_real(eval(ctxt, &initial - float(k * &step))?)?;

        sum_left += c * &b * a;
        sum_right += d * &b * a;
    }

    let result_left = sum_left / float(&step).pow(derivatives);
    let result_right = sum_right / float(-step).pow(derivatives);
    Ok(Value::Float((result_left + result_right) / 2))
}

impl Eval for Call {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        let func = ctxt.get_func(&self.name.name)
            .ok_or_else(|| Error::new(vec![self.name.span.clone()], UndefinedFunction {
                name: self.name.name.clone(),
                suggestions: ctxt.get_similar_funcs(&self.name.name)
                    .into_iter()
                    .map(|s| s.to_string())
                    .collect(),
            }))?;
        let mut ctxt = ctxt.clone();

        match func {
            Func::Builtin(builtin) => {
                let mut args = Vec::with_capacity(self.args.len());
                for arg in self.args.iter() {
                    args.push(eval_break!(arg, &mut ctxt));
                }

                if self.derivatives == 0 {
                    // no eval_break!; cannot break out of loops from within a function
                    builtin.eval(&ctxt, &mut args.into_iter())
                        .map_err(|err| err.into_error(self))
                } else {
                    if builtin.num_args() != 1 {
                        return Err(Error::new(self.outer_span().to_vec(), InvalidDerivativeArguments {
                            name: self.name.name.clone(),
                        }));
                    }
                    compute_derivative(self, func, &mut ctxt, args.swap_remove(0))
                }
            },
            Func::UserFunc(UserFunc { header, body, recursive }) => {
                let mut ctxt = ctxt.clone();
                if *recursive {
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
                let mut params = header.params.iter();
                loop {
                    match (args.next(), params.next()) {
                        // a positional argument
                        // evaluate it and add it to the context for use in the function body
                        (Some(arg), Some(param)) => {
                            let value = eval_break!(arg, &mut ctxt);
                            ctxt.add_var(&param.symbol().name, value);
                        },

                        // too many arguments were given
                        (Some(_), None) => return Err(Error::new(self.outer_span().to_vec(), TooManyArguments {
                            name: self.name.name.clone(),
                            expected: header.params.len(),
                            given: self.args.len(),
                            signature: header.to_string(),
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
                                        expected: header.params.len(),
                                        given: self.args.len(),
                                        signature: header.to_string(),
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
                    // no eval_break!; cannot break out of loops from within a function
                    body.eval(&mut ctxt)
                } else {
                    if header.params.len() != 1 {
                        return Err(Error::new(self.outer_span().to_vec(), InvalidDerivativeArguments {
                            name: self.name.name.clone(),
                        }));
                    }
                    let initial = ctxt.get_var(&header.params[0].symbol().name).unwrap();
                    compute_derivative(self, func, &mut ctxt, initial)
                };
                if *recursive {
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
            },
        }
    }
}
