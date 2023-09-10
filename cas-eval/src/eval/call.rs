use cas_parser::parser::{assign::Param, call::Call};
use crate::{
    builtins,
    ctxt::{MAX_RECURSION_DEPTH, Ctxt},
    error::{
        kind::{MissingArgument, StackOverflow, TooManyArguments, UndefinedFunction},
        Error,
    },
    eval::Eval,
    value::Value,
};

impl Eval for Call {
    fn eval(&self, ctxt: &mut Ctxt) -> Result<Value, Error> {
        // try using a builtin function
        if let Some(builtin) = builtins::get_builtin(&self.name.name) {
            let args = self.args.iter()
                .map(|arg| arg.eval(ctxt))
                .collect::<Result<Vec<_>, _>>()?;
            return builtin.eval(ctxt, &args)
                .map_err(|err| err.into_error(self));
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

        let result = func.body.eval(&mut ctxt);
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
