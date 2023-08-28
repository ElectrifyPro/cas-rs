use cas_parser::parser::{assign::Param, call::Call};
use crate::{
    builtins,
    ctxt::Ctxt,
    error::{
        kind::{MissingArgument, TooManyArguments, UndefinedFunction},
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
            return builtin(ctxt, &args)
                .map_err(|err| err.into_error(self));
        }

        let (header, body) = ctxt.get_func(&self.name.name)
            .ok_or_else(|| Error::new(vec![self.name.span.clone()], UndefinedFunction {
                name: self.name.name.clone(),
                suggestions: ctxt.get_similar_funcs(&self.name.name)
                    .into_iter()
                    .map(|(header, _)| header.name.name.clone())
                    .collect(),
            }))?;
        let mut ctxt = ctxt.clone();

        let mut index = 0;
        let mut args = self.args.iter();
        let mut params = header.params.iter();
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
                    expected: header.params.len(),
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
                                expected: header.params.len(),
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

        body.eval(&mut ctxt)
    }
}
