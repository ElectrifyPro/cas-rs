use cas_compiler::{item::{Item, SymbolDecl}, Compile, Compiler};
use cas_compute::numerical::value::Value;
use cas_vm::Vm;
use super::{analyzed::{AnalyzedExpr, Variable}, GraphOptions, GraphPoint};

/// Creates a VM ready to evaluate the given expression.
fn create_vm(analyzed: &AnalyzedExpr) -> Vm {
    let mut compiler = Compiler::new();

    // TODO: make the compiler think there is already a symbol declared at the start so that no
    // compile error is thrown on the first run
    compiler.add_item(
        &cas_parser::parser::ast::LitSym {
            name: analyzed.independent.as_str().to_string(),
            span: 0..0,
        },
        Item::Symbol(SymbolDecl { id: 0 }),
    ).unwrap();
    analyzed.expr.compile(&mut compiler).unwrap();
    Vm::from(compiler)
}

/// Evaluate the given expression and returns the points to draw.
///
/// The options of the graph are used alongside the estimated derivative of the expression as an
/// optimization. If the evaluation of the expression results in a point outside the viewport, we
/// can start to assume that the next points might be outside the viewport as well. This allows us
/// to cut down on the number of points we need to evaluate.
///
/// Also in general, when the slope of the expression does not vary wildly, the step size can be
/// somewhat larger, since variation in the slope will not be as noticeable in terms of someone
/// observing the graph.
///
/// However, when the slope of the expression does start varying, the step size must be smaller.
/// For example, if the step size is too large, we could end up skipping past a relative minimum /
/// maximum of the expression, which would be extremely obvious.
pub(crate) fn evaluate_expr(
    analyzed: &AnalyzedExpr,
    options: GraphOptions,
) -> Vec<GraphPoint<f64>> {
    let mut vm = create_vm(analyzed);
    let mut points = Vec::new();

    let mut last_point = None;
    let mut current_point = None;

    let (bounds, cross_axis_bounds) = {
        let x_bounds = (options.center.0 - options.scale.0, options.center.0 + options.scale.0);
        let y_bounds = (options.center.1 - options.scale.1, options.center.1 + options.scale.1);
        match analyzed.independent {
            Variable::X => (x_bounds, y_bounds),
            Variable::Y => (y_bounds, x_bounds),
            Variable::Theta => todo!("polar coordinates"),
        }
    };
    let compute_slope = |previous: GraphPoint<f64>, current: GraphPoint<f64>| {
        match analyzed.independent {
            Variable::X => (current.1 - previous.1) / (current.0 - previous.0) / options.scale.1,
            Variable::Y => (current.0 - previous.0) / (current.1 - previous.1) / options.scale.0,
            Variable::Theta => todo!("polar coordinates"),
        }
    };

    let mut current_trace = bounds.0;
    let min_step_len = options.scale.0 / 64.0;
    let max_step_len = options.scale.0 / 16.0;
    let mut last_slope: Option<f64> = None;

    while current_trace <= bounds.1 {
        let Some(symbol) = vm.sym_table.resolve_symbol(analyzed.independent.as_str()) else {
            unreachable!("symbol must exist");
        };
        vm.variables.insert(symbol.id, current_trace.into());

        if let Ok(Value::Float(float)) = vm.run().map(|v| v.coerce_float()) {
            let point = match analyzed.independent {
                Variable::X => GraphPoint(current_trace, float.to_f64()),
                Variable::Y => GraphPoint(float.to_f64(), current_trace),
                Variable::Theta => todo!("polar coordinates"),
            };
            points.push(point);

            last_point = current_point;
            current_point = Some(point);
        }

        // adjust our step length based on the slope of the expression
        // NOTE: this would be so nice with let chains
        let step_len = if current_point.map(|p| p.1 < cross_axis_bounds.0 || p.1 > cross_axis_bounds.1).unwrap_or(false) {
            // if the expression moves outside the graph viewport, we hardcode the step length to
            // be an arbitrary high step length so we can get to a visible point more quickly
            min_step_len * 2.0
        } else if let (Some(last), Some(current)) = (last_point, current_point) {
            // in our slope calculation, we divide by the y-scale again to account for the scale
            // changing the visual slope of the expression
            //
            // for example, if we graph y=x^3 with a very large y-scale, an observer looking at the
            // graph will see a very shallow slope in the area around x=0, even though the true
            // slope of the expression past x=0 gets very steep very fast
            let slope = compute_slope(last, current);

            if let Some(last) = last_slope {
                // if the slope of the expression is changing too much, lower the step length so we
                // can catch discontinuities
                let rate = (slope - last).abs() / last.abs();
                last_slope = Some(slope);
                (0.05 / rate).min(max_step_len).max(min_step_len)
            } else {
                last_slope = Some(slope);
                min_step_len
            }
        } else {
            min_step_len
        };
        current_trace += step_len;
    }

    points
}
