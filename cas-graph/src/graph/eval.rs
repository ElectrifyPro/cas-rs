use cas_eval::{ctxt::Ctxt, eval::Eval, value::Value};
use cas_parser::parser::expr::Expr;
use super::{GraphOptions, GraphPoint};

/// Evaluate the given expression and returns the points to draw.
///
/// The options of the graph are used alongside the estimated derivative of the expression as an
/// optimization. If the evaluation of the expression results in a point outside the viewport, we
/// can start to assume that the next points might be outside the viewport as well. This allows us
/// to cut down on the number of points we need to evaluate.
///
/// Also in general, when the slope of the expression is steep, the step size can be somewhat
/// larger, since variation in the slope will not be as noticeable, in terms of someone observing
/// the graph.
///
/// However, when the slope of the expression is shallow, the step size must be smaller. For
/// example, if the step size is too large, we could end up skipping past a relative minimum /
/// maximum of the expression, which would be extremely obvious.
pub(crate) fn evaluate_expr(
    expr: &Expr,
    x_bounds: (f64, f64),
    options: GraphOptions,
) -> Vec<GraphPoint<f64>> {
    let mut ctxt = Ctxt::default();
    let mut points = Vec::new();

    let mut last_point = None;
    let mut current_point = None;

    let mut x = x_bounds.0;
    let min_step_len = options.scale.0 / 32.0;

    let y_bounds = (options.center.1 - options.scale.1, options.center.1 + options.scale.1);

    while x <= x_bounds.1 {
        ctxt.add_var("x", x.into());
        if let Ok(Value::Number(float)) = expr.eval(&mut ctxt).map(|v| v.coerce_real()) {
            let point = GraphPoint(x, float.to_f64());
            points.push(point);

            last_point = current_point;
            current_point = Some(point);
        }

        // adjust our step length based on the slope of the expression
        // NOTE: this would be so nice with let chains
        let step_len = if current_point.map(|p| p.1 < y_bounds.0 || p.1 > y_bounds.1).unwrap_or(false) {
            // if the expression moves outside the graph viewport, we hardcode the step length to
            // be an arbitrary high step length so we can get to a visible point more quickly
            min_step_len * 4.0
        } else if let (Some(last), Some(current)) = (last_point, current_point) {
            // in our slope calculation, we divide by the y-scale again to account for the scale
            // changing the visual slope of the expression
            //
            // for example, if we graph y=x^3 with a very large y-scale, an observer looking at the
            // graph will see a very shallow slope in the area around x=0, even though the true
            // slope of the expression past x=0 gets very steep very fast
            let slope = (current.1 - last.1) / (current.0 - last.0) / options.scale.1;

            // the larger the slope, the larger the step length can be
            // not too large though, so we can catch discontinuities
            min_step_len.max(slope.abs() / 10.0).min(min_step_len * 4.0)
        } else {
            min_step_len
        };
        x += step_len;
    }

    points
}
