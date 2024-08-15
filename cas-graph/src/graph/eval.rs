use cas_compute::numerical::{ctxt::Ctxt, eval::Eval, value::Value};
use super::{analyzed::{AnalyzedExpr, Variable}, GraphOptions, GraphPoint};

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
    match analyzed.independent {
        Variable::X => {
            let (bounds, cross_axis_bounds) = {
                let x_bounds = (options.center.0 - options.scale.0, options.center.0 + options.scale.0);
                let y_bounds = (options.center.1 - options.scale.1, options.center.1 + options.scale.1);
                (x_bounds, y_bounds)
            };
            evaluate_cartesian_expr_helper(
                analyzed,
                bounds,
                cross_axis_bounds,
                |previous, current| (current.1 - previous.1) / (current.0 - previous.0) / options.scale.1,
                (options.scale.0 / 64.0, options.scale.0 / 16.0),
                |x, y| GraphPoint(x, y),
            )
        },
        Variable::Y => {
            let (bounds, cross_axis_bounds) = {
                let x_bounds = (options.center.0 - options.scale.0, options.center.0 + options.scale.0);
                let y_bounds = (options.center.1 - options.scale.1, options.center.1 + options.scale.1);
                (y_bounds, x_bounds)
            };
            evaluate_cartesian_expr_helper(
                analyzed,
                bounds,
                cross_axis_bounds,
                |previous, current| (current.0 - previous.0) / (current.1 - previous.1) / options.scale.0,
                (options.scale.1 / 64.0, options.scale.1 / 16.0),
                |y, x| GraphPoint(x, y),
            )
        },
        Variable::Theta => evaluate_polar_expr(analyzed, options),
    }
}

/// Helper function to iteratively evaluate a cartesian expression (either in terms of `x` or `y`).
fn evaluate_cartesian_expr_helper(
    analyzed: &AnalyzedExpr,
    bounds: (f64, f64),
    cross_axis_bounds: (f64, f64),
    compute_slope: impl Fn(GraphPoint<f64>, GraphPoint<f64>) -> f64,
    step_len: (f64, f64),
    create_point: impl Fn(f64, f64) -> GraphPoint<f64>,
) -> Vec<GraphPoint<f64>> {
    let mut ctxt = Ctxt::default();
    let mut points = Vec::new();

    let mut last_point = None;
    let mut current_point = None;

    let mut current_trace = bounds.0;
    let (min_step_len, max_step_len) = step_len;
    let mut last_slope: Option<f64> = None;

    while current_trace <= bounds.1 {
        ctxt.add_var(analyzed.independent.as_str(), current_trace.into());
        if let Ok(Value::Float(float)) = analyzed.expr.eval(&mut ctxt).map(|v| v.coerce_float()) {
            let point = create_point(current_trace, float.to_f64());
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

/// Helper function to iteratively evaluate a polar expression (in terms of `theta`).
fn evaluate_polar_expr(
    analyzed: &AnalyzedExpr,
    options: GraphOptions,
) -> Vec<GraphPoint<f64>> {
    let mut ctxt = Ctxt::default();
    let mut points = Vec::new();

    let mut current_trace = 0.0;
    let mut r_at_zero = None;
    let mut next_trace_revolution = 2.0 * std::f64::consts::PI;

    // arbitrary bounds to prevent infinite loops
    let bound = (options.scale.0 * options.scale.1) * 2.0 * std::f64::consts::PI;
    let step_len = (options.scale.0 + options.scale.1) / 1024.0;

    while current_trace <= bound {
        ctxt.add_var(analyzed.independent.as_str(), current_trace.into());
        if let Ok(Value::Float(r)) = analyzed.expr.eval(&mut ctxt).map(|v| v.coerce_float()) {
            let f64_r = r.to_f64();

            points.push(GraphPoint(
                f64_r * current_trace.cos(),
                f64_r * current_trace.sin(),
            ));

            if current_trace == 0.0 {
                r_at_zero = Some(f64_r);
            } else if current_trace > next_trace_revolution {
                if let Some(r_at_zero) = r_at_zero {
                    // make a test for periodicity: if the evaluation at increments of 2π are the
                    // same, assume the expression is periodic and stop evaluating
                    // TODO: this is very naive and can still fail for some expressions
                    let r_at_next_revolution = {
                        ctxt.add_var(analyzed.independent.as_str(), next_trace_revolution.into());
                        if let Ok(Value::Float(r)) = analyzed.expr.eval(&mut ctxt).map(|v| v.coerce_float()) {
                            r.to_f64()
                        } else {
                            break;
                        }
                    };

                    let epsilon = (options.scale.0 + options.scale.1) / 2.0f64.powi(32);
                    if (r_at_zero - r_at_next_revolution).abs() <= epsilon {
                        break;
                    }

                    next_trace_revolution += 2.0 * std::f64::consts::PI;
                }
            }
        }

        current_trace += step_len;
    }

    points
}
