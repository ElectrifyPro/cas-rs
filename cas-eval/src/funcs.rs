/// Computes the factorial of a number.
pub fn factorial(n: f64) -> f64 {
    (2..=(n as u64)).product::<u64>() as f64
}
