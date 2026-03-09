/// GEMM Rust example — compute homology of Eilenberg-MacLane spaces.
///
/// Build:
///   GEMM_LIB_DIR=/path/to/dir cargo run --example example
fn main() {
    // H*(K(Z/2, 2); Z) up to degree 10
    println!("=== K(Z/2, 2) ===");
    match gemm::homology_p(2, 1, 2, 10) {
        Ok(result) => println!("{}", serde_json::to_string_pretty(&result).unwrap()),
        Err(e) => eprintln!("Error: {}", e),
    }

    println!();

    // H*(K(Z, 3); Z) up to degree 10
    println!("=== K(Z, 3) ===");
    match gemm::homology_z(3, 10) {
        Ok(result) => println!("{}", serde_json::to_string_pretty(&result).unwrap()),
        Err(e) => eprintln!("Error: {}", e),
    }

    println!();

    // Lean certificate for K(Z/3, 4)
    println!("=== Lean certificate for K(Z/3, 4) ===");
    match gemm::certificate(3, 1, 4, 15) {
        Ok(cert) => println!("{}...", &cert[..cert.len().min(300)]),
        Err(e) => eprintln!("Error: {}", e),
    }
}
