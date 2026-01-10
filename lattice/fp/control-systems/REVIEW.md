# Control Systems Review

## Overview
The `lattice/fp/control-systems` module provides a solid foundation for linear control theory in a pure functional style. It covers core representations (State Space, Transfer Functions) and standard design methodologies (PID, Pole Placement, LQR, LQG).

## Strengths
- **Clean Abstractions**: The separation between `state-space.ss` and `transfer-function.ss` is clear and idiomatic.
- **Comprehensive Basic Design**: Includes both classical (PID, Lead/Lag) and modern (LQR, LQG, Pole Placement) control techniques.
- **Algorithm Variety**: Provides multiple tuning methods for PID (Ziegler-Nichols, IMC) and pole placement (Ackermann, Bass-Gura).
- **Pure Implementation**: The side-effect-free implementation is excellent for testing, reasoning, and parallel execution.

## Critical Gaps & Missed Opportunities

### 1. Discretization (c2d)
**Severity: High**
There is no visible function to convert a continuous-time state-space model to a discrete-time one (C2D) using Zero-Order Hold (ZOH). This is essential for implementing designed controllers on digital hardware.
*   **Requirement**: Implement `ss-c2d` which computes $\Phi = e^{AT_s}$ and $\Gamma = \int_0^{T_s} e^{A\tau}B d\tau$.
*   **Method**: Compute matrix exponential of the block matrix $\begin{bmatrix} A & B \\ 0 & 0 \end{bmatrix} T_s$.

### 2. Numerical Robustness of Matrix Exponential
**Severity: Medium**
`matrix-exp-taylor` in `state-space.ss` uses a naive Taylor series expansion. This is known to be numerically unstable and inefficient for many systems (the "Dubious Ways to Compute the Matrix Exponential" problem).
*   **Recommendation**: Implement **Scaling and Squaring** with Padé approximation for a robust $e^A$.

### 3. Riccati & Lyapunov Solvers
**Severity: Medium**
The LQR/LQG synthesis (`solve-care`) relies on a Newton-Kleinman iteration coupled with a simple gradient-based Lyapunov solver (`lyapunov-solve-simple`).
*   **Issue**: The gradient-based solver ($X_{k+1} = X_k + \alpha R$) is extremely slow for high accuracy and can be unstable for stiff systems.
*   **Recommendation**: Implement a direct solver for the Lyapunov equation $A^TP + PA = -Q$. For small $n$ ($n < 20$), solving the linear system $(I \otimes A^T + A^T \otimes I)vec(P) = -vec(Q)$ is robust and likely faster than the current iteration. For larger $n$, the Bartels-Stewart algorithm is standard.

### 4. MIMO Pole Placement
**Severity: Low**
`pole-placement-ackermann` and `pole-placement-bass-gura` are explicitly SISO (Single Input Single Output).
*   **Opportunity**: Generalize pole placement for MIMO systems (e.g., using Kautsky-Nichols-Van Dooren or simple cyclic input selection) to leverage the full state-space capabilities.

### 5. Kalman Filter Duality
**Severity: Low (Confusion)**
There are two "Kalman Filters":
1.  `kalman.ss`: A scalar/log-space filter for online estimation (e.g., fuel costs).
2.  `lqg` in `controller-design.ss`: A matrix-based steady-state filter for control.
*   **Recommendation**: Clarify naming. Perhaps rename `kalman.ss` to `scalar-estimator.ss` or `online-filter.ss`, or implement a full `matrix-kalman.ss` that supports time-varying updates ($P_{k+1} = A P_k A^T + Q ...$) for consistency.

## Minor Considerations
- **Rank Computation**: `matrix-rank` uses QR decomposition with a diagonal threshold. While often sufficient, Singular Value Decomposition (SVD) is the gold standard for numerical rank determination.
- **H-infinity**: The current implementation only *analyzes* the H-infinity norm via brute-force frequency gridding. It does not support H-infinity *synthesis* (finding a controller to minimize the norm), which is a significant jump in complexity but a logical next step.

## Action Plan
1.  **Implement `ss-c2d`** to enable digital implementation of continuous designs.
2.  **Upgrade `matrix-exp`** to use Scaling and Squaring.
3.  **Refactor `solve-care`** to use a direct linear solver for the underlying Lyapunov steps for better performance/stability.
