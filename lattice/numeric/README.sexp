((name "numeric")
 (purpose "Numerical computing, signal processing, and interpolation")
 (description "Numerical algorithms including complex numbers, FFT/DFT,
convolution, digital filters, wavelets, spectral analysis, and interpolation.
Provides the mathematical foundation for signal processing, curve fitting,
and scientific computing.")
 (modules
  ((complex.ss "Complex number arithmetic - 56 tests")
   (dft.ss "FFT and DFT algorithms - 46 tests")
   (convolution.ss "Convolution operations")
   (polynomial.ss "Numeric polynomials - descending order, vector-based")
   (digital-filters.ss "IIR/FIR digital filters, Butterworth, Chebyshev")
   (window-functions.ss "Hann, Hamming, Blackman, Kaiser windows")
   (wavelet.ss "Haar, Daubechies wavelet transforms")
   (spectral-analysis.ss "STFT, spectrogram, Welch PSD")
   (signal-poly.ss "Filter polynomial algebra - stability, simplification, cascade")
   (interpolate.ss "Interpolation, splines, Bezier, curve fitting - 69 tests")))
 (dependencies (base linalg))
 (exports
  ;; === interpolate.ss ===
  ;; Linear interpolation
  (lerp "Linear interpolation: a + t*(b-a)")
  (lerp-inverse "Inverse lerp: find t given value")
  (interp-linear "Piecewise linear interpolation from points")

  ;; Polynomial interpolation
  (lagrange-basis "Lagrange basis polynomial L_i(x)")
  (interp-lagrange "Lagrange polynomial interpolation")
  (divided-differences "Newton's divided differences")
  (interp-newton "Newton polynomial interpolation")

  ;; Hermite interpolation
  (interp-hermite "Cubic Hermite interpolation with tangents")
  (hermite-tangent-estimate "Catmull-Rom tangent estimation")

  ;; Cubic splines
  (cubic-spline-natural "Natural cubic spline coefficients")
  (spline-eval "Evaluate spline at point")
  (interp-cubic-spline "Cubic spline interpolation")

  ;; Bezier curves
  (bezier-linear "Linear Bezier (line segment)")
  (bezier-quadratic "Quadratic Bezier curve")
  (bezier-cubic "Cubic Bezier curve")
  (bezier-general "Arbitrary degree Bezier via de Casteljau")
  (bezier-derivative "Tangent vector of Bezier curve")

  ;; Curve fitting
  (polyfit "Least squares polynomial fitting")
  (linreg "Simple linear regression (slope . intercept)")
  (linreg-r2 "Coefficient of determination R^2")

  ;; Chebyshev approximation
  (chebyshev-nodes "Chebyshev nodes in [-1,1]")
  (chebyshev-nodes-interval "Chebyshev nodes in [a,b]")
  (chebyshev-t "Chebyshev polynomial T_n(x)")
  (chebyshev-coeffs "Chebyshev expansion coefficients")
  (chebyshev-eval "Evaluate Chebyshev series (Clenshaw)")

  ;; B-splines
  (bspline-basis "B-spline basis N_{i,p}(x) via Cox-de Boor")
  (bspline-curve "B-spline curve evaluation"))

 (features
  "=== Signal Processing ==="
  "  Complex numbers (rectangular/polar, transcendentals)"
  "  FFT/DFT (radix-2, O(n log n))"
  "  Convolution (direct, FFT-based, 2D)"
  "  Digital filters (FIR, IIR, Butterworth, Chebyshev)"
  "  Wavelets (Haar, Daubechies)"
  "  Spectral analysis (STFT, spectrogram, Welch PSD)"
  ""
  "=== Interpolation ==="
  "  Linear (lerp, piecewise)"
  "  Polynomial (Lagrange, Newton divided differences)"
  "  Hermite (cubic with tangents)"
  "  Natural cubic splines"
  "  B-splines (Cox-de Boor recursion)"
  ""
  "=== Curve Fitting ==="
  "  Least squares polynomial fitting"
  "  Linear regression with R^2"
  ""
  "=== Approximation ==="
  "  Chebyshev nodes and polynomials"
  "  Chebyshev series expansion"
  "  Clenshaw evaluation algorithm"
  ""
  "=== Curves ==="
  "  Bezier curves (linear, quadratic, cubic)"
  "  De Casteljau algorithm for arbitrary degree"
  "  Bezier derivatives/tangents")

 (usage-examples
  ((interpolation
    "(lerp 0 10 0.5)                    ; => 5"
    "(interp-lagrange '(0 1 2) '(0 1 4) 1.5) ; => 2.25"
    "(interp-cubic-spline xs ys 1.5)   ; smooth interpolation")

   (bezier
    "(bezier-cubic p0 p1 p2 p3 0.5)    ; point on curve"
    "(bezier-general control-pts t)    ; arbitrary degree"
    "(bezier-derivative pts t)         ; tangent vector")

   (fitting
    "(linreg xs ys)                    ; => (slope . intercept)"
    "(linreg-r2 xs ys)                 ; => 0.99..."
    "(polyfit xs ys 2)                 ; quadratic fit")

   (chebyshev
    "(chebyshev-nodes 5)               ; optimal sampling points"
    "(chebyshev-t 3 x)                 ; T_3(x) = 4x^3 - 3x")))

 (performance
  "FFT: O(n log n)"
  "Lagrange interpolation: O(n^2)"
  "Newton interpolation: O(n^2) setup, O(n) evaluation"
  "Cubic spline: O(n) setup (tridiagonal), O(log n) evaluation"
  "Bezier (de Casteljau): O(n^2)"
  "Chebyshev evaluation (Clenshaw): O(n)")

 (notes
  "Transcendental functions are in fp/numeric/transcendental.ss"
  "For polynomial GCD/division, use algebra/poly-bridge.ss alongside polynomial.ss"
  "Load polynomial.ss BEFORE poly-bridge.ss to avoid name collisions"
  "Splines use natural boundary conditions (S''=0 at endpoints)"
  "Bezier curves are parametric in t from [0,1]"))
