(skill numeric
  (version "0.5.0")
  (tier 0)
  (path "lattice/numeric")
  (purity total)
  (stability stable)
  (fuel-bound "O(n log n) for FFT, O(n) for FIR/spline/interval-ops, O(log n) for spline-eval, O(n²) for Lagrange, O(k) for affine-ops where k=noise-symbols")
  (deps (linalg algebra))

  (description
   "Numerical computing, signal processing, interpolation, interval, and affine arithmetic.
    Provides complex number arithmetic, discrete Fourier transform (radix-2 FFT),
    digital filters (FIR, IIR, Butterworth, Chebyshev), convolution and correlation,
    wavelet transforms (Haar, Daubechies), spectral analysis (STFT, spectrogram),
    interpolation (linear, polynomial, spline, Hermite), Bezier curves, curve
    fitting (least squares, polynomial), Chebyshev approximation, rigorous
    interval arithmetic for verified numerical computation, and affine arithmetic
    for tighter bounds via correlation tracking (solves the dependency problem).")

  (keywords (numerics signal-processing fft dft complex-numbers digital-filters
             wavelets convolution spectral-analysis iir fir butterworth
             interpolation spline bezier curve-fitting regression chebyshev
             interval-arithmetic affine-arithmetic verified-computation bounds
             correlation-tracking dependency-problem noise-symbols))
  (aliases (signal dsp interp interval affine))

  (exports
   (complex
    make-complex complex? complex-real complex-imag real-num->complex i-value
    make-polar complex-magnitude complex-angle complex->polar polar->complex
    complex-add complex-sub complex-mul complex-div
    complex-negate complex-conjugate complex-reciprocal
    complex-equal? complex-zero? complex-real? complex-imaginary?
    complex-exp complex-log complex-pow complex-sqrt
    complex-sin complex-cos complex-tan complex-sinh complex-cosh complex-tanh
    complex->string complex-scale complex-dot complex-hermitian-product
    complex-zero complex-one complex-i complex-pi complex-e)

   (dft
    power-of-2? log2-int bit-reverse bit-reverse-copy
    dft-naive idft-naive
    fft-radix2-inplace! fft-radix2 ifft-radix2-inplace! ifft-radix2
    dft idft
    real->complex-vec complex->real-vec dft-real idft-real
    magnitude-spectrum phase-spectrum power-spectrum freq-bins
    zero-pad next-power-of-2 zero-pad-power-of-2)

   (window-functions
    rectangular-window hann-window hamming-window blackman-window
    bartlett-window kaiser-window bessel-i0
    apply-window apply-window-complex
    window-energy window-power make-window)

   (digital-filters
    sinc fir-lowpass fir-highpass fir-bandpass fir-bandstop fir-filter
    make-iir-filter iir-filter? iir-filter-b iir-filter-a iir-filter-signal
    make-biquad biquad? biquad-b0 biquad-b1 biquad-b2 biquad-a1 biquad-a2
    biquad-filter cascade-biquads
    butterworth-lowpass-poles bilinear-transform butterworth-lowpass
    chebyshev1-lowpass chebyshev1-poles
    poles-zeros->tf poly-from-roots poly-mul-binomial
    freqz eval-transfer-function eval-poly magnitude-response phase-response
    make-filter-state filter-state? filter-process-sample! filter-reset!
    dc-blocker one-pole-lowpass one-pole-highpass moving-average
    impulse-response step-response)

   (convolution
    convolve-direct-full convolve-direct-same convolve-direct-valid
    convolve-fft convolve convolve-circular convolve-2d
    correlate-direct correlate autocorrelate
    matched-filter find-peaks vector-reverse normalize-signal)

   (wavelet
    haar-scaling-filter haar-wavelet-filter
    daubechies-4-scaling-filter daubechies-4-wavelet-filter
    daubechies-6-scaling-filter daubechies-6-wavelet-filter
    qmf-wavelet-from-scaling reverse-filter
    convolve-downsample upsample-convolve
    dwt-step dwt idwt-step idwt
    get-wavelet-filters dwt-family idwt-family
    wavelet-decompose wavelet-reconstruct
    get-approximation get-details get-detail-level
    hard-threshold soft-threshold threshold-coefficients
    wavelet-denoise wavelet-energy wavelet-energy-distribution wavelet-energy-ratio)

   (spectral-analysis
    stft-frame stft istft
    spectrogram power-spectrogram log-spectrogram
    periodogram welch-psd
    spectrogram-frequencies spectrogram-times
    spectral-centroid spectral-bandwidth spectral-rolloff)

   (signal-poly
    filter-num-poly filter-den-poly filter-coprime? filter-simplify
    filter-stable? filter-cascade filter-parallel
    deconvolve deconvolve-exact?
    numeric->signal-poly signal->numeric-poly
    sig-poly-degree sig-poly-coeffs sig-poly-gcd
    sig-poly->string filter->string)

   (interpolate
    ;; Utilities
    binary-search-segment thomas-algorithm
    ;; Linear interpolation
    lerp lerp-inverse interp-linear
    ;; Polynomial interpolation
    lagrange-basis interp-lagrange
    divided-differences interp-newton
    ;; Hermite interpolation
    interp-hermite hermite-tangent-estimate
    ;; Cubic splines
    cubic-spline-natural spline-eval interp-cubic-spline
    ;; Bezier curves
    bezier-linear bezier-quadratic bezier-cubic
    bezier-general bezier-derivative
    ;; Least squares and fitting
    polyfit linreg linreg-r2
    ;; Chebyshev approximation
    chebyshev-nodes chebyshev-nodes-interval
    chebyshev-t chebyshev-coeffs chebyshev-eval
    ;; B-splines
    bspline-basis bspline-curve)

   (interval
    ;; Constructors and type
    make-interval interval interval? interval-singleton entire-interval
    ;; Accessors
    interval-lo interval-hi interval-mid interval-width interval-radius
    interval-magnitude interval-mignitude
    ;; Predicates
    interval-empty? interval-singleton? interval-contains? interval-contains-zero?
    interval-positive? interval-negative? interval-subset?
    intervals-overlap? intervals-disjoint?
    ;; Comparisons (three-valued)
    interval-definitely< interval-definitely<= interval-definitely> interval-definitely>=
    interval-possibly< interval-possibly<= interval-possibly> interval-possibly>=
    interval-definitely= interval-possibly=
    ;; Arithmetic (standard - fast, round-to-nearest)
    interval-neg interval-add interval-sub interval-mul interval-sqr
    interval-recip interval-div interval-scale
    ;; Arithmetic (rigorous - directed rounding, guaranteed enclosure)
    interval-add-rigorous interval-sub-rigorous interval-mul-rigorous
    interval-div-rigorous interval-sqrt-rigorous interval-sqr-rigorous
    interval-scale-rigorous
    ;; Directed rounding primitives
    fl-next-up fl-next-down
    add-down add-up sub-down sub-up mul-down mul-up div-down div-up
    sqrt-down sqrt-up
    ;; Elementary functions
    interval-abs interval-sqrt interval-pow interval-min interval-max
    interval-exp interval-log interval-sin interval-cos interval-tan
    interval-asin interval-acos interval-atan
    interval-sinh interval-cosh interval-tanh
    ;; Set operations
    interval-union interval-hull interval-hull-list interval-intersection interval-bisect
    ;; Coercion
    real->interval interval->string interval-print
    ;; Multi-dimensional boxes
    make-box box-dimension box-volume box-contains?
    ;; Constants
    pi-interval e-interval
    ;; Short aliases
    iv+ iv- iv* iv/)

   (affine
    ;; Noise symbol management
    affine-fresh-noise-id! affine-reset-noise-counter!
    ;; Constructors and type
    make-affine affine? affine-center affine-terms
    affine-constant affine-noise
    ;; Interval conversion
    affine-from-interval affine->interval affine-radius
    ;; Affine operations (correlation-preserving)
    affine-neg affine-add affine-sub affine-scale affine-add-constant
    ;; Non-affine operations
    affine-mul affine-sqr affine-recip affine-div affine-sqrt
    ;; Elementary functions
    affine-exp affine-log
    ;; Min/max
    affine-min affine-max affine-abs
    ;; Predicates
    affine-definitely-positive? affine-definitely-negative? affine-possibly-zero?
    affine-definitely< affine-definitely<= affine-definitely> affine-definitely>=
    ;; Display
    affine->string affine-print
    ;; Short aliases
    af+ af- af* af/ af-neg af-sqr af-sqrt af-exp af-log
    ;; Higher-level operations
    affine-sum affine-product affine-linear-combination affine-horner))

  (modules
   (complex "complex.ss"
    "Complex number representation and arithmetic. Rectangular and polar forms.
     Transcendental functions: exp, log, sqrt, trig, hyperbolic.")
   (dft "dft.ss"
    "Discrete Fourier Transform: naive O(n^2) and radix-2 FFT O(n log n).
     Forward and inverse transforms. Spectrum analysis utilities.")
   (window-functions "window-functions.ss"
    "Windowing functions for spectral analysis: Hann, Hamming, Blackman,
     Bartlett, Kaiser. Window energy and power normalization.")
   (digital-filters "digital-filters.ss"
    "FIR and IIR digital filter design. Butterworth and Chebyshev lowpass.
     Biquad sections, filter cascades, frequency response analysis.")
   (convolution "convolution.ss"
    "Linear convolution: direct and FFT-based. Correlation and autocorrelation.
     2D convolution for image processing. Matched filtering.")
   (wavelet "wavelet.ss"
    "Discrete wavelet transform: Haar, Daubechies-4, Daubechies-6 families.
     Multi-level decomposition and reconstruction. Denoising via thresholding.")
   (spectral-analysis "spectral-analysis.ss"
    "Short-time Fourier transform and spectrogram computation. Welch power
     spectral density estimation. Spectral features: centroid, bandwidth, rolloff.")
   (signal-poly "signal-poly.ss"
    "Polynomial algebra integration for signal processing. Filter stability analysis
     via Jury criterion. Filter simplification, cascade/parallel combination,
     deconvolution as polynomial division.")
   (interpolate "interpolate.ss"
    "Numerical interpolation and curve fitting. Linear, Lagrange, and Newton
     polynomial interpolation. Hermite and natural cubic spline interpolation.
     Bezier curves (linear, quadratic, cubic, arbitrary degree). Least squares
     polynomial fitting, linear regression. Chebyshev approximation and nodes.
     B-spline basis functions and curves.")
   (interval "interval.ss"
    "Interval arithmetic for verified numerical computation. Represents sets of
     real numbers with guaranteed enclosure. Supports arithmetic, comparisons
     (three-valued logic), set operations, and transcendental elementary functions
     (exp, log, sin, cos, tan, hyperbolic). Multi-dimensional boxes for n-dimensional
     computations. Foundation for global optimization, monotonicity analysis, and
     computer-aided proofs.")
   (affine "affine.ss"
    "Affine arithmetic for tighter bounds via correlation tracking. Solves the
     'dependency problem' in interval arithmetic where x - x yields [-r, r] instead
     of [0, 0]. Affine forms represent values as x₀ + Σxᵢεᵢ where εᵢ are noise
     symbols in [-1, 1]. When forms share noise symbols, operations automatically
     account for correlations. Supports all arithmetic ops plus exp, log, sqrt.
     Use affine-from-interval to convert intervals, affine->interval to convert back.")))
