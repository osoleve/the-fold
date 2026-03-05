;;; skills/numeric.sexp — Skill curation file

(skill numeric
  (description
    "Foundation numerical transforms, approximation, and generic solvers: complex\n    number arithmetic, DFT/FFT, windowing, convolution, numeric polynomials,\n    interpolation/curve fitting, and vector-space-parameterized ODE integrators.\n    The vector-space abstraction enables the same algorithms (Euler, RK4, etc.)\n    to work over lists, Scheme vectors, or any custom representation.")

  (keywords (numerics fft dft complex-numbers convolution correlation polynomial window-functions hann hamming blackman kaiser interpolation spline bezier hermite lagrange chebyshev b-spline curve-fitting regression least-squares vector-space ode euler rk4 midpoint heun integrator))

  (concepts (numerical-computing))

  (modules
    complex
    complex-bridge
    dft
    numeric/polynomial
    window-functions
    convolution
    fft-convolve
    interpolate
    vector-space
    ode-explicit
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
