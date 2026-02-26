((name "numeric")
(purpose "Foundation numerical transforms: complex numbers, FFT, convolution, windowing")
(description "Core numerical primitives that signal processing, autodiff, and statistics\nbuild upon. Complex number arithmetic (rectangular/polar, transcendentals),\nradix-2 FFT, windowing functions, convolution/correlation, and numeric\npolynomials in descending coefficient order.")
(modules
  ((complex.ss "Complex number arithmetic - 67 tests")
  (complex-bridge.ss "Complex-linalg bridge")
  (dft.ss "FFT and DFT algorithms - 29 tests")
  (polynomial.ss "Numeric polynomials - descending order, vector-based")
  (window-functions.ss "Hann, Hamming, Blackman, Kaiser windows")
  (convolution.ss "Convolution and correlation operations")
  (fft-convolve.ss "FFT-based convolution for large signals")))
(dependencies (linalg algebra))
(notes "Tier 0 foundation skill"
       "Signal processing (filters, wavelets) moved to lattice/signal/"
       "Interpolation/fitting moved to lattice/interpolate/"
       "Interval/affine arithmetic moved to lattice/interval/"
       "PDE methods (FEM, time stepping) moved to lattice/pde/"))
