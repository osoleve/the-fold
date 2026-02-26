((name "signal")
(purpose "Digital signal processing: filters, wavelets, spectral analysis")
(description "FIR/IIR digital filter design (Butterworth, Chebyshev, biquad cascades),\ndiscrete wavelet transforms (Haar, Daubechies), spectral analysis (STFT,\nspectrogram, Welch PSD), and polynomial algebra for filter stability analysis.")
(modules
  ((digital-filters.ss "FIR/IIR filters, Butterworth, Chebyshev, biquad cascades")
  (wavelet.ss "Haar, Daubechies wavelet transforms, denoising")
  (spectral-analysis.ss "STFT, spectrogram, Welch PSD, spectral features")
  (signal-poly.ss "Filter polynomial algebra - stability, cascade, deconvolution")))
(dependencies (numeric algebra))
(notes "Split from numeric skill for clearer tier separation (tier 1 vs numeric's tier 0)"
       "digital-filters requires complex, dft, convolution from numeric"
       "signal-poly bridges numeric polynomial algebra with filter representations"))
