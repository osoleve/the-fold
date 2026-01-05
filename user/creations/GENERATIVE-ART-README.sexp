;;; GENERATIVE ART AND MUSIC TOOLKIT
;;; A collection of algorithmic creative tools for The Fold

((title . "Generative Art and Music Toolkit")
 (description . "Algorithmic systems for creating music patterns, visual fractals, and procedural art")
 (created . "2026-01-05")
 (tier . builder)

 (modules
   ((wave-synth
      (location . "shell/wave-synth.ss")
      (type . library)
      (description . "Audio waveform synthesis with oscillators, modulation, and effects")
      (features
        ("Pure functional oscillators: sine, square, triangle, sawtooth, noise")
        ("Modulation: amplitude (tremolo), frequency (vibrato), ADSR envelope")
        ("Effects: harmonics, distortion")
        ("ASCII waveform visualization and spectrum analysis"))
      (demo . "(load \"shell/wave-synth.ss\") (import (shell wave-synth)) (synth-demo)"))

    (music-gen
      (location . "shell/music-gen.ss")
      (type . library)
      (description . "Algorithmic music pattern generation")
      (features
        ("Euclidean rhythms - mathematically perfect beat distribution")
        ("Fibonacci melodies - golden ratio sequences")
        ("Cellular automaton rhythms - emergent patterns from simple rules")
        ("Golden ratio sequences - natural harmonic progressions")
        ("ASCII notation and multi-track rendering"))
      (demo . "(load \"shell/music-gen.ss\") (import (shell music-gen)) (music-gen-demo)"))

    (lsystem-gen
      (location . "user/creations/lsystem-gen.ss")
      (type . script)
      (description . "L-System fractal generator with turtle graphics")
      (features
        ("Classic fractals: Koch curve, Dragon curve, Sierpinski, plant growth")
        ("Turtle graphics interpreter")
        ("ASCII rendering of fractal patterns"))
      (usage . "scheme --script user/creations/lsystem-gen.ss"))

    (hash-art
      (location . "user/creations/hash-art.ss")
      (type . script)
      (description . "Generative art from cryptographic hashes")
      (features
        ("Deterministic patterns from SHA-256 hashes")
        ("Multiple visualization modes")
        ("Demonstrates content-addressing as art"))
      (usage . "scheme --script user/creations/hash-art.ss"))

    (music-gen-demo
      (location . "user/creations/music-gen-demo.ss")
      (type . script)
      (description . "Interactive music generation demonstrations")
      (usage . "scheme --script user/creations/music-gen-demo.ss")))

 (philosophy
   "These tools embrace algorithmic creativity - using mathematical rules
    and emergent processes to create art. The synthesis is pure functional:
    oscillators and generators return lambdas that can be composed,
    modulated, and combined. Everything is deterministic yet surprising.")

 (related-modules
   "harmonograph.ss" "rose-curve.ss" "mandala.ss" "wireworld.ss")

 (examples
   ((basic-tone
      "Create a simple sine wave visualization"
      "(let ((wave (sine-wave 440 0)))
         (render-waveform-ascii wave 0.01 60 10))")

    (tremolo-effect
      "Apply amplitude modulation for tremolo"
      "(let ((tremolo (modulate-amplitude
                        (sine-wave 440 0)
                        (sine-wave 6 0)
                        0.5)))
         (render-waveform-ascii tremolo 0.5 80 12))")

    (euclidean-pattern
      "Generate Euclidean rhythm (common in world music)"
      "(euclidean-rhythm 5 8)  ; => (1 0 1 0 1 0 1 0)")

    (fractal-tree
      "Generate plant growth L-system"
      "(lsystem-generate plant-rules \"X\" 5)")))

 (notes
   "All code is self-contained with no external dependencies.
    Visualizations use ASCII art for universal terminal compatibility.
    Designed for creative exploration and education."))
