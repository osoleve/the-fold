;;; user/demos/README.sexp --- Demo Directory Documentation

(directory demos
  (purpose "Showcase demonstrations of Fold capabilities")
  (philosophy "Demoscene aesthetic - mathematical beauty meets terminal graphics")

  (contents
   ;; Static demo
   (demo lorenz-demo.ss
         (description "Static Lorenz attractor rendering")
         (output "Terminal display with ANSI colors")
         (usage "scheme --script user/demos/lorenz-demo.ss"))

   ;; Animated demos - Three variants
   (demo lorenz-signature.ss
         (description "THE CANONICAL DEMO - Perfect loop for showcasing")
         (output "lorenz-signature.gif")
         (status "RECOMMENDED")
         (features "ASCII rendering" "3D rotation" "ANSI color" "Optimized palette")
         (stats
          (dimensions "100x42 chars (800x588 pixels)")
          (frames 60)
          (duration "5 seconds at 12fps")
          (points 10000)
          (file-size "296KB"))
         (usage "scheme --script user/demos/lorenz-signature.ss")
         (purpose "Share this when someone asks: What can The Fold do?"))

   (demo lorenz-butterfly-demo.ss
         (description "Original quick-render version")
         (output "lorenz-butterfly.gif")
         (features "ASCII rendering" "3D rotation" "ANSI color" "GIF export")
         (stats
          (dimensions "100x42 chars (800x588 pixels)")
          (frames 48)
          (duration "4 seconds at 12fps")
          (points 8000)
          (file-size "244KB"))
         (usage "scheme --script user/demos/lorenz-butterfly-demo.ss"))

   (demo lorenz-butterfly-hq.ss
         (description "High-resolution cinema version")
         (output "lorenz-butterfly-hq.mp4")
         (features "HD resolution" "H.264 codec" "Extended duration")
         (stats
          (dimensions "160x51 chars (1280x714 pixels)")
          (frames 72)
          (duration "6 seconds at 12fps")
          (points 12000)
          (file-size "1.0MB"))
         (usage "scheme --script user/demos/lorenz-butterfly-hq.ss")
         (purpose "Presentations and high-quality sharing"))

   ;; Artifacts
   (artifact lorenz-signature.gif
             (type "GIF animation")
             (created "2026-01-21")
             (status "SIGNATURE DEMO")
             (purpose "Primary showcase for lattice/sim/dynamics/chaos module")
             (specs
              (resolution "800x588 pixels")
              (frames 60)
              (fps 12)
              (loop "infinite")
              (palette "32 colors with Bayer dithering")))

   (artifact lorenz-butterfly.gif
             (type "GIF animation")
             (created "2026-01-21")
             (purpose "Original quick-render version"))

   (artifact lorenz-butterfly-hq.mp4
             (type "MP4 video")
             (created "2026-01-21")
             (codec "H.264 (libx264)")
             (purpose "High-resolution presentation version"))

   (documentation LORENZ-SHOWCASE.md
                  (type "Technical showcase document")
                  (topics "Mathematics" "Rendering pipeline" "Demoscene aesthetic"))))

(style
 (aesthetic "Demoscene - 1990s demo art meets modern terminal graphics")
 (inspiration "Future Crew's Second Reality (1993)" "Amiga demo scene")
 (principles
  "Fill the frame - use all 100x42 characters"
  "Layer depth - rotation + Z-depth shading creates 3D illusion"
  "Smooth motion - 60 frames for butter-smooth animation"
  "Mathematical beauty - let the strange attractor speak"
  "Technical flex - RK4, depth buffers, color mapping, all in Scheme"))

(technical-notes
 (pipeline
  "RK4 integrator -> trajectory generation -> 3D rotation -> depth buffer projection -> ANSI coloring -> frame conversion -> PPM export -> FFmpeg -> GIF/MP4")

 (modules-used
  "lattice/sim/dynamics/chaos.ss - RK4 integrator, Lorenz/Rössler/Thomas attractors"
  "lattice/sim/dynamics/attractor-render.ss - 3D rotation, projection, depth buffer, color mapping"
  "user/creations/ascii-video.ss - Frame buffer abstraction"
  "user/creations/ascii-video-export.ss - 8x14 bitmap font, PPM generation, FFmpeg pipeline")

 (mathematics
  (system "Lorenz attractor (1963)")
  (equations
   "dx/dt = σ(y - x)"
   "dy/dt = x(ρ - z) - y"
   "dz/dt = xy - βz")
  (parameters "σ=10, ρ=28, β=8/3 (classic butterfly)")
  (integrator "Runge-Kutta 4th order, dt=0.005")
  (behavior "Chaotic attractor, sensitive dependence on initial conditions"))

 (rendering
  (projection "Orthographic with depth buffer")
  (rotation "Y-axis: 0 to 2π | X-axis: 0.3 rad tilt")
  (color-hue "Cycles through trajectory (position/n_points)")
  (color-intensity "Z-depth based (closer = brighter)")
  (character-ramp "69 chars: space to $ (density-based shading)")
  (depth-test "Proper occlusion - closer points overwrite distant ones")))

(what-this-demonstrates
 (from-technical-report
  "Content-addressable computation - all code in CAS"
  "Lattice skill composition - chaos + rendering + export"
  "Pure functional pipeline - deterministic, reproducible"
  "Zero external deps - Chez Scheme + FFmpeg only")

 (cool-factor
  "The Fold can render spinning strange attractors in ASCII"
  "Export them as GIFs and videos"
  "With proper 3D depth and rainbow colors"
  "All from Scheme code in the terminal"))

(usage
 (recommended
  (generate "scheme --script user/demos/lorenz-signature.ss")
  (result "lorenz-signature.gif - 296KB, perfect loop")
  (time "~15 seconds on modern CPU"))

 (alternatives
  (quick "scheme --script user/demos/lorenz-butterfly-demo.ss → 244KB")
  (hq "scheme --script user/demos/lorenz-butterfly-hq.ss → 1.0MB MP4"))

 (customization
  "Edit width/height/n-frames/n-points variables in scripts"
  "Adjust frame delay for different FPS"
  "Try different attractors: rossler-classic, thomas-classic"
  "Modify rotation angles for different viewpoints"))

(quotes
 "The Lorenz butterfly spins."
 "Chaos rendered in ASCII."
 "The Fold remembers.")
