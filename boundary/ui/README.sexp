((name "ui")
 (purpose "Graphics, visualization, and rendering subsystem")
 (description
  "Comprehensive UI toolkit for visual output including text rendering,
   SVG generation, turtle graphics, particle systems, animation, and
   data visualization. Supports color management, geometric transforms,
   layout combinators, and layer composition. Used for rendering forum
   visualizations, profiler output, and interactive graphics.")
 (modules
  ((animation.ss "Animation timeline and keyframe interpolation")
   (color.ss "Color representation and manipulation utilities")
   (easing.ss "Easing functions for smooth animation curves")
   (forum-viz.ss "Forum activity visualization")
   (fuel-viz.ss "Fuel consumption visualization for bounded computation")
   (graphics-primitives.ss "Basic shape primitives: circles, rectangles, polygons")
   (graphics.ss "High-level graphics composition")
   (layers.ss "Layer-based rendering with z-ordering")
   (layout-color.ss "Color schemes for layout rendering")
   (layout-combinators.ss "Composable layout primitives for UI construction")
   (layout.ss "Canvas and grid-based layout system")
   (particles.ss "Particle system for visual effects")
   (profile-viz.ss "Profiler output visualization")
   (svg-renderer.ss "SVG output generation for shapes and canvases")
   (text.ss "Text canonicalization and sanitization against glitchlings")
   (transforms.ss "2D transform matrices: translate, rotate, scale")
   (turtle.ss "Logo-style turtle graphics interpreter")
   (turtle-block.ss "Block-based turtle graphics")
   (turtle-color.ss "Color management for turtle graphics")
   (turtle-path.ss "Path recording and replay for turtle graphics")
   (turtle-svg.ss "SVG export for turtle graphics")))
 (dependencies (base linalg))
 (load-order "color.ss -> transforms.ss -> graphics-primitives.ss -> layout.ss -> svg-renderer.ss, turtle modules independent"))
