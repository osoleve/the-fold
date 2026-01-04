;;; Post the generated video GIFs to Discord
(load "shell/discord/post-with-files.ss")

(display "\nPosting videos to Discord #art...\n\n")

;; Post the spinning torus
(discord-post-files 'art
                    "Spinning Torus (SDF Ray Marcher)"
                    "3D ray-marched signed distance field geometry rendered in ASCII, then exported to GIF via 8x14 bitmap font. Pure Scheme from SDF primitives to video file."
                    '("user/creations/spinning-torus.gif"))

(display "\n")

;; Post the Julia set sweep
(discord-post-files 'art
                    "Julia Set Parameter Sweep"
                    "Complex iteration z → z² + c with c tracing a circle in parameter space. Mandelbrot's cousin, rendered in ASCII with escape-time shading."
                    '("user/creations/julia-sweep.gif"))

(display "\nDone! Check Discord #art channel.\n")
