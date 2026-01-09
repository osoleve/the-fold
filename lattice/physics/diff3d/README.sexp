((name "diff-physics-3d")
 (purpose "Differentiable 3D physics simulation with quaternion rotations")
 (description
  "Automatic differentiation through 3D rigid body physics.
   Enables gradient-based optimization of physical trajectories,
   inverse physics problems, and sensitivity analysis. Uses
   quaternions for singularity-free rotation representation
   and supports soft contact models for differentiable collisions.")
 (modules
  ((traced-vec3.ss "Differentiable 3D vector operations with autodiff")
   (traced-quaternion.ss "Differentiable quaternion operations for 3D rotations")
   (traced-body3d.ss "3D rigid body state: position, velocity, orientation, angular velocity")
   (traced-integrators3d.ss "Symplectic Euler integration with quaternion normalization")
   (smooth-collision3d.ss "Signed distance functions and soft contact models")
   (diff-collision3d.ss "Differentiable collision response with friction")
   (diff-constraints3d.ss "Constraint solver: springs, hinges, ball-socket joints")
   (rollout3d.ss "Multi-step simulation with checkpointing for memory efficiency")
   (optimize3d.ss "Trajectory optimization: gradient descent, Adam, momentum")))
 (dependencies (base linalg autodiff))
 (load-order
  "traced-vec3.ss -> traced-quaternion.ss -> traced-body3d.ss -> traced-integrators3d.ss -> smooth-collision3d.ss -> diff-collision3d.ss -> diff-constraints3d.ss -> rollout3d.ss -> optimize3d.ss"))
