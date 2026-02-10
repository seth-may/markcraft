! Fortran 2018: Parallel N-body gravitational simulation
module nbody_mod
  use iso_fortran_env, only: real64, int64
  implicit none
  private
  public :: Body, Simulation, run_simulation

  real(real64), parameter :: G = 6.67430e-11_real64
  real(real64), parameter :: DT = 0.01_real64
  integer, parameter :: DIMS = 3

  type :: Body
    real(real64) :: mass
    real(real64) :: pos(DIMS) = 0.0_real64
    real(real64) :: vel(DIMS) = 0.0_real64
    real(real64) :: acc(DIMS) = 0.0_real64
    character(len=32) :: name = ''
  end type

  type :: Simulation
    type(Body), allocatable :: bodies(:)
    real(real64) :: time = 0.0_real64
    integer(int64) :: step = 0
    real(real64) :: total_energy
  contains
    procedure :: init => sim_init
    procedure :: compute_forces => sim_forces
    procedure :: integrate => sim_integrate
    procedure :: kinetic_energy => sim_ke
    procedure :: potential_energy => sim_pe
  end type

contains
  subroutine sim_init(self, n)
    class(Simulation), intent(inout) :: self
    integer, intent(in) :: n
    integer :: i
    real(real64) :: r
    allocate(self%bodies(n))
    do i = 1, n
      call random_number(r)
      self%bodies(i)%mass = 1.0e10_real64 * (0.5_real64 + r)
      call random_number(self%bodies(i)%pos)
      self%bodies(i)%pos = (self%bodies(i)%pos - 0.5_real64) * 100.0_real64
      write(self%bodies(i)%name, '(A,I0)') 'body_', i
    end do
  end subroutine

  subroutine sim_forces(self)
    class(Simulation), intent(inout) :: self
    integer :: i, j, n
    real(real64) :: dx(DIMS), dist, force_mag, force(DIMS)
    n = size(self%bodies)
    self%bodies%acc(1) = 0.0_real64
    self%bodies%acc(2) = 0.0_real64
    self%bodies%acc(3) = 0.0_real64
    !$omp parallel do private(j, dx, dist, force_mag, force) schedule(dynamic)
    do i = 1, n
      do j = i + 1, n
        dx = self%bodies(j)%pos - self%bodies(i)%pos
        dist = max(norm2(dx), 1.0e-3_real64)
        force_mag = G * self%bodies(i)%mass * self%bodies(j)%mass / (dist * dist)
        force = force_mag * dx / dist
        !$omp atomic
        self%bodies(i)%acc = self%bodies(i)%acc + force / self%bodies(i)%mass
        !$omp atomic
        self%bodies(j)%acc = self%bodies(j)%acc - force / self%bodies(j)%mass
      end do
    end do
    !$omp end parallel do
  end subroutine

  subroutine sim_integrate(self)
    class(Simulation), intent(inout) :: self
    integer :: i
    do i = 1, size(self%bodies)
      self%bodies(i)%vel = self%bodies(i)%vel + self%bodies(i)%acc * DT
      self%bodies(i)%pos = self%bodies(i)%pos + self%bodies(i)%vel * DT
    end do
    self%time = self%time + DT
    self%step = self%step + 1
  end subroutine

  real(real64) function sim_ke(self)
    class(Simulation), intent(in) :: self
    integer :: i
    sim_ke = 0.0_real64
    do i = 1, size(self%bodies)
      sim_ke = sim_ke + 0.5_real64 * self%bodies(i)%mass * sum(self%bodies(i)%vel**2)
    end do
  end function

  real(real64) function sim_pe(self)
    class(Simulation), intent(in) :: self
    integer :: i, j; real(real64) :: dist
    sim_pe = 0.0_real64
    do i = 1, size(self%bodies)
      do j = i + 1, size(self%bodies)
        dist = norm2(self%bodies(j)%pos - self%bodies(i)%pos)
        sim_pe = sim_pe - G * self%bodies(i)%mass * self%bodies(j)%mass / max(dist, 1e-3_real64)
      end do
    end do
  end function
end module

program run_simulation
  use nbody_mod
  implicit none
  type(Simulation) :: sim
  integer :: t
  call sim%init(100)
  do t = 1, 1000
    call sim%compute_forces()
    call sim%integrate()
    if (mod(t, 100) == 0) print '(A,I6,A,ES12.4)', 'Step ', t, '  Energy: ', sim%kinetic_energy() + sim%potential_energy()
  end do
end program
