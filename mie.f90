program mie
use math
implicit none

    double precision, dimension(:), allocatable :: j, y, xv
    double precision :: m
    integer :: i, numx

    numx = 10
    allocate(xv(numx))
    allocate(j(numx))
    allocate(y(numx))

    do i = 1, numx
        xv(i) = dble(i)*10.d0/dble(numx)
    end do

    m = 5.d0
    j = bessel_first(xv, m)
    y = bessel_second1(xv)

    do i = 1, numx
        print *, xv(i), j(i), y(i)
    end do

end program
