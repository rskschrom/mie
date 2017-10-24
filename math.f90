module math
implicit none

    double precision, parameter :: pi = 3.14159265d0, gam = 0.5772156649d0

    contains

    !-------------------------------------------------
    ! bessel function of the first kind - mth term
    !-------------------------------------------------
    function bessel_first(x, m) result(j)

        double precision, intent(in) :: x(:), m
        double precision :: j(size(x))
        integer :: l

        j = 0.d0

        do l = 0, 20
            j = j+(-1.d0)**l&
                /(2.d0**(2.d0*l+dabs(m))*gamma(dble(l+1))*gamma(dble(dabs(m)+l+1)))*&
                (x)**(2.d0*l+dabs(m))
        end do

        ! deal with negative m arguments
        j = sign(m/m, m)**int(m)*j

    end function

    !-------------------------------------------------
    ! bessel function of the second kind - mth term
    !-------------------------------------------------
    function bessel_second(x, m) result(y)

        double precision, intent(in) :: x(:), m
        double precision :: y(size(x))
        integer :: l, intm, nt, i

        ! recursion formula

    end function

    !-------------------------------------------------
    ! bessel function of the second kind - 0th term
    !-------------------------------------------------
    function bessel_second0(x) result(y)

        double precision, intent(in) :: x(:)
        double precision :: y(size(x)), const(size(x)), sum1(size(x))
        integer :: nt, k

        ! calculate "constant" part of expansion
        const = (dlog(x/2.d0)+gam)*bessel_first(x, 0.d0)

        ! loop over sum
        nt = 50
        sum1 = 0.d0
        do k = 1, nt
            sum1 = sum1+(-1.d0)**k*bessel_first(x, 2.d0*k)/dble(k)
        end do

        y = 2.d0/pi*(const-2.d0*sum1)

    end function

    !-------------------------------------------------
    ! bessel function of the second kind - 1st term
    !-------------------------------------------------
    function bessel_second1(x) result(y)

        double precision, intent(in) :: x(:)
        double precision :: y(size(x)), const(size(x)), sum1(size(x))
        integer :: nt, k

        ! calculate "constant" part of expansion
        const = (dlog(x/2.d0)+gam-1.d0)*bessel_first(x, 1.d0)-bessel_first(x, 0.d0)/x

        ! loop over sum
        nt = 50
        sum1 = 0.d0
        do k = 1, nt
            sum1 = sum1+(-1.d0)**k*bessel_first(x, 1.d0+2.d0*k)*dble(1+2*k)/dble(k*(1+k))
        end do

        y = 2.d0/pi*(const-sum1)

    end function

    !-------------------------------------------------
    ! digamma function
    !-------------------------------------------------
    function digamma(x) result(d)

        double precision, intent(in) :: x
        double precision :: d

        d = dlog(x)-1.d0/(2.d0*x)-1.d0/(12.d0*x**2)+&
               1.d0/(120.d0*x**4)-1.d0/(252.d0*x**6)+&
               1.d0/(240.d0*x**8)-5.d0/(660.d0*x**10)+&
               691.d0/(32760.d0*x**12)-1.d0/(12.d0*x**14)

    end function

    !-------------------------------------------------
    ! gamma function regularized for argument of 0
    !-------------------------------------------------
    function reg_gamma(x) result(g)

        double precision, intent(in) :: x
        double precision :: g, reg_x

        reg_x = max(x, 1.d0)
        g = gamma(reg_x)

    end function


end module
