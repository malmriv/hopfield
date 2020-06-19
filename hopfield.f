c   DESCRIPTION. This is an implementation of a Hopfield network. This file includes a module
c   dedicated to generating random numbers and the actual program (at line 100).

      module randomnumber
      integer*8::ip,iq,is,np,nbit,ic
      parameter(ip=1279)
      parameter(iq=418)
      parameter(is=ip-iq)
      parameter(np=14)
      parameter(nbit=31)
      integer*8,dimension(1:ip)::ix
      save ic,ix
      contains

      real*8 function gaus()
      real*8::v1,v2,fac,r,gset
      integer*8::iset
      save iset,gset

      if(iset.eq.0)then
      r=2.d0
      do while(r>1.or.r.eq.0.d0)
        v1=2.d0*dran_u()-1.d0
        v2=2.d0*dran_u()-1.d0
        r=v1*v1+v2*v2
      enddo
      fac=dsqrt(-2.d0*dlog(r)/r)
      gset=v1*fac
      gaus=v2*fac
      iset=1
      else
        gaus=gset
        iset=0
      endif
      end function gaus

      subroutine dran_ini(iseed0)
      integer*8::m,np1,nn,nn1,i,j
      real*8::dseed,p,t,x
      dseed=iseed0
      do i=1,ip
        ix(i)=0
        do j=0,nbit-1
            if(rand_xx(dseed).lt.0.5d0) ix(i)=ibset(ix(i),j)
      enddo
      enddo
      ic=0
      end subroutine dran_ini

        subroutine dran_read(iunit)
      integer*8::i
      read(iunit,*)ic
        read(iunit,*)(ix(i),i=1,ip)
        end subroutine dran_read

        subroutine dran_write(iunit)
        integer*8::i
      write(iunit,*) ic
        write(iunit,*) (ix(i),i=1,ip)
        end subroutine dran_write

        integer*8 function i_dran(n)
        integer*8::i_ran,n
      ic=ic+1
        if(ic.gt.ip) ic=1
      if(ic.gt.iq)then
      ix(ic)=ieor(ix(ic),ix(ic-iq))
      else
          ix(ic)=ieor(ix(ic),ix(ic+is))
        endif
        i_ran=ix(ic)
        if(n.gt.0)i_dran=mod(i_ran,n)+1
      end function i_dran


      real*8 function dran_u()
      real*8::rmax
        parameter (rmax=2147483647.0)
      ic=ic+1
        if(ic.gt.ip) ic=1
      if(ic.gt.iq)then
      ix(ic)=ieor(ix(ic),ix(ic-iq))
      else
          ix(ic)=ieor(ix(ic),ix(ic+is))
        endif
      dran_u=dble(ix(ic))/rmax
      end function dran_u

        real*8 function rand_xx(dseed)
        real*8:: a,c,xm,rm,dseed
        parameter (xm=2.d0**32,rm=1.d0/xm,a=69069.d0,c=1.d0)
        dseed=mod(dseed*a+c,xm)
        rand_xx=dseed*rm
        end function rand_xx
      end module randomnumber


c THE ACTUAL PROGRAM STARTS HERE. (Explain variables here).

      program hopfield
      use randomnumber
      implicit none
      integer, dimension(:), allocatable :: trainingset, neurons
      real*8, dimension(:), allocatable :: bias
      real*8, dimension(:,:), allocatable :: w
      integer N, montecarlo, i, j, k, l, loop
      integer randseed(1:8), switch, patterns
      real*8 a, energy, ksi, p, ovl, T
      character(len=50), dimension(:), allocatable :: files
      character(len=50) enigma

c     Set seed by reading current time (array of dim. 8) and multiplying every
c     number (yr*mo*day*(...)*miliseconds) to get a different seed every time
      call date_and_time(values=randseed)
      call dran_ini(abs(product(randseed)))

c     Set network size, no. of MC steps & no. of patterns
      N = 32**2
      montecarlo = 15
      patterns = 10
      write(*,*) "Parameters set."

c     Allocate memory
      allocate(files(1:patterns))
      allocate(trainingset(1:N),neurons(1:N))
      allocate(bias(1:N),w(1:N,1:N))
      write(*,*) "Memory allocated."

c     Set weight and bias matrices as zero
      w = 0.d0
      bias = 0.d0
      write(*,*) "Matrices set."

c     Choose patterns to memorise
      files(1) = "./sets/pattern1.txt"
      files(2) = "./sets/pattern2.txt"
      files(3) = "./sets/pattern3.txt"
      files(4) = "./sets/pattern4.txt"
      files(5) = "./sets/pattern5.txt"
      files(6) = "./sets/pattern6.txt"
      files(7) = "./sets/pattern7.txt"
      files(8) = "./sets/pattern8.txt"
      files(9) = "./sets/pattern9.txt"
      files(10) = "./sets/pattern10.txt"
      write(*,*) "Files listed."

c     Run through each pattern and compute weights
      do k=1,patterns
        trainingset = 0
        open(10,file=files(k))
          read(10,*) trainingset
        close(10)

        a = float(sum(trainingset))/N
        do i=1,N-1
          do j=i+1,N
            w(i,j) = w(i,j) + (trainingset(i)-a)*(trainingset(j)-a)/
     & (a*(1-a)*N)
            w(j,i) = w(i,j)
          end do
        end do
      end do
      write(*,*) "Weights calculated."


c     Compute biases
      do i=1,N
        do j=i,N
          bias(i) = bias(i) + 0.5d0*w(i,j)
        end do
      end do
      write(*,*) "Biases calculated."

c     Compute overlap with each pattern to plot evolution.
      trainingset = 0
      ovl = 0.d0
      open(10,file=files(8))
        read(10,*) trainingset
      close(10)
      a = float(sum(trainingset))/N

      T = 0.d0
c     Read file with corrupted pattern
      open(30,file="./sets/pattern6c.txt",status="old")
      read(30,*) neurons
      close(30)

c     Prepare results file and overlap file
      open(20,file="./results.txt")
      open(150,file="./overlap.txt",status="unknown")
      open(180,file="./energy6.txt")

c     Save initial state
      write(20,"(I3)",advance='yes') neurons

      do l=1,montecarlo
c     Apply Metropoli's algorithm.
      do i=1,N
        k = nint((N-1)*dran_u()+1)
        energy = 0.d0
        do j=1,N
          energy = energy + w(k,j)*(1.d0-neurons(j))
        end do
        energy = 0.5d0*(switch(neurons(k))-neurons(k))*energy
        p = min(1.d0,exp(-energy/T))
        if(dran_u() .lt. p) then
          neurons(k) = switch(neurons(k))
          write(180,*) energy
        else
          write(180,*) 0.d0
        end if

c     Initialize overlap parameter as zero
        ovl = 0.d0
        do j=1,N
          ovl = ovl + (float(trainingset(j))-a)*
     &    (float(neurons(j))-a)
        end do
        ovl = ovl/(a*(1.d0-a)*N)
        if(l .eq. montecarlo .and. i .eq. N) write(150,"(F16.8)") ovl
        if((l .eq. montecarlo) .and. (i .ne. N)) then
          write(150,"(F16.8)",advance="no") ovl
        end if
        if (l .ne. montecarlo) write(150,"(F16.8)",advance="no") ovl
      end do
      write(20,"(I3)",advance='yes') neurons
      end do

c     Close files
      close(20)
      close(150)
      close(180)
      end program hopfield

      function switch(a) result(b)
        integer a,b
        if(a .eq. 1) b = 0
        if(a .eq. 0) b = 1
      end function switch
