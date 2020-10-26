        subroutine normc(ee,ex,nmat)
c-----
c       This routine is an important step to control over- or
c       underflow.
c       The Haskell or Dunkin vectors are normalized before
c       the layer matrix stacking.
c       Note that some precision will be lost during normalization.
c-----
        implicit none
        complex*16 ee(5)
        real*8 ex
        integer nmat

        real*8 t1 
        complex*16 ztmp
        integer i
        ex = 0.0d+00
        t1 = 0.0d+00
        do  i = 1,nmat
          if(cdabs(ee(i)).gt.t1) then
               t1 = cdabs(ee(i))
          endif
        enddo
        if(t1.lt.1.d-40) t1=1.d+00
        do i =1,nmat
          ztmp=ee(i)
          ztmp=ztmp/t1
          ee(i)=ztmp
        enddo
c-----
c       store the normalization factor in exponential form.
c-----
        ex=dlog(t1)
        return
        end
