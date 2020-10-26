        subroutine evalg(jbdry,m,m1,gbr,indx,
     1      wvno,om,om2,wvno2)
        implicit none
        integer jbdry, m, m1, indx
        complex*16 gbr(2,5)
        real*8 wvno,om,wvno2,om2
        integer NL
        parameter(NL=200)
        common/timod/d(NL),TA(NL),TC(NL),TL(NL),TN(NL),TF(NL),
     1      TRho(NL),
     2      qa(NL),qb(NL),etap(NL),etas(NL),
     3      frefp(NL), frefs(NL)
        real*4 d,TA,TC,TN,TL,TF,TRho,qa,
     1       qb,etap,etas,frefp,frefs
        common/modlly/mmax
        integer mmax
        common/modspec/allfluid
        logical allfluid

        complex*16 cg(6)
        complex*16 g(4,4)
        COMPLEX*16 zA,zB,zC,zD,zE,zF
        COMPLEX*16 rp, rsv
        COMPLEX*16 NP, NSV
        COMPLEX*16 X11, X21, X31, X41
        COMPLEX*16 X12, X22, X32, X42
        integer iwat

c       complex*16 e(4,4), einv(4,4)

c       integer i,j,k
c       complex*16 zsum


C        WRITE(6,*)'allfluid:',allfluid
c-----
c       set up halfspace conditions
c-----
            if(TL(m).eq. 0.0 .or.TN(m).eq.0.0)then
                iwat = 1
            else
                iwat = 0
            endif
c-----
c       HALFSPACE
c-----
            call gettiegn(za,zb,zc,zd,ze,zf,om2,wvno2,rp, rsv, 
     1          x11,x21,x31,x41,x12,x22,x32,x42,NP, NSV,m,om,wvno,
     2          dcmplx(1.0d+00,0.0d+00),dcmplx(1.0d+00,0.0d+00))
            if(iwat.eq.0)then
c-----
c               ELASTIC HALFSPACE
c       This is E^-1
c-----
        G(1,1) =  x41*rp /( 2.*rp*NP)
        G(2,1) =  x42    /( 2.*rsv*NSV)
        G(3,1) = -x41*rp /(-2.*rp*NP)
        G(4,1) =  x42    /(-2.*rsv*NSV)
        G(1,2) = -x31    /( 2.*rp*NP)
        G(2,2) = -x32*rsv/( 2.*rsv*NSV)
        G(3,2) = -x31    /(-2.*rp*NP)
        G(4,2) =  x32*rsv/(-2.*rsv*NSV)
        G(1,3) = -x21*rp /( 2.*rp*NP)
        G(2,3) = -x22    /( 2.*rsv*NSV)
        G(3,3) =  x21*rp /(-2.*rp*NP)
        G(4,3) = -x22    /(-2.*rsv*NSV)
        G(1,4) =  x11    /( 2.*rp*NP)
        G(2,4) =  x12*rsv/( 2.*rsv*NSV)
        G(3,4) =  x11    /(-2.*rp*NP)
        G(4,4) = -x12*rsv/(-2.*rsv*NSV)
c-----
c       This is E
c-----
c          E(1,1) =  x11
c          E(2,1) =  x21 * rp
c          E(3,1) =  x31
c          E(4,1) =  x41 * rp
c
c          E(1,2) =  x12 * rsv
c          E(2,2) =  x22
c          E(3,2) =  x32 * rsv
c          E(4,2) =  x42
c
c          E(1,3) =  x11
c          E(2,3) = -x21 * rp
c          E(3,3) =  x31
c          E(4,3) = -x41 * rp
c
c          E(1,4) = -x12 * rsv
c          E(2,4) =  x22
c          E(3,4) = -x32 * rsv
c          E(4,4) =  x42
c
C          do j=1,4
C              do i = 1,4
C                   write(6,*)'E   (',I ,',' , J, ')=',E(i,J)
C             enddo
C          enddo
C          do j=1,4
C              do i = 1,4
C                   write(6,*)'EINV(',I ,',' , J, ')=',G(i,J)
C             enddo
C          enddo
C          do i=1,4
C              do j = 1,4
C                zsum = dcmplx(0.0d+00,0.0d+00)
C                do k=1,4
C                zsum = zsum + E(i,k)*g(k,j)
C                enddo
C                write(6,*)'E INV(',I ,',' , J, ')=',ZSUM
C             enddo
C          enddo

c CG(1) = G 12 12 = 11 22 - 12 21
c CG(2) = G 12 13 = 11 23 - 13 21
c CG(3) = G 12 14 = 11 24 - 14 21
c CG(4) = G 12 23 = 12 23 - 13 22
c CG(5) = G 12 24 = 12 24 - 14 22
c CG(6) = G 12 34 = 13 24 - 14 23
                CG(1) = G(1,1)*G(2,2) - G(1,2)*G(2,1)
                CG(2) = G(1,1)*G(2,3) - G(1,3)*G(2,1)
                CG(3) = G(1,1)*G(2,4) - G(1,4)*G(2,1)
                CG(4) = G(1,2)*G(2,3) - G(1,3)*G(2,2)
                CG(5) = G(1,2)*G(2,4) - G(1,4)*G(2,2)
                CG(6) = G(1,3)*G(2,4) - G(1,4)*G(2,3)
                gbr(indx,1) = CG(1)
                gbr(indx,2) = CG(2)
                gbr(indx,3) = CG(3)
                gbr(indx,4) = CG(5)
                gbr(indx,5) = CG(6)

            else if(iwat.eq.1)then
c-----
c               FLUID HALFSPACE
c-----
                if(allfluid)then
                    gbr(indx,1) = dble(TRho(m))*om2
                    gbr(indx,2) = -rp
                    gbr(indx,3) = dcmplx(0.0d+00,0.0d+00)
                    gbr(indx,4) = dcmplx(0.0d+00,0.0d+00)
                    gbr(indx,5) = dcmplx(0.0d+00,0.0d+00)
                else
                    gbr(indx,1) = dcmplx(0.0d+00,0.0d+00)
                    gbr(indx,2) = dcmplx(0.0d+00,0.0d+00)
                    gbr(indx,3) = dcmplx(0.0d+00,0.0d+00)
                    gbr(indx,4) = -dble(TRho(m))*om2
                    gbr(indx,5) = rp
                endif
            endif
        return
        end
