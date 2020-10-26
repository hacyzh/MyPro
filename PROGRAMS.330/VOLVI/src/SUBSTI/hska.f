        subroutine hska(AA,tcosp,trsinp,tsinpr,tcossv,trsinsv,tsinsvr,
     1      NP,NSV,
     1      X11, X21, X31, X41,X12, X22, X32, X42,
     2      TRho,iwat,ex,om2)
c-----
c       Changes
c
c       01 May 2002  - defined cosp = tcosp/NP to 
c             reduce nmber of complex divisions
c-----
        implicit none
        real*8 AA(4,4)
        complex*16 tcosp , tcossv 
        complex*16 trsinp, trsinsv
        complex*16 tsinpr, tsinsvr
        COMPLEX*16 NP, NSV
        COMPLEX*16 X11, X21, X31, X41
        COMPLEX*16 X12, X22, X32, X42
        real TRho
        integer iwat
        real*8 ex, dfac
        real*8  om2
        complex*16 zrho

c-----
c       introduce shorthand to reduce work
c-----
        COMPLEX*16 cosp , cossv 
        COMPLEX*16 rsinp, rsinsv
        COMPLEX*16 sinpr, sinsvr

        integer i, j
        zrho = dcmplx(dble(TRho),0.0d+00)
        if(iwat.eq.1)then
c-----
c       fluid layer
c-----
            do 100 j=1,4
                do 101 i=1,4
                    AA(i,j) = 0.0d+00
  101           continue
  100       continue
            if(ex.gt.35.0d+00)then
                dfac = 0.0d+00
            else
                dfac = dexp(-ex)
            endif
            AA(1,1) = dfac
            AA(4,4) = dfac
            AA(2,2) = tcosp
            AA(3,3) = tcosp
            AA(2,3) = -trsinp/(zrho*om2)
            AA(3,2) = - zrho*om2*tsinpr
        else
c-----
c       elastic layer
c-----
            cosp   = tcosp/NP
            sinpr  = tsinpr/NP
            rsinp  = trsinp/NP
            cossv  = tcossv/NSV
            sinsvr = tsinsvr/NSV
            rsinsv = trsinsv/NSV

            AA(1,1) = dreal(  x11*x41*cosp  + x12*x42*cossv  )
            AA(1,2) = dreal(- x11*x31*sinpr - x12*x32*rsinsv )
            AA(1,3) = dreal(- x11*x21*cosp  - x12*x22*cossv  )
            AA(1,4) = dreal(  x11*x11*sinpr + x12*x12*rsinsv )
            AA(2,1) = dreal(  x21*x41*rsinp + x22*x42*sinsvr )
            AA(2,2) = dreal(- x21*x31*cosp  - x22*x32*cossv  )
            AA(2,3) = dreal(- x21*x21*rsinp - x22*x22*sinsvr )
            AA(3,1) = dreal(  x31*x41*cosp  + x32*x42*cossv  )
            AA(3,2) = dreal(- x31*x31*sinpr - x32*x32*rsinsv )
            AA(4,1) = dreal(  x41*x41*rsinp + x42*x42*sinsvr )
            AA(2,4) = - AA(1,3)
            AA(3,3) =   AA(2,2)
            AA(3,4) = - AA(1,2)
            AA(4,2) = - AA(3,1)
            AA(4,3) = - AA(2,1)
            AA(4,4) =   AA(1,1)
        endif
        return
        end
