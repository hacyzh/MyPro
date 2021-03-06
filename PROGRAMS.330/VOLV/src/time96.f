        program time96
c---------------------------------------------------------------------c
c                                                                     c
c      COMPUTER PROGRAMS IN SEISMOLOGY                                c
c      VOLUME V                                                       c
c                                                                     c
c      PROGRAM: TIME96                                                c
c                                                                     c
c      COPYRIGHT 2001 R. B. Herrmann                                  c
c                                                                     c
c      Department of Earth and Atmospheric Sciences                   c
c      Saint Louis University                                         c
c      221 North Grand Boulevard                                      c
c      St. Louis, Missouri 63103                                      c
c      U. S. A.                                                       c
c                                                                     c
c---------------------------------------------------------------------c
c       This program provides the first arrival time
c
c       return P-wave dt/dD in sec/km as function of 
c           epicentral distance and source depth
c           or travel time
c
c Usage: time96 -GCARC gcarc -DIST dist -EVDP evdp  
c   [ -T | -RAYP | -GEOM | -TS]
c   [ -P | -SV | -SH | -pP | -sP ] 
c   -M model96  [-h] [-?]
c    Example: time96 -GCARC 31.0 -EVDP 170.0 -RAYP -M model
c  -GCARC gcarc (default none) epicentral dist deg
c  -DIST dist   (default none) epicentral dist km
c  -EVDP evdp   (default 10.0) source depth  km
c  -T           (default false) output travel time
c  -RAYP        (default false) output ray param
c        Compute P-wave ray parameter in sec/km
c  -GEOM        (default false) output geometrical spreading
c  -TS          (default false) output ray T*
c  -P           (default true) compute for P-wave
c  -pP          (default false) compute for pP-wave
c  -sP          (default false) compute for sP-wave
c  -sS          (default false) compute for sS-wave
c  -SV          (default false) compute for SV-wave
c  -SH          (default false) compute for SH-wave
c  -M model_name     Model96 velocity model
c  -h                            this command help
c  -?                            this command help
c
c-----
c       CHANGES
c       04 AUG 2006 - corrected error in first arrival pick that
c               falsely gave the refraction time instead of the
c               direct time because the refraction arrival was
c               unphysical
c       03 JAN 2007 - increased to 200 layers and implemented ray parameter
c       08 MAR 2007 - corrected only help to show -SH -SV
c       06 JAN 2008 - corrected error in depth insertion for spherical model
c       21 JAN 2008 - modified to permit pP sP geometrical spreading
c       25 JAN 2008 - put Radius of Earth into common/earth/radius for
c                      generality 
c                   -  define a separate common block for the
c                      SH velocity and density 
c                   -  have sphericity correction
c                      work on common blocks instead of procedure call
c                   -  create a default adomod  to fill the SH for a flat model
c                      note the separation of SH is important for wavenumber
c                      integration code
c       08 FEB 2008 -  subtle change in fstarr for source receiver in same layer -
c                      spherical mapping was not done
c       18 FEB 2009 -  took care to hand the case that EVDP or GCARC or DIST 
c                      not defined in the SAC file, e.g., = -12345
c                      corrected some routines by adding 'implicit none' so
c                      that all variables are used
c       18 MAR 2009 -  added sS phase to the list
c       01 JUN 2013 -  Modified subroutine to prevent NaN for direct ray by replacing
c                      pnew = 0.999d+00 * pupper to
c                      pnew = 0.99d+00 * pupper
c                      also modified subrpoutine frstar to have the dogeom argument.
c-----
        implicit none
        character mname*80
        real delta, depth, dist
        logical dop, dot, dogeom, dotstar
        integer  ipsvsh
        logical verbose

        real r, hs, hr, time, pvel, svel, vsa, vsb, vsr, den
        real rayp, geom, tstar
        logical dolock

        common/earth/radius
        real radius
        real kmdeg

        integer LER,LOT
        parameter (LER=0,LOT=6)
c-----
c       initialize
c-----
        radius = 6371.
        kmdeg=radius*3.1415927/180.0
c-----
c       get command line arguments
c-----

        call gcmdln(delta, depth,dist,dop,dot,dogeom,
     1     dotstar,mname,ipsvsh,verbose)

        hr = 0
        hs = depth
c-----
c       everything is driven from distance
c       so get distance from degrees, or degrees from distance
c-----
        if(dist.ge.0.0)then
            r = dist
            delta = dist/kmdeg
        else if(delta.ge.0.0)then
            r = delta*kmdeg
        endif
        dolock = .false.
        if(r.ge.0.0 .and. r.le.40000.0)then
        call frstar(r,hs,hr,mname,ipsvsh,time,pvel,svel,den,
     1      vsa, vsb, vsr, rayp, geom, tstar, dolock, dogeom,verbose)
        else
          rayp = -12345.
          tstar = -12345.
          time = -12345.
          geom = -12345.
c-----
c       this is not a valide distance
c-----
        endif
c-----
c       output the information
c-----
        if(dop)then
            write(LOT,*)rayp
        endif
        if(dot)then
            write(LOT,*)time
        endif
C        if(dogeom)then
C            write(LOT,*)geom
C        endif
        if(dotstar)then
            write(LOT,*)tstar
        endif
        end

        subroutine gcmdln(delta,depth,dist,dop,dot,dogeom,
     1     dotstar,mname,ipsvsh,verbose)
c-----
c       delta   R   - epicentral distance in degrees (no default)
c       dist    R   - epicentral distance in km (no default)
c       depth   R   - source depth in km    (10.0 default)
c       dot     L   - give travel time  (.true. default)
c       dop     L   - give ray parameter    (.false. default)
c       dogeom  L   - give geometrical spreading    (.false. default)
c       dotstar L   - give ray T*     (.false. default)
c       mname   C*80    - model name
c       ipsvsh  I   - = 1 P first arrival 
c                     = 2 SV first arrival
c                     = 3 SH first arrival
c                     = 4 pP 
c                     = 5 sP
c                     = 6 sS
c       verbose L     more output to stderr
c-----
        implicit none
        real delta, depth, dist
        logical dop, dot , dogeom, dotstar
        integer ipsvsh
        logical verbose
        character mname*(*)

        integer LER,LOT
        parameter (LER=0,LOT=6)

        character names*80
        integer mnmarg
        integer i, nmarg
        real badvalue

        i = 0
        nmarg = mnmarg()

        delta = -12345.
        dist  = -12345.
        depth = 10.0
        dot   = .true.
        dop   = .false.
        dogeom   = .false.
        dotstar   = .false.
        ipsvsh = 1
        mname = ' '
        badvalue = -12345.
        verbose = .false.
 1000   continue
            i = i + 1
            if(i.gt.nmarg)go to 2000
            call mgtarg(i,names)
            if(names(1:6).eq.'-GCARC')then
                i = i + 1
                call mgtarg(i,names)
                read(names,'(bn,f10.0)')delta
            else if(names(1:5).eq.'-DIST')then
                i = i + 1
                call mgtarg(i,names)
                read(names,'(bn,f10.0)')dist
            else if(names(1:5).eq.'-EVDP')then
                i = i + 1
                call mgtarg(i,names)
                read(names,'(bn,f10.0)')depth
            else if(names(1:2).eq.'-R')then
                dop = .true.
                dot = .false.
            else if(names(1:2).eq.'-T' .and.
     1          names(1:3).ne.'-TS')then
                dot = .true.
            else if(names(1:3).eq.'-TS')then
                dotstar = .true.
                dot = .false.
            else if(names(1:5).eq.'-GEOM')then
                dogeom = .true.
            else if(names(1:2).eq.'-P')then
                ipsvsh = 1
            else if(names(1:3).eq.'-SV')then
                ipsvsh = 2
            else if(names(1:3).eq.'-SH')then
                ipsvsh = 3
            else if(names(1:3).eq.'-pP')then
                ipsvsh = 4
            else if(names(1:3).eq.'-sP')then
                ipsvsh = 5
            else if(names(1:3).eq.'-sS')then
                ipsvsh = 6
            else if(names(1:2).eq.'-M')then
                i = i + 1
                call mgtarg(i,mname)
            else if(names(1:2).eq.'-V')then
                verbose = .true.
            else if(names(1:2).eq.'-h' .or. names(1:2).eq.'-?')then
                call usage(' ')
            endif
        go to 1000
 2000   continue
c-----
c       safety
c-----
        if(delta.lt.0.0 .and. dist.lt.0.0)then
            write(LOT,*)badvalue
            call usage( ' DIST or GCARC must be >= 0')
        endif
        if(mname .eq. ' ')then
            write(LOT,*)badvalue
            call usage( ' Model name not specified')
        endif
c-----
c     if the distance is less than 500 km do not permit the computation of 
c     geometrical spreading since this is done numerically using a deltax of
c     500 km
c-----
        if (dist .gt. 0.0 .and . dist .le. 500.0)then
              dogeom = .false.
        endif
        if(delta.gt.0.0 .and. delta .le. 5.0)then
              dogeom = .false.
        endif
            
        return
        end

        subroutine usage(ostr)
        implicit none
        character ostr*(*)
        integer LER
        parameter (LER=0)
        write(LER,*)ostr
        write(LER,*)'Usage: time96 -GCARC gcarc -DIST dist '
       write(LER,*)'     ',
     1      '-EVDP evdp  [ -T | -RAYP | -GEOM | -TS] '
       write(LER,*)'     ',
     2      '[ -P | -SV | -SH | -pP | -sP ] ',
     3      ' -M model96  [-h] [-?]'
        write(LER,*)' Example: time96 -GCARC 31.0 -EVDP 170.0',
     1      ' -RAYP -M model'
        write(LER,*)
     1      ' -GCARC gcarc (default none) epicentral dist deg'
        write(LER,*)
     1      ' -DIST dist   (default none) epicentral dist km'
        write(LER,*)
     1      ' -EVDP evdp   (default 10.0) source depth  km'
        write(LER,*)
     1      ' -T           (default false) output travel time'
        write(LER,*)
     1      ' -RAYP        (default false) output ray param'
        write(LER,*)'       Compute P-wave ray parameter in sec/km'
        write(LER,*)
     1      ' -GEOM        (default false) output geometrical spreading'
        write(LER,*)
     1      ' -TS          (default false) output ray T* '
        write(LER,*)
     1      ' -P           (default true) compute for P-wave'
        write(LER,*)
     1      ' -pP          (default false) compute for pP-wave'
        write(LER,*)
     1      ' -sP          (default false) compute for sP-wave'
        write(LER,*)
     1      ' -SV          (default false) compute for SV-wave'
        write(LER,*)
     1      ' -SH          (default false) compute for SH-wave'
        write(LER,*)
     1      ' -sS          (default false) compute for sS-wave'
        write(LER,*)
     1      ' -M model_name     Model96 velocity model'
        write(LER,*)
     1      ' -h                            this command help'
        write(LER,*)
     1      ' -?                            this command help'
        
        stop
        end

        subroutine insert(dph)
        implicit none
        real dph
        integer LER, LIN, LOT
        parameter (LER=0, LIN=5, LOT=6)
        integer NL
        parameter(NL=200)
        common/isomod/d(NL),a(NL),b(NL),rho(NL),
     1      qa(NL),qb(NL),etap(NL),etas(NL),
     2      frefp(NL), frefs(NL)
        real d, a, b, rho, qa, qb, etap, etas, frefp, frefs
        common/modlly/mmax
        integer mmax
        common/shwave/bsh(NL), rhosh(NL), qbsh(NL)
        real bsh, rhosh, qbsh

        real dp, dphh, hsave, dep
        integer m, ls
c-----
c       Insert a depth point into the model by splitting a layer
c       so that the point appears at the top boundary of the layer
c       dph = depth of interest
c-----
c       determine the layer in which the depth dph lies.
c       if necessary, adjust  layer thickness at the base
c-----
c       Here determine layer corresponding to specific depth dph
c       If the bottom layer is not thick enough, extend it
c
c       dep - depth to bottom of layer
c       dphh    - height of specific depth above bottom of the layer
c-----
        if(dph.le.0)then
            d(1) = d(1) - dph
            return
        else if(dph.ge.0)then
            dep = 0.0 
            dp = 0.0 
            dphh = -1.0
            do 100 m = 1,mmax 
                dp = dp + d(m) 
                dphh = dp - dph 
                if(m.eq.mmax)then
                    if(d(mmax).le.0.0 .or. dphh.lt.0.0)then
                        d(mmax) = (dph - dp)
                    endif
                endif
                dep = dep + d(m) 
                dphh = dep - dph 
                ls = m 
                if(dphh.ge.0.0) go to 101 
  100       continue 
  101       continue 
        endif
c-----
c       In the current model, the depth point is in the ls layer
c       with a distance dphh to the bottom of the layer
c
c       Do not create unnecessary layers, e.g., 
c           at surface and internally
c       However do put in a zero thickness layer 
c           at the base if necessary
c-----
        if(dphh .eq. 0.0 .and. ls.ne.mmax)then
            return
        else
c-----
c           adjust layering
c-----
             do 102 m = mmax,ls,-1
                d(m+1) = d(m)
                a(m+1) = a(m)
                b(m+1) = b(m)
                rho(m+1) = rho(m)
                qa(m+1) = qa(m)
                qb(m+1) = qb(m)
                etap(m+1) = etap(m)
                etas(m+1) = etas(m)
                frefp(m+1) = frefp(m)
                frefs(m+1) = frefs(m)
                bsh(m+1) = b(m)
                qbsh(m+1) = qb(m)
                rhosh(m+1) = rho(m)
  102       continue
            hsave = d(ls)
            d(ls) = hsave - dphh
            d(ls+1) = dphh
            ls = ls + 1
            mmax = mmax + 1
            if(d(mmax).lt.0.0)d(mmax)=0.0
        endif
        return
        end

        subroutine frstar(r,hs,hr,mname,ipsvsh,time,pvel,svel,den,
     1      vsa, vsb, vsr, rayp, geom, tstar, dolock, dogeom,verbose)
c-----
c       r   R   Epicentral distance
c       hs  R   Source depth
c       hr  R   Receiver depth
c       mname   Ch*(*)  Name of model file
c       ipsvsh  I*4 1 - get P time
c               2 - get SV time
c               3 - get SH time
c               4 - get pP time
c               5 - get sP time
c               6 - get sS time
c       time    R   First arrival time
c       pvel    R   Velocity of P wave at receiver
c       svel    R   Velocity of S wave at receiver
c       den     R   Density at receiver
c       vsa R   P-wave velocity at source
c       vsb R   S-wave velocity at source
c       vsr R   Density at source
c       rayp R   Ray parameter in sec/km
c       geom R   geometrical spreading factor
c       tstar R   geometrical spreading factor
c       dolock L .true. apply locked mode which means to ignore the
c                bottom layer
c       dogeom L .true. compute geometrical spreading. Only do this for
c                teleseisms
c-----
        implicit none
        real r, hs, hr, time, pvel, svel, den, vsa, vsb, vsr
        real rayp, geom, tstar
        logical dolock, dogeom
        character mname*(*)
        integer ipsvsh
        logical verbose
c-----
c-----
c       internal variables
c-----
        real depths, depthr
        real dphs, dphr, dphref
        integer lmaxs, lmaxr, lmaxref
        real tm, rp(2),ts
        real dpdx, deginrad, sinis, sinir, cosir, cosis, sindeg
        real vs, vr, rs, rr

        integer iflsph
        real varec, vbrec, rhorec, vasrc, vbsrc, rhosrc
        real fac

        
        common/earth/radius
        real radius

        common/depref/refdep
        real refdep

c-----
c       compute the travel time
c-----
        call fstarr(r,time,lmaxs, lmaxr, lmaxref,
     1      hs, hr, ipsvsh,iflsph, rayp,
     2      tstar, dolock,
     3      mname, varec, vbrec, rhorec,
     4      vasrc, vbsrc, rhosrc,verbose)

        svel = vbrec
        pvel = varec
        den  = rhorec
        vsa  = vasrc
        vsb  = vbsrc
        vsr  = rhosrc
c-----
C       compute the geometrical spreading
C
C       since a flat earth is always used, we use the flat layered
C       geometrical spreading from Officer
C       The geometrical spreading is dimensionless and gives the decrease in amplitude from
C       a distance of 1 km from the source to the receiver
C                        2                            2
C            ( rhos vs  a sin Is Vs                  d T      )
C       sqrt |  -------------------------------     -----     |
C            |                                         2      |
C            ( rhor vr sin DEG  cos Ir Rs Cos Is     dx       )
C
C       where p (sec/km) = dT/dx
C       a = radius of sphere about source - we use 1.0 km
C       Is= incident angle at source
C       Ir= incident angle at receiver
C       Rs= distance from center of Earth to source
C       Rr= distance from center of Earth to receiver
C       DEG=epicental distance in degrees
C       rhos and vs are the density and wave velocity at the source depth
c       rhor and vr are the density and wave velocity at the receiver depth
c
c       To get the dp/dx, we determine p at different distances, and then form
c       an interpolating polynomial
c
        if(dogeom)then
              call fstarr(r-500,tm,lmaxs, lmaxr, lmaxref,
     1            hs+refdep, hr+refdep, ipsvsh,iflsph, rp(1),
     2            ts, dolock,
     3            mname, varec, vbrec, rhorec,
     4            vasrc, vbsrc, rhosrc,verbose)

              call fstarr(r+500,tm,lmaxs, lmaxr, lmaxref,
     1            hs+refdep, hr+refdep, ipsvsh,iflsph, rp(2),
     2            ts, dolock,
     3            mname, varec, vbrec, rhorec,
     4            vasrc, vbsrc, rhosrc,verbose)


              dpdx = abs(rp(1) - rp(2))/(500.0 - ( - 500.0))
              deginrad = r/radius
              sindeg = sin(deginrad)
  
              if(ipsvsh.eq.1)then
c               1 - get P time
                  vs = vsa
                  vr = pvel
                  rs = vsr
                  rr = den
              else if(ipsvsh.eq.2)then
c               2 - get SV time
                  vs = vsb
                  vr = svel
                  rs = vsr
                  rr = den
              else if(ipsvsh.eq.3)then
c               3 - get SH time
                  vs = vsb
                  vr = svel
                  rs = vsr
                  rr = den
              else if(ipsvsh.eq.4)then
c               4 - get pP time
                  vs = vsa
                  vr = pvel
                  rs = vsr
                  rr = den
              else if(ipsvsh.eq.5)then
c               5 - get sP time
                  vs = vsb
                  vr = pvel
                  rs = vsr
                  rr = den
              else if(ipsvsh.eq.6)then
c               6 - get sS time
                  vs = vsb
                  vr = svel
                  rs = vsr
                  rr = den
              endif
              sinis = rayp*vs
              cosis = sqrt(1.0-sinis*sinis)
              sinir = rayp*vr
              cosir = sqrt(1.0-sinir*sinir)

              fac = (rs*vs*sinis*vs*dpdx)/
     1           (rr*vr*sindeg*cosir*(radius-hs)*cosis)
              geom = sqrt(abs(fac))
      else
c-----
c     default to have a defined return value
c-----
              geom = 1.0
      endif
              

c-----
        return
        end

        subroutine fstarr(dist,tfirst,lmaxs,lmaxr,lmaxref,
     1      hs,hr,ipsvsh,iflsph, rayp,
     2      tstar, dolock,
     3      mname, varec, vbrec, rhorec,
     4      vasrc, vbsrc, rhosrc,verbose)
c-----
c       given a distance, the source depth, receiver depth,
c       get time of first arrival of P
c-----
c       dist    R   - distance
c       tfirst  R   - first arrival time
c       mmax    I*4 - number of layers in model
c       lmaxs   I*4 - layer index for source
c       lmaxr   I*4 - layer index for receiver
c       lmaxref I*4 - layer index for reference depth,
c                     used only for pP and sS
c       hs      R   - depth of source
c       hs      R   - depth of receiver
c       ipsvsh  I*4 1 - get P time
c               2 - get SV time
c               3 - get SH time
c               4 - get pP time
c               5 - get sP time
c       iflsph  I*4 - 0 flat earth model
c                 1 spherical earth model
c       rayp    R   - ray parameter in sec/km
c       geom R   geometrical spreading factor
c       dolock L .true. apply locked mode which means to ignore the
c                bottom layer
c       mname   Ch* - name of the velocity model
c       varec   R - P velocity at receiver (untransformed)
c       vbrec   R - S velocity at receiver (untransformed)
c       rhorec  R - Density at receiver (untransformed)
c       vasrc   R - P velocity at source (untransformed)
c       vbsrc   R - S velocity at source (untransformed)
c       rhosrc  R - Density at source (untransformed)
c-----
c       since this routine is to be used for omega-k,
c       we will approximate the direct arrival
c
c       18 JAN 2008 - everything is straightforward. The addition of
c          the request for pP and sP changes the logic in that
c          the direct arrival is ignored, and that the upgoing refraction 
c          from the source is ignored. We handle this by just setting
c          a very large tfirst before trying to do the modified 
c          downward path refraction to avoid another level of
c          if/then/else/endif
c       24 MAR 2008 - modified to place the model read into this
c          routine instead of in frstar
c-----
        implicit none
        real dist, tfirst, dphs, dphr, hs, hr, depths, depthr
        real rayp, tstar
        real varec, vbrec, rhorec
        real vasrc, vbsrc, rhosrc
        logical dolock
        integer lmaxs, lmaxr, lmaxref
        integer ipsvsh
        logical verbose
        character mname*(*)

        integer NL
        parameter (NL=200)
        common/isomod/d(NL),a(NL),b(NL),rho(NL),
     1      qa(NL),qb(NL),etap(NL),etas(NL), 
     2      frefp(NL), frefs(NL)
        real d, a, b, rho, qa, qb, etap, etas, frefp, frefs
        common/modlly/mmmax
        integer mmmax
        common/depref/refdep
        real refdep
        common/shwave/bsh(NL), rhosh(NL), qbsh(NL)
        real bsh, rhosh, qbsh

        integer mmax

        integer iunit, iiso, iflsph, idimen, icnvel, ierr
        character title*80

        real v(NL), h(NL), qi(NL)

        real*8 c, s, t, x, p, tint, dxdp, vel, pnew, pupper
        real*8 ts
        real*8 sumx, sumt
        logical ext

        real tds, tdr
        common/earth/radius
        real radius

        real tt, ps
        real csa, csb, dphref, pr, cs
        integer l, lmn, lmx, i, m, iter
        integer lgstr

c-----
c       first read in the model and determine the medium parameters at the
c       source and receiver depths
c-----
c       get the earth model
c-----
        inquire(file=mname,exist=ext)
        if(.not. ext) call usage('Model file does not exist')
        l = lgstr(mname)

                call getmod(1,mname,mmax,title,iunit,iiso,iflsph,
     1          idimen,icnvel,ierr,.false.)
        mmmax = mmax
        if(ierr .lt. 0)return
                call adomod()
c-----
c       insert the source and receiver depths into the model
c       placing the source and receiver on a layer boundary
c-----
        call insert(hs+refdep)
        call insert(hr+refdep)
        call insert(   refdep)

c-----
c       get the layer in which the source lies
c-----
        call srclyr(hs+refdep, lmaxs, dphs)
        call srclyr(hr+refdep, lmaxr, dphr)
        call srclyr(   refdep, lmaxref, dphref)
c-----
c       get the medium parameters at the source and reciever depths
c-----
        varec = a(lmaxr)
        vbrec = b(lmaxr)
        rhorec = rho(lmaxr)
        vasrc = a(lmaxs)
        vbsrc = b(lmaxs)
        rhosrc = rho(lmaxs)
c-----
c       prepare for the computations
c-----
        depths = hs + refdep
        depthr = hr + refdep

c-----
c       set up default
c-----
        tfirst = 1.0e+30
c-----
c       special case for locked mode
c-----
        if(dolock)then
            mmax = mmmax -1
        else
            mmax = mmmax
        endif

c-----
c       get specifics about upward and downward distances
c       with a layer. We need his to define ray paths
c       We will also use the fact that the source/receiver are
c       on layer boundaries
c
c       lmn = layer number of shallowest of source/receiver
c       lmx = layer number of deepest    of source/receiver
c-----
        lmn = min(lmaxs,lmaxr)
        lmx = max(lmaxs,lmaxr)

c-----
c       perform spherical -> flat earth model transformation
c-----
        if(iflsph.ne.0)then
            if(verbose)then
               do i=1,mmax
                WRITE(0,*)i,d(i),a(i),b(i),rho(i)
               enddo
            endif
            call adosph()
            if(verbose)then
               do i=1,mmax
                WRITE(0,*)i,d(i),a(i),b(i),rho(i)
               enddo
            endif
            tds = radius*alog(radius/(radius-hs))
            tdr = radius*alog(radius/(radius-hr))
        else
            tds = depths
            tdr = depthr
        endif
c-----
c       now fill in velocity array according to desired first arrival
c       for SH there can be no water layer
c       for SV can be a water layer
c       Also define the Q for the T* analysis. Note we define
c        eventually q = 1/Q based on whether the given Q > or < 1
c-----
        do 100 i=1,mmax
            if(ipsvsh.eq.1)then
c               1 - get P time
                v(i) = a(i)
                qi(i) = qa(i)
            else if(ipsvsh.eq.2)then
c               2 - get SV time
                v(i) = b(i)
                qi(i) = qb(i)
                if(b(i).le.0.001)then
                    v(i) = a(i)
                    qi(i) = qa(i)
                endif
            else if(ipsvsh.eq.3)then
c               3 - get SH time
                v(i) = bsh(i)
                qi(i) = qbsh(i)
            else if(ipsvsh.eq.4)then
c               4 - get pP time
                v(i) = a(i)
                qi(i) = qa(i)
            else if(ipsvsh.eq.5)then
c               5 - get sP time
                v(i) = a(i)
                qi(i) = qa(i)
            else if(ipsvsh.eq.6)then
c               6 - get sS time
                v(i) = b(i)
                qi(i) = qb(i)
            endif
            if(qi(i) .gt. 1.0)then
                qi(i) = 1.0 / qi(i)
            endif
            h(i) = d(i)
 100    continue
c-----
c       For the computations we look at four cases
c       1) direct path between source and receiver 
c          a) source and receiver in the same layer
c          b) source and receiver in different layers
c       2) refracted arrivals       
c          a) path is downward from source and then up to
c             receiver
c          b) path is upward from the source and then down to
c             receiver
c          This recognized the possibility that velocity does
c          not increase uniformly with depth
c-----
                    
c-----
c       direct arrival source/receiver at same layer
c-----
        if(v(lmaxs).eq.0.0)return
        if(v(lmaxr).eq.0.0)return
        if(lmaxs .eq. lmaxr)then
            tfirst = sqrt(dist**2 + abs(tds - tdr)**2)/
     1          v(lmaxs)
            rayp = (dist/sqrt(dist**2 + abs(tds - tdr)**2))/
     1          v(lmaxs)
            tstar = tfirst*qi(lmaxs)
        else
c-----
c       direct arrival source/receiver in different layers
c-----
c       Newton Iteration for direct arrival source/receiver at
c           different depths
c           
c           x = SUM h  tan theta
c                    i          i
c
c           t = SUM h  / v  cos theta
c                    i    i          i
c                                                          2 2
c       where sin theta  = p V  , cos theta  = sqrt ( 1 - p V )
c                      i      i                              i
c       and p is the ray parameter bounded by [0, 1/V  ] where V
c                                                    sr         sr
c       is the wave velocity at the starting point of the ray. 
c       Since the ray must also reach the receiver, we consider
c       that possibility too. The upper bound is MIN ( 1/Vs, 1/Vr)
c       Also we test for a real ray path, between source and receiver
c
c       Because source/receiver at top of a layer boundary, we have
c
c           -----------X----------
c           h(lmn)      \
c           ----------------------
c                      ....
c           ----------------------
c           h(lmx-1)        \
c                            \
c           ------------------X---
c            
c-----
            ps = 1.0/v(lmaxs)
            pr = 1.0/v(lmaxr)
            if(ps.lt.pr)then
                pupper = ps
            else
                pupper = pr
            endif
            do 1000 l=lmn,lmx
                if(v(l).eq.0.0)return
                p = 1.0/v(l)
                if(p.lt.pupper)pupper = p
 1000       continue
            p = 0.5d+00  * pupper
            do 1111 iter=1,10
                x = 0.0d+00
                t = 0.0d+00
                ts = 0.0d+00
                tint = 0.0d+00
                dxdp = 0.0d+00
                do 1500 l=lmn,lmx - 1
                    vel = dble(v(l))
                    s = p*vel
                    c = dsqrt(1.0d+00 - s*s)
                    t = t + dble(h(l)) /(vel*c)
                    x = x + dble(h(l)) * s / c
                    dxdp  = dxdp + dble(h(l)) *
     1                  vel/(c*c*c)
                    tint = tint + dble(h(l)) * c / vel
                    ts = ts + qi(l) * dble(h(l))/(c*vel)
                   

 1500           continue
                pnew = p - (x-dble(dist))/dxdp
c-----
c       safety - we must have a real ray, with upper bound
c       of  min[ 1/v(src), 1/v(rec)]
c-----  
                if(pnew .gt. pupper)then
                    if(iter.lt.10)then
                        pnew = 0.99d+00 * pupper
                    else
c-----
c       this is propably working like a refraction, so stop iterations
c-----  
                        t = tint + p * (dist)
                        go to 1112
                    endif
                endif
                p = pnew
 1111       continue
 1112       continue
            tfirst = t
            rayp = p
            tstar = ts
        endif
c-----
c       now proceed through the possible refracted arrivals
c       considering first upward rays from the source
c-----  
        if(lmn.gt.1)then
        do 3020 m=1,lmn-1
c-----
c       m is the refracting layer
c
c       get velocity of refractor
c-----
            vel = v(m)
            if(v(m).eq.0.0)return
            p = 1.0/vel
c-----
c
c           --------------------------------
c           h(1)
c           --------------------------------
c                      ....
c           --------------------------------
c           h(m)
c           ----------------...-------------
c           h(m+1)         /   \
c           --------------------------------
c                         /     \
c                      ....
c           --------------------------------
c           h(lmn-1)              \
c           -----------------------X--------
c               
c           h(lmn)     /    
c           --------------------------------
c                      ....
c           --------------------------------
c           h(lmx-1) /
c           --------X-----------------------
c
c       safety check, velocity at source or receiver must be less than
c       refraction velocity
c-----
        if(v(lmn).ge.vel)go to 3020
        if(v(lmx).ge.vel)go to 3020
c-----
c       single leg
c-----
        sumt = 0.0
        sumx = 0.0
        ts = 0.0
            do 3021 l=1,lmx-1,lmn
                if(v(l).gt.vel)go to 3020
                cs = sqrt(abs(1.0 - p*p*v(l)*v(l)))
                sumt = sumt + h(l)*cs/v(l)
                sumx = sumx + h(l)*p/cs
                ts = ts + qi(l)*h(l)/(cs * v(l))
 3021       continue
            do 3022 l=m+1,lmn-1
                if(v(l).gt.vel)go to 3020
                cs = sqrt(abs(1.0 - p*p*v(l)*v(l)))
                sumt = sumt + 2.0*h(l)*cs/v(l)
                sumx = sumx + 2.0*h(l)*p/cs
                ts = ts + 2.0*qi(l)*h(l)/(cs * v(l))
 3022       continue
            tint = sumt
            tt = tint + dist / vel
            ts = ts + qi(m)*(dist-sumx)/v(m)
            if(tt .lt. tfirst .and. dist.ge.sumx)then
                  tfirst = tt
                  rayp = p
                 tstar = ts
            endif
 3020       continue
        endif
c-----
c       For the special case of the depth phases, ignore previous
c       first arrival times
c-----
        if(ipsvsh.eq.4 .or. ipsvsh.eq.5 .or. ipsvsh.eq.6)then
             tfirst = 1.0e+30
        endif
c-----
c       now proceed through the possible refracted arrivals
c       considering first downward rays from the source
c
c       We start considering the deepest point since we place
c       a source/receiver position just below a layer boundary
c       and thus should consider a horizontal ray
c
c       The refraction is accepted only if the desired distance >
c       first refraction from the source - this puts physics in the problem
c           
c           x = SUM h  tan theta
c                    i          i
c
c           t = SUM h  cos theta / V
c                    i          i   i
c                                                          2 2
c       where sin theta  = p V  , cos theta  = sqrt ( 1 - p V )
c                      i      i                              i
c       For the T* computation we need to follow the path, e.g.,
c       SUM h qi / ( cos theta  / V ) + qi (dist -  SUM h tan theta / V )/V
c            i  i             i    i      i              i         i   i   r
c-----  
        do 2020 m=lmx+1, mmax
c-----
c       m is the refracting layer
c
c       get velocity of refractor
c-----
            vel = v(m)
            if(v(m).eq.0.0)return
            p = 1.0/vel
c-----
c
c           -----------X--------------------
c           h(lmn)      \
c           --------------------------------
c                      ....
c           --------------------------------
c           h(lmx-1)        \             
c                            \           
c           ------------------X--------X----
c           h(lmx)             \       /
c           --------------------\-----/-----
c                      ....      \   /
c           ----------------------...-------
c           h(m)
c
c-----
c       safety check, velocity at source or receiver must be less than
c       refraction velocity
c-----
        if(v(lmn).ge.vel)go to 2020
        if(v(lmx).ge.vel)go to 2020
c-----
c       single leg
c-----
        sumx = 0.0
        sumt = 0.0
        ts = 0.0
c-----
c       special case for depth phases
c-----
            if(ipsvsh.eq.4)then
c-----
c               pP
c-----
                  do  l=lmaxref,lmaxs - 1
                      if(a(l).gt.vel)go to 2020
                      cs = sqrt(abs(1.0 - p*p*a(l)*a(l)))
                      sumt = sumt + 2.*h(l)*cs/a(l)
                      sumx = sumx + 2.*h(l)*p*a(l)/cs
                      if(qa(l).gt.1.0)qa(l) = 1.0/qa(l)
                      ts = ts + 2.*qa(l)*h(l)/(cs * a(l))
                  enddo
            else if(ipsvsh.eq.5)then
c-----
c               sP
c-----
                  do  l=lmaxref,lmaxs - 1
                      if(a(l).gt.vel)go to 2020
                      if(b(l).gt.vel)go to 2020
                      csa = sqrt(abs(1.0 - p*p*a(l)*a(l)))
                      csb = sqrt(abs(1.0 - p*p*b(l)*b(l)))
                      sumt = sumt + h(l)*csa/a(l)
     1                        +h(l)*csb/b(l)
                      sumx = sumx + 2.*h(l)*p*a(l)/csa
                      if(qa(l).gt.1.0)qa(l) = 1.0/qa(l)
                      if(qb(l).gt.1.0)qb(l) = 1.0/qb(l)
                      ts = ts + qa(l)*h(l)/(csa * a(l))
     1                        + qb(l)*h(l)/(csb * b(l))
                  enddo
            else if(ipsvsh.eq.6)then
c-----
c               sS
c-----
                  do  l=lmaxref,lmaxs - 1
                      if(b(l).gt.vel)go to 2020
                      csb = sqrt(abs(1.0 - p*p*b(l)*b(l)))
                      sumt = sumt + h(l)*csb/b(l)
     1                        +h(l)*csb/b(l)
                      sumx = sumx + 2.*h(l)*p*b(l)/csb
                      if(qb(l).gt.1.0)qb(l) = 1.0/qb(l)
                      ts = ts + qb(l)*h(l)/(csb * b(l))
     1                        + qb(l)*h(l)/(csb * b(l))
                  enddo
            endif
c-----
c       continue
c-----
            do 2021 l=lmn,lmx - 1
                if(v(l).gt.vel)go to 2020
                if(v(l).eq.0.0)return
                cs = sqrt(abs(1.0 - p*p*v(l)*v(l)))
                sumt = sumt + h(l)*cs/v(l)
                sumx = sumx + h(l)*p*v(l)/cs
                ts = ts + qi(l)*h(l)/(cs * v(l))
 2021       continue
c-----
c       double leg
c-----
            do 2022 l=lmx,m-1
                if(v(l).gt.vel)go to 2020
                if(v(l).eq.0.0)return
                cs = sqrt(abs(1.0 - p*p*v(l)*v(l)))
                sumt = sumt + 2.0*h(l)*cs/v(l)
                sumx = sumx + 2.0*h(l)*p*v(l)/cs
                ts = ts + 2.*qi(l)*h(l)/(cs * v(l))
 2022       continue
            tint = sumt
            tt = tint + dist / vel
            ts = ts + qi(m)*(dist-sumx)/vel
            if(tt .lt. tfirst .and. dist.ge.sumx)then
                 tfirst = tt
                 rayp = p
                 tstar = ts
            endif
 2020       continue
             if(tfirst .eq. 1.0e+30)then
                tfirst = -12345.
                rayp   = -12345.
                tstar  = -12345.
             endif
        return
        end

        subroutine adosph()
c-----
c       Transform spherical earth to flat earth
c
c       Schwab, F. A., and L. Knopoff (1972). 
c           Fast surface wave and free
c       mode computations, in  
c           Methods in Computational Physics, Volume 11,
c       Seismology: Surface Waves and Earth Oscillations,  
c           B. A. Bolt (ed),
c       Academic Press, New York
c
c       Love Wave Equations  44, 45 , 41 pp 112-113
c       Rayleigh Wave Equations 102, 108, 109 pp 142, 144
c
c
c-----
c       mmax    I*4 number of layers
c       ipsvsh  I*4     1 - get P time
c                       2 - get SV time
c                       3 - get SH time
c-----
        implicit none
        integer NL
        parameter (NL=200)
        common/isomod/d(NL),a(NL),b(NL),rho(NL),
     1      qa(NL),qb(NL),etap(NL),etas(NL), 
     2      frefp(NL), frefs(NL)
        real d, a, b, rho, qa, qb, etap, etas, frefp, frefs
        common/modlly/mmax
        integer mmax
        common/depref/refdep
        real refdep
        common/shwave/bsh(NL), rhosh(NL), qbsh(NL)
        real bsh, rhosh, qbsh

        double precision z0,z1,r0,r1,dr,ar,tmp,rhosph

        common/earth/radius
        real radius

        integer i

        ar=radius
        dr=0.0d0
        r0=ar + refdep
        d(mmax)=1.0
        do 10 i=1,mmax
            r1=r0-dble(d(i))
            z0=ar*dlog(ar/r0)
            z1=ar*dlog(ar/r1)
            d(i)=z1-z0
c-----
c        attempt 7 15 2007 - use standard rule but at mid layer depth as per DGH
c-----
            TMP=(ar+ar)/(r0+r1)

            a(i)=a(i)*tmp
            b(i)=b(i)*tmp
            bsh(i)=b(i)
            qbsh(i)=qb(i)
            rhosph=rho(i)
            rhosh(i) = rhosph * tmp **(-5.0)
            rho(i) = rhosph * tmp **(-2.275)
            r0 = r1
   10   continue
        d(mmax)=0.0
        return
        end

        subroutine adomod()
c-----
c       just fill the rhosh, bsh and qbsh arrays 
c-----
        implicit none
        integer NL
        parameter (NL=200)
        common/isomod/d(NL),a(NL),b(NL),rho(NL),
     1      qa(NL),qb(NL),etap(NL),etas(NL), 
     2      frefp(NL), frefs(NL)
        real d, a, b, rho, qa, qb, etap, etas, frefp, frefs
        common/modlly/mmax
        integer mmax
        common/depref/refdep
        real refdep
        common/shwave/bsh(NL), rhosh(NL), qbsh(NL)
        real bsh, rhosh, qbsh

        integer i

        do  i=1,mmax
            bsh(i)=b(i)
            qbsh(i)=qb(i)
            rhosh(i) = rho(i) 
        enddo
        return
        end

        subroutine srclyr(depth,lmax,dph)
        implicit none
        real depth, dph
        integer lmax
        integer LER, LIN, LOT
        parameter (LER=0, LIN=5, LOT=6)
        integer NL
        parameter(NL=200)
        common/isomod/d(NL),a(NL),b(NL),rho(NL),
     1      qa(NL),qb(NL),etap(NL),etas(NL),
     2      frefp(NL), frefs(NL)
        real d, a, b, rho, qa, qb, etap, etas, frefp, frefs
        common/modlly/mmax
        integer mmax
        common/shwave/bsh(NL), rhosh(NL), qbsh(NL)
        real bsh, rhosh, qbsh

        integer m
        real dep
c-----
c       Find source/receiver boundary. It is assumed that
c       it will lie upon a boundary
c
c       lmax = source layer 
c            = 0 is the free surface 
c       depth = source depth 
c       dph = height of  source above lmax + 1 interface 
c       lmax = 0 is the free surface 
c-----
        if(depth.le.0.0)then
            lmax = 1
            dph = 0.0
        else
            dep = 0.0 
            do 100 m = 2,mmax
                dep = dep + d(m-1) 
                dph = dep - depth 
                lmax = m 
                if(abs(dph).lt. 0.0001*d(m-1) .or.
     1              abs(dph).lt.1.0e-6)go to 101
  100       continue 
  101   continue 
        endif
        return 
        end 

