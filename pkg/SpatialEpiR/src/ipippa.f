      function ipippa(x,y,xc,yc,nc)
c
c point in polygon routine.
c
c returns 0 if point x,y not in the bound polygon defined by xc,yc
c
c fortran version of C routine by Ken McElvain
c

      implicit real*8 (a-h,o-z)

      include 'bounds.cmn'

      dimension xc(nc+1),yc(nc+1)

        iwind = 0
        xlastp = xc(nc)
        ylastp = yc(nc)
        ioldq = iquad(xlastp,ylastp,x,y)
        do i=1,nc
c for each point in the polygon
                xthisp=xc(i)
                ythisp=yc(i)
                inewq = iquad(xthisp,ythisp,x,y)
                if(ioldq.ne.inewq) then
                        if(mod(ioldq+1,4).eq.inewq) then
                          iwind=iwind+1
                        else if(mod(inewq+1,4).eq.ioldq) then
                          iwind = iwind - 1
                        else
                          a = (ylastp-ythisp)*(x-xlastp)
                          b = xlastp-xthisp
                          a = a + ylastp * b
                          b=b*y
                             if (a.gt.b) then
                               iwind=iwind+2
                             else
                               iwind=iwind-2
                             end if
                        end if
                end if
                xlastp=xthisp
                ylastp=ythisp
                ioldq=inewq
      end do
c
c quadrant winding is either -4,0,+4 so divide down and take abs.
c
      ipippa = abs(iwind/4)

      end

      function iquad(xp,yp,xo,yo)
c
c determine which quadrant xp,yp is in relative to xo,yo as origin
c
      implicit real*8 (a-h,o-z)

        if(xp.lt.xo)then
                if(yp.lt.yo) then
                   iquad=2
                else
                   iquad=1
                end if
        else
                if(yp.lt.yo)then
                   iquad = 3
                else
                   iquad = 0
                end if
        end if

      return
      end