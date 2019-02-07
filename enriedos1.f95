program enriedos
! gfortran -O2 bmp.f95 enriedos.f95 -o enriedos.exe
! gfortran -fbounds-check bmp.f95 enriedos.f95 -o enriedos.exe
use bmp
implicit none
interface
  subroutine dibujapuntos(puntos,dir,lineas,out)
    implicit none
    real dir(1000,2)
    logical lineas(1000,1000)
    integer*2,dimension(1:2048,1:2048,1:3) :: out
    integer puntos
  end subroutine dibujapuntos
  function cruce(A1,A2,B1,B2)
    implicit none
    logical cruce
    real A1(2),A2(2),B1(2),B2(2)
  end function cruce
end interface
integer*2,dimension(1:2048,1:2048,1:3):: in, out
integer*4:: hor,ver,horres,verres, io, jo, h, l

integer semilla(12), i, j, k, n, m, p1, p2, u, v, discret
integer ancho, alto, puntos, enlaces, orden(1000), extremos(1000,2), salen(1000,7)
real aleat(1000), dir(1000,2), centros(1000,2), fza_cent(1000,1000,2), fza_punto(1000,2)
real m_c_o, m_p_o, masa_cent, masa_punto, long_o, long, umbral
real theta, vec(2), p(2), q(2), fza(2)
logical horiz(100,100), vert(100,100), diag(100,100), lineas(1000,1000), lista(1000)
character*40 salida

out=0
hor=1000; ver=1000; horres=1; verres=1

ancho=12; alto=8; puntos=ancho*alto
umbral=0.5
discret=9
m_c_o=0.1; m_p_o=10.0; long_o=10.0
masa_cent=m_c_o; masa_punto=1.0

! etapa de enriedo
call system_clock(n); semilla=n
call random_seed(put=semilla)
call random_number(aleat)
n=100

lista=.true.

do i=1,puntos
  k=int(aleat(n)*(1+puntos-i))+1
  n=n+1
  if(k>1+puntos-i)k=1+puntos-i
  j=1;l=1
  do while (j<=k)
    do while (lista(l).eqv. .false.)
      l=l+1
    end do
    j=j+1
    l=l+1
  end do
  l=l-1
  lista(l)=.false.
  orden(i)=l
end do

! etapa de enlazamiento
lineas=.false.
call system_clock(n); semilla=n
call random_seed(put=semilla)
call random_number(aleat)
n=10
! para los n puntos ubicados en una grilla de alto x ancho
! establece relaciones entre todos los vecinos verticales,
! horizontales y diagonales de un solo sentido.
forall(i=1:alto-1,j=1:ancho-1)lineas(((i-1)*ancho+j),((i-1)*ancho+j+1))=.true.
forall(i=1:alto-1,j=1:ancho-1)lineas((i*ancho+j),((i-1)*ancho+j))=.true.
forall(i=1:alto-1,j=1:ancho-1)lineas((i*ancho+j),((i-1)*ancho+j+1))=.true.
forall(i=1:alto-1)lineas(((i+1)*ancho),(i*ancho))=.true.
forall(j=1:ancho-1)lineas(((alto-1)*ancho+j),((alto-1)*ancho+j+1))=.true.
lineas=lineas .or. transpose(lineas)


! Y ahora decide cuáles de esas relaciones eliminar
do i=1,alto-1
  do j=1,ancho-1
    p1=(i-1)*ancho+j+1
    ! vínculos horizontales
    if (aleat(n)>umbral) then
      if (count(lineas(p1,:))>2) then
        p2=(i-1)*ancho+j
        if(count(lineas(p2,:))>2) then
          lineas(p1,p2)=.false.
          lineas(p2,p1)=.false.
        end if
      end if
    end if
    n=n+1
    ! vínculos verticales
    if (aleat(n)>umbral) then
      if (count(lineas(p1,:))>2) then
        p2=i*ancho+j
        if(count(lineas(p2,:))>2) then
          lineas(p1,p2)=.false.
          lineas(p2,p1)=.false.
        end if
      end if
    end if
    n=n+1
    ! vínculos diagonales
    if (aleat(n)>umbral) then
      if (count(lineas(p1,:))>2) then
        p2=i*ancho+j
        if(count(lineas(p2,:))>2) then
          lineas(p1,p2)=.false.
          lineas(p2,p1)=.false.
        end if
      end if
    end if
    n=n+1
  end do
  ! vínculos verticales del fondo
  if (aleat(n)>umbral) then
    j=ancho
    p2=i*ancho+j
    if (count(lineas(p1,:))>2 .and. count(lineas(p2,:))>2) then
      lineas(p1,p2)=.false.
      lineas(p2,p1)=.false.
    end if
  end if
  n=n+1
end do
i=alto
do j=1,ancho-1
  ! vínculos horizontales del fondo
  if (aleat(n)>umbral) then
    p1=(i-1)*ancho+j+1
    p2=(i-1)*ancho+j
    if (count(lineas(p1,:))>2 .and. count(lineas(p2,:))>2) then
      lineas(p1,p2)=.false.
      lineas(p2,p1)=.false.
    end if
  end if
  n=n+1
end do

if (all(lineas .eqv. transpose(lineas)))then
  write(*,*)"Los puntos están bien unidos"
else
  write(*,*)"Algo no anda con los puntos, así q se corrige como pinta"
  lineas=lineas .or. transpose(lineas)
end if

! Anota los extremos de las líneas y sus centros
salen=0
extremos=0
centros=0
enlaces=0
do i=1,puntos
  do j=i+1,puntos
    if(lineas(i,j).eqv. .true.)then
      enlaces=enlaces+1
      extremos(enlaces,:)=(/i,j/) ! Indica los puntos extremos de cada línea
      centros(enlaces,:)=0.5*(/dir(i,1)+dir(j,1),dir(i,2)+dir(j,2)/)
      salen(i,salen(i,1)+2)=enlaces; salen(i,1)=salen(i,1)+1
      salen(j,salen(j,1)+2)=enlaces; salen(j,1)=salen(j,1)+1
      ! En la matriz "salen" se listan las líneas q le corresponden a cada
      ! punto. El primer numero es la cantidad de enlaces del punto.
    end if
  end do
end do
write(*,*)"total de lineas ",enlaces


! Dibuja la grilla
do n=1,puntos
  i=990-30*((n-1)/ancho)
  j=10+30*mod(n-1,ancho)
  dir(n,:)=(/i,j/)
  do k=2,salen(n,1)+1
    m=extremos(salen(n,k),1)
    if(m==n)m=extremos(salen(n,k),2)
    centros(salen(n,k),:)=0.5*(/dir(n,1)+dir(m,1),dir(n,2)+dir(m,2)/)
  end do
end do
! Cuenta los cruces al comienzo
n=0
do i=1,enlaces
  do j=i+1,enlaces
    if (cruce(dir(extremos(i,1),:), &
      dir(extremos(i,2),:), &
      dir(extremos(j,1),:), &
      dir(extremos(j,2),:)).eqv. .true.) n=n+1
  end do
end do
write(*,*)"Cantidad de cruces iniciales ",n

call dibujapuntos(puntos,dir,lineas,out)

! Ubica los puntos sobre una circunferencia
do i=1,puntos
  theta=2*3.1415962/puntos*(i-1)
  io=int(300*cos(theta))+500; jo=int(300*sin(theta))+500
  n=orden(i)
  dir(n,:)=(/io,jo/)
  do k=2,salen(n,1)+1
    m=extremos(salen(n,k),1)
    if(m==n)m=extremos(salen(n,k),2)
    centros(salen(n,k),:)=0.5*(/io+dir(m,1),jo+dir(m,2)/)
  end do
end do

call dibujapuntos(puntos,dir,lineas,out)

call saveBMP("problema.bmp",out,hor,ver,horres,verres,1)

! Cuenta los cruces
n=0
do i=1,enlaces
  do j=i+1,enlaces
    if (cruce(dir(extremos(i,1),:), &
      dir(extremos(i,2),:), &
      dir(extremos(j,1),:), &
      dir(extremos(j,2),:)).eqv. .true.) n=n+1
  end do
end do
write(*,*)"Cantidad de cruces ",n

! Ahora viene toda la etapa de movimiento. Cada punto se mueve empujado
! por la suma de las repulsiones q los centros de los enlaces se ejercen
! entre sí y la tensión de tracción en los enlaces q los tiran hacia
! los puntos a los q están unidos.

! Calcula las fuerzas de rechazo entre los centros de las líneas
fza_cent=0
l=0
do while (l<300.and.k>0)
  l=l+1
  masa_cent=m_c_o/(((300-l)/50.)*sin(l*4*3.141596/100)**2+1.0)
  masa_punto=m_p_o/(((300-l)/50.)*sin(l*5*3.141596/100)**2+1.0)
  fza_punto=0
  do i=1,enlaces
    do j=i+1,enlaces
      do u=1,discret
        vec(1)=(u-0.5)/discret
        p=dir(extremos(i,1),:)*(1.-vec(1)) + dir(extremos(i,2),:)*vec(1)
        do v=1,discret
          vec(2)=(v-0.5)/discret
          q=dir(extremos(j,1),:)*(1.-vec(2)) + dir(extremos(j,2),:)*vec(2)
          fza=(p-q)/((max(0.001,sum((p-q)**2))**1.5)*discret*masa_cent)
          fza_punto(extremos(i,1),:)=fza_punto(extremos(i,1),:)+fza*(1-vec(1))
          fza_punto(extremos(i,2),:)=fza_punto(extremos(i,2),:)+fza*vec(1)
          fza_punto(extremos(j,1),:)=fza_punto(extremos(j,1),:)-fza*(1-vec(2))
          fza_punto(extremos(j,2),:)=fza_punto(extremos(j,2),:)-fza*vec(2)
        end do
      end do
    end do
  end do

  ! Mueve los puntos según la dinámica de fuerzas
  do n=1,puntos
    vec=fza_punto(n,:)
    do j=2,salen(n,1)+1
      i=salen(n,j)
      m=extremos(i,1);if(m==n)m=extremos(i,2)
      long=sum((dir(m,:)-dir(n,:))**2)**0.5
      vec=vec+(dir(m,:)-dir(n,:))*(long-long_o)/(long*masa_punto)
!       vec(1)=vec(1)+(dir(m,1)-dir(n,1))
    end do
    ! Si el impulso es muy grande se lo limita a 100
    if (any(abs(vec)>long_o)) then
      vec=vec*long_o/sum(vec**2)**0.5
    end if
    dir(n,:)=dir(n,:)+vec
  end do

  ! Recalcula los centros de los enlaces
  forall(i=1:enlaces)centros(i,:)=(dir(extremos(i,1),:)+dir(extremos(i,2),:))*0.5
  ! Cuenta los cruces
  n=k
  k=0
  do i=1,enlaces
    do j=i+1,enlaces
      if (cruce(dir(extremos(i,1),:), &
        dir(extremos(i,2),:), &
        dir(extremos(j,1),:), &
        dir(extremos(j,2),:)).eqv. .true.) k=k+1
    end do
  end do
  write(*,*)l,"Cantidad de cruces ",k
!   if(k==0)then
!     masa_cent=masa_cent/0.75
!     masa_punto=masa_punto/0.75
!   else if(n==k)then
!     masa_cent=masa_cent*0.7
!     masa_punto=masa_punto*0.9
!   end if

  out=0

  call dibujapuntos(puntos,dir,lineas,out)

  write(salida,*)"solucion",l,".bmp"

  call saveBMP(trim(salida),out,hor,ver,horres,verres,1)

!   if(n==0) exit

end do

end program enriedos
!::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine dibujapuntos(puntos,dir,lineas,out)
! Dibuja las líneas y los puntos
  implicit none
  real dir(1000,2)
  logical lineas(1000,1000)
  integer*2,dimension(1:2048,1:2048,1:3) :: out
  integer puntos
  integer i, j, k, l, n, cx, cy
  real factor, dir1(1000,2)

  ! Verifica q nada se halla ido de pantalla y si sí achica todo
  factor=1.
  if (any(abs(dir(1:puntos,:)-500)>490)) factor=490/maxval(abs(dir(1:puntos,:)-500))
  dir1=((dir-500)*factor)+500
!   write(*,*)"maxval(abs(dir))=",maxval(abs(dir(1:puntos,:))),"minimo viejo=",minval(dir(1:puntos,:))&
!     ,"maximo viejo=",maxval(dir(1:puntos,:))
!   write(*,*)"lugar del minimo viejo=",minloc(dir(1:puntos,:)),"lugar del minimo maximo viejo=",maxloc(dir(1:puntos,:))
!   write(*,*)"maxval(abs(dir))=",maxval(abs(dir(1:puntos,:))),"minimo nuevo=",minval(dir1(1:puntos,:))&
!     ,"maximo nuevo=",maxval(dir1(1:puntos,:))
!   write(*,*)"lugar del minimo nuevo=",minloc(dir1(1:puntos,:)),"lugar del minimo maximo nuevo=",maxloc(dir1(1:puntos,:))
  if (factor<0.99) then
    i=int(500*factor)
    out((/-i,i/)+500,500-i:500+i,2)=100
    out(500-i:500+i,(/-i,i/)+500,2)=100
  end if
  do i=1,puntos
    out(int(dir1(i,1)-1):int(dir1(i,1)+1),int(dir1(i,2)-1):int(dir1(i,2)+1),1)=255
    out(int(dir1(i,1)-1):int(dir1(i,1)+1),int(dir1(i,2)-1):int(dir1(i,2)+1),2:3)=0
  end do
  do i=1,puntos
    do j=i,puntos
      if(lineas(i,j).eqv. .true.)then
        k=dir1(j,1)-dir1(i,1); l=dir1(j,2)-dir1(i,2)
        cx=int((dir1(j,1)+dir1(i,1))*0.5); cy=int((dir1(j,2)+dir1(i,2))*0.5)
        forall(n=1:300)out(int(dir1(i,1)+k*n/300.0),int(dir1(i,2)+l*n/300.0),:)=255
        out(cx-1:cx+1,cy-1:cy+1,1:2)=255
        out(cx-1:cx+1,cy-1:cy+1,  3)=0
      end if
    end do
  end do


end subroutine dibujapuntos

function cruce(A1,A2,B1,B2)
  implicit none
  logical cruce
  real A1(2),A2(2),B1(2),B2(2)
  real Sa(2),Sb(2),X1(2),X2(2),Y1(2),Y2(2)

  Sa=A2-A1; Sb=B2-B1
  X1=A1-B1; X2=A2-B1
  Y1=B1-A1; Y2=B2-A1

  cruce=((X1(1)*Sb(2)-X1(2)*Sb(1))*(X2(1)*Sb(2)-X2(2)*Sb(1)) < -1).and.&
    ((Y1(1)*Sa(2)-Y1(2)*Sa(1))*(Y2(1)*Sa(2)-Y2(2)*Sa(1)) < -1)

end function cruce
