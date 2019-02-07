!*******************************************************************************
!** This modules provide the functions readBMP and saveBMP to read and write
!**  Windows bmp version 3.x 24-bit files
module bmp
private
public readBMP, saveBMP

contains
subroutine readBMP (filename,iRGB,Width,Height,HorzResolution,VertResolution,un)
implicit none
integer*4,parameter:: max=2048
integer*2,intent(out):: iRGB(1:max,1:max,1:3)
integer*4,intent(out):: Height,Width
integer*4,intent(out):: HorzResolution,VertResolution
character(LEN=*),intent(in):: filename
integer i,j,k,reg
integer un
!integer*4 ch4i4,ch4s4
!integer*2 ch2i2,ch1i2
character(LEN=14):: FileHeader
character(LEN=40):: BitmapHeader
character(LEN=1) :: RGB
integer*4 FileSize,BitmapOffset,Size
integer*4 Compresion,SizeOfBitmap,ColorsUsed,ColorsImportant
integer*2 FileType,Reserved1,Reserved2,Planes,BitsPerPixel

if (ubound(iRGB,DIM=3)/=3) then
  print *,'ERROR_readBMP:: first parameter must be an integer*2, NxMx3 array'
  return
endif
if (lbound(iRGB,DIM=1)>1 .or. lbound(iRGB,DIM=2)>1) then
  print *,'ERROR_readBMP:: first parameter must be an integer*2, 1:N x 1:M x 1:3 array'
  return
endif

!call openr@ (filename,unit,err)
open (unit=un,file=filename,status='old',form='unformatted',access='direct',recl=1)

!call readf@(FileHeader,unit,14,b,err)
!call readf@(BitmapHeader,unit,40,b,err)
do reg=1,14
  read (un,rec=reg) FileHeader(reg:reg)
end do
do reg=15, 54
  read (un,rec=reg) BitmapHeader(reg-14:reg-14)
end do

FileType=ch2i2(FileHeader(1:2))
FileSize=ch4i4(FileHeader(3:6))
Reserved1=ch2i2(FileHeader(7:8))
Reserved2=ch2i2(FileHeader(9:10))
BitmapOffset=ch4i4(FileHeader(11:14))
Size=ch4i4(BitmapHeader(1:4))
!print *,'FileType',FileHeader(1:2)
!print *,'FIleSize',FileSize
!print *,'Reserved1',Reserved1
!print *,'Reserved2',Reserved2
!print *,'BitmapOffset',BitmapOffset
!print *,'Size', Size

if (Size==40) then
  Width=ch4s4(BitmapHeader(5:8))
  Height=ch4s4(BitmapHeader(9:12))
  Planes=ch2i2(BitmapHeader(13:14))
  BitsPerPixel=ch2i2(BitmapHeader(15:16))
  Compresion=ch4i4(BitmapHeader(17:20))
  SizeOfBitmap=ch4i4(BitmapHeader(21:24))
  HorzResolution=ch4s4(BitmapHeader(25:28))
  VertResolution=ch4s4(BitmapHeader(29:32))
  ColorsUsed=ch4i4(BitmapHeader(33:36))
  ColorsImportant=ch4i4(BitmapHeader(37:40))
!  print *,'Width',Width
!  print *,'Height',Height
!  print *,'Planes',Planes
!  print *,'BitsPerPixel',BitsPerPixel
!  print *,'Compresion',Compresion
!  print *,'SizeOfBitmap',SizeOfBitmap
!  print *,'HorzResolution',HorzResolution
!  print *,'VertResolution',VertResolution
!  print *,'ColorsUsed',ColorsUsed
!  print *,'ColorsImportant',ColorsImportant
else
  print *,'ERROR_readBMP:: The file must be a BMP version 3.x'
  return
endif
if (BitsPerPixel/=24 .or. BitmapOffset/=54) then
  print *,'ERROR_readBMP:: The file must be a 24-Bit BMP'
  return
endif

reg=54
if (Width<=ubound(iRGB,DIM=2) .and. Height<=ubound(iRGB,DIM=1)) then
  do i=1, Height
    do j=1, Width
      do k=3, 1, -1
!        call readf@(RGB,unit,1,b,err)
        reg=reg+1
        read (un,rec=reg) RGB
        iRGB(i,j,k)=ch1i2(RGB)
      enddo
    enddo
    if (mod(Width*3,4)/=0) then !fills until a multiple of four is reached
      do k=1,4-mod(Width*3,4)
        reg=reg+1
        read (un,rec=reg) RGB
      end do
    end if
  enddo
else
  print *,'ERROR_readBMP:: The image is bigger than the first parameter'
  return
endif

!call closef@(unit,err)
close(unit=un)

end subroutine readBMP


subroutine saveBMP (filename,iRGB,Width,Height,HorzResolution,VertResolution,un)
implicit none
integer*4,parameter:: max=2048
integer*2,intent(in):: iRGB(1:max,1:max,1:3)
integer*4,intent(in):: Height,Width
integer*4,intent(in):: HorzResolution,VertResolution
character(LEN=*),intent(in):: filename
integer*4 i,j,k,reg
integer*4 un
!character(LEN=4):: i4ch4
!character(LEN=2):: i2ch2
character(LEN=14):: FileHeader
character(LEN=40):: BitmapHeader
character(LEN=1) :: RGB
integer*4 FileSize,BitmapOffset,Size
integer*4 Compresion,SizeOfBitmap,ColorsUsed,ColorsImportant
integer*2 Reserved1,Reserved2,Planes,BitsPerPixel

if (Width>max .or. abs(Height)>max) then
  print *,'ERROR_readBMP:: Height or Width is greater than provided array'
  return
endif

!call openw@ (filename,unit,err)
open (unit=un,file=filename,status='unknown',form='unformatted',access='direct',recl=1)

FileSize=54+abs(Height)*(3*Width+4-mod(3*Width,4)) !mod is to fill up to a multipleof four
Reserved1=0
Reserved2=0
BitmapOffset=54

FileHeader='BM'//i4ch4(FileSize)//i2ch2(Reserved1)//i2ch2(Reserved2)//i4ch4(BitmapOffset)

!call writef@(FileHeader,unit,14,b,err)
do reg=1,14 
  write (un,rec=reg) FileHeader(reg:reg)
end do

Size=40
!Width
!Height
Planes=1
BitsPerPixel=24
Compresion=0
SizeOfBitmap=abs(Height)*(3*Width+4-mod(3*Width,4)) !mod is to fill up to a multipleof four
!HorzResolution
!VertResolution
ColorsUsed=0
ColorsImportant=0
  
BitmapHeader=i4ch4(Size)//i4ch4(Width)//i4ch4(Height)//i2ch2(Planes)//i2ch2(BitsPerPixel)//i4ch4(Compresion)//&
             i4ch4(SizeOfBitmap)//i4ch4(HorzResolution) //i4ch4(VertResolution) //&
             i4ch4(ColorsUsed)//i4ch4(ColorsImportant)
!call writef@(BitmapHeader,unit,40,b,err)
do reg=15,54
  write (un,rec=reg) BitmapHeader(reg-14:reg-14)
end do

reg=54
do i=1, Height
  do j=1, Width
    do k=3, 1, -1
      RGB=char(iRGB(i,j,k))
!      call writef@(RGB,unit,1,b,err)
       reg=reg+1
       write (un,rec=reg) RGB
    enddo
  enddo
  if (mod(Width*3,4)/=0) then !fills until a multiple of four is reached
    do k=1,4-mod(Width*3,4)
      RGB=char(1)
      reg=reg+1
      write (un,rec=reg) RGB
    end do
  end if
enddo

!call closef@(unit,err)
close (unit=un)

end subroutine saveBMP


integer*4 function ch4i4 (word)!character*4 -> unsigned integer*4
character(LEN=4):: word

ch4i4=ishft(ichar(word(4:4)),24)+ishft(ichar(word(3:3)),16)+ishft(ichar(word(2:2)),8)+ichar(word(1:1))

end function ch4i4

integer*2 function ch2i2 (word)!character*2 -> unsigned integer*2
character(LEN=2):: word

ch2i2=ishft(ichar(word(2:2)),8)+ichar(word(1:1))

end function ch2i2

integer*2 function ch1i2 (word)!character*1 -> unsigned integer*2
character(LEN=1):: word

ch1i2=ichar(word)

end function ch1i2

integer*4 function ch4s4 (word)!character*4 -> signed integer*4
character(LEN=4):: word

if (ishft(ichar(word(4:4)),-7)==1) then
  ch4s4=-1*(ishft(ichar(word(4:4))-128,24)+ishft(ichar(word(3:3)),16)+ishft(ichar(word(2:2)),8)+ichar(word(1:1)))
else
  ch4s4=ishft(ichar(word(4:4)),24)+ishft(ichar(word(3:3)),16)+ishft(ichar(word(2:2)),8)+ichar(word(1:1))
endif

end function ch4s4 

character(LEN=4) function i4ch4 (i)!unsigned integer*4 -> character*4
integer*4 i,i1,i2,i3,i4

i1=ishft(i,-24)
i2=ishft(ishft(i,8),-24)
i3=ishft(ishft(i,16),-24)
i4=ishft(ishft(i,24),-24)
            
i4ch4=char(i4)//char(i3)//char(i2)//char(i1)

end function i4ch4

character(LEN=2) function i2ch2 (i)!unsigned integer*2 -> character*2
integer*2 i,i1,i2

i1=ishft(i,-8)
i2=ishft(ishft(i,8),-8)
            
i2ch2=char(i2)//char(i1)

end function i2ch2
!*****************************************************************************************************
end module bmp
 
