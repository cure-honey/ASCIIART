    module m_bmp
      implicit none
      type :: t_bmp_file_header
        sequence  
        !integer(2) :: bfType = transfer('BM', 0_2, 1) ! BitMap
        character(len = 2) :: bfType = 'BM'
        integer(4) :: bfSize          ! file size in bytes
        integer(2) :: bfReserved1 = 0 ! always 0
        integer(2) :: bfReserved2 = 0 ! always 0
        integer(4) :: bfOffBits
      end type t_bmp_file_header
      !
      type :: t_bmp_info_header
        sequence
        integer(4) :: biSize     = Z'28' ! size of bmp_info_header ; 40bytes 
        integer(4) :: biWidth
        integer(4) :: biHeight
        integer(2) :: biPlanes   = 1 ! always 1
        integer(2) :: biBitCount
        integer(4) :: biCompression = 0 ! 0:nocompression, 1:8bitRLE, 2:4bitRLE, 3:bitfield
        integer(4) :: biSizeImage
        integer(4) :: biXPelsPerMeter = 3780 ! 96dpi
        integer(4) :: biYPelsPerMeter = 3780 ! 96dpi 
        integer(4) :: biClrUsed      = 0
        integer(4) :: biClrImportant = 0 
      end type t_bmp_info_header
      type :: t_rgb
        sequence
        character :: r, g, b
      end type t_rgb 
      type :: t_bmp
        type(t_rgb), allocatable :: rgb(:, :)
      contains 
        procedure :: rd => rd_bmp
      end type  
    contains   
      subroutine rd_bmp(bmp, fn)
        class(t_bmp), intent(out) :: bmp
        character(len = *), intent(in) :: fn
        integer :: i, j
        character :: dummy
        type(t_bmp_file_header) :: bmp_file_header
        type(t_bmp_info_header) :: bmp_info_header
        open(10, file = fn//'.bmp', access = 'stream', status = 'old')
        read(10) bmp_file_header, bmp_info_header
        associate(nx => bmp_info_header%biWidth, ny => bmp_info_header%biHeight)
          allocate( bmp%rgb(nx, ny) )
          read(10) (bmp%rgb(:, i), (dummy, j = 1, mod(nx, 4)), i = 1, ny) ! data + padding 
        end associate
        close(10)
      end subroutine rd_bmp  
    end module m_bmp

    
    
    module m_font
      use m_bmp 
      implicit none
      integer, parameter :: mx = 8, my = 8
      character(len = *), parameter :: font_name = 'font8x8' 
      type :: t_bmp_array
        type(t_bmp), allocatable :: bmp(:, :)  
      end type t_bmp_array 
    contains
      subroutine rd_font(font)
        type(t_bmp_array), intent(out) :: font
        call rd_and_chop(font, font_name)
      end subroutine rd_font
    
      subroutine rd_and_chop(pic, fn)
        type(t_bmp_array), intent(out) :: pic
        character(len = *), intent(in) :: fn
        type(t_bmp) :: bmp 
        integer :: ix, iy, kx, ky  
        call bmp%rd(fn)
        associate(nx => size(bmp%rgb, 1) / mx, ny => size(bmp%rgb, 2) / my)
          allocate(pic%bmp(nx, ny))
          do ix = 1, nx
            do iy = 1, ny
              kx = (ix - 1) * mx + 1 
              ky = (iy - 1) * my + 1    
              pic%bmp(ix, iy)%rgb = bmp%rgb(kx:kx + mx - 1, ky:ky + my - 1)
            end do
          end do  
        end associate  
      end subroutine rd_and_chop

      pure elemental character function to_aa(ft, font)
        type(t_bmp      ), intent(in) :: ft                
        type(t_bmp_array), intent(in) :: font
        to_aa = char_font( minloc( distance(ft, font%bmp) ) ) ! find min. from font-array
      contains
        pure elemental integer function distance(ft1, ft0) 
          type(t_bmp), intent(in) :: ft1, ft0
          real, parameter :: f = 1.45 !1.16      ! f empirical parameter  1.0~2.5 larger is darker
          distance = sum( ( (iachar(ft1%rgb%r) + iachar(ft1%rgb%g) + iachar(ft1%rgb%b) ) &
                      - f * (iachar(ft0%rgb%r) + iachar(ft0%rgb%g) + iachar(ft0%rgb%b) ) )**2 )
        end function distance   
        
        pure character function char_font(ipos)
          integer, intent(in) :: ipos(2)
          associate(nx => size(font%bmp, 1), ny => size(font%bmp, 2))
            char_font = achar( ipos(1) + (ny - ipos(2)) * nx - 1 + x'20' )  
          end associate  
        end function char_font
      end function to_aa
      
      pure elemental character function adjust(ft, chars)
        type(t_bmp), intent(in) :: ft                
        character (len = *), intent(in) :: chars 
        integer :: k
        k = luminosity(ft) * len(chars) / 256 / size(ft%rgb) + 1
        adjust = chars(k:k) 
      contains
        pure elemental integer function luminosity(ft) 
          type(t_bmp), intent(in) :: ft
          real, parameter :: fr = 0.3, fg = 0.6, fb = 0.1 ! luminosity factor
          luminosity = sum( fr * iachar(ft%rgb%r) + fg * iachar(ft%rgb%g) + fb * iachar(ft%rgb%b) )
        end function luminosity
      end function adjust
    end module m_font
    
    
    
    program BMP_to_AA
      use m_font
      implicit none
      character, allocatable :: aa(:, :)
! make ASCII ART      
      block 
        type(t_bmp_array) :: font, pic 
        call rd_font(font) 
        call rd_and_chop(pic, 'tktt')
        aa = to_aa(pic%bmp, font)
        where (aa == 'M') aa = adjust(pic%bmp, '***===&$#M'  ) ! white gradation
        where (aa == ' ') aa = adjust(pic%bmp, '   .,`''"-:;') ! black gradation
      end block  
! output to console
      block 
        integer :: iy  
        do iy = size(aa, 2), 1, -1       
          print '(*(g0))', aa(:, iy)
        end do
      end block  
    end program BMP_to_AA
    
    
 