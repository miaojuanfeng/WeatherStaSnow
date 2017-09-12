!经纬度数值规则
  !纬度:北纬为正，南纬为负，赤道为0
  !经度:西经为正，东经为负，
module JL_Mod_BHCommon
     type hgt_t  !位势高度数据结构
		real, pointer :: H(:,:) !指向数据的指针，用于存放位势高度数据
		real :: block_width  !经度分辨率
		real :: block_height !纬度分辨率
	 end type
	 	 
	 real, parameter :: longitude_max = 360.0
	 real, parameter :: latitude_max = 180.0
	 integer, parameter :: FILE_NAME_MAX_LENGTH = 50
	 
 contains
    
    subroutine hgt_create(hgt, H, block_width, block_height)
		type(hgt_t) :: hgt
		real, target :: H(:,:)
		real :: block_width, block_height

		hgt % H => H
		hgt % block_width = block_width
		!write(*,*)hgt%block_width
		hgt % block_height = block_height
		!write(*,*)hgt%block_height
		
	end subroutine

	subroutine hgt_destory(hgt)
        type(hgt_t)::hgt
        deallocate(hgt%H)
	end subroutine

	function hgt_longitude_index(hgt, x)!经度转换为数组下标
		type(hgt_t) :: hgt
		real :: x
		integer :: hgt_longitude_index

        !write(*,*)hgt%block_width,hgt%block_height,x
		if (x < 0) then
			hgt_longitude_index = x + longitude_max / hgt % block_width + 1
		else
			hgt_longitude_index = x / hgt % block_width + 1
		end if

	end function

	function hgt_longitude_value(hgt, ix)!数组下标转换为经度
		type(hgt_t) :: hgt
		integer :: ix
		real :: hgt_longitude_value

		hgt_longitude_value = (ix - 1) * hgt % block_width
		if (hgt_longitude_value > longitude_max / 2) then
			hgt_longitude_value = hgt_longitude_value - longitude_max
		end if

	end function

	function hgt_latitude_index(hgt, y)!纬度转换为数组下标
		type(hgt_t) :: hgt
		real :: y
		integer :: hgt_latitude_index


		hgt_latitude_index = (y + latitude_max / 2) / hgt % block_height + 1
	end function

	function hgt_latitude_value(hgt, iy)!数组下标转换为纬度
		type(hgt_t) :: hgt
		integer :: iy
		real :: hgt_latitude_value

		hgt_latitude_value = (iy - 1) * hgt % block_height - latitude_max / 2
	end function
	
	!-----------判断闰年--------------------------------------
        logical function IsLeapYear(year)
	         integer year
	         IsLeapYear=(mod(year,4)==0 .and. mod(year,100)/=0 ).or. mod(year,400)==0
	    end function IsLeapYear
    !--------------------------------------------------------- 
    
	!求给定日期的前一天的日期
	!给定年月日y1,m1,d1，求前一天的年月日y2,m2,d2
	subroutine Prev_day(y1,m1,d1,y2,m2,d2)
	    integer::y1,m1,d1,y2,m2,d2
	    
	    y2=y1
	    m2=m1
	    d2=d1-1
	    if(d2<1) then
	        m2=m1-1
	        d2=31
	        if(m2<1)then 
	            m2=12
	            y2=y1-1
	        else
	            if(m2 .EQ. 3 .OR. m2 .EQ. 5 .OR. m2 .EQ. 7 .OR. m2 .EQ. 8 .OR. m2 .EQ. 10 .OR. m2 .EQ. 12)then
	                d2=31
	            endif
	            if(m2 .EQ. 4 .OR. m2 .EQ. 6 .OR. m2 .EQ. 9 .OR. m2 .EQ. 11)then
	                d2=30
	            endif
	            if(m2 .EQ. 2) then
	                if(IsLeapYear(y2))then
	                    d2=29
	                else
	                d2=28
	                endif
	            endif
	         endif
	     endif
	    
	end subroutine Prev_day
	
	!求给定日期的后一天的日期
	!给定年月日y1,m1,d1，求后一天的年月日y2,m2,d2
	subroutine Next_day(y1,m1,d1,y2,m2,d2)
	    integer::y1,m1,d1,y2,m2,d2
	    
	    y2=y1
	    m2=m1
	    d2=d1+1
	    
	    if(m2 .EQ. 2) then
	        if(.not. IsLeapYear(y2)) then
	            d2=1
	            m2=3	            
	        endif
	    endif
	    if(m2 .EQ. 1 .OR. m2 .EQ. 3 .OR. m2 .EQ. 5 .OR. m2 .EQ. 7 .OR. m2 .EQ. 8 .OR. m2 .EQ. 10 )then
	        if(d2 .EQ. 32) then
	            d2=1
	            m2=m1+1
	        endif
	    endif
	    if(m2 .EQ. 12) then
	        y2=y1+1
	        m2=1
	        d2=1
	    endif
	    
	end subroutine Next_day

end module JL_Mod_BHCommon