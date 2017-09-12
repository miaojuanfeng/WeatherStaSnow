!program main
!    use JL_Mod_StaSnow_Common
!    implicit none
!    character*(FileNameMaxLen)::FileName
!    integer::Year,Month,Day
!    integer::i,j,k
!    integer,dimension(8)::val1,val2
!    type(DaySnow),allocatable::DayData(:)
!    type(daysnow):: SnowData(2,778)
!    integer:: MaxDepthDay(778)
!    integer::FactRL
!    integer::F
!    integer :: Numbers(5)
!    integer :: SnowDepth(5)
!    integer::a(778)
!    type(MonthSnow)::MonData(778)
!    
!    Year=2009
!    Month=11
!    Day=30
!    call ReadMonSnowData(2010,12,MonData,778)
!    write(*,*)MonData(1:10)
!  !  call CreateMonData(2010,10)
!    !计算最大深度积雪出现日
!!    allocate(DayData(778))
!!    call CreateMonData(2010,10)
!!
!!
!    !call CreateMonData(2010,12)
!    !生成2010月积雪数据
!!    call date_and_time(values=val1)
!!    do j=1951,2010
!!        do i=1,12
!!            call CreateMonData(j,i)
!!        enddo
!!    end do
!!    call date_and_time(values=val2)    
!!    write(*,"('消耗',I4,'分',I4,'秒',I4,'毫秒')") val2(6)-val1(6),val2(7)-val1(7),val2(8)-val1(8)
! !   call CreateMonData(2010,6)
!    !----------------------------------------
!    
!!    !求多个积雪数据中的最大积雪深度值
!!    data SnowDepth  /32766,89,1,32766,32744/
!!    F= FindMaxDepth(SnowDepth,5)
!!    write(*,*)F
!!    !计算最大积雪深度出现日
!!     F=GetMaxDepthDate(Year,Month,Day)
!!     write(*,*) F
!    
!!    !计算日积雪中>=1cm,5cm,10cm,20cm,30cm 的日数
!!    call GetDaySnowNumber(Year,Month,Day,Numbers)
!!    write(*,*)numbers
!!    !-------------------------------------------
!    
!!    !获取日积雪最大深度
!!    F=GetDayMaxDepth(Year,Month,Day)
!!    write(*,*)F
!!    !---------------------------------------------
!    
!!    !获取月所包含的天数
!!    F=GetMonDays(Year,Month)
!!    write(*,*)F
!!    !---------------------------------------
!    
!!    !获取年月日下的日积雪数据记录个数
!!    F= GetDayRL(Year,Month,Day,FactRL)
!!    write(*,*)FactRL
!!    !-----------------------------------
!    
!!    !读取某日积雪数据
!!    FactRL=778
!!    allocate(DayData(FactRL))
!!    call ReadDaySnowData(Year,Month,Day,DayData,FactRL)
!!    write(*,*)DayData(1:10)
!!    deallocate(DayData)
!!    --------------------------------------------------
!
!    !产生文件名
!!    call CreateDayFileName(FileName,Year,Month,Day)
!   ! call CreateMonthFileName(FileName,Year,Month)
!!   -------------------------------------------------------
!   
!!   !下面程序生成日积雪测试数据，从1951年1月到2010年12月共18000多个文件
!!    call date_and_time(values=val1)
!!    write(*,*)"该程序需要生成大量文件，可能会耗费较长时间"
!!    write(*,*)"程序运行中................"
!!  do i=1951,2010
!!    do j=1,12
!!        if(j==1 .OR. j==3 .OR. j==5 .OR. j==7 .OR.j==8 .OR. j==10 .OR. j==12)then
!!            do k=1,31
!!                call CreateDayData(i,j,k) 
!!            enddo
!!        endif
!!        if(j==4 .OR. j==6 .OR. j==9 .OR. j==11 )then
!!            do k=1,30
!!                call CreateDayData(i,j,k) 
!!            enddo
!!        endif
!!        if(j==2 .and. IsLeapYear(i))then
!!            do k=1,29
!!                call CreateDayData(i,j,k) 
!!            enddo
!!        endif
!!        if(j==2 .and. .not. IsLeapYear(i))then
!!            do k=1,28
!!                call CreateDayData(i,j,k) 
!!            enddo
!!        endif
!!    enddo
!!  enddo
!!    call date_and_time(values=val2)    
!!    write(*,"('消耗',I4,'分',I4,'秒',I4,'毫秒')") val2(6)-val1(6),val2(7)-val1(7),val2(8)-val1(8)
!!    !生成测试数据完毕---------------------------------------------------------------------------
!end program main