!program Main_Ten
!    use JL_ModStaSnow_Ten
!    implicit none
!    character*(FileNameMaxLen):: TenFileName
!    character*(FileNameMaxLen):: FileName
!    integer::Year,Month,Ten
!    integer::TenNum(3)
!    integer::i,j,k
!    integer:: val1(8),val2(8)
!    integer:: StaID
!    integer:: StartYear,EndYear
!    
!    Year=2007
!    
!    StartYear=1951
!    EndYear=2010
!    Month=3
!    Ten=1
!    StaID = 50425 
!    
!    call TenSnowDepthSort(StaID,Month,Ten,StartYear,EndYear)
!    
!!    !测试台站旬最大积雪深度历史排序文件名----------------------
!!    call CreateTenDepthSortFileName(FileName,StaID,Month,Ten)
!!    write(*,*)FileName
!!    !---------------------------------------------------------
!
!!    do i=1,3
!!        call TenMaxDepth(1951,1,i)
!!    end do
! !   call TenMaxDepth(1951,1,1)
!    
!!    !测度旬最大积雪深度计算--------------------------------
!!    !call TenMaxDepth(Year,Month,Ten)
!!    !测度表明，批量计算时出问题，单个计算没问题，是否出在数组的问题上？
!!    call date_and_time(values=val1)
!!    do i=1951,2010
!!        do j=1,12
!!           do k=1,3
!!                call TenMaxDepth(i,j,k)
!!           enddo
!!        enddo
!!    enddo
!!    call date_and_time(values=val2)    
!!    write(*,"('消耗',I4,'分',I4,'秒',I4,'毫秒')") val2(6)-val1(6),val2(7)-val1(7),val2(8)-val1(8)
!!      !--------------------------------------------------------------
!    
!!    !测试生成旬文件名-----------------------------------
!!    call CreateTenFileName(TenFileName,Year,Month,Ten)
!!    write(*,*)tenfilename
!!    !--------------------------------------------------
!!
!!    !测度旬的天数及起止日------------------------
!!    if(GetTenDayNum(TenNum,Year,Month,Ten)) then
!!         write(*,*)TenNum   
!!    endif
!!   !--------------------------------------------
!end program Main_Ten