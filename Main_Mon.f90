program Main_Mon
    use JL_Mod_StaSnow_Month
    use JL_Mod_StaSnow_Season
    use JL_Mod_StaSnow_Year
    implicit none
    integer :: StaID,Month,StartYear,EndYear
    integer:: Year,Season
    integer i,j,k
    integer ::v1(8),v2(8)
    
    StartYear=1951
    EndYear=2010
    Month = 12
    StaID= 50739 
    
    
    write(*,*)"下面计算从1951-2010年最大积雪深度数据"   
    write(*,*)"数据处理中..........."
    do i=1951,2010  !处理过程
        call YearMaxDepth(i) !调用年最大积雪深度子程序  
    enddo
   write(*,*)"1951-2010年最大积雪深度数据计算完成"
    
!    call YeaSnowDepthSort(StaID,StartYear,EndYear)
!    !call MonSnowDepthSort(StaID,Month,StartYear,EndYear)
!    Year=2009
!    Season=3
!   ! call SeaSnowDepthSort(StaID,Season,StartYear,EndYear)
!     !3,4,5月
!!    do i=1951,2009
!!        do j=1,4
!!            call SeaMaxDepth(i,j)
!!        enddo
!!    enddo
!!    call SeaMaxDepth(2010,1)
!!    call SeaMaxDepth(2010,2)
!!    call SeaMaxDepth(2010,3)
    
    
end program Main_Mon