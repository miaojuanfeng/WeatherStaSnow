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
    
    
    write(*,*)"��������1951-2010������ѩ�������"   
    write(*,*)"���ݴ�����..........."
    do i=1951,2010  !�������
        call YearMaxDepth(i) !����������ѩ����ӳ���  
    enddo
   write(*,*)"1951-2010������ѩ������ݼ������"
    
!    call YeaSnowDepthSort(StaID,StartYear,EndYear)
!    !call MonSnowDepthSort(StaID,Month,StartYear,EndYear)
!    Year=2009
!    Season=3
!   ! call SeaSnowDepthSort(StaID,Season,StartYear,EndYear)
!     !3,4,5��
!!    do i=1951,2009
!!        do j=1,4
!!            call SeaMaxDepth(i,j)
!!        enddo
!!    enddo
!!    call SeaMaxDepth(2010,1)
!!    call SeaMaxDepth(2010,2)
!!    call SeaMaxDepth(2010,3)
    
    
end program Main_Mon