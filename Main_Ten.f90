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
!!    !����̨վѮ����ѩ�����ʷ�����ļ���----------------------
!!    call CreateTenDepthSortFileName(FileName,StaID,Month,Ten)
!!    write(*,*)FileName
!!    !---------------------------------------------------------
!
!!    do i=1,3
!!        call TenMaxDepth(1951,1,i)
!!    end do
! !   call TenMaxDepth(1951,1,1)
!    
!!    !���Ѯ����ѩ��ȼ���--------------------------------
!!    !call TenMaxDepth(Year,Month,Ten)
!!    !��ȱ�������������ʱ�����⣬��������û���⣬�Ƿ��������������ϣ�
!!    call date_and_time(values=val1)
!!    do i=1951,2010
!!        do j=1,12
!!           do k=1,3
!!                call TenMaxDepth(i,j,k)
!!           enddo
!!        enddo
!!    enddo
!!    call date_and_time(values=val2)    
!!    write(*,"('����',I4,'��',I4,'��',I4,'����')") val2(6)-val1(6),val2(7)-val1(7),val2(8)-val1(8)
!!      !--------------------------------------------------------------
!    
!!    !��������Ѯ�ļ���-----------------------------------
!!    call CreateTenFileName(TenFileName,Year,Month,Ten)
!!    write(*,*)tenfilename
!!    !--------------------------------------------------
!!
!!    !���Ѯ����������ֹ��------------------------
!!    if(GetTenDayNum(TenNum,Year,Month,Ten)) then
!!         write(*,*)TenNum   
!!    endif
!!   !--------------------------------------------
!end program Main_Ten