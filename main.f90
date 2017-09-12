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
!    !���������Ȼ�ѩ������
!!    allocate(DayData(778))
!!    call CreateMonData(2010,10)
!!
!!
!    !call CreateMonData(2010,12)
!    !����2010�»�ѩ����
!!    call date_and_time(values=val1)
!!    do j=1951,2010
!!        do i=1,12
!!            call CreateMonData(j,i)
!!        enddo
!!    end do
!!    call date_and_time(values=val2)    
!!    write(*,"('����',I4,'��',I4,'��',I4,'����')") val2(6)-val1(6),val2(7)-val1(7),val2(8)-val1(8)
! !   call CreateMonData(2010,6)
!    !----------------------------------------
!    
!!    !������ѩ�����е�����ѩ���ֵ
!!    data SnowDepth  /32766,89,1,32766,32744/
!!    F= FindMaxDepth(SnowDepth,5)
!!    write(*,*)F
!!    !��������ѩ��ȳ�����
!!     F=GetMaxDepthDate(Year,Month,Day)
!!     write(*,*) F
!    
!!    !�����ջ�ѩ��>=1cm,5cm,10cm,20cm,30cm ������
!!    call GetDaySnowNumber(Year,Month,Day,Numbers)
!!    write(*,*)numbers
!!    !-------------------------------------------
!    
!!    !��ȡ�ջ�ѩ������
!!    F=GetDayMaxDepth(Year,Month,Day)
!!    write(*,*)F
!!    !---------------------------------------------
!    
!!    !��ȡ��������������
!!    F=GetMonDays(Year,Month)
!!    write(*,*)F
!!    !---------------------------------------
!    
!!    !��ȡ�������µ��ջ�ѩ���ݼ�¼����
!!    F= GetDayRL(Year,Month,Day,FactRL)
!!    write(*,*)FactRL
!!    !-----------------------------------
!    
!!    !��ȡĳ�ջ�ѩ����
!!    FactRL=778
!!    allocate(DayData(FactRL))
!!    call ReadDaySnowData(Year,Month,Day,DayData,FactRL)
!!    write(*,*)DayData(1:10)
!!    deallocate(DayData)
!!    --------------------------------------------------
!
!    !�����ļ���
!!    call CreateDayFileName(FileName,Year,Month,Day)
!   ! call CreateMonthFileName(FileName,Year,Month)
!!   -------------------------------------------------------
!   
!!   !������������ջ�ѩ�������ݣ���1951��1�µ�2010��12�¹�18000����ļ�
!!    call date_and_time(values=val1)
!!    write(*,*)"�ó�����Ҫ���ɴ����ļ������ܻ�ķѽϳ�ʱ��"
!!    write(*,*)"����������................"
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
!!    write(*,"('����',I4,'��',I4,'��',I4,'����')") val2(6)-val1(6),val2(7)-val1(7),val2(8)-val1(8)
!!    !���ɲ����������---------------------------------------------------------------------------
!end program main