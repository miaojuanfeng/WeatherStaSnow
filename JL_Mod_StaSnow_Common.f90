module JL_Mod_StaSnow_Common
    implicit none
        integer,parameter:: FileNameMaxLen=80 !�ļ�����󳤶�
        integer,parameter:: StaNameMaxLen=20 !̨վ�ļ�����󳤶�
        integer,parameter:: MaxRecordLen=10000 !���ݼ�¼��󳤶�
        integer,parameter:: BaseVal=9950 !����ѩ������Ϊ����ʱ���ӵĻ���
        integer,parameter:: DefaultVal = 32766 !��ѩ��ȣ���ѩ������ȱ��ֵ
        integer,parameter:: MicroscaleVal=32744 !��ѩ��ȣ���ѩ������΢��ֵ
        
        !-------------�ļ���д����ֵ����----------------------------
        type FileReturnInfo
            integer:: index !ȡֵΪ0��-1��-2��-3��-4
            character*(50)::info !-1Ϊ�ļ��򿪳���-2Ϊ̨վ�ļ�Ϊ�գ�������,
                                    !0Ϊ��ȡ�ļ���¼������,-3Ϊ��ȡ�ļ���¼ʱ����-4Ϊ�ļ�������
        end type FileReturnInfo
        !------------------------------------------------------------
       
        !------------------------̨վ������Ϣ�ṹ--------------------
        type StaInfo
            integer:: StaID !��վ��
            character*(StaNameMaxLen)::StaName !��վ����
            real(8)::Longitude !��վγ��
            real(8)::Latitude !��վ����
            real(8)::StaHeight !��վ�߶�
         end type StaInfo
        !------------------------------------------------------------
       
       !----------�ջ�ѩ�������------------------------------------
        type DaySnow !�ջ�ѩ����
            integer:: StaID !��վ��
            character*(StaNameMaxLen)::StaName !��վ����
            real(8)::Longitude !��վγ��
            real(8)::Latitude !��վ����
            real(8)::StaHeight !��վ�߶�
            integer(4)::Year !��
            integer(4)::Month !��
            integer(2)::Day !��
            integer(4)::Depth !��ѩ��ȣ���ȱ��ֵ��΢��ֵ           
        end type DaySnow
       !------------------------------------------------------------  
       
       !------------�»�ѩ����--------------------------------------
        type MonthSnow !�»�ѩ����
            integer:: StaID !��վ��
            character*(StaNameMaxLen)::StaName !��վ����
            real(8)::Longitude !��վγ��
            real(8)::Latitude !��վ����
            real(8)::StaHeight !��վ�߶�
            integer(4)::Year !��
            integer(4)::Month !��
            integer(2)::SnowDays !��ѩ����
            integer(4)::MaxDepth !����ѩ���
            integer(2)::MaxDay !����ѩ��ȳ�����
            integer(2),dimension(5)::DepthDays  !��ѩ��ȳ��ֵ�����,1cm,5cm,10cm,20cm,30cm
        end type MonthSnow
       !------------------------------------------------------------
       
       !---------����ѩ����-----------------------------------------
        type SeaSnow
            integer:: StaID !��վ��
            character*(StaNameMaxLen)::StaName !��վ����
            real(8)::Longitude !��վγ��
            real(8)::Latitude !��վ����
            real(8)::StaHeight !��վ�߶�
            integer(4)::Year !��
            integer(4)::Season !��
            integer(2)::SnowDays !��ѩ����
            integer(4)::MaxDepth !����ѩ���
            character*(5)::MaxDay !����ѩ��ȳ�����(MMDD)
            integer(2),dimension(5)::DepthDays  !��ѩ��ȳ��ֵ�����,1cm,5cm,10cm,20cm,30cm
       end type SeaSnow
       !------------------------------------------------------------
       
       !---------���ѩ����-----------------------------------------
        type YeaSnow
            integer:: StaID !��վ��
            character*(StaNameMaxLen)::StaName !��վ����
            real(8)::Longitude !��վγ��
            real(8)::Latitude !��վ����
            real(8)::StaHeight !��վ�߶�
            integer(4)::Year !��
            !integer(4)::Season !��
            integer(2)::SnowDays !��ѩ����
            integer(4)::MaxDepth !����ѩ���
            character*(5)::MaxDay !����ѩ��ȳ�����(MMDD)
            integer(2),dimension(5)::DepthDays  !��ѩ��ȳ��ֵ�����,1cm,5cm,10cm,20cm,30cm
       end type YeaSnow
       
       
       !---------------����ѩ���/�ջ�ѩ��� ��ʷ��������---------------------
       type MaxDepthHisorder
            integer :: StaID 
            character*(StaNameMaxLen) :: StaName
            integer :: MaxDepth !
            integer :: Year
       end type MaxDepthHisorder       
       !------------------------------------------------------------
        
    contains
    !******************����Ϊ���ܺ���*******************************
        
        !------------��������ջ�ѩ���ݹ�������----------------------
        subroutine CreateDayData(Year,Month,Day)
        !˵������̨վ��Ϣ�ļ��ж�����ʵ̨վ��Ϣ��Ϊÿ��̨վ����һ�����ݣ�
        !��д���ļ�.��̨վ��ѩ���������ɡ�
        !Ϊ�õ���ʵ���ݣ������ɹ����в������¹���
        !(1)���̨վγ�ȳ���40��ʱ��3-4,9��ֵΪСѩ(3cm����),5��Ϊ΢��ֵ
        !    6-8��Ϊȱ��ֵ,10-11��2Ϊ��ѩ��3-5cm����12-1��Ϊ��ѩ(����5cm������Ϊ50cm)
        !(2)���γ��Ϊ30-40�ȼ�ʱ��12-1��Ϊ����ѩ(5-50cm),2,11Ϊ��ѩ(3-5cm),10ΪСѩ(1-3),
        !   9��3��Ϊ΢��,����Ϊȱ��ֵ
        !(3)���γ�ȵ���30��12-1Ϊ��ѩ(3-5cm)��11��1ΪСѩ(1-3cm)��10��2��Ϊ΢��,����Ϊȱ��        
        !------------------------------------------------------------
            integer::Year,Month,Day !�꣬�£���
            character*(FileNameMaxLen)::FileName
            type(FileReturnInfo)::FN !���ڵ�����̨վ��������
            character*(FileNameMaxLen)::StaFileName
            integer::FactRL !̨վ����
            type(StaInfo),allocatable::Sta(:) !̨վ��Ϣ��̬����
            type(DaySnow),allocatable::DSnow(:) !�ջ�ѩ��̬����
            integer::i !ѭ��������
            integer::stat
            
            !��1��׼����������ȡ̨����������̨վ���ݣ�׼���ô洢����ļ�������
            StaFileName="./Data/ResourceData/Stainfo.txt"
            FN=GetStaNumber(StaFileName,FactRL) !��ȡ̨վ����
            allocate(Sta(FactRL))
            call ReadStaInfoData(StaFileName,Sta,FactRL) !��̨վ����
            call CreateDayFileName(FileName,Year,Month,Day) !�����ջ�ѩ�����ļ���
            open(10,file=filename,form="formatted",iostat=stat) !�����ջ�ѩ�ļ�
            if(stat/=0) then
                write(*,*) "�����ջ�ѩ�ļ�����"
                return
            endif
            
            !(2)�����꣬�£��մ�����ѩ�������
            allocate(DSnow(FactRL))
            call random_seed() 
            do i=1,FactRL
                DSnow(i).StaID=Sta(i).StaID
                DSnow(i).StaName=Sta(i).StaName
                DSnow(i).Latitude=Sta(i).Latitude
                DSnow(i).Longitude=Sta(i).Longitude
                DSnow(i).StaHeight=Sta(i).StaHeight
                DSnow(i).Year=Year
                DSnow(i).Month=Month
                DSnow(i).Day=Day
                
                !�������ʵ���������ģ���ѩ�����ֵ
                !1)��������,���̨վγ�ȳ���40��ʱ��3-4,9��ֵΪСѩ(5cm����),5��Ϊ΢��ֵ
                !    6-8��Ϊȱ��ֵ,10-11��2Ϊ��ѩ��5-20cm����12-1��Ϊ��ѩ(����20cm������Ϊ100cm)
                if (DSnow(i).longitude>40.0) then
                    if(DSnow(i).Month == 9 .OR. DSnow(i).Month == 3 .OR. DSnow(i).Month == 4 ) then !Сѩ
                        DSnow(i).Depth = rand(0,5)
                        if(DSnow(i).Depth==0) DSnow(i).Depth=MicroscaleVal
                    endif
                    if(DSnow(i).Month == 10 .OR. DSnow(i).Month == 11 .OR. DSnow(i).Month == 2 )  then !��ѩ
                        DSnow(i).Depth = rand(5,20)
                    endif
                    if(DSnow(i).Month == 12 .OR. DSnow(i).Month == 1 )  then !��ѩ
                        DSnow(i).Depth = rand(20,100)
                    endif
                    if(DSnow(i).Month == 5 )  then !΢ѩ
                        DSnow(i).Depth = MicroscaleVal
                    endif              
                    if(DSnow(i).Month >=6 .and. DSnow(i).Month <=8 )  then !��ѩ
                        DSnow(i).Depth = DefaultVal
                    endif
                endif
                !2)�в�����,���γ��Ϊ30-40�ȼ�ʱ��12-1��Ϊ��ѩ(20-100cm),2,11Ϊ��ѩ(5-20cm),10ΪСѩ(0-5),
                !   9��3��Ϊ΢��,����Ϊȱ��ֵ
                if(DSnow(i).longitude>30.0 .and. DSnow(i).longitude<=40.0) then 
                    if(DSnow(i).Month == 10 ) then !Сѩ
                        DSnow(i).Depth = rand(0,5)
                        if(DSnow(i).Depth==0) DSnow(i).Depth=MicroscaleVal
                    endif
                    if(DSnow(i).Month == 2 .OR. DSnow(i).Month == 11 )  then !��ѩ
                        DSnow(i).Depth = rand(5,20)
                    endif
                    if(DSnow(i).Month == 12 .OR. DSnow(i).Month == 1 )  then !��ѩ
                        DSnow(i).Depth = rand(20,100)
                    endif
                    if(DSnow(i).Month == 3 .OR. DSnow(i).Month == 9)  then !΢ѩ
                        DSnow(i).Depth = MicroscaleVal
                    endif              
                    if(DSnow(i).Month >=4 .and. DSnow(i).Month <=8 )  then !��ѩ
                        DSnow(i).Depth = DefaultVal
                    endif
                endif
                !3)�Ϸ�����,���γ�ȵ���30��12-1Ϊ��ѩ(3-5cm)��11��2ΪСѩ(0-5cm)��10��3��Ϊ΢��,����Ϊȱ��  
                if (DSnow(i).longitude<=30.0) then 
                    if(DSnow(i).Month == 11 .OR. DSnow(i).Month == 2  ) then !Сѩ
                        DSnow(i).Depth = rand(0,5)
                        if(DSnow(i).Depth==0) DSnow(i).Depth=MicroscaleVal
                    endif
                    if(DSnow(i).Month == 12 .OR. DSnow(i).Month == 1 )  then !��ѩ
                        DSnow(i).Depth = rand(5,20)
                    endif
                    if(DSnow(i).Month == 3 .OR. DSnow(i).Month == 10)  then !΢ѩ
                        DSnow(i).Depth = MicroscaleVal
                    endif              
                    if(DSnow(i).Month >=4 .and. DSnow(i).Month <=9 )  then !��ѩ
                        DSnow(i).Depth = DefaultVal
                    endif
                endif
                
                !(3)���ջ�ѩ��������д���ļ�   
                write(10,*)DSnow(i)
            end do
            deallocate(DSnow) !�ͷ�����
            deallocate(Sta)
            close(10)
            return
        end subroutine CreateDayData
        !------------------------------------------------------------
        
        !----------���ջ�ѩ����---------------------------------------
        subroutine ReadDaySnowData(Year,Month,Day,DayData,FactRL)
        !˵������������������գ������յ��ջ�ѩ���ݣ���DayData���鷵��,
        !�ڵ��øú���ǰ��Ҫ�Ȼ�ȡ�������ݵļ�¼��FactRL
            implicit none
            integer::Year,Month,Day !������
            integer::FactRL !�ļ��е�ʵ�ʼ�¼����
            type(DaySnow):: DayData(FactRL)
            character*(FileNameMaxLen)::FileName
            integer::stat
            logical::alive
            integer::i !ѭ��������
            !���������������ļ���
            call CreateDayFileName(FileName,Year,Month,Day)
            !���ļ������ݣ������ݽ�������DayData������
            inquire(file=FileName,exist=alive)
            if(.not. alive) then
                write(*,"(A80,'�ջ�ѩ�ļ������ڣ������˳�')")filename
                return
            endif
            open(10,file=filename,form="formatted",iostat=stat)
            if(stat/=0) then
                write(*,"(A80,'�ջ�ѩ�ļ���ʱ���������˳�')")filename
            endif
            do i=1,FactRL
                read(10,*)DayData(i)
            enddo
            
            close(10)
            return
        end subroutine ReadDaySnowData
        !-------------------------------------------------------------
        
        !--------�����»�ѩ����-----------------------------------------
        !˵�������ջ�ѩ����ͳ�Ʒ����õ��»�ѩ����
        subroutine CreateMonData(Year,Month)
            implicit none
            integer::Year,Month !���������
            character*(FileNameMaxLen)::FileName !�ջ�ѩ�ļ���
            type(MonthSnow),allocatable:: MonData(:) !��̬���飬�洢�»�ѩ���ݣ�����ά����¼������
            type(StaInfo),allocatable:: StationInfo(:)
            type(DaySnow),allocatable:: SnowData(:,:) !�����̨վ���ջ�ѩ����
            integer,allocatable::SnowDayNums(:) !һ�����и�̨վ�Ļ�ѩ����
            integer,allocatable::MaxDepth(:) !һ�����е�����̨վ������ѩ���ֵ
            integer,allocatable:: MaxDepthDay(:) !һ�����и�̨վ������ѩ�����
            integer,allocatable :: MaxDepthNum(:,:) !������ѩ��ȳ��ֵ�����            
            type(FileReturnInfo):: FN !�ļ�����ֵ��Ϣ
            character*(FileNameMaxLen):: StaFileName !̨վ��Ϣ�ļ���
            character*(FileNameMaxLen):: DayFileName !�ջ�ѩ�ļ���
            integer:: Days !һ���µ�����
            integer:: FactRL !�ջ�ѩ��¼����
            integer::stat
            integer::i,j !ѭ��������
            logical::alive
            
            
            !������̣���1��׼�����������°�������������ȡ�ջ�ѩ�ļ��Ĵ�С,���ջ�ѩ����װ���»�ѩ��̬�����С�
            !           (2)���㹤��������������ѩ��ȣ���������ѩ��ȳ�����,�������ֵ�µĳ�������
            
            !׼��������������ø��µ���������̬��������鲢��ʼ��
            Days=GetMonDays(Year,Month) !�������»�ø��µ�����
            StaFileName="./Data/ResourceData/StaInfo.txt"
            FN = GetStaNumber(StaFileName,FactRL)
            if(FN.index/=0) then 
                write(*,*)"��ȡ̨վ�������������˳�"
                return
            endif
            
            allocate(StationInfo(FactRL)) !̨վ��Ϣ����
            allocate(MonData(FactRL)) !�»�ѩ����
            allocate(SnowData(Days,FactRL))
            allocate(MaxDepth(FactRL)) !һ�����и�̨վ������ѩֵ
            allocate(MaxDepthDay(FactRL))!һ�����и�̨վ������ѩ������
            allocate(MaxDepthNum(FactRL,5)) !����̨վ�ĸ�����ϳ������� 
            allocate(SnowDayNums(FactRL)) !һ�����и�̨վ�Ļ�ѩ����
            
            call ReadStaInfoData(StaFileName,StationInfo,FactRL) !��ȡ̨վ��Ϣ
            !��һ������������ջ�ѩ����װ�ؽ�SnowData(Days,FactRL)����
            do i=1,Days !������ջ�ѩ����
!                !�ȵõ��ջ�ѩ�����ļ���
!                call CreateDayFileName(FileName,Year,Month,i)
                !��ȡ�ջ�ѩ����
                call ReadDaySnowData(Year,Month,i,SnowData(i,:),FactRL)
            enddo
            
            !�����̨վ�Ļ�ѩ����
            call GetSnowDayNubmers_common(SnowData,SnowDayNums,Days,FactRL)
           !  GetSnowDayNubmers_common(SnowData,SnowDayNums,DayNum,StaNum)
           
            !��������ѩ���,MaxDepth(FactRL)����װ���Ǹ���̨վ������ѩ���
            call MaxSnowDepth_common(SnowData,MaxDepth,Days,FactRL)
          
            !��������ѩ��ȳ�����,��MaxDepthDay(FactRL)����
            call MaxSnowDepthDay_common(SnowData,MaxDepthDay,Days,FactRL)
          
            !��������ϵĳ�������,��MaxDepthNum����
            call GetSnowDepthNumbers_common(SnowData,MaxDepthNum,Days,FactRl)
            
            !�����»�ѩ�����ļ������������ļ�
            call CreateMonthFileName(FileName,Year,Month)
            inquire(file=FileName,exist=alive)
            if(alive) then
                write(*,"(I80,'���ļ��Ѵ��ڣ�����Ҫ������')") FileName
                return
            endif
            open(10,File= FileName, form='formatted',iostat=stat)
            if(stat/=0) then 
                write(*,"(I80,'�ļ��������裬�����˳�')") FileName
            endif
            !����������װ�ؽ����������飬����ÿ����¼д���ļ�
            write(10,*)"��վ�� ��վ��  γ�� ���� �߶� �� �� ��ѩ���� ������ ������ >1 >5 >10 >20 >30"
            do i=1,FactRL
                MonData(i).StaID=StationInfo(i).StaID
                MonData(i).StaName=StationInfo(i).StaName
                MonData(i).Longitude=StationInfo(i).Longitude
                MonData(i).Latitude=StationInfo(i).Latitude
                MonData(i).StaHeight=StationInfo(i).StaHeight
                MonData(i).Year = Year
                MonData(i).Month = Month
                MonData(i).SnowDays = SnowDayNums(i) !��ѩ����
                MonData(i).MaxDepth = MaxDepth(i) !����ѩֵ
                MonData(i).MaxDay = MaxDepthDay(i) !����ѩ������
                MonData(i).DepthDays(1:5) =  MaxDepthNum(i,1:5)
                !write(10,*,iostat=stat) MonData(i)
                
                write(10,100,iostat=stat) MonData(i).StaID,MonData(i).StaName,MonData(i).Longitude,MonData(i).Latitude,&
                                        MonData(i).StaHeight,MonData(i).Year,MonData(i).Month,MonData(i).SnowDays,&
                                        MonData(i).MaxDepth,MonData(i).MaxDay,MonData(i).DepthDays(1:5)
                100 format(I8,' ',A20,F8.2,F8.2,F8.2,I6,I4,I6,I6,I6,I6,I6,I6,I6,I6)
                if(stat/=0) then
                    write(*,*) "������д���ļ�ʱ���������˳�"
                    close(10)
                    return
                endif
            enddo
            
            deallocate(StationInfo) !̨վ��Ϣ����
            deallocate(MonData) !�»�ѩ����
            deallocate(SnowData)
            deallocate(MaxDepth) !һ�����и�̨վ������ѩֵ
            deallocate(MaxDepthDay)!һ�����и�̨վ������ѩ������
            deallocate(MaxDepthNum) !����̨վ�ĸ�����ϳ������� 
            deallocate(SnowDayNums) !һ�����и�̨վ�Ļ�ѩ����
            close(10)
            return
        end subroutine CreateMonData
        !---------------------------------------------------------------
        
        !----------���»�ѩ����-----------------------------------------
        subroutine ReadMonSnowData(Year,Month,MonData,FactRL)
            integer::Year,Month,FactRL !�꣬�£��»�ѩ��¼����
            type(MonthSnow) :: MonData(FactRL) !�»�ѩ�������� 
            character*(FileNameMaxLen)::FileName
            integer::stat
            logical::alive
            integer::i !ѭ��������
            
            !�������������ļ���
            call CreateMonthFileName(FileName,Year,Month)
            !���ļ������ݣ������ݽ�������DayData������
            inquire(file=FileName,exist=alive)
            if(.not. alive) then
                write(*,"(A80,'�»�ѩ�ļ������ڣ������˳�')")filename
                return
            endif
            open(10,file=filename,form="formatted",iostat=stat)
            if(stat/=0) then
                write(*,"(A80,'�ջ�ѩ�ļ���ʱ���������˳�')")filename
            endif
            read(10,*)
            do i=1,FactRL
                read(10,100,iostat=stat) MonData(i).StaID,MonData(i).StaName,MonData(i).Longitude,MonData(i).Latitude,&
                                        MonData(i).StaHeight,MonData(i).Year,MonData(i).Month,MonData(i).SnowDays,&
                                        MonData(i).MaxDepth,MonData(i).MaxDay,MonData(i).DepthDays(1:5)
                100 format(I8,' ',A20,F8.2,F8.2,F8.2,I6,I4,I6,I6,I6,I6,I6,I6,I6,I6)
            enddo
            
            close(10)
            return
        
        end subroutine ReadMonSnowData
        !---------------------------------------------------------------
        
        !-------------ͨ�û�ѩ����(����Ϊ�ջ�ѩ����)--------------------------------------
        !˵����������ڸ���̨վ�Ļ�ѩ������ͨ��SnowdayNums����
        subroutine GetSnowDayNubmers_common(SnowData,SnowDayNums,DayNum,StaNum)
            implicit none
            integer :: DayNum !����
            integer :: StaNum !̨վ��
            type(DaySnow):: SnowData(DayNum,StaNum) !DayNum����ջ�ѩ���ݣ�ÿ�յļ�¼����ΪStaNum
            integer :: SnowDayNums(StaNum)
            integer :: i,j
            
            SnowDayNums=0 !��ʼ������ȫΪ0
            
            do i=1,StaNum !��̨վ
                do j=1,DayNum !����
                    if(SnowData(j,i).Depth /= DefaultVal .and. SnowData(j,i).depth /= MicroscaleVal) then
                        SnowDayNums(i) = SnowDayNums(i) + 1
                    endif
                enddo
            enddo
            return
        end subroutine GetSnowDayNubmers_common
        !---------------------------------------------------------------
        
        !----------ͨ������ѩ���(����Ϊ�ջ�ѩ����)-------------------
        !˵���������ջ�ѩ���ݣ������ÿ��̨վ��ĳ��ʱ���ڵ�����ѩ���
        !SnowDataΪDayNum X StaNum �Ķ�ά���飬DayNum����������StaNum����̨վ��
        !MaxDepth��һά���飬ά��Ϊn�������鷵�ظ���̨վ������ѩ���
        subroutine MaxSnowDepth_common(SnowData,MaxDepth,DayNum,StaNum)
            integer :: DayNum !����
            integer :: StaNum !̨վ��
            type(DaySnow):: SnowData(DayNum,StaNum) !DayNum����ջ�ѩ���ݣ�ÿ�յļ�¼����ΪStaNum
            integer:: MaxDepth(StaNum) !����ѩ��ȣ������齫����
            integer:: DepthData(DayNum) !ĳ��̨��DayNum�����ڵĻ�ѩ�������
            integer:: i,j !ѭ������
           ! write(*,*)"������ѩ���ʱװ�ص����ֵ"
            do i=1,StaNum !ÿ��̨վ���м���
                do j=1,DayNum !ÿ��̨վ���������ڵĻ�ѩ���װ��DepthData����
                    DepthData(j)= SnowData(j,i).Depth                               
                enddo
                !if(i<=10) write(*,*)DepthData   
                MaxDepth(i)=FindMaxDepth(DepthData,DayNum) !����ÿ��̨վ���������ڵ�����ѩ���
            enddo           
            
            return
        end subroutine 
        !---------------------------------------------------------------
        
        !--------���Ҷ����ѩ����е�����ѩ���ֵ(����Ϊ�ջ�ѩ����)------------------
        !SnowDepth(n)������Ϊ�����ѩ������ݣ��ҳ����ֵ����������Ϊ������ѩ���ֵ
        integer function FindMaxDepth(SnowDepth,n)
            integer :: n !�����Сά��
            integer :: SnowDepth(n) !��ѩ��������
            integer :: Temp(n) !��ʱ����洢������Ļ�ѩ���ݣ������ǽ�ȱ��ֵ��΢��ֵ��Ϊ0���Է��������ֵ
            integer :: i !ѭ������          
            integer :: Max  !�����ֵ           
            integer :: cnt1,cnt2 !��¼ȱ��(cnt1)�ͼ�¼΢��ֵ(cnt2)���ֵĴ���
            cnt1=0
            cnt2=0
            
            !�����������Ԥ������ȱ��ֵ��΢��ֵȫ��Ϊ0�����������ֵ��
            Max=0
            do i=1,n
                if(SnowDepth(i)==DefaultVal) then
                    cnt1=cnt1+1
                    temp(i)=0
                endif
                if(SnowDepth(i)==MicroscaleVal) then
                    cnt2=cnt2+1
                    temp(i)=0
                endif
                if(SnowDepth(i)/=DefaultVal .and. SnowDepth(i)/=MicroscaleVal) then
                    temp(i)=SnowDepth(i)
                endif
                if(temp(i)>max) max=temp(i)!�����ֵ
            enddo
            
            if(cnt1+cnt2==n) then !ȫΪȱ���΢��ֵ
                if(cnt2>0) then !��΢��ֵ
                    FindMaxDepth=MicroscaleVal
                    return
                else !ȫΪȱ��ֵ
                    FindMaxDepth=DefaultVal
                    return
                endif
            endif
            
            FindMaxDepth = max
            return
        end function FindMaxDepth
        !---------------------------------------------------------------
        
        !---------ͨ��������ѩ��ȳ�����(����Ϊ�ջ�ѩ����)-----------------------
        !˵���������ջ�ѩ����SnowData��DayNum,StaNum����DayNum��ʾ����,StaNum��ʾ̨վ��
        !��������ֵΪDayNum�еĵڼ��죬����BaseVal + xx��BaseVal=9950,xx��ʾ��������
        !ѩ����ռ���,������ѩֵΪȱ��ֵ���򷵻�ֵҲΪȱ�⣬��Ϊ΢�����򷵻�Ϊ΢��ֵ
        subroutine MaxSnowDepthDay_common(SnowData,MaxDepthDay,DayNum,StaNum)
            implicit none
            integer :: DayNum !����
            integer :: StaNum !̨վ��
            type(DaySnow):: SnowData(DayNum,StaNum) !DayNum����ջ�ѩ���ݣ�ÿ�յļ�¼����ΪStaNum
            integer:: MaxDepthDay(StaNum) !����ѩ���
            integer:: i,j !ѭ������
            integer:: n !��������ѩ��ȵĴ���
            integer:: MaxDepth(StaNum) !��̨վ����ѩ���
            integer:: d !��¼���ڣ������µĵڼ���
            
            MaxDepthDay=MicroscaleVal
            d=0
            call MaxSnowDepth_common(SnowData,MaxDepth,DayNum,StaNum) !��ø�̨վ������ѩ���,���ݴ����MaxDepth������
           
            do i=1,StaNum !����ÿ��վ��������ȳ�����
                
                n=0
                do j=1,DayNum !����
                    if(MaxDepth(i)==Defaultval) then !������ѩ���Ϊȱ��ֵ��������Ϊȱ��ֵ
                        MaxDepthDay(i)=DefaultVal
                        exit
                    endif
                    if(MaxDepth(i)==MicroscaleVal) then !������ѩ���Ϊ΢��ֵ��������Ϊ΢��ֵ
                        MaxDepthDay(i)=MicroscaleVal
                        exit
                    endif
                    if(SnowData(j,i).Depth == MaxDepth(i)) then
                        n=n+1
                        if(n==1) d=j
                    endif
                enddo
                if(n==1) MaxDepthDay(i) = d !���ֻ��һ�Σ���ȡDayNum�еĵڼ���
                if(n>1) MaxDepthDay(i) = BaseVal + n !�����ȳ������ж�Σ���ȡ 9950  + n                
            enddo         
            return
        end subroutine MaxSnowDepthDay_common
        !---------------------------------------------------------------
        
        !-------------ͨ�÷�����������ȳ�������(����Ϊ�ջ�ѩ����)----------------------
        !˵�����Ӷ���ջ�ѩ�����л�ȡÿ��̨վ>=1cm,5cm,10cm,20cm,30cm������
        !SnowData(DayNum,StaNum)��Ŷ��ն�̨վ��ѩ���ݣ�MaxDepthNum(StaNum)��Ÿ�����εĳ�������
        !DayNumΪ������StaNumΪ̨վ��
        subroutine GetSnowDepthNumbers_common(SnowData,MaxDepthNum,DayNum,StaNum)
            implicit none
            integer :: DayNum !����
            integer :: StaNum !̨վ��
            type(DaySnow):: SnowData(DayNum,StaNum) !DayNum����ջ�ѩ���ݣ�ÿ�յļ�¼����ΪStaNum
            integer :: MaxDepthNum(StaNum,5) !������ѩ��ȳ��ֵ�����
            integer i,j !ѭ������
            logical :: IsDefault1=.true. !��ʼ��Ϊ��ȱ�⣬һ���з�ȱ��ֵ���֣����Ϊfalse
            logical :: IsDefault5=.true. !��ʼ��Ϊ��ȱ�⣬һ���з�ȱ��ֵ���֣����Ϊfalse
            logical :: IsDefault10=.true. !��ʼ��Ϊ��ȱ�⣬һ���з�ȱ��ֵ���֣����Ϊfalse
            logical :: IsDefault20=.true. !��ʼ��Ϊ��ȱ�⣬һ���з�ȱ��ֵ���֣����Ϊfalse
            logical :: IsDefault30=.true. !��ʼ��Ϊ��ȱ�⣬һ���з�ȱ��ֵ���֣����Ϊfalse
             
            MaxDepthNum(1:StaNum,1:5)=0
            do i=1,StaNum !����̨վ
                do j=1,DayNum !����
                    if(SnowData(j,i).Depth >=1 .and. SnowData(j,i).Depth /=DefaultVal .and. SnowData(j,i).Depth /= MicroscaleVal ) then
                        IsDefault1=.false.
                        MaxDepthNum(i,1)=MaxDepthNum(i,1)+1
                    endif
                    if(SnowData(j,i).Depth >=5 .and. SnowData(j,i).Depth /=DefaultVal .and. SnowData(j,i).Depth /= MicroscaleVal ) then
                        IsDefault5=.false.
                        MaxDepthNum(i,2)=MaxDepthNum(i,2)+1
                    endif
                    if(SnowData(j,i).Depth >=10 .and. SnowData(j,i).Depth /=DefaultVal .and. SnowData(j,i).Depth /= MicroscaleVal ) then
                        IsDefault10=.false.
                        MaxDepthNum(i,3)=MaxDepthNum(i,3)+1
                    endif
                    if(SnowData(j,i).Depth >=20 .and. SnowData(j,i).Depth /=DefaultVal .and. SnowData(j,i).Depth /= MicroscaleVal ) then
                        IsDefault20=.false.
                        MaxDepthNum(i,4)=MaxDepthNum(i,4)+1
                    endif
                    if(SnowData(j,i).Depth >=30 .and. SnowData(j,i).Depth /=DefaultVal .and. SnowData(j,i).Depth /= MicroscaleVal ) then
                        IsDefault30=.false.
                        MaxDepthNum(i,5)=MaxDepthNum(i,5)+1
                    endif
                enddo
                if(IsDefault1) MaxDepthNum(i,1) = DefaultVal !���ȫΪ���������ȫΪȱ��ֵ��������������ҲΪȱ��ֵ����ͬ
                if(IsDefault5) MaxDepthNum(i,2) = DefaultVal
                if(IsDefault10) MaxDepthNum(i,3) = DefaultVal
                if(IsDefault20) MaxDepthNum(i,4) = DefaultVal
                if(IsDefault30) MaxDepthNum(i,5) = DefaultVal                
            enddo
            return
            
        end subroutine GetSnowDepthNumbers_common              
        !---------------------------------------------------------------
        
        !----------------ͨ������ѩ���/�ջ�ѩ�������----------------
        !˵������Ҫ����ȱ��ֵ��΢��ֵ���Ƚ��о���ֵ>΢��ֵ>ȱ��ֵ.Ϊ���
        !�����⣬�����Ƚ�΢��ֵ��0��ȱ��ֵ��-1��ʾ��Ȼ���������������
        !�Ժ��ٽ�0,-1ת����ȱ��ֵ��΢��ֵ
        subroutine MaxDepthSort(MaxDepth,n) !����ѡ������
            implicit none
            integer :: n
            type(MaxDepthHisorder) :: MaxDepth(n) !����ѩ���������������
            type(MaxDepthHisorder) :: temp
            integer :: i,j,k
            integer :: min
            
            !���潫ȱ��ֵ��-1,΢��ֵ��0��ʾ�������ݽ���Ԥ����
            do i=1,n
                if(MaxDepth(i).MaxDepth==DefaultVal) MaxDepth(i).MaxDepth=-1
                if(MaxDepth(i).MaxDepth==MicroscaleVal) MaxDepth(i).MaxDepth=0
            enddo
            
            !����Ϊѡ������
            do i=1,n-1
                k=i
                do j=i+1,n
                    if(MaxDepth(j).MaxDepth>MaxDepth(k).MaxDepth) k=j              
                enddo
                if(k/=i) then
                    temp = MaxDepth(i)
                    MaxDepth(i)=MaxDepth(k)
                    MaxDepth(k)=temp
                endif                
            enddo
            
            !���潫�����е�-1��0��ȱ��ֵ��΢��ת������
            do i=1,n
                if(MaxDepth(i).MaxDepth==-1) MaxDepth(i).MaxDepth=DefaultVal
                if(MaxDepth(i).MaxDepth==0) MaxDepth(i).MaxDepth=MicroscaleVal
            enddo
            
        end subroutine MaxDepthSort
!        !-------������ʱ�ε��ջ�ѩ����----------------------------------
!        subroutine DuringSnowData(SnowData,Year1,Month1,Day1,Year2,Month2,Day2)
!        
!        end subroutine DuringSnowData
!        !---------------------------------------------------------------
        
        
!        !---------�����ջ�ѩ�����е�����ѩ���-----------------------
!        ! !!!!����Ĵ����д��󣡣�����������������������������
!        integer function GetDayMaxDepth(Year,Month,Day)
!        !˵������������̨վ�е�����ѩ��ȣ��������̨վ��ѩ��Ⱦ�Ϊȱ
!        !      ���΢��ֵʱ���ȿ���΢��ֵ����ֻ��ȱ��ֵ����Ϊȱ��
!            implicit none
!            integer::Year,Month,Day !������
!            character*(FileNameMaxLen)::FileName !�ջ�ѩ�ļ���
!            integer::FactRL !�ջ�ѩ��¼����
!            type(DaySnow),allocatable::DayData(:)
!            integer :: i
!            logical :: t1=.false. !��ΪY�����л�ѩ
!            logical :: t2=.false. !��ΪY����Ϊ΢��
!            !logical :: t3=.false. !��ΪY����Ϊȱ��
!            integer :: max = DefaultVal !Ĭ��Ϊȱ��ֵ
!            
!            
!            !������̣��ȶ�ȡ�ջ�ѩ���ݣ��ٵ���ѩ�����в�������ѩ���
!            !(1)��ȡ�ջ�ѩ���ݣ��������ջ�ѩ�ļ������ٻ�ȡ�ļ���¼����
!            !   �ٶ�̬�������飬����ȡ�ջ�ѩ����
!            
!            call CreateDayFileName(FileName,Year,Month,Day) !�����ջ�ѩ�ļ���filename
!            if(GetDayRL(Year,Month,Day,FactRL)/=0) then !��ȡ�ջ�ѩ��¼����FactRL
!                write(*,*)"��ȡ�ջ�ѩ��¼����ʱ����,�����˳�"
!                return
!            endif
!            allocate(DayData(FactRL)) !��̬��������
!            call ReadDaySnowData(Year,Month,Day,DayData,FactRL) !���ջ�ѩ����DayData
!            
!            !(2)��������ѩ���
!            !!!!����Ĵ����д��󣡣�����������������������������
!            do i=1,FactRL
!                if(DayData(i).Depth < max .and. t1==.false. .and. (max==DefaultVal .OR. max==MicroscaleVal)) then !δ���ֻ�ѩ������΢��
!                    max=DayData(i).Depth !ȡ΢��ֵ
!                    t2 = .true. !������΢��
!                endif
!                if((DayData(i).Depth < max) .and. t2== .true. .and. t1== .false.) then !��΢������һ�η����˻�ѩ
!                    max=DayData(i).Depth
!                    t1= .true. !�����˻�ѩ
!                endif
!                if(DayData(i).Depth > max .and. DayData(i).Depth < MicroscaleVal .and. t1== .true.) then !�л�ѩ�������ֵ
!                    max=DayData(i).Depth
!                endif
!            enddo
!            
!            deallocate(DayData)
!            GetDayMaxDepth=max
!            return
!        end function GetDayMaxDepth
!        !--------------------------------------------------------------
!        
!        !------------����Ѯ����ѩ���-------------------------------------
!        subroutine GetTenMaxSnowDepth(Year,Month,Ten)
!        
!        end subroutine GetTenMaxSnowDepth
!        !---------------------------------------------------------------
        
        !------------Ѯ����ѩ��ȳ�����-------------------------------
        
        !---------------------------------------------------------------
        
        !------------Ѯ����ѩ��ȳ�����-------------------------------
        
        !---------------------------------------------------------------
        
        
        
!        !----------�ջ�ѩ������������ȳ�������----------------------------------
!        !˵���������ջ�ѩ�����и���ѩֵ���ֵ�����,1cm,5cm,10cm,20cm,30cm
!        !����������ȱ�⣬���������ݾ�Ϊȱ�⣬������Ϊȱ��
!        subroutine GetDaySnowNumber(Year,Month,Day,Numbers)
!            implicit none
!            integer :: Year,Month,Day !������
!            integer :: Numbers(5) !����ѩֵ���ֵ�������>1cm,5cm,10cm,20cm,30cm
!            character*(FileNameMaxLen):: FileName !�ջ�ѩ�ļ���
!            integer :: FactRL  !�ļ���¼��
!            type(DaySnow),allocatable :: DayData(:) !�ջ�ѩ����
!            integer :: i
!           
!            
!            !������̣���ȡ�ջ�ѩ�ļ������Ҹ�����������������
!            !(1)׼�������������ʼ���������ջ�ѩ�ļ�������ȡ�ļ���¼�������ļ�����
!            Numbers=0 !�����ʼ��
!            call CreateDayFileName(FileName,Year,Month,Day) !�����ջ�ѩ�ļ���filename
!            if(GetDayRL(Year,Month,Day,FactRL)/=0) then !��ȡ�ջ�ѩ��¼����FactRL
!                write(*,*)"��ȡ�ջ�ѩ��¼����ʱ����,�����˳�"
!                return
!            endif
!            allocate(DayData(FactRL)) !��̬��������
!            call ReadDaySnowData(Year,Month,Day,DayData,FactRL) !���ջ�ѩ����DayData
!            
!            !(2)�ҷ�������������
!            do i=1,FactRL
!                if(DayData(i).Depth >=1 .and. (DayData(i).Depth/=DefaultVal .and. DayData(i).Depth/=MicroscaleVal)) then
!                    Numbers(1)=Numbers(1)+1  !>=1cm������
!                endif
!                if(DayData(i).Depth >=5 .and. (DayData(i).Depth/=DefaultVal .and. DayData(i).Depth/=MicroscaleVal)) then
!                    Numbers(2)=Numbers(2)+1  !>=5cm������
!                endif
!                if(DayData(i).Depth >=10 .and. (DayData(i).Depth/=DefaultVal .and. DayData(i).Depth/=MicroscaleVal)) then
!                    Numbers(3)=Numbers(3)+1  !>=10cm������
!                endif
!                if(DayData(i).Depth >=20 .and. (DayData(i).Depth/=DefaultVal .and. DayData(i).Depth/=MicroscaleVal)) then
!                    Numbers(4)=Numbers(4)+1  !>=20cm������
!                endif
!                if(DayData(i).Depth >=30 .and. (DayData(i).Depth/=DefaultVal .and. DayData(i).Depth/=MicroscaleVal)) then
!                    Numbers(5)=Numbers(5)+1  !>=30cm������
!                endif
!            enddo
!            
!            deallocate(DayData)
!            return        
!        end subroutine GetDaySnowNumber
!        !--------------------------------------------------------------
!        
!        !--------�����ջ�ѩ����ѩ��ȳ�����---------------------------
!        !˵����������������ж��ʱ������������9950���б�ʾ��
!        !   �磺�������յ�ֵΪ2,��ʾ�ڸ���2�ų����˸������������յ�ֵΪ
!        !   9952����ʾ�ڸ��³�����2�θ�����
!        integer function GetMaxDepthDate(Year,Month,Day)
!            implicit none
!            integer:: Year,Month,Day !������
!            character*(FileNameMaxLen)::FileName !
!            integer::FactRL
!            type(DaySnow),allocatable:: DayData(:) !��̬�����ջ�ѩ����
!            integer::i !ѭ������
!            integer::d !������
!            integer::n !���ִ���
!            integer::max !����ѩ���
!            
!            !������̣��ȴ򿪸��ջ�ѩ�ļ���Ȼ���ȡ��������ѩ��ȣ�Ȼ���������ѩ��ȳ�����
!            !(1)׼�����������ļ�����������ѩ���
!            call CreateDayFileName(FileName,Year,Month,Day) !�����ջ�ѩ�ļ���filename
!            if(GetDayRL(Year,Month,Day,FactRL)/=0) then !��ȡ�ջ�ѩ��¼����FactRL
!                write(*,*)"��ȡ�ջ�ѩ��¼����ʱ����,�����˳�"
!                return
!            endif
!            allocate(DayData(FactRL)) !��̬��������
!            call ReadDaySnowData(Year,Month,Day,DayData,FactRL) !���ջ�ѩ����DayData
!            max=GetDayMaxDepth(Year,Month,Day) 
!            if(max==DefaultVal .OR. max==MicroscaleVal) then !��Ϊ΢��ֵʱ���޻�ѩ��������ȡ32744
!                GetMaxDepthDate=MicroscaleVal
!                return
!            endif
!            n=0 !���ִ�����ʼ��Ϊ0
!                       
!            !(2)������ѩ��ȳ�����
!            do i=1,FactRL
!                if(DayData(i).Depth == max)  n=n+1 !���ִ�����1��                
!            enddo
!            
!            if(n==1) GetMaxDepthDate=day !ֻ����һ�Σ��򷵻ظ�����
!            if(n>1) GetMaxDepthDate=BaseVal + n !������ֶ��գ�����9950+n
!            return            
!        end function GetMaxDepthDate
!        
!        !---------------------------------------------------------------
    
    !*******************���ܺ�������********************************
    
    
    
    
    
    
    
    
    
    
    
    
    
    !*******************����Ϊ��������******************************
    
       !------------����m~n���������---------------------------------------------------------
        integer function Rand(m,n)!ע�⣬�ڵ��ô˺���ǰ��Ҫ����������ӣ���call random_seed()
             implicit none
             real::t
             integer::m,n
             if(m>=n) then
                  write(*,*)"������������Ͻ��������½�,�����˳�"
                  return
             endif
             call random_number(t)
             Rand=m+int(t*(n-m))
             return  
        end function Rand  
       !--------------------------------------------------------
       !-----------�ж�����--------------------------------------
        logical function IsLeapYear(year)
	         integer year
	         IsLeapYear=(mod(year,4)==0 .and. mod(year,100)/=0 ).or. mod(year,400)==0
	    end function IsLeapYear
       !--------------------------------------------------------- 
       
       !------------��ȡĳ��������������---------------------------
       integer function GetMonDays(Year,Month)
            implicit none
            integer::Year,Month
            
            if(Month==1 .OR. Month==3 .OR. Month==5 .OR. Month==7 .OR. Month==8 .OR. Month==10 .OR. Month==12) then
                GetMonDays=31
                return
            endif
            if(Month==4 .OR. Month==6 .OR. Month==9 .OR. Month==11 ) then
                GetMonDays=30
                return
            endif
            if(Month==2 .and. IsLeapYear(year)) then
                GetMonDays=29
                return
            endif
            if(Month==2 .and. (.not. IsLeapYear(year))) then
                GetMonDays=28
                return
            endif
            
       end function GetMonDays
       !-----------------------------------------------------------
        
       !----------�����ջ�ѩ�����ļ���------------------------------
        subroutine CreateDayFileName(FileName,Year,Month,Day)
        !˵�������������꣬�£��գ������ջ�ѩ���ݵ��ļ�����ͨ��filename����
            implicit none
            character*(FileNameMaxLen) :: FileName
            integer::Year,Month,Day
            character*(4)::YearChar
            character*(2)::MonthChar
            character*(2)::DayChar
            integer::stat
        
            write(YearChar,"(I4)",iostat=stat)year !�����������ת�����ַ���
            if(stat/=0) write(*,*)"�������ת������" 
        
            write(monthChar,"(I2)",iostat=stat)month !���·�������ת�����ַ���
            if(stat/=0) write(*,*)"�·�����ת������"         
            if(month>0 .and. month<=9) then 
                write(monthChar,"(I2)") Month
                MonthChar='0'//adjustl(MonthChar)
            endif        
            if(month>=10 .and. month<=12) then
                write(monthChar,"(I2)") Month
            endif        
            if(month<1 .or. month >12) then
                write(*,*) "������·ݳ�����1-12�������˳�"
                return
            endif
            
            write(DayChar,"(I2)",iostat=stat)Day !����������ת�����ַ���
            if(stat/=0) write(*,*)"�·�����ת������"         
            if(Day>0 .and. Day<=9) then 
                write(DayChar,"(I2)") Day
                DayChar='0'//adjustl(DayChar)
            endif        
            if(Day>=10 .and. Day<=31) then
                write(DayChar,"(I2)") Day
            endif        
            if(Day<1 .or. Day >31) then
                write(*,*) "������·ݳ�����1-12�������˳�"
                return
            endif
 
            FileName="./Data/ResourceData/Day/snow_day_"//YearChar//MonthChar//DayChar//".txt"  !�ϳ��ļ��� 
           !  write(*,*)filename
            return
        end subroutine CreateDayFileName
       !------------------------------------------------------------
       
       !----------�����»�ѩ�����ļ���------------------------------
        subroutine CreateMonthFileName(FileName,Year,Month)
        !˵���������꣬�£������»�ѩ�����ļ�����ͨ��FileName����
            implicit none
            character*(FileNameMaxLen) :: FileName
            integer::Year,Month
            character*(4)::YearChar
            character*(2)::MonthChar
            integer::stat
        
            write(YearChar,"(I4)",iostat=stat)year !�����������ת�����ַ���
            if(stat/=0) write(*,*)"�������ת������" 
        
            write(monthChar,"(I2)",iostat=stat)month
            if(stat/=0) write(*,*)"�·�����ת������"         
            if(month>0 .and. month<=9) then !���·�������ת�����ַ���
                write(monthChar,"(I2)") Month
                MonthChar='0'//adjustl(MonthChar)
            endif        
            if(month>=10 .and. month<=12) then
                write(monthChar,"(I2)") Month
            endif        
            if(month<1 .or. month >12) then
                write(*,*) "������·ݳ�����1-12�������˳�"
                return
            endif
 
            FileName="./Data/ResourceData/Month/snow_mon_"//YearChar//MonthChar//".txt"  !�ϳ��ļ��� 
             !write(*,*)filename
            return
        end subroutine CreateMonthFileName
       !------------------------------------------------------------
       
       !------------��ȡ�ջ�ѩ���ݼ�¼����--------------------------
       integer function GetDayRL(Year,Month,Day,FactRL)
       !˵������ȡĳ�������µ��ջ�ѩ��¼������FactRL���ظü�¼��,
       !��������ֵ��ʾ������Ϣ����FileRetureInfo�е�indexֵ
            implicit none
            integer::Year,Month,Day !������
            integer::FactRL !�ջ�ѩ��¼��
            integer::cnt !���ڼ���,��¼�ջ�ѩ��¼����
            character*(FileNameMaxLen)::FileName
            type(DaySnow)::DayData
            logical::alive
            integer::stat
            
            !�����ļ���
            call CreateDayFileName(FileName,Year,Month,Day)
            !���ļ������ļ���¼������
            inquire(file=FileName,exist=alive)
            if(.not. alive) then
                GetDayRL=-4
                write(*,"(A80,'�ջ�ѩ�ļ������ڣ������˳�')")filename
                return
            endif
            open(10,file=filename,form="formatted",iostat=stat)
            if(stat/=0) then
                write(*,"(A80,'�ջ�ѩ�ļ���ʱ���������˳�')")filename
                return
            endif
            cnt=0
            do while(.true.)
                read(10,*,iostat=stat)DayData
                if(stat==0) cnt=cnt+1
                if(stat<0 .and. cnt==1) then
                        GetDayRL=-2 !̨վ�ļ�Ϊ�գ�������                         
					    close(10)
					    return
			    endif
			    if(stat<0 .and. cnt>0) then
					    
					    GetDayRL=0 !��ȡ�ļ���¼������
					    FactRL=cnt !�����ļ���¼��					    
					    close(10)
					    return
                endif
                if(stat>0) then
                        GetDayRL=-3 !��ȡ�ļ���¼ʱ����
                        close(10)
					    return
                endif                                        
            enddo
       end function GetDayRL
       !------------------------------------------------------------
       
       !--------��̨վ��Ϣ�ļ��л�ȡ̨վ����------------------------ 
       type(FileReturnInfo) function GetStaNumber(StaFileName,FactRL)
            implicit none
            character*(FileNameMaxLen)::StaFileName !̨վ��Ϣ�ļ�
            integer::FactRL !����̨վ����
            type(StaInfo):: Sta(10000)!������װ�ص�������
            integer::cnt !������ʱ��¼̨վ����
            integer::stat
            logical::alive
            character*(60)::c !���ļ���1���ַ��������ַ����������¼��
            
            !��ѯ̨վ��Ϣ�ļ��Ƿ���ڣ��粻�������˳���
            inquire(file=stafilename,exist=alive)
            if(.not.alive) then
                write(*,"(A60,'̨վ��Ϣ�ļ�������')") stafilename            
                GetStaNumber.index=-4
                GetStaNumber.info=StaFileName//"̨վ��Ϣ�ļ�������"
                return 
            endif
            
            !̨վ��Ϣ�ļ����ڣ����ļ�
            open(unit=12,file=StaFileName,form="formatted",iostat=stat)
                if(stat/=0) then
                     GetStaNumber.index=-1
                     GetStaNumber.info=StaFileName//"�ļ��򿪳���"                     
                     close(12)
                     return
                endif
            read(12,"(A60)") c !����1���ַ��������в�ռ̨վ����
            
            cnt=1
            do while(.true.)
                    read(12,"(I5,' ',A20,F8.2,F8.2,F8.2)",iostat=stat)Sta(cnt).StaID,Sta(cnt).StaName,Sta(cnt).longitude,&
                                Sta(cnt).Latitude,Sta(cnt).StaHeight
                    if(stat==0) cnt=cnt+1 !������ȡ�����ݣ�̨վ������1
                    if(stat<0 .and. cnt==1) then
                        GetStaNumber.index=-2
                        GetStaNumber.info=StaFileName//"̨վ�ļ�Ϊ�գ�������"                           
					    close(12)
					    return
					endif
					if(stat<0 .and. cnt>1) then
					    GetStaNumber.index=0
                        GetStaNumber.info=StaFileName//"��ȡ�ļ���¼������"
					    FactRL=cnt-1 !�����ļ���¼��
					    close(12)
					    return
                    endif
                    if(stat>0) then
                        GetStaNumber.index=-3
                        GetStaNumber.info=StaFileName//"��ȡ�ļ���¼ʱ����"
                        close(12)
					    return
                    endif 
            end do
       end function GetStaNumber
    !----------------------------------------------------------------------------
 
    !-----------��̨վ��Ϣ����---------------------------------------------
    !����FactRLΪ̨վ�������ڵ��ú���ǰ�������ȶ�ȡ̨վ��Ϣ�ļ���ȡFactRLֵ 
    subroutine ReadStaInfoData(FileName,StationInfo,FactRL)
        implicit none
        character*(30)::FileName
        integer::FactRL
        type(StaInfo) ::StationInfo(FactRL)
        integer :: i
        character*(60) :: c11
    
        open(21,file="./data/ResourceData/StaInfo.txt",form="formatted")
        read(21,"(A60)")c11
        do i=1,FactRL
        read(21,"(I5,' ',A20,F8.2,F8.2,F8.2)")StationInfo(i).StaID,StationInfo(i).StaName,&
                    StationInfo(i).latitude,StationInfo(i).longitude,StationInfo(i).staHeight
        enddo
        close(21)
    end subroutine ReadStaInfodata
   !---------------------------------------------------------------------------------------------
       
    !*******************������������********************************
    

end module JL_Mod_StaSnow_Common