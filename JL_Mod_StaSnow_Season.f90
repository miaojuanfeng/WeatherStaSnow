module JL_Mod_StaSnow_Season
    use JL_Mod_StaSnow_Common
    implicit none
    
    contains
    
    !-----------���㼾��ѩ�������-------------------------------------
    !˵����ͨ���ṩ�꣬���ӿڣ������ʱ�ε�����ѩ��ȣ�
        !�������ŵ��й�̨վ����ѩ��������ļ�
        !������̣������꼾�ӿڣ�������ʱ�ε��ջ�ѩ���ݣ�Ȼ��
        !��������ѩ������ݣ�ÿ��̨վһ�����ݡ����ս�ÿ��̨վ
        !������ѩ��ȴ�ŵ�������ѩ����ļ�
    subroutine SeaMaxDepth(Year,Season)
        implicit none
            integer::Year,Season !������꼾
            character*(FileNameMaxLen)::SeaFileName !����ѩ�ļ���
            type(SeaSnow),allocatable:: SeaData(:) !��̬���飬�洢����ѩ���ݣ�����ά��Ϊ��¼������
            type(StaInfo),allocatable:: StationInfo(:)
            type(MonthSnow),allocatable:: SnowData(:,:) !���¶�̨վ���»�ѩ����
            integer,allocatable::SnowDayNums(:) !һ���и�̨վ�Ļ�ѩ����
            integer,allocatable::MaxDepth(:) !һ���е�����̨վ������ѩ���ֵ
            character*(5),allocatable:: MaxDepthDay(:) !һ���и�̨վ������ѩ�����
            integer,allocatable :: MaxDepthNum(:,:) !������ѩ��ȳ��ֵ�����                       
            type(FileReturnInfo):: FN !�ļ�����ֵ��Ϣ
            character*(FileNameMaxLen):: StaFileName !̨վ��Ϣ�ļ���
            character*(FileNameMaxLen):: MonFileName !�»�ѩ�ļ���
            integer:: MonNum !����
            integer:: FactRL !�»�ѩ��¼����
            integer::stat
            integer::i,j !ѭ��������
            logical::alive
            
            !������̣���1��׼�������������ڸ��»�ѩ����װ�뼾��ѩ��̬�����С�
            !           (2)���㹤�������㼾����ѩ��ȣ���������ѩ��ȳ�����,�������ֵ�µĳ�������
            
            !׼��������������øü�����������̬��������鲢��ʼ��     
            
            StaFileName="./Data/ResourceData/StaInfo.txt"
            FN = GetStaNumber(StaFileName,FactRL)
            if(FN.index/=0) then 
                write(*,*)"��ȡ̨վ�������������˳�"
                return
            endif
             MonNum=3
             allocate(SnowData(MonNum,FactRL))
             allocate(SnowDayNums(FactRL))
             allocate(MaxDepth(FactRL)) !һ���и�̨վ������ѩֵ
             allocate(MaxDepthDay(FactRL))!һ�����и�̨վ������ѩ������
             allocate(MaxDepthNum(FactRL,5))!���������Ļ�ѩ����
             allocate(SeaData(FactRL)) !����ѩ����
             
            if(Season==1) then !����
                do i=1,3
                    call ReadMonSnowData(Year,i+2,SnowData(i,:),FactRL) !��������
                enddo
            endif
            if(Season==2) then !�ļ�
                do i=1,3
                    call ReadMonSnowData(Year,i+5,SnowData(i,:),FactRL) !��������
                enddo
            endif
            if(Season==3) then !�＾
                do i=1,3
                    call ReadMonSnowData(Year,i+8,SnowData(i,:),FactRL) !��������
                enddo
            endif
            if(Season==4) then !����
                call ReadMonSnowData(Year,12,SnowData(1,:),FactRL) !��12������
                call ReadMonSnowData(Year+1,1,SnowData(2,:),FactRL) !������1������
                call ReadMonSnowData(Year+1,2,SnowData(3,:),FactRL) !������2������                
            endif
            
             !�����̨վ�Ļ�ѩ����            
            call GetSnowDayNubmers_Mon(SnowData,SnowDayNums,MonNum,FactRL) !��ѩ���������SnowDayNums��
            
           !��������ѩ���,MaxSnowDepth_Mon(FactRL)����װ���Ǹ���̨վ������ѩ���!           
            call MaxSnowDepth_Mon(SnowData,MaxDepth,MonNum,FactRL)
            
            !��������ѩ��ȳ�����,��MaxDepthDay(FactRL)����
            call MaxSnowDepthDay_Mon(SnowData,MaxDepthDay,MonNum,FactRL)
            
!            !��������ϵĳ�������,��MaxDepthNum����
            call GetSnowDepthNumbers_Mon(SnowData,MaxDepthNum,MonNum,FactRL)
            
            !���ɼ��ļ���
            call CreateSeaFileName(SeaFileName,Year,Season)

            open(10,File= SeaFileName, form='formatted',iostat=stat)
            if(stat/=0) then 
                write(*,"(I80,'�ļ��������裬�����˳�')") SeaFileName
            endif
            !����������װ�ؽ����������飬����ÿ����¼д���ļ�
            write(10,*)"��վ�� ��վ��  γ�� ���� �߶� �� �� ��ѩ���� ������ ������ >1 >5 >10 >20 >30"

            do i=1,FactRL
                SeaData(i).StaID=SnowData(1,i).StaID
                SeaData(i).StaName=SnowData(1,i).StaName
                SeaData(i).Longitude=SnowData(1,i).Longitude
                SeaData(i).Latitude=SnowData(1,i).Latitude
                SeaData(i).StaHeight=SnowData(1,i).StaHeight
                SeaData(i).Year = Year
                SeaData(i).Season = Season
                SeaData(i).SnowDays = SnowDayNums(i) !��ѩ����
                SeaData(i).MaxDepth = MaxDepth(i) !����ѩֵ
                SeaData(i).MaxDay = MaxDepthDay(i) !����ѩ������
                SeaData(i).DepthDays(1:5) =  MaxDepthNum(i,1:5)              
                
                write(10,100,iostat=stat) SeaData(i).StaID,SeaData(i).StaName,SeaData(i).Longitude,SeaData(i).Latitude,&
                                        SeaData(i).StaHeight,SeaData(i).Year,SeaData(i).Season,SeaData(i).SnowDays,&
                                        SeaData(i).MaxDepth,SeaData(i).MaxDay,SeaData(i).DepthDays(1:5)
                100 format(I8,' ',A20,F8.2,F8.2,F8.2,I6,I4,I6,I6,' ',A5,I6,I6,I6,I6,I6)
                if(stat/=0) then
                    write(*,*) "������д���ļ�ʱ���������˳�"
                    close(10)
                    return
                endif
            enddo
            
            deallocate(SnowData)
            deallocate(SnowDayNums)
            deallocate(MaxDepth) !һ�����и�̨վ������ѩֵ
            deallocate(MaxDepthNum)
            deallocate(MaxDepthDay)!һ�����и�̨վ������ѩ������
            deallocate(SeaData) !����ѩ����
            close(10)
    end subroutine SeaMaxdepth
    !------------------------------------------------------------------
    
    !------------�Լ�����ѩ�������(���뼾����ѩ����)-------------------------
    !˵�������������������̨վ�ţ�������ֹ��ļ�����ѩ���ݣ�Ȼ������ÿһվÿ����Ϊһ���ļ�
    !������̣��ȶ��뼾��ѩ���ݣ�Ȼ���������
    subroutine SeaSnowDepthSort(StaID,Season,StartYear,EndYear)
        implicit none
        integer::StaID,Season !̨վ�ţ���
        integer::StartYear,EndYear !��ֹ��
        type(MaxDepthHisorder),allocatable:: SeaHisorderData(:) !Ҫ���������,����ά����С��1951����ǰ���ܼ���
        type(SeaSnow) :: SnowData !��������ѩ����
        type(FileReturnInfo):: FN
        integer :: SeaSum !�������� 
        character*(FileNameMaxLen) :: HisFileName !��ʷ�����ļ���
        character*(FileNameMaxLen) :: MaxFileName !������ѩ�ļ���
        character*(FileNameMaxLen) :: StaFileName !̨վ�ļ���
        integer :: StaNum !̨վ����
        integer :: i,j,k !ѭ��������
        integer :: stat
        logical :: alive
        
        !������̣���1�����뼾��ѩ���ݣ���2������д���ļ�
        SeaSum = EndYear-StartYear + 1  !���㼾����
        allocate(SeaHisorderData(SeaSum))
        !��ȡ̨վ��
        StaFileName="./Data/ResourceData/StaInfo.txt"
        FN = GetStaNumber(StaFileName,StaNum)
        if(FN.index /=0) then
                write(*,*)"��ȡ̨վ�����������̨վ��Ϣ�ļ��Ƿ���ڻ������Ƿ���ȷ"
                return
        endif 
         
         !��̨վ��ΪStaID�ļ���ѩ���ݶ��뵽������������     
        do i=StartYear,EndYear                
                call CreateSeaFileName(MaxFileName,i,Season) !���ɼ�����ѩ�ļ���
                inquire(file=MaxFileName,exist=alive) !����ļ��Ƿ����
                if(.not. alive) then
                    write(*,"(I80,'�ļ�������')")MaxFileName
                    return
                endif
                open(10,file=MaxFileName,iostat=stat) 
                if(stat/=0) then
                    write(*,"('�򿪼�����ѩ�����ļ�',A80,'ʱ��������ļ������Ƿ���ȷ')")MaxFileName
                    return
                endif 
                
                read(10,*) !�ȶ�����һ���ַ���       
                do j=1,StaNum !����StaID������ҵ����������Ϣ��������������飬Ȼ���˳�ѭ��                    
                    read(10,*) SnowData
                    if(SnowData.StaID == StaID) then
                        SeaHisorderData(i-StartYear+1).StaID = SnowData.StaID
                        SeaHisorderData(i-StartYear+1).StaName = SnowData.StaName
                        SeaHisorderData(i-StartYear+1).Year = SnowData.Year                        
                        SeaHisorderData(i-StartYear+1).MaxDepth = SnowData.MaxDepth
                        close(10)
                        exit
                    endif
                enddo   
        enddo
    
        call MaxDepthSort(SeaHisorderData,SeaSum) 
        
        !��ŵ��ļ�
        !call CreateMonDepthSortFileName(HisFileName,StaID,Month)!����������ѩ��ʷ�����ļ���
        call CreateSeaDepthSortFileName(HisFileName,StaID,Season)
        open(11,file= HisFileName, form="formatted")
            
        write(11,*) "    ��վ�� ��վ��                  ����ѩ���     ���"
        do j=1,SeaSum
                write(11,*) SeaHisorderData(j)
        enddo
        close(11)
        deallocate(SeaHisorderData)
        
    end subroutine SeaSnowDepthSort
    
    !------------------------------------------------------------------
    
    
    !----------���ɼ�����ѩ����ļ���--------------------------------
    subroutine CreateSeaFileName(SeaFileName,Year,Season)
        implicit none
        integer Year,Season !�꣬��
        character*(FileNameMaxLen):: SeaFileName
        character*(1):: SeaChar
        character*(4)::YearChar
        
        write(SeaChar,"(I1)")Season
        write(YearChar,"(I4)")Year        
        SeaFileName="./Data/ResultData/SnowDepth/Season/Snow_Sea_"//YearChar//SeaChar//".txt"
        !write(*,*)SeaFileName
    !snow_sea_YYYYS.txt
    end subroutine CreateSeaFileName
    !------------------------------------------------------------------
    
    !---------���ɼ���ѩ��������ļ���---------------------------------
    subroutine CreateSeaDepthSortFileName(FileName,StaID,Season)
        implicit none
        integer::StaID,Season
        character*(FileNameMaxLen)::FileName
        character*(5)::StaIDChar
        character*(1)::SeaChar
        
        write(StaIDChar,"(I5)")StaID
        write(SeaChar,"(I1)")Season
        FileName="./Data/ResultData/SnowDepthSort/Season/Snow_hisorder_sea_"//SeaChar//"_"//StaIDChar//".txt"
        
    !snow_hisorder_sea_S_xxxxx.txt
    end subroutine CreateSeaDepthSortFileName
    !------------------------------------------------------------------
    
    !-------------ͨ�û�ѩ����(����Ϊ�»�ѩ����)--------------------------------------
        !˵����������ڸ���̨վ�Ļ�ѩ������ͨ��SnowdayNums����
        subroutine GetSnowDayNubmers_Mon(SnowData,SnowDayNums,MonNum,StaNum)
            implicit none
            integer :: MonNum !����
            integer :: StaNum !̨վ��
            type(MonthSnow):: SnowData(MonNum,StaNum) !MonNum�µĻ�ѩ���ݣ�ÿ�µļ�¼����ΪStaNum
            integer :: SnowDayNums(StaNum)
            integer :: i,j
            
            SnowDayNums(1:StaNum)=0 !��ʼ������ȫΪ0
            
            do i=1,StaNum !��̨վ
                do j=1,MonNum !����                    
                        SnowDayNums(i) = SnowDayNums(i) + SnowData(j,i).SnowDays                   
                enddo
            enddo
            return
        end subroutine GetSnowDayNubmers_Mon
        !---------------------------------------------------------------
        
        !----------ͨ������ѩ���(����Ϊ�»�ѩ����)-------------------
        !˵���������»�ѩ���ݣ������ÿ��̨վ��ĳ��ʱ���ڵ�����ѩ���
        !SnowDataΪMonNum X StaNum �Ķ�ά���飬MonNum����������StaNum����̨վ��
        !MaxDepth��һά���飬ά��Ϊ̨վ�����������鷵�ظ���̨վ������ѩ���
        subroutine MaxSnowDepth_Mon(SnowData,MaxDepth,MonNum,StaNum)
            integer :: MonNum !����
            integer :: StaNum !̨վ��
            type(MonthSnow):: SnowData(MonNum,StaNum) !MonNum�µĻ�ѩ���ݣ�ÿ�յļ�¼����ΪStaNum
            integer:: MaxDepth(StaNum) !����ѩ��ȣ������齫����
            integer:: DepthData(MonNum) !ĳ��̨��MonNum���ڵĻ�ѩ�������
            integer:: i,j !ѭ������
           ! write(*,*)"������ѩ���ʱװ�ص����ֵ"
            do i=1,StaNum !ÿ��̨վ���м���
                do j=1,MonNum !ÿ��̨վ���������ڵĻ�ѩ���װ��DepthData����
                    DepthData(j)= SnowData(j,i).MaxDepth                               
                enddo
                !if(i<=10) write(*,*)DepthData   
                MaxDepth(i)=FindMaxDepth(DepthData,MonNum) !����ÿ��̨վ���������ڵ�����ѩ���
            enddo           
            
            return
        end subroutine MaxSnowDepth_Mon
        !---------------------------------------------------------------
        
        !--------���Ҷ����ѩ����е�����ѩ���ֵ(����Ϊ�»�ѩ����)------------------
        !SnowDepth(n)������Ϊ�����ѩ������ݣ��ҳ����ֵ����������Ϊ������ѩ���ֵ
        integer function FindMonMaxDepth(SnowDepth,n)
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
                    FindMonMaxDepth=MicroscaleVal
                    return
                else !ȫΪȱ��ֵ
                    FindMonMaxDepth=DefaultVal
                    return
                endif
            endif
            
            FindMonMaxDepth = max
            return
        end function FindMonMaxDepth
        !---------------------------------------------------------------

        !--------������ѩ��ȳ�����(����Ϊ��������)-------------------------
        subroutine MaxSnowDepthDay_Mon(SnowData,MaxDepthDay,MonNum,FactRL)
            implicit none
            integer::MonNum,FactRL !������ÿ�����ڵļ�¼����
            type(MonthSnow)::SnowData(MonNum,FactRL) !�»�ѩ����
            integer:: MaxDepth(FactRL) !������ѩ���
            character*(5):: MaxDepthDay(FactRL) !ÿ��̨վ��MonNum���ڵ�����ѩ��,���ֻ��1�γ��֣���ΪMMDD������Ϊ9950+n
            integer::MM,DD !�£���
            character*(2)::MMch,DDch !���ַ������ַ�
            integer::i,j
            integer::n !��ѩ��ȳ��ֵĴ���
            
            call MaxSnowDepth_Mon(SnowData,MaxDepth,MonNum,FactRL) !������ѩ���
            
            do i=1,FactRL
                n=0
                do j=1,MonNum
                        if(MaxDepth(i)==Defaultval) then !������ѩ���Ϊȱ��ֵ��������Ϊȱ��ֵ
                            write(MaxDepthDay(i),"(I5)")DefaultVal
                            exit
                    endif
                    if(MaxDepth(i)==MicroscaleVal) then !������ѩ���Ϊ΢��ֵ��������Ϊ΢��ֵ                        
                        write(MaxDepthDay(i),"(I5)")MicroscaleVal
                        exit
                    endif
                    if(SnowData(j,i).MaxDepth==MaxDepth(i))  then
                        if(SnowData(j,i).MaxDay<BaseVal)then !����ֻ����һ������ѩ�����
                            n=n+1
                            if(n==1) then
                                MM=SnowData(j,i).Month
                                DD=SnowData(j,i).MaxDay
                            endif                        
                        else  !���³��ֶ������ѩ�����
                            n=n + SnowData(j,i).MaxDay - BaseVal
                        endif
                    endif
                    if(n==1) MaxDepthDay(i)=MMDDchar(MM,DD) !���º���תΪMMDD�͵��ַ���
                    if(n>1) write(MaxDepthDay(i),"(I5)")BaseVal + n !������9952������תΪ�ַ�������ֵ
                enddo
            enddo
            return
        end subroutine MaxSnowDepthDay_Mon
        !---------------------------------------------------------------
        
         !-------------ͨ�÷�����������ȳ�������(����Ϊ�»�ѩ����)----------------------
        !˵�����Ӷ���»�ѩ�����л�ȡÿ��̨վ>=1cm,5cm,10cm,20cm,30cm������
        !SnowData(MonNum,StaNum)��Ŷ��¶�̨վ��ѩ���ݣ�MaxDepthNum(StaNum)��Ÿ�����εĳ�������
        !MonNumΪ������StaNumΪ̨վ��
        subroutine GetSnowDepthNumbers_Mon(SnowData,MaxDepthNum,MonNum,StaNum)
            implicit none
            integer :: MonNum !����
            integer :: StaNum !̨վ��
            type(MonthSnow):: SnowData(MonNum,StaNum) !MonNum����ջ�ѩ���ݣ�ÿ�յļ�¼����ΪStaNum
            integer :: MaxDepthNum(StaNum,5) !������ѩ��ȳ��ֵ�����
            integer i,j !ѭ������
            
            MaxDepthNum(1:StaNum,1:5)=0
            do i=1,StaNum !����̨վ
                do j=1,MonNum !����
                    MaxDepthNum(i,:)=MaxDepthNum(i,:)+SnowData(j,i).DepthDays(:)
                enddo       
            enddo
            return
            
        end subroutine GetSnowDepthNumbers_Mon              
        !---------------------------------------------------------------
       
        !------����������תΪ�ַ���----------------------------------------
        character*(5) function MMDDchar(MM,DD)
            integer::MM,DD !�£���
            character*(2)::MMch,DDch !���ַ������ַ�
            
            write(MMch,"(I2)")MM
            if(MM>0 .and. MM<=9) MMch="0"//adjustl(MMch)
            write(DDch,"(I2)")DD
            if(DD>0 .and. DD<=9) DDch="0"//adjustl(DDch)
            MMDDchar=MMch//DDch//' '
            
        end function MMDDchar
        !------------------------------------------------------------------
        
end module JL_Mod_StaSnow_Season