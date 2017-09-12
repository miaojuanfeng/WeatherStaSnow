module JL_ModStaSnow_Ten
    use JL_Mod_StaSnow_Common
    implicit none
        type TenSnow
            integer:: StaID !��վ��
            character*(StaNameMaxLen)::StaName !��վ����
            real(8)::Longitude !��վγ��
            real(8)::Latitude !��վ����
            real(8)::StaHeight !��վ�߶�
            integer(4)::Year !��
            integer(4)::Month !��
            integer(2)::Ten !Ѯ
            integer(4)::MaxDep !����ѩ��ȣ���ȱ��ֵ��΢��ֵ 
        end type TenSnow
    contains
    
    !-------------Ѯ���ܴ���ģ��------------------------------- 
       
        !---------����Ѯ����ѩ��Ȳ�����---------------------------
        !˵����ͨ���ṩ�꣬�£�Ѯ�ӿڣ������ʱ�ε�����ѩ��ȣ�
        !�������ŵ��й�̨վѮ��ѩ��������ļ�
        !������̣���������Ѯ�ӿڣ�������ʱ�ε��ջ�ѩ���ݣ�Ȼ��
        !��������ѩ������ݣ�ÿ��̨վһ�����ݡ����ս�ÿ��̨վ
        !������ѩ��ȴ�ŵ�Ѯ����ѩ����ļ�
        subroutine TenMaxDepth(Year,Month,Ten)
            integer :: Year,Month,Ten !����Ѯ
            character*(FileNameMaxLen):: DayFileName,TenFileName,StaFileName !�ջ�ѩ�ļ�����Ѯ��ѩ�ļ���,̨վ��Ϣ�ļ���
            type(DaySnow),allocatable:: SnowData(:,:) !��ѩ���ݶ�̬���飬����M��N��̨վ�Ļ�ѩ����
            type(TenSnow),allocatable:: TenData(:) !Ѯ����
            integer,allocatable:: MaxDepth(:) !����ѩ��ȶ�̬���飬ά��Ϊ̨վ����
            type(FileReturnInfo):: FN !�ļ�����ֵ��Ϣ
            integer :: i,stat
            integer:: DayNum,StaNum !�ֱ�Ϊ������̨վ��
            integer:: TenNum(3)
            integer:: StartD,EndD !Ѯ�Ŀ�ʼ�պͽ�����
            
            call CreateTenFileName(TenFileName,Year,Month,Ten)!����Ѯ�ļ���TenFileName
            if(GetTenDayNum(TenNum,Year,Month,Ten)) then !��ȡѮ����������ֹ��
                  DayNum=TenNum(1) !Ѯ�ڵ�����
                  StartD=TenNum(2) !Ѯ�Ŀ�ʼ��
                  EndD = TenNum(3)  !Ѯ�Ľ�����
            endif
            
            !��ȡ̨վ��Ϣ��̨վ����
            StaFileName="./Data/ResourceData/StaInfo.txt"
            FN = GetStaNumber(StaFileName,StaNum)
            if(FN.index/=0) then 
                write(*,*)"��ȡ̨վ�������������˳�"
                return
            endif
            
            !��̬���������
            allocate(SnowData(DayNum,StaNum))
            allocate(TenData(StaNum))
            allocate(MaxDepth(StaNum))
            
            !��Ѯ������װ�ص�SnowData����
            do i=1,DayNum
                call ReadDaySnowData(Year,Month,i+StartD-1,SnowData(i,:),StaNum)
            enddo

            !����Ѯ������ѩ���ֵ
            call MaxSnowDepth_common(SnowData,MaxDepth,DayNum,StaNum) !����ѩ���
          
            !����Ѯ���ݣ�����Ѯ���ݴ���Ѯ��ѩ��������ļ�
            call CreateTenFileName(TenFileName,Year,Month,Ten)!����Ѯ�ļ���TenFileName
            open(10,file=TenFileName,form='formatted',iostat=stat)
            if(stat/=0) then
                write(*,*) "����Ѯ�����������ļ�ʱ���������˳�"
                close(10)
                return
            endif
            do i=1,StaNum
                TenData(i).StaID = SnowData(1,i).StaID
                TenData(i).StaName = SnowData(1,i).StaName
                TenData(i).Longitude = SnowData(1,i).Longitude
                TenData(i).Latitude = SnowData(1,i).Latitude
                TenData(i).StaHeight = SnowData(1,i).StaHeight
                TenData(i).Year = Year
                TenData(i).Month = Month
                TenData(i).Ten = Ten
                TenData(i).MaxDep = MaxDepth(i)
                write(10,*)TenData(i)
            enddo
            
            deallocate(SnowData)
            deallocate(TenData)
            deallocate(MaxDepth)
            close(10)
            return

        end subroutine TenMaxDepth
        !-------------------------------------------------------------
        
        !--------Ѯ��ѩ�����ʷ����-----------------------------------
        !˵�����й�̨վѮ����ѩ�����ʷ����ÿһվ��ÿ����������ѩ
        !������ϴ�Ϊһ�������ļ�����MM��DD�յ�����ѩ�������
        !������̣�����ĳѮ����ÿ��̨վ�ڸ�Ѯ�Ļ�ѩ�����Ƚ�������
        !ע�⣺��ģ���ڴ���ǰ�����ȼ������Ѯ����ѩ���ݣ�����ԴΪ������
        subroutine TenSnowDepthSort(StaID,Month,Ten,StartYear,EndYear)
            implicit none
            integer::StaID,Month,Ten !̨վ�ţ��£���
            integer::StartYear,EndYear !��ֹ��
            type(MaxDepthHisorder),allocatable:: TenHisorderData(:) !Ҫ���������,����ά����С��1951����ǰ��Ѯ����
            type(TenSnow) :: SnowData !��Ѯ����ѩ����
            type(FileReturnInfo):: FN
            integer :: TenSum !Ѯ������ 
            character*(FileNameMaxLen) :: HisFileName !��ʷ�ļ���
            character*(FileNameMaxLen) :: MaxFileName !Ѯ����ѩ�ļ���
            character*(FileNameMaxLen) :: StaFileName !Ѯ����ѩ�ļ���
            integer :: StaNum !̨վ����
            integer :: i,j,k !ѭ��������
            integer :: stat
            logical :: alive
            
            !��Ҫ���еĴ�������������ļ�������ȡ��̨վ����������ݣ�Ȼ���������
            
            TenSum = EndYear-StartYear + 1  !����Ѯ����
            allocate(TenHisorderData(TenSum))
            !��ȡ̨վ��
            StaFileName="./Data/ResourceData/StaInfo.txt"
            FN = GetStaNumber(StaFileName,StaNum)
            if(FN.index /=0) then
                write(*,*)"��ȡ̨վ�����������̨վ��Ϣ�ļ��Ƿ���ڻ������Ƿ���ȷ"
                return
            endif
            !��Ѯ����ѩ�����ļ��е�Ѯ����
            !�����ļ��������ļ�����ÿһ����¼��
            !�ж��Ƿ�Ϊ����Ҫ�����վ:������ǣ����������һ������;����ǣ��������վ��վ�ţ�վ���������ȣ����
            
            do i=StartYear,EndYear
                call CreateTenFileName(MaxFileName,i,Month,Ten) !����Ѯ����ѩ�ļ���
                inquire(file=MaxFileName,exist=alive) !����ļ��Ƿ����
                if(.not. alive) then
                    write(*,"(I80,'�ļ�������')")MaxFileName
                    return
                endif
                open(10,file=MaxFileName,iostat=stat) 
                if(stat/=0) then
                    write(*,"('��Ѯ����ѩ�ļ�',I80,'ʱ��������ļ������Ƿ���ȷ')")MaxFileName
                    return
                endif
                
                do j=1,StaNum !����StaID������ҵ��������Ϣ��������������飬Ȼ���˳�ѭ��
                    read(10,*) SnowData
                    if(SnowData.StaID == StaID) then
                        TenHisorderData(i-StartYear+1).StaID = SnowData.StaID
                        TenHisorderData(i-StartYear+1).StaName = SnowData.StaName
                        TenHisorderData(i-StartYear+1).Year = SnowData.Year                        
                        TenHisorderData(i-StartYear+1).MaxDepth = SnowData.MaxDep
                        close(10)
                        exit
                    endif
                enddo   
            enddo
            
            !����
            !write(*,*) TenHisorderData(1:tensum)
            call MaxDepthSort(TenHisorderData,TenSum)            
            !��ŵ��ļ�
            call CreateTenDepthSortFileName(HisFileName,StaID,Month,Ten)!����Ѯ����ѩ��ʷ�����ļ���
            open(11,file= HisFileName, form="formatted")
            
            write(11,*) "    ��վ�� ��վ��                  ����ѩ���     ���"
            do j=1,TenSum
                write(11,*) TenHisorderData(j)
            enddo
            close(11)
            deallocate(TenHisorderData)
        end subroutine TenSnowDepthSort
        !-------------------------------------------------------------
        
    !----------Ѯ���ܴ���ģ�����-------------------------------
    
    
    
    
    
    
    
    !----------Ѯ����������-----------------------------------
    
        !--------����Ѯ����ѩ����ļ���-----------------------------------
        !˵������������Ѯ�ӿ������ļ���,ͨ��TenFileName����
        subroutine CreateTenFileName(TenFileName,Year,Month,Ten)
            implicit none 
            character*(FileNameMaxLen):: TenFileName
            integer:: Year,Month,Ten !����Ѯ
            character*(4):: YearChar !���ַ�
            character*(2):: MonthChar !���ַ�
            character*(1):: TenChar !Ѯ�ַ�
            integer:: stat
            
            write(YearChar,"(I4)",iostat=stat)Year !�����������ת�����ַ���
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
            
            write(TenChar,"(I1)",iostat=stat)Ten !�����������ת�����ַ���
                if(stat/=0) write(*,*)"Ѯ����ת������" 
 
        TenFileName="./Data/ResultData/SnowDepth/Ten/Snow_10day_"//yearChar//MonthChar//TenChar//".txt"  !�ϳ��ļ��� 
        return
        end subroutine CreateTenFileName
        !-------------------------------------------------------
    
        !--------��ȡĳѮ����������ֹʱ��---------------------------------
        !˵��:�ú�����ȡ����ֵ�����TenNum�����У���һ��ֵΪ�������ڶ�ֵΪ
        !��ʼ���ڣ�����ֵΪ�������ڡ�����ֵΪTrue��ʾ��ȡ�ɹ������򲻳ɹ�
        logical function GetTenDayNum(TenNum,Year,Month,Ten)
            integer :: Year,Month,Ten 
            integer :: TenNum(3)        !Ѯ��������ֹ����
            
            if(Month<1 .or. Month>12) then
                write(*,"('�·�Ϊ',I2,'�·�ֵ������1-12֮��!')") Month
                GetTenDayNum = .false.
                return
            endif
            if(Ten<1 .or. Ten >3) then
                write(*,"('ѮֵΪ',I1,'Ѯֵ������1-3֮��')")Ten
                GetTenDayNum = .false.
                return
            endif
            
            if(Ten==1) then
                TenNum(1)=10
                TenNum(2)=1
                TenNum(3)=10
                GetTenDayNum=.True.
            endif
            if(Ten==2) then
                TenNum(1)=10
                TenNum(2)=11
                TenNum(3)=20
                GetTenDayNum=.True.
            endif
            if(Ten==3) then
                    if(Month==1 .or. Month==3 .or. Month==5 .or. Month==7 .or. Month==8 .or. Month==10 .or. Month==12) then
                        TenNum(1)=11
                        TenNum(2)=21
                        TenNum(3)=31
                        GetTenDayNum=.True.
                    endif
                    if(Month==4 .or. Month==6 .or. Month==9 .or. Month==11) then
                        TenNum(1)=10
                        TenNum(2)=21
                        TenNum(3)=30
                        GetTenDayNum=.True.
                    endif
                    if(Month == 2) then
                        if(IsLeapYear(year)) then
                            TenNum(1)=9
                            TenNum(2)=21
                            TenNum(3)=29
                            GetTenDayNum=.True.
                        else
                            TenNum(1)=8
                            TenNum(2)=21
                            TenNum(3)=28
                            GetTenDayNum=.True.                        
                        endif                        
                    endif
            endif  
            return
        end function GetTenDayNum
        !-------------------------------------------------------
        
        !-----------����Ѯ����ѩ�����ʷ�����ļ���-----------
        subroutine CreateTenDepthSortFileName(FileName,StaID,Month,Ten)
            implicit none
            character*(FileNameMaxLen) :: FileName
            integer:: StaID,Month,Ten !̨վ�ţ��£�Ѯ
            character*(5):: StaIDChar !̨վ���ַ�
            character*(2):: MonthChar !���ַ�
            character*(1):: TenChar !Ѯ�ַ�
            integer :: stat
            write(StaIDChar,"(I5)",iostat=stat)StaID !��̨վ��������ת�����ַ���
                if(stat/=0) write(*,*)"̨վ������ת������" 
        
!            write(MonthChar,"(I2)",iostat=stat)Month
!                if(stat/=0) write(*,*)"�·�����ת������" 
        
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
            
            write(TenChar,"(I1)",iostat=stat)Ten !��Ѯ������ת�����ַ���
                if(stat/=0) write(*,*)"Ѯ����ת������" 
 
        FileName="./Data/ResultData/SnowDepthSort/Ten/Snow_hisorder_10day_"//MonthChar//TenChar//'_'//StaIDChar//".txt"  !�ϳ��ļ��� 
        return
        
        end subroutine CreateTenDepthSortFileName
    !----------Ѯ��������������-------------------------------
end module JL_ModStaSnow_Ten