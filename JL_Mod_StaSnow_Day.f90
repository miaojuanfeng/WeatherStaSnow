module JL_Mod_StaSnow_Day
    use JL_Mod_StaSnow_Common
    contains
        !-----�й�̨վ�ջ�ѩ�����ʷ����------------------
        !���ܣ�ÿһվ��ÿ�����ڻ�ѩ������ݽ������򲢴�Ϊһ�������ļ�
        subroutine DayDepthSort(StaID,Month,Day,StartYear,EndYear)
            implicit none
            integer::StaID,Month,Day !̨վ�ţ��£���
            integer::StartYear,EndYear !��ֹ��
            
            type(MaxDepthHisorder),allocatable:: DayHisorderData(:) !Ҫ���������,����ά����С��1951����ǰ��������
            type(DaySnow) :: SnowData !���ջ�ѩ����
            type(FileReturnInfo):: FN
            integer :: DaySum !�յ����� 
            character*(FileNameMaxLen) :: HisFileName !��ʷ�ջ�ѩ�ļ���
            character*(FileNameMaxLen) :: DayFileName !�ջ�ѩ�ļ���
            character*(FileNameMaxLen) :: StaFileName !̨վ�ļ���
            integer :: StaNum !̨վ����
            integer :: i,j,k !ѭ��������
            integer :: stat
            logical :: alive
            
            DaySum = EndYear-StartYear + 1  !����������
            allocate(DayHisorderData(DaySum))
            !��ȡ̨վ��
            StaFileName="./Data/ResourceData/StaInfo.txt"
            FN = GetStaNumber(StaFileName,StaNum)
            if(FN.index /=0) then
                write(*,*)"��ȡ̨վ�����������̨վ��Ϣ�ļ��Ƿ���ڻ������Ƿ���ȷ"
                return
            endif
            !���ջ�ѩ�����ļ��е��ջ�ѩ����
            !�����ļ��������ļ�����ÿһ����¼��
            !�ж��Ƿ�Ϊ����Ҫ�����վ:������ǣ����������һ������;����ǣ��������վ��վ�ţ�վ���������ȣ����
            
            do i=StartYear,EndYear
                call CreateDayFileName(DayFileName,i,Month,Day) !�����ջ�ѩ�ļ���
                !call CreateDayFileName(FileName,Year,Month,Day)
                inquire(file=DayFileName,exist=alive) !����ļ��Ƿ����
                if(.not. alive) then
                    write(*,"(I80,'�ļ�������')")DayFileName
                    return
                endif
                open(10,file=DayFileName,iostat=stat) 
                if(stat/=0) then
                    write(*,"('���ջ�ѩ�ļ�',I80,'ʱ��������ļ������Ƿ���ȷ')")DayFileName
                    return
                endif
                
                do j=1,StaNum !����StaID������ҵ��������Ϣ��������������飬Ȼ���˳�ѭ��
                    read(10,*) SnowData
                    if(SnowData.StaID == StaID) then
                        DayHisorderData(i-StartYear+1).StaID = SnowData.StaID
                        DayHisorderData(i-StartYear+1).StaName = SnowData.StaName
                        DayHisorderData(i-StartYear+1).Year = SnowData.Year                        
                        DayHisorderData(i-StartYear+1).MaxDepth = SnowData.Depth
                        close(10)
                        exit
                    endif
                enddo   
            enddo
            
            !����         
            call MaxDepthSort(DayHisorderData,DaySum)            
            !��ŵ��ļ�
            call CreateDayDepthSortFileName(HisFileName,StaID,Month,Day)!�����ջ�ѩ��ʷ�����ļ���
            open(11,file= HisFileName, form="formatted")
            
            do j=1,DaySum
                write(11,*) DayHisorderData(j)
            enddo
            close(11)
            deallocate(DayHisorderData)
        end subroutine DayDepthSort
        !-------------------------------------------------
        
        
        !---------------�����ջ�ѩ��ʷ�����ļ���-----------------------------
        subroutine CreateDayDepthSortFileName(HisFileName,StaID,Month,Day)
            implicit none 
            character*(FileNameMaxLen):: HisFileName
            integer:: StaID,Month,Day !̨վ�ţ��£���
            character*(5):: StaIDChar !̨վ���ַ�
            character*(2):: MonthChar !���ַ�
            character*(2):: DayChar !���ַ�
            integer:: stat
            
            write(StaIDChar,"(I5)",iostat=stat)StaID !��̨վ��������ת�����ַ���
                if(stat/=0) write(*,*)"̨վ������ת������" 
        
            write(monthChar,"(I2)",iostat=stat)month
                if(stat/=0) write(*,*)"�·�����ת������" 
        
            if(month>0 .and. month<=9) MonthChar='0'//adjustl(MonthChar)
            if(month<1 .or. month >12) then
                write(*,*) "������·ݳ�����1-12�������˳�"
            return
            endif
            
            write(DayChar,"(I2)")Day 
            if(Day>0 .and. Day<=9)  DayChar = '0'//adjustl(DayChar)               
 
            HisFileName="./Data/ResultData/SnowDepthSort/Day/Snow_hisorder_day_"//MonthChar//DayChar//'_'//StaIDChar//".txt"  !�ϳ��ļ��� 
            return
        
        end subroutine CreateDayDepthSortFileName

end module JL_Mod_StaSnow_Day