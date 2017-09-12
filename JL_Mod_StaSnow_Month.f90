module JL_Mod_StaSnow_Month
    use JL_Mod_StaSnow_Common
    implicit none
    
    contains 
    !-------������ѩ�������----------------------------------------
    !˵����ÿһվ��ÿ����������ѩ����������ϴ�Ϊһ�������ļ�
        subroutine MonSnowDepthSort(StaID,Month,StartYear,EndYear)
            implicit none       
            integer::StaID,Month !̨վ�ţ��� 
            integer::StartYear,EndYear !��ֹ��
            type(MaxDepthHisorder),allocatable:: MonHisorderData(:) !Ҫ���������,����ά����С��1951����ǰ��������
            type(MonthSnow) :: SnowData !��������ѩ����
            type(FileReturnInfo):: FN
            integer :: MonSum !�µ����� 
            character*(FileNameMaxLen) :: HisFileName !��ʷ�ļ���
            character*(FileNameMaxLen) :: MaxFileName !������ѩ�ļ���
            character*(FileNameMaxLen) :: StaFileName !̨վ�ļ���
            integer :: StaNum !̨վ����
            integer :: i,j,k !ѭ��������
            integer :: stat
            logical :: alive
            
            
            
            !��Ҫ���еĴ�������������ļ�������ȡ��̨վ����������ݣ�Ȼ���������
            
            MonSum = EndYear-StartYear + 1  !����������
            allocate(MonHisorderData(MonSum))
            !��ȡ̨վ��
            StaFileName="./Data/ResourceData/StaInfo.txt"
            FN = GetStaNumber(StaFileName,StaNum)
            if(FN.index /=0) then
                write(*,*)"��ȡ̨վ�����������̨վ��Ϣ�ļ��Ƿ���ڻ������Ƿ���ȷ"
                return
            endif
            !��������ѩ�����ļ��е�������
            !�����ļ��������ļ�����ÿһ����¼��
            !�ж��Ƿ�Ϊ����Ҫ�����վ:������ǣ����������һ������;����ǣ��������վ��վ�ţ�վ���������ȣ����
            
            do i=StartYear,EndYear                
                call CreateMonthFileName(MaxFileName,i,Month) !����������ѩ�ļ���
                inquire(file=MaxFileName,exist=alive) !����ļ��Ƿ����
                if(.not. alive) then
                    write(*,"(I80,'�ļ�������')")MaxFileName
                    return
                endif
                open(10,file=MaxFileName,iostat=stat) 
                if(stat/=0) then
                    write(*,"('�������ѩ�����ļ�',I80,'ʱ��������ļ������Ƿ���ȷ')")MaxFileName
                    return
                endif
                
                do j=1,StaNum !����StaID������ҵ��������Ϣ��������������飬Ȼ���˳�ѭ��
                    read(10,*) SnowData
                    if(SnowData.StaID == StaID) then
                        MonHisorderData(i-StartYear+1).StaID = SnowData.StaID
                        MonHisorderData(i-StartYear+1).StaName = SnowData.StaName
                        MonHisorderData(i-StartYear+1).Year = SnowData.Year                        
                        MonHisorderData(i-StartYear+1).MaxDepth = SnowData.MaxDepth
                        close(10)
                        exit
                    endif
                enddo   
            enddo
            
            !����
            !write(*,*) TenHisorderData(1:tensum)
            call MaxDepthSort(MonHisorderData,MonSum)            
            !��ŵ��ļ�
            call CreateMonDepthSortFileName(HisFileName,StaID,Month)!����������ѩ��ʷ�����ļ���
            open(11,file= HisFileName, form="formatted")
            
            write(11,*) "    ��վ�� ��վ��                  ����ѩ���     ���"
            do j=1,MonSum
                write(11,*) MonHisorderData(j)
            enddo
            close(11)
            deallocate(MonHisorderData)
        end subroutine MonSnowDepthSort
        
        !---------����������ѩ��ʷ�����ļ���----------------------------
        subroutine CreateMonDepthSortFileName(HisFileName,StaID,Month)
            character*(FileNameMaxLen):: HisFileName
            integer:: StaID,Month !̨վ�ţ���
            character*(5):: StaIDChar !̨վ���ַ�
            character*(2):: MonthChar !���ַ�
            integer :: stat
            
            write(StaIDChar,"(I5)",iostat=stat)StaID !��̨վ��������ת�����ַ���
                if(stat/=0) write(*,*)"̨վ������ת������"
            
            write(MonthChar,"(I2)")Month !��������ת��Ϊ�ַ�
            if(month>0 .and. month<=9) MonthChar='0'//adjustl(MonthChar)

            HisFileName="./Data/ResultData/SnowDepthSort/Month/Snow_hisorder_Mon_"//MonthChar//'_'//StaIDChar//".txt"  !�ϳ��ļ��� 
        return
            
        end subroutine CreateMonDepthSortFileName

end module JL_Mod_StaSnow_Month