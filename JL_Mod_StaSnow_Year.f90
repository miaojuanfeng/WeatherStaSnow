module JL_Mod_StaSnow_Year
    use JL_Mod_StaSnow_Common
    use JL_Mod_StaSnow_Season
    use JL_Mod_StaSnow_Month
    
    contains
        !-----------�������ѩ�������-------------------------------------
        !˵����ͨ���ṩ��ӿڣ������ʱ�ε�����ѩ��ȣ�
        !�������ŵ��й�̨վ���ѩ��������ļ�
        !������̣�������ӿڣ�������ʱ�ε��ջ�ѩ���ݣ�Ȼ��
        !��������ѩ������ݣ�ÿ��̨վһ�����ݡ����ս�ÿ��̨վ
        !������ѩ��ȴ�ŵ�������ѩ����ļ�
    subroutine YearMaxDepth(Year)
        implicit none
            integer::Year !�������
            character*(FileNameMaxLen)::YearFileName !���ѩ�ļ���
            type(YeaSnow),allocatable:: YearData(:) !��̬���飬�洢���ѩ���ݣ�����ά��Ϊ��¼������
                                                      !ע�⣬�˴��õĻ�ѩ�����뼾����ͬ�������е�season�ֶ������ѩ�����в�Ҫд��
            !type(StaInfo),allocatable:: StationInfo(:)
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
            
            !������̣���1��׼�������������ڸ��»�ѩ����װ�����ѩ��̬�����С�
            !           (2)���㹤��������������ѩ��ȣ���������ѩ��ȳ�����,�������ֵ�µĳ�������
            
            !׼��������������ø������������̬��������鲢��ʼ��     
            
            StaFileName="./Data/ResourceData/StaInfo.txt"
            FN = GetStaNumber(StaFileName,FactRL)
            if(FN.index/=0) then 
                write(*,*)"��ȡ̨վ�������������˳�"
                return
            endif
             MonNum=12
             allocate(SnowData(MonNum,FactRL))
             allocate(SnowDayNums(FactRL))
             allocate(MaxDepth(FactRL)) !һ���и�̨վ������ѩֵ
             allocate(MaxDepthDay(FactRL))!һ�����и�̨վ������ѩ������
             allocate(MaxDepthNum(FactRL,5))!���������Ļ�ѩ����
             allocate(YearData(FactRL)) !���ѩ����
             
             !��������
             do i=1,12
                call ReadMonSnowData(Year,i,SnowData(i,:),FactRL) !��������
             enddo
            
            
             !�����̨վ�Ļ�ѩ����            
            call GetSnowDayNubmers_Mon(SnowData,SnowDayNums,MonNum,FactRL) !��ѩ���������SnowDayNums��
            
           !��������ѩ���,MaxSnowDepth_Mon(FactRL)����װ���Ǹ���̨վ������ѩ���!           
            call MaxSnowDepth_Mon(SnowData,MaxDepth,MonNum,FactRL)
            
            !��������ѩ��ȳ�����,��MaxDepthDay(FactRL)����
            call MaxSnowDepthDay_Mon(SnowData,MaxDepthDay,MonNum,FactRL)
            
            !��������ϵĳ�������,��MaxDepthNum����
            call GetSnowDepthNumbers_Mon(SnowData,MaxDepthNum,MonNum,FactRL)
            
            !���ɼ��ļ���
            !call CreateSeaFileName(SeaFileName,Year,Season)
            call CreateYearFileName(YearFileName,Year)

            open(10,File= YearFileName, form='formatted',iostat=stat)
            if(stat/=0) then 
                write(*,"(A80,'�ļ��������裬�����˳�')") YearFileName
            endif
            !����������װ�ؽ����������飬����ÿ����¼д���ļ�
            write(10,*)"��վ�� ��վ��  γ�� ���� �߶� ��  ��ѩ���� ������ ������ >1 >5 >10 >20 >30"

            do i=1,FactRL
                YearData(i).StaID=SnowData(1,i).StaID
                YearData(i).StaName=SnowData(1,i).StaName
                YearData(i).Longitude=SnowData(1,i).Longitude
                YearData(i).Latitude=SnowData(1,i).Latitude
                YearData(i).StaHeight=SnowData(1,i).StaHeight
                YearData(i).Year = Year
                !YearData(i).Season = Season
                YearData(i).SnowDays = SnowDayNums(i) !��ѩ����
                YearData(i).MaxDepth = MaxDepth(i) !����ѩֵ
                YearData(i).MaxDay = MaxDepthDay(i) !����ѩ������
                YearData(i).DepthDays(1:5) =  MaxDepthNum(i,1:5)              
                
                write(10,100,iostat=stat) YearData(i).StaID,YearData(i).StaName,YearData(i).Longitude,YearData(i).Latitude,&
                                        YearData(i).StaHeight,YearData(i).Year,YearData(i).SnowDays,&
                                        YearData(i).MaxDepth,YearData(i).MaxDay,YearData(i).DepthDays(1:5)
                100 format(I8,' ',A20,F8.2,F8.2,F8.2,I6,I6,I6,' ',A5,I6,I6,I6,I6,I6)
                if(stat/=0) then
                    write(*,*) "������д���ļ�ʱ���������˳�"
                    close(10)
                    return
                endif
            enddo
            
            deallocate(SnowData)
            deallocate(SnowDayNums)
            deallocate(MaxDepth) 
            deallocate(MaxDepthNum)
            deallocate(MaxDepthDay)
            deallocate(YearData) 
            close(10)
    end subroutine YearMaxDepth
    !------------------------------------------------------------------
    
    !------------��������ѩ�������(����������ѩ����)-------------------------
    !˵�����������̨վ�ţ� ������ֹ���������ѩ���ݣ�Ȼ������ÿһվ��Ϊһ���ļ�
    !������̣��ȶ������ѩ���ݣ�Ȼ���������
    subroutine YeaSnowDepthSort(StaID,StartYear,EndYear)
        implicit none
        integer::StaID  !̨վ�� 
        integer::StartYear,EndYear !��ֹ��
        type(MaxDepthHisorder),allocatable:: YeaHisorderData(:) !Ҫ���������,����ά����С��1951����ǰ��������
        type(YeaSnow) :: SnowData !��������ѩ����
        type(FileReturnInfo):: FN
        integer :: YeaSum !������� 
        character*(FileNameMaxLen) :: HisFileName !��ʷ�����ļ���
        character*(FileNameMaxLen) :: MaxFileName !������ѩ�ļ���
        character*(FileNameMaxLen) :: StaFileName !̨վ�ļ���
        integer :: StaNum !̨վ����
        integer :: i,j,k !ѭ��������
        integer :: stat
        logical :: alive
        
        !������̣���1�������»�ѩ���ݣ���2������д���ļ�
        YeaSum = EndYear-StartYear + 1  !����������
        allocate(YeaHisorderData(YeaSum))
        !��ȡ̨վ��
        StaFileName="./Data/ResourceData/StaInfo.txt"
        FN = GetStaNumber(StaFileName,StaNum)
        if(FN.index /=0) then
                write(*,*)"��ȡ̨վ�����������̨վ��Ϣ�ļ��Ƿ���ڻ������Ƿ���ȷ"
                return
        endif 
         
         !��̨վ��ΪStaID���»�ѩ���ݶ��뵽������������     
        do i=StartYear,EndYear                
                call CreateYearFileName(MaxFileName,i) !���ɼ�����ѩ�ļ���                
                inquire(file=MaxFileName,exist=alive) !����ļ��Ƿ����
                if(.not. alive) then
                    write(*,"(A80,'�ļ�������')")MaxFileName
                    return
                endif
                open(10,file=MaxFileName,iostat=stat) 
                if(stat/=0) then
                    write(*,"('�򿪼�����ѩ�����ļ�',A80,'ʱ��������ļ������Ƿ���ȷ')")MaxFileName
                    return
                endif 
                
                read(10,*) !�ȶ�����һ���ַ���       
                do j=1,StaNum !����StaID������ҵ����������Ϣ��������������飬Ȼ���˳�ѭ�� 
                    read(10,100,iostat=stat) SnowData.StaID,SnowData.StaName,SnowData.Longitude,SnowData.Latitude,&
                                        SnowData.StaHeight,SnowData.Year,SnowData.SnowDays,&
                                        SnowData.MaxDepth,SnowData.MaxDay,SnowData.DepthDays(1:5)
                    100 format(I8,' ',A20,F8.2,F8.2,F8.2,I6,I6,I6,' ',A5,I6,I6,I6,I6,I6)
                    
                    if(SnowData.StaID == StaID) then
                        YeaHisorderData(i-StartYear+1).StaID = SnowData.StaID
                        YeaHisorderData(i-StartYear+1).StaName = SnowData.StaName
                        YeaHisorderData(i-StartYear+1).Year = SnowData.Year                        
                        YeaHisorderData(i-StartYear+1).MaxDepth = SnowData.MaxDepth
                        close(10)
                        exit
                    endif
                enddo
        enddo
        
        call MaxDepthSort(YeaHisorderData,YeaSum) 
        
        !��ŵ��ļ�
        !call CreateMonDepthSortFileName(HisFileName,StaID,Month)!
        call CreateYeaDepthSortFileName(HisFileName,StaID )!����������ѩ��ʷ�����ļ���
       
        open(11,file= HisFileName, form="formatted")
        write(11,*) "    ��վ�� ��վ��                  ����ѩ���     ���"
        do j=1,YeaSum
                write(11,*) YeaHisorderData(j)
        enddo
        close(11)
        deallocate(YeaHisorderData)
        
    end subroutine YeaSnowDepthSort
    
    !------------------------------------------------------------------
    
    !-------�������ѩ�����ļ���----------------
    subroutine CreateYearFileName(YearFileName,Year)
        character*(FileNameMaxLen):: YearFileName
        integer::Year
        character*(4) YearChar
        
        write(YearChar,"(I4)")Year
        YearFileName="./Data/ResultData/SnowDepth/Year/Snow_Yea_"//YearChar//".txt"
      !  snow_yea_YYYY.txt
    end subroutine CreateYearFileName
    
    !-------�������ѩ���������ļ���------------------------------
    subroutine CreateYeaDepthSortFileName(FileName,StaID )
        implicit none
        character*(FileNameMaxLen):: FileName
        integer::StaID
        character*(5)::StaIDChar
        write(StaIDChar,"(I5)")StaID       
        FileName="./Data/ResultData/SnowDepthSort/Year/Snow_hisorder_yea_"//StaIDChar//".txt"
       ! snow_hisorder_yea_xxxxx.txt
    end subroutine CreateYeaDepthSortFileName
    !-----------------------------------------------------------
end module JL_Mod_StaSnow_Year