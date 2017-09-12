module JL_Mod_StaSnow_Year
    use JL_Mod_StaSnow_Common
    use JL_Mod_StaSnow_Season
    use JL_Mod_StaSnow_Month
    
    contains
        !-----------计算年积雪深度数据-------------------------------------
        !说明：通过提供年接口，计算该时段的最大积雪深度，
        !并将其存放到中国台站年积雪深度数据文件
        !处理过程：根据年接口，读出该时段的日积雪数据，然后
        !查找最大积雪深度数据，每个台站一个数据。最终将每个台站
        !的最大积雪深度存放到季最大积雪深度文件
    subroutine YearMaxDepth(Year)
        implicit none
            integer::Year !输入的年
            character*(FileNameMaxLen)::YearFileName !年积雪文件名
            type(YeaSnow),allocatable:: YearData(:) !动态数组，存储年积雪数据，数组维数为记录的条数
                                                      !注意，此处用的积雪数据与季的相同，但季中的season字段在年积雪数据中不要写入
            !type(StaInfo),allocatable:: StationInfo(:)
            type(MonthSnow),allocatable:: SnowData(:,:) !多月多台站的月积雪数据
            integer,allocatable::SnowDayNums(:) !一年中各台站的积雪日数
            integer,allocatable::MaxDepth(:) !一年中的所有台站的最大积雪深度值
            character*(5),allocatable:: MaxDepthDay(:) !一年中各台站的最大积雪深度日
            integer,allocatable :: MaxDepthNum(:,:) !各个积雪深度出现的日数                       
            type(FileReturnInfo):: FN !文件返回值信息
            character*(FileNameMaxLen):: StaFileName !台站信息文件名
            character*(FileNameMaxLen):: MonFileName !月积雪文件名
            integer:: MonNum !月数
            integer:: FactRL !月积雪记录个数
            integer::stat
            integer::i,j !循环计数器
            logical::alive
            
            !处理过程：（1）准备工作，将年内各月积雪数据装入年积雪动态数组中。
            !           (2)计算工作：计算年最大积雪深度，计算最大积雪深度出现日,计算各数值下的出现日数
            
            !准备工作，包括获得该年的月数，动态分配各数组并初始化     
            
            StaFileName="./Data/ResourceData/StaInfo.txt"
            FN = GetStaNumber(StaFileName,FactRL)
            if(FN.index/=0) then 
                write(*,*)"获取台站个数出错，程序将退出"
                return
            endif
             MonNum=12
             allocate(SnowData(MonNum,FactRL))
             allocate(SnowDayNums(FactRL))
             allocate(MaxDepth(FactRL)) !一年中各台站的最大积雪值
             allocate(MaxDepthDay(FactRL))!一个月中各台站的最大积雪出现日
             allocate(MaxDepthNum(FactRL,5))!符合条件的积雪日数
             allocate(YearData(FactRL)) !年积雪数据
             
             !读月数据
             do i=1,12
                call ReadMonSnowData(Year,i,SnowData(i,:),FactRL) !读月数据
             enddo
            
            
             !计算各台站的积雪日数            
            call GetSnowDayNubmers_Mon(SnowData,SnowDayNums,MonNum,FactRL) !积雪日数存放在SnowDayNums中
            
           !计算最大积雪深度,MaxSnowDepth_Mon(FactRL)数组装的是各个台站的最大积雪深度!           
            call MaxSnowDepth_Mon(SnowData,MaxDepth,MonNum,FactRL)
            
            !计算最大积雪深度出现日,由MaxDepthDay(FactRL)返回
            call MaxSnowDepthDay_Mon(SnowData,MaxDepthDay,MonNum,FactRL)
            
            !计算各层上的出现日数,由MaxDepthNum返回
            call GetSnowDepthNumbers_Mon(SnowData,MaxDepthNum,MonNum,FactRL)
            
            !生成季文件名
            !call CreateSeaFileName(SeaFileName,Year,Season)
            call CreateYearFileName(YearFileName,Year)

            open(10,File= YearFileName, form='formatted',iostat=stat)
            if(stat/=0) then 
                write(*,"(A80,'文件创建出借，程序退出')") YearFileName
            endif
            !将所有数据装载进月数据数组，并将每条记录写进文件
            write(10,*)"区站号 区站名  纬度 经度 高度 年  积雪日数 最大深度 出现日 >1 >5 >10 >20 >30"

            do i=1,FactRL
                YearData(i).StaID=SnowData(1,i).StaID
                YearData(i).StaName=SnowData(1,i).StaName
                YearData(i).Longitude=SnowData(1,i).Longitude
                YearData(i).Latitude=SnowData(1,i).Latitude
                YearData(i).StaHeight=SnowData(1,i).StaHeight
                YearData(i).Year = Year
                !YearData(i).Season = Season
                YearData(i).SnowDays = SnowDayNums(i) !积雪日数
                YearData(i).MaxDepth = MaxDepth(i) !最大积雪值
                YearData(i).MaxDay = MaxDepthDay(i) !最大积雪出现日
                YearData(i).DepthDays(1:5) =  MaxDepthNum(i,1:5)              
                
                write(10,100,iostat=stat) YearData(i).StaID,YearData(i).StaName,YearData(i).Longitude,YearData(i).Latitude,&
                                        YearData(i).StaHeight,YearData(i).Year,YearData(i).SnowDays,&
                                        YearData(i).MaxDepth,YearData(i).MaxDay,YearData(i).DepthDays(1:5)
                100 format(I8,' ',A20,F8.2,F8.2,F8.2,I6,I6,I6,' ',A5,I6,I6,I6,I6,I6)
                if(stat/=0) then
                    write(*,*) "年数据写入文件时出错，程序退出"
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
    
    !------------对年最大积雪深度排序(输入年最大积雪数据)-------------------------
    !说明：输入给定台站号， 给定起止年的年最大积雪数据，然后排序。每一站存为一个文件
    !处理过程：先读入年积雪数据，然后对其排序
    subroutine YeaSnowDepthSort(StaID,StartYear,EndYear)
        implicit none
        integer::StaID  !台站号 
        integer::StartYear,EndYear !起止年
        type(MaxDepthHisorder),allocatable:: YeaHisorderData(:) !要排序的数据,数组维数大小由1951到当前的总年数
        type(YeaSnow) :: SnowData !读年最大积雪数据
        type(FileReturnInfo):: FN
        integer :: YeaSum !年的总数 
        character*(FileNameMaxLen) :: HisFileName !历史排序文件名
        character*(FileNameMaxLen) :: MaxFileName !月最大积雪文件名
        character*(FileNameMaxLen) :: StaFileName !台站文件名
        integer :: StaNum !台站个数
        integer :: i,j,k !循环计数器
        integer :: stat
        logical :: alive
        
        !处理过程：（1）读入月积雪数据；（2）排序并写入文件
        YeaSum = EndYear-StartYear + 1  !计算年总数
        allocate(YeaHisorderData(YeaSum))
        !获取台站数
        StaFileName="./Data/ResourceData/StaInfo.txt"
        FN = GetStaNumber(StaFileName,StaNum)
        if(FN.index /=0) then
                write(*,*)"获取台站个数出错，检查台站信息文件是否存在或数据是否正确"
                return
        endif 
         
         !将台站号为StaID的月积雪数据读入到待排序数组中     
        do i=StartYear,EndYear                
                call CreateYearFileName(MaxFileName,i) !生成季最大积雪文件名                
                inquire(file=MaxFileName,exist=alive) !检查文件是否存在
                if(.not. alive) then
                    write(*,"(A80,'文件不存在')")MaxFileName
                    return
                endif
                open(10,file=MaxFileName,iostat=stat) 
                if(stat/=0) then
                    write(*,"('打开季最大积雪数据文件',A80,'时出错，检查文件数据是否正确')")MaxFileName
                    return
                endif 
                
                read(10,*) !先读出第一行字符串       
                do j=1,StaNum !查找StaID，如果找到，则将相关信息读进待排序的数组，然后退出循环 
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
        
        !存放到文件
        !call CreateMonDepthSortFileName(HisFileName,StaID,Month)!
        call CreateYeaDepthSortFileName(HisFileName,StaID )!生成年最大积雪历史排序文件名
       
        open(11,file= HisFileName, form="formatted")
        write(11,*) "    区站号 区站名                  最大积雪深度     年份"
        do j=1,YeaSum
                write(11,*) YeaHisorderData(j)
        enddo
        close(11)
        deallocate(YeaHisorderData)
        
    end subroutine YeaSnowDepthSort
    
    !------------------------------------------------------------------
    
    !-------生成年积雪数据文件名----------------
    subroutine CreateYearFileName(YearFileName,Year)
        character*(FileNameMaxLen):: YearFileName
        integer::Year
        character*(4) YearChar
        
        write(YearChar,"(I4)")Year
        YearFileName="./Data/ResultData/SnowDepth/Year/Snow_Yea_"//YearChar//".txt"
      !  snow_yea_YYYY.txt
    end subroutine CreateYearFileName
    
    !-------生成年积雪数据排序文件名------------------------------
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