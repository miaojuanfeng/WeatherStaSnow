module JL_Mod_StaSnow_Day
    use JL_Mod_StaSnow_Common
    contains
        !-----中国台站日积雪深度历史排序------------------
        !功能：每一站的每个日期积雪深度数据进行排序并存为一个数据文件
        subroutine DayDepthSort(StaID,Month,Day,StartYear,EndYear)
            implicit none
            integer::StaID,Month,Day !台站号，月，日
            integer::StartYear,EndYear !起止年
            
            type(MaxDepthHisorder),allocatable:: DayHisorderData(:) !要排序的数据,数组维数大小由1951到当前的总天数
            type(DaySnow) :: SnowData !读日积雪数据
            type(FileReturnInfo):: FN
            integer :: DaySum !日的总数 
            character*(FileNameMaxLen) :: HisFileName !历史日积雪文件名
            character*(FileNameMaxLen) :: DayFileName !日积雪文件名
            character*(FileNameMaxLen) :: StaFileName !台站文件名
            integer :: StaNum !台站个数
            integer :: i,j,k !循环计数器
            integer :: stat
            logical :: alive
            
            DaySum = EndYear-StartYear + 1  !计算总日数
            allocate(DayHisorderData(DaySum))
            !获取台站数
            StaFileName="./Data/ResourceData/StaInfo.txt"
            FN = GetStaNumber(StaFileName,StaNum)
            if(FN.index /=0) then
                write(*,*)"获取台站个数出错，检查台站信息文件是否存在或数据是否正确"
                return
            endif
            !读日积雪数据文件中的日积雪数据
            !产生文件名，打开文件，读每一条记录，
            !判断是否为现在要计算的站:如果不是，则继续读下一条数据;如果是，则读出该站的站号，站名，最大深度，年份
            
            do i=StartYear,EndYear
                call CreateDayFileName(DayFileName,i,Month,Day) !生成日积雪文件名
                !call CreateDayFileName(FileName,Year,Month,Day)
                inquire(file=DayFileName,exist=alive) !检查文件是否存在
                if(.not. alive) then
                    write(*,"(I80,'文件不存在')")DayFileName
                    return
                endif
                open(10,file=DayFileName,iostat=stat) 
                if(stat/=0) then
                    write(*,"('打开日积雪文件',I80,'时出错，检查文件数据是否正确')")DayFileName
                    return
                endif
                
                do j=1,StaNum !查找StaID，如果找到则将相关信息读进待排序的数组，然后退出循环
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
            
            !排序         
            call MaxDepthSort(DayHisorderData,DaySum)            
            !存放到文件
            call CreateDayDepthSortFileName(HisFileName,StaID,Month,Day)!生成日积雪历史排序文件名
            open(11,file= HisFileName, form="formatted")
            
            do j=1,DaySum
                write(11,*) DayHisorderData(j)
            enddo
            close(11)
            deallocate(DayHisorderData)
        end subroutine DayDepthSort
        !-------------------------------------------------
        
        
        !---------------生成日积雪历史排序文件名-----------------------------
        subroutine CreateDayDepthSortFileName(HisFileName,StaID,Month,Day)
            implicit none 
            character*(FileNameMaxLen):: HisFileName
            integer:: StaID,Month,Day !台站号，月，日
            character*(5):: StaIDChar !台站号字符
            character*(2):: MonthChar !月字符
            character*(2):: DayChar !日字符
            integer:: stat
            
            write(StaIDChar,"(I5)",iostat=stat)StaID !将台站号由整型转换成字符串
                if(stat/=0) write(*,*)"台站号数字转换出错" 
        
            write(monthChar,"(I2)",iostat=stat)month
                if(stat/=0) write(*,*)"月份数字转换出错" 
        
            if(month>0 .and. month<=9) MonthChar='0'//adjustl(MonthChar)
            if(month<1 .or. month >12) then
                write(*,*) "输入的月份超出了1-12，程序退出"
            return
            endif
            
            write(DayChar,"(I2)")Day 
            if(Day>0 .and. Day<=9)  DayChar = '0'//adjustl(DayChar)               
 
            HisFileName="./Data/ResultData/SnowDepthSort/Day/Snow_hisorder_day_"//MonthChar//DayChar//'_'//StaIDChar//".txt"  !合成文件名 
            return
        
        end subroutine CreateDayDepthSortFileName

end module JL_Mod_StaSnow_Day