module JL_ModStaSnow_Ten
    use JL_Mod_StaSnow_Common
    implicit none
        type TenSnow
            integer:: StaID !区站号
            character*(StaNameMaxLen)::StaName !区站名称
            real(8)::Longitude !区站纬度
            real(8)::Latitude !区站经度
            real(8)::StaHeight !区站高度
            integer(4)::Year !年
            integer(4)::Month !月
            integer(2)::Ten !旬
            integer(4)::MaxDep !最大积雪深度，有缺测值和微量值 
        end type TenSnow
    contains
    
    !-------------旬功能处理模块------------------------------- 
       
        !---------计算旬最大积雪深度并保存---------------------------
        !说明：通过提供年，月，旬接口，计算该时段的最大积雪深度，
        !并将其存放到中国台站旬积雪深度数据文件
        !处理过程：根据年月旬接口，读出该时段的日积雪数据，然后
        !查找最大积雪深度数据，每个台站一个数据。最终将每个台站
        !的最大积雪深度存放到旬最大积雪深度文件
        subroutine TenMaxDepth(Year,Month,Ten)
            integer :: Year,Month,Ten !年月旬
            character*(FileNameMaxLen):: DayFileName,TenFileName,StaFileName !日积雪文件名，旬积雪文件名,台站信息文件名
            type(DaySnow),allocatable:: SnowData(:,:) !积雪数据动态数组，将入M天N个台站的积雪数据
            type(TenSnow),allocatable:: TenData(:) !旬数据
            integer,allocatable:: MaxDepth(:) !最大积雪深度动态数组，维数为台站个数
            type(FileReturnInfo):: FN !文件返回值信息
            integer :: i,stat
            integer:: DayNum,StaNum !分别为天数和台站数
            integer:: TenNum(3)
            integer:: StartD,EndD !旬的开始日和结束日
            
            call CreateTenFileName(TenFileName,Year,Month,Ten)!生成旬文件名TenFileName
            if(GetTenDayNum(TenNum,Year,Month,Ten)) then !获取旬的天数及起止日
                  DayNum=TenNum(1) !旬内的天数
                  StartD=TenNum(2) !旬的开始日
                  EndD = TenNum(3)  !旬的结束日
            endif
            
            !获取台站信息及台站个数
            StaFileName="./Data/ResourceData/StaInfo.txt"
            FN = GetStaNumber(StaFileName,StaNum)
            if(FN.index/=0) then 
                write(*,*)"获取台站个数出错，程序将退出"
                return
            endif
            
            !动态分配各数组
            allocate(SnowData(DayNum,StaNum))
            allocate(TenData(StaNum))
            allocate(MaxDepth(StaNum))
            
            !将旬内数据装载到SnowData数组
            do i=1,DayNum
                call ReadDaySnowData(Year,Month,i+StartD-1,SnowData(i,:),StaNum)
            enddo

            !计算旬内最大积雪深度值
            call MaxSnowDepth_common(SnowData,MaxDepth,DayNum,StaNum) !最大积雪深度
          
            !生成旬数据，并将旬数据存入旬积雪深度数据文件
            call CreateTenFileName(TenFileName,Year,Month,Ten)!生成旬文件名TenFileName
            open(10,file=TenFileName,form='formatted',iostat=stat)
            if(stat/=0) then
                write(*,*) "创建旬最大深度数据文件时出错，程序退出"
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
        
        !--------旬积雪深度历史排序-----------------------------------
        !说明：中国台站旬最大积雪深度历史排序，每一站的每个日期最大积雪
        !深度资料存为一个数据文件。求MM月DD日的最大积雪深度排序。
        !处理过程：给定某旬，将每个台站在该旬的积雪最大深度进行排序
        !注意：该模块在处理前必须先计算完成旬最大积雪数据，数据源为该数据
        subroutine TenSnowDepthSort(StaID,Month,Ten,StartYear,EndYear)
            implicit none
            integer::StaID,Month,Ten !台站号，月，日
            integer::StartYear,EndYear !起止年
            type(MaxDepthHisorder),allocatable:: TenHisorderData(:) !要排序的数据,数组维数大小由1951到当前的旬总数
            type(TenSnow) :: SnowData !读旬最大积雪数据
            type(FileReturnInfo):: FN
            integer :: TenSum !旬的总数 
            character*(FileNameMaxLen) :: HisFileName !历史文件名
            character*(FileNameMaxLen) :: MaxFileName !旬最大积雪文件名
            character*(FileNameMaxLen) :: StaFileName !旬最大积雪文件名
            integer :: StaNum !台站个数
            integer :: i,j,k !循环计数器
            integer :: stat
            logical :: alive
            
            !需要进行的处理包括：生成文件名，读取该台站在历年的数据，然后进行排序
            
            TenSum = EndYear-StartYear + 1  !计算旬总数
            allocate(TenHisorderData(TenSum))
            !获取台站数
            StaFileName="./Data/ResourceData/StaInfo.txt"
            FN = GetStaNumber(StaFileName,StaNum)
            if(FN.index /=0) then
                write(*,*)"获取台站个数出错，检查台站信息文件是否存在或数据是否正确"
                return
            endif
            !读旬最大积雪数据文件中的旬数据
            !产生文件名，打开文件，读每一条记录，
            !判断是否为现在要计算的站:如果不是，则继续读下一条数据;如果是，则读出该站的站号，站名，最大深度，年份
            
            do i=StartYear,EndYear
                call CreateTenFileName(MaxFileName,i,Month,Ten) !生成旬最大积雪文件名
                inquire(file=MaxFileName,exist=alive) !检查文件是否存在
                if(.not. alive) then
                    write(*,"(I80,'文件不存在')")MaxFileName
                    return
                endif
                open(10,file=MaxFileName,iostat=stat) 
                if(stat/=0) then
                    write(*,"('打开旬最大积雪文件',I80,'时出错，检查文件数据是否正确')")MaxFileName
                    return
                endif
                
                do j=1,StaNum !查找StaID，如果找到则将相关信息读进待排序的数组，然后退出循环
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
            
            !排序
            !write(*,*) TenHisorderData(1:tensum)
            call MaxDepthSort(TenHisorderData,TenSum)            
            !存放到文件
            call CreateTenDepthSortFileName(HisFileName,StaID,Month,Ten)!生成旬最大积雪历史排序文件名
            open(11,file= HisFileName, form="formatted")
            
            write(11,*) "    区站号 区站名                  最大积雪深度     年份"
            do j=1,TenSum
                write(11,*) TenHisorderData(j)
            enddo
            close(11)
            deallocate(TenHisorderData)
        end subroutine TenSnowDepthSort
        !-------------------------------------------------------------
        
    !----------旬功能处理模块结束-------------------------------
    
    
    
    
    
    
    
    !----------旬处理辅助函数-----------------------------------
    
        !--------生成旬最大积雪深度文件名-----------------------------------
        !说明：根据年月旬接口生成文件名,通过TenFileName返回
        subroutine CreateTenFileName(TenFileName,Year,Month,Ten)
            implicit none 
            character*(FileNameMaxLen):: TenFileName
            integer:: Year,Month,Ten !年月旬
            character*(4):: YearChar !年字符
            character*(2):: MonthChar !月字符
            character*(1):: TenChar !旬字符
            integer:: stat
            
            write(YearChar,"(I4)",iostat=stat)Year !将年份由整型转换成字符串
                if(stat/=0) write(*,*)"年份数字转换出错" 
        
            write(monthChar,"(I2)",iostat=stat)month
                if(stat/=0) write(*,*)"月份数字转换出错" 
        
            if(month>0 .and. month<=9) then !将月份由整型转换成字符串
                 write(monthChar,"(I2)") Month
                 MonthChar='0'//adjustl(MonthChar)
            endif        
            if(month>=10 .and. month<=12) then
                write(monthChar,"(I2)") Month
            endif        
            if(month<1 .or. month >12) then
                write(*,*) "输入的月份超出了1-12，程序退出"
            return
            endif
            
            write(TenChar,"(I1)",iostat=stat)Ten !将年份由整型转换成字符串
                if(stat/=0) write(*,*)"旬数字转换出错" 
 
        TenFileName="./Data/ResultData/SnowDepth/Ten/Snow_10day_"//yearChar//MonthChar//TenChar//".txt"  !合成文件名 
        return
        end subroutine CreateTenFileName
        !-------------------------------------------------------
    
        !--------获取某旬的天数及起止时间---------------------------------
        !说明:该函数获取三个值存放在TenNum数组中，第一个值为天数，第二值为
        !起始日期，第三值为结束日期。返回值为True表示获取成功，否则不成功
        logical function GetTenDayNum(TenNum,Year,Month,Ten)
            integer :: Year,Month,Ten 
            integer :: TenNum(3)        !旬天数及起止日期
            
            if(Month<1 .or. Month>12) then
                write(*,"('月份为',I2,'月份值必须在1-12之间!')") Month
                GetTenDayNum = .false.
                return
            endif
            if(Ten<1 .or. Ten >3) then
                write(*,"('旬值为',I1,'旬值必须在1-3之间')")Ten
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
        
        !-----------生成旬最大积雪深度历史排序文件名-----------
        subroutine CreateTenDepthSortFileName(FileName,StaID,Month,Ten)
            implicit none
            character*(FileNameMaxLen) :: FileName
            integer:: StaID,Month,Ten !台站号，月，旬
            character*(5):: StaIDChar !台站号字符
            character*(2):: MonthChar !月字符
            character*(1):: TenChar !旬字符
            integer :: stat
            write(StaIDChar,"(I5)",iostat=stat)StaID !将台站号由整型转换成字符串
                if(stat/=0) write(*,*)"台站号数字转换出错" 
        
!            write(MonthChar,"(I2)",iostat=stat)Month
!                if(stat/=0) write(*,*)"月份数字转换出错" 
        
            if(month>0 .and. month<=9) then !将月份由整型转换成字符串
                 write(monthChar,"(I2)") Month
                 MonthChar='0'//adjustl(MonthChar)
            endif        
            if(month>=10 .and. month<=12) then
                write(monthChar,"(I2)") Month
            endif        
            if(month<1 .or. month >12) then
                write(*,*) "输入的月份超出了1-12，程序退出"
            return
            endif
            
            write(TenChar,"(I1)",iostat=stat)Ten !将旬由整型转换成字符串
                if(stat/=0) write(*,*)"旬数字转换出错" 
 
        FileName="./Data/ResultData/SnowDepthSort/Ten/Snow_hisorder_10day_"//MonthChar//TenChar//'_'//StaIDChar//".txt"  !合成文件名 
        return
        
        end subroutine CreateTenDepthSortFileName
    !----------旬处理辅助函数结束-------------------------------
end module JL_ModStaSnow_Ten