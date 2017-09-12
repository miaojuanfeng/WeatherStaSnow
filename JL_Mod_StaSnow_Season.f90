module JL_Mod_StaSnow_Season
    use JL_Mod_StaSnow_Common
    implicit none
    
    contains
    
    !-----------计算季积雪深度数据-------------------------------------
    !说明：通过提供年，季接口，计算该时段的最大积雪深度，
        !并将其存放到中国台站季积雪深度数据文件
        !处理过程：根据年季接口，读出该时段的日积雪数据，然后
        !查找最大积雪深度数据，每个台站一个数据。最终将每个台站
        !的最大积雪深度存放到季最大积雪深度文件
    subroutine SeaMaxDepth(Year,Season)
        implicit none
            integer::Year,Season !输入的年季
            character*(FileNameMaxLen)::SeaFileName !季积雪文件名
            type(SeaSnow),allocatable:: SeaData(:) !动态数组，存储季积雪数据，数组维数为记录的条数
            type(StaInfo),allocatable:: StationInfo(:)
            type(MonthSnow),allocatable:: SnowData(:,:) !多月多台站的月积雪数据
            integer,allocatable::SnowDayNums(:) !一季中各台站的积雪日数
            integer,allocatable::MaxDepth(:) !一季中的所有台站的最大积雪深度值
            character*(5),allocatable:: MaxDepthDay(:) !一季中各台站的最大积雪深度日
            integer,allocatable :: MaxDepthNum(:,:) !各个积雪深度出现的日数                       
            type(FileReturnInfo):: FN !文件返回值信息
            character*(FileNameMaxLen):: StaFileName !台站信息文件名
            character*(FileNameMaxLen):: MonFileName !月积雪文件名
            integer:: MonNum !月数
            integer:: FactRL !月积雪记录个数
            integer::stat
            integer::i,j !循环计数器
            logical::alive
            
            !处理过程：（1）准备工作，将季内各月积雪数据装入季积雪动态数组中。
            !           (2)计算工作：计算季最大积雪深度，计算最大积雪深度出现日,计算各数值下的出现日数
            
            !准备工作，包括获得该季的月数，动态分配各数组并初始化     
            
            StaFileName="./Data/ResourceData/StaInfo.txt"
            FN = GetStaNumber(StaFileName,FactRL)
            if(FN.index/=0) then 
                write(*,*)"获取台站个数出错，程序将退出"
                return
            endif
             MonNum=3
             allocate(SnowData(MonNum,FactRL))
             allocate(SnowDayNums(FactRL))
             allocate(MaxDepth(FactRL)) !一季中各台站的最大积雪值
             allocate(MaxDepthDay(FactRL))!一个月中各台站的最大积雪出现日
             allocate(MaxDepthNum(FactRL,5))!符合条件的积雪日数
             allocate(SeaData(FactRL)) !季积雪数据
             
            if(Season==1) then !春季
                do i=1,3
                    call ReadMonSnowData(Year,i+2,SnowData(i,:),FactRL) !读月数据
                enddo
            endif
            if(Season==2) then !夏季
                do i=1,3
                    call ReadMonSnowData(Year,i+5,SnowData(i,:),FactRL) !读月数据
                enddo
            endif
            if(Season==3) then !秋季
                do i=1,3
                    call ReadMonSnowData(Year,i+8,SnowData(i,:),FactRL) !读月数据
                enddo
            endif
            if(Season==4) then !冬季
                call ReadMonSnowData(Year,12,SnowData(1,:),FactRL) !读12月数据
                call ReadMonSnowData(Year+1,1,SnowData(2,:),FactRL) !读次年1月数据
                call ReadMonSnowData(Year+1,2,SnowData(3,:),FactRL) !读次年2月数据                
            endif
            
             !计算各台站的积雪日数            
            call GetSnowDayNubmers_Mon(SnowData,SnowDayNums,MonNum,FactRL) !积雪日数存放在SnowDayNums中
            
           !计算最大积雪深度,MaxSnowDepth_Mon(FactRL)数组装的是各个台站的最大积雪深度!           
            call MaxSnowDepth_Mon(SnowData,MaxDepth,MonNum,FactRL)
            
            !计算最大积雪深度出现日,由MaxDepthDay(FactRL)返回
            call MaxSnowDepthDay_Mon(SnowData,MaxDepthDay,MonNum,FactRL)
            
!            !计算各层上的出现日数,由MaxDepthNum返回
            call GetSnowDepthNumbers_Mon(SnowData,MaxDepthNum,MonNum,FactRL)
            
            !生成季文件名
            call CreateSeaFileName(SeaFileName,Year,Season)

            open(10,File= SeaFileName, form='formatted',iostat=stat)
            if(stat/=0) then 
                write(*,"(I80,'文件创建出借，程序退出')") SeaFileName
            endif
            !将所有数据装载进月数据数组，并将每条记录写进文件
            write(10,*)"区站号 区站名  纬度 经度 高度 年 季 积雪日数 最大深度 出现日 >1 >5 >10 >20 >30"

            do i=1,FactRL
                SeaData(i).StaID=SnowData(1,i).StaID
                SeaData(i).StaName=SnowData(1,i).StaName
                SeaData(i).Longitude=SnowData(1,i).Longitude
                SeaData(i).Latitude=SnowData(1,i).Latitude
                SeaData(i).StaHeight=SnowData(1,i).StaHeight
                SeaData(i).Year = Year
                SeaData(i).Season = Season
                SeaData(i).SnowDays = SnowDayNums(i) !积雪日数
                SeaData(i).MaxDepth = MaxDepth(i) !最大积雪值
                SeaData(i).MaxDay = MaxDepthDay(i) !最大积雪出现日
                SeaData(i).DepthDays(1:5) =  MaxDepthNum(i,1:5)              
                
                write(10,100,iostat=stat) SeaData(i).StaID,SeaData(i).StaName,SeaData(i).Longitude,SeaData(i).Latitude,&
                                        SeaData(i).StaHeight,SeaData(i).Year,SeaData(i).Season,SeaData(i).SnowDays,&
                                        SeaData(i).MaxDepth,SeaData(i).MaxDay,SeaData(i).DepthDays(1:5)
                100 format(I8,' ',A20,F8.2,F8.2,F8.2,I6,I4,I6,I6,' ',A5,I6,I6,I6,I6,I6)
                if(stat/=0) then
                    write(*,*) "季数据写入文件时出错，程序退出"
                    close(10)
                    return
                endif
            enddo
            
            deallocate(SnowData)
            deallocate(SnowDayNums)
            deallocate(MaxDepth) !一个月中各台站的最大积雪值
            deallocate(MaxDepthNum)
            deallocate(MaxDepthDay)!一个月中各台站的最大积雪出现日
            deallocate(SeaData) !季积雪数据
            close(10)
    end subroutine SeaMaxdepth
    !------------------------------------------------------------------
    
    !------------对季最大积雪深度排序(输入季最大积雪数据)-------------------------
    !说明：输入给定季，给定台站号，给定起止年的季最大积雪数据，然后排序。每一站每季存为一个文件
    !处理过程：先读入季积雪数据，然后对其排序
    subroutine SeaSnowDepthSort(StaID,Season,StartYear,EndYear)
        implicit none
        integer::StaID,Season !台站号，季
        integer::StartYear,EndYear !起止年
        type(MaxDepthHisorder),allocatable:: SeaHisorderData(:) !要排序的数据,数组维数大小由1951到当前的总季数
        type(SeaSnow) :: SnowData !读季最大积雪数据
        type(FileReturnInfo):: FN
        integer :: SeaSum !季的总数 
        character*(FileNameMaxLen) :: HisFileName !历史排序文件名
        character*(FileNameMaxLen) :: MaxFileName !季最大积雪文件名
        character*(FileNameMaxLen) :: StaFileName !台站文件名
        integer :: StaNum !台站个数
        integer :: i,j,k !循环计数器
        integer :: stat
        logical :: alive
        
        !处理过程：（1）读入季积雪数据；（2）排序并写入文件
        SeaSum = EndYear-StartYear + 1  !计算季总数
        allocate(SeaHisorderData(SeaSum))
        !获取台站数
        StaFileName="./Data/ResourceData/StaInfo.txt"
        FN = GetStaNumber(StaFileName,StaNum)
        if(FN.index /=0) then
                write(*,*)"获取台站个数出错，检查台站信息文件是否存在或数据是否正确"
                return
        endif 
         
         !将台站号为StaID的季积雪数据读入到待排序数组中     
        do i=StartYear,EndYear                
                call CreateSeaFileName(MaxFileName,i,Season) !生成季最大积雪文件名
                inquire(file=MaxFileName,exist=alive) !检查文件是否存在
                if(.not. alive) then
                    write(*,"(I80,'文件不存在')")MaxFileName
                    return
                endif
                open(10,file=MaxFileName,iostat=stat) 
                if(stat/=0) then
                    write(*,"('打开季最大积雪数据文件',A80,'时出错，检查文件数据是否正确')")MaxFileName
                    return
                endif 
                
                read(10,*) !先读出第一行字符串       
                do j=1,StaNum !查找StaID，如果找到，则将相关信息读进待排序的数组，然后退出循环                    
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
        
        !存放到文件
        !call CreateMonDepthSortFileName(HisFileName,StaID,Month)!生成月最大积雪历史排序文件名
        call CreateSeaDepthSortFileName(HisFileName,StaID,Season)
        open(11,file= HisFileName, form="formatted")
            
        write(11,*) "    区站号 区站名                  最大积雪深度     年份"
        do j=1,SeaSum
                write(11,*) SeaHisorderData(j)
        enddo
        close(11)
        deallocate(SeaHisorderData)
        
    end subroutine SeaSnowDepthSort
    
    !------------------------------------------------------------------
    
    
    !----------生成季最大积雪深度文件名--------------------------------
    subroutine CreateSeaFileName(SeaFileName,Year,Season)
        implicit none
        integer Year,Season !年，季
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
    
    !---------生成季积雪深度排序文件名---------------------------------
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
    
    !-------------通用积雪日数(输入为月积雪数据)--------------------------------------
        !说明：求多月内各个台站的积雪日数，通过SnowdayNums返回
        subroutine GetSnowDayNubmers_Mon(SnowData,SnowDayNums,MonNum,StaNum)
            implicit none
            integer :: MonNum !天数
            integer :: StaNum !台站数
            type(MonthSnow):: SnowData(MonNum,StaNum) !MonNum月的积雪数据，每月的记录条数为StaNum
            integer :: SnowDayNums(StaNum)
            integer :: i,j
            
            SnowDayNums(1:StaNum)=0 !初始化天数全为0
            
            do i=1,StaNum !各台站
                do j=1,MonNum !各月                    
                        SnowDayNums(i) = SnowDayNums(i) + SnowData(j,i).SnowDays                   
                enddo
            enddo
            return
        end subroutine GetSnowDayNubmers_Mon
        !---------------------------------------------------------------
        
        !----------通用最大积雪深度(输入为月积雪数据)-------------------
        !说明：给入月积雪数据，计算出每个台站在某段时间内的最大积雪深度
        !SnowData为MonNum X StaNum 的二维数组，MonNum代表月数，StaNum代表台站数
        !MaxDepth是一维数组，维数为台站个数，该数组返回各个台站的最大积雪深度
        subroutine MaxSnowDepth_Mon(SnowData,MaxDepth,MonNum,StaNum)
            integer :: MonNum !天数
            integer :: StaNum !台站数
            type(MonthSnow):: SnowData(MonNum,StaNum) !MonNum月的积雪数据，每日的记录条件为StaNum
            integer:: MaxDepth(StaNum) !最大积雪深度，该数组将返回
            integer:: DepthData(MonNum) !某个台在MonNum月内的积雪深度数据
            integer:: i,j !循环计数
           ! write(*,*)"求最大积雪深度时装载的深度值"
            do i=1,StaNum !每个台站进行计算
                do j=1,MonNum !每个台站的所有日期的积雪深度装入DepthData数组
                    DepthData(j)= SnowData(j,i).MaxDepth                               
                enddo
                !if(i<=10) write(*,*)DepthData   
                MaxDepth(i)=FindMaxDepth(DepthData,MonNum) !查找每个台站的所有日期的最大积雪深度
            enddo           
            
            return
        end subroutine MaxSnowDepth_Mon
        !---------------------------------------------------------------
        
        !--------查找多个积雪深度中的最大积雪深度值(输入为月积雪数据)------------------
        !SnowDepth(n)数组中为多个积雪深度数据，找出最大值，函数返回为该最大积雪深度值
        integer function FindMonMaxDepth(SnowDepth,n)
            integer :: n !数组大小维数
            integer :: SnowDepth(n) !积雪数据数组
            integer :: Temp(n) !临时数组存储处理过的积雪数据，处理是将缺测值和微量值变为0，以方便求最大值
            integer :: i !循环计数          
            integer :: Max  !找最大值
            integer :: cnt1,cnt2 !记录缺测(cnt1)和记录微量值(cnt2)出现的次数
            cnt1=0
            cnt2=0
            
            !对深度数据做预处理，将缺测值和微量值全变为0，方便找最大值。
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
                if(temp(i)>max) max=temp(i)!找最大值
            enddo
            
            if(cnt1+cnt2==n) then !全为缺测和微量值
                if(cnt2>0) then !有微量值
                    FindMonMaxDepth=MicroscaleVal
                    return
                else !全为缺测值
                    FindMonMaxDepth=DefaultVal
                    return
                endif
            endif
            
            FindMonMaxDepth = max
            return
        end function FindMonMaxDepth
        !---------------------------------------------------------------

        !--------求最大积雪深度出现日(又月为输入数据)-------------------------
        subroutine MaxSnowDepthDay_Mon(SnowData,MaxDepthDay,MonNum,FactRL)
            implicit none
            integer::MonNum,FactRL !月数，每个月内的记录条数
            type(MonthSnow)::SnowData(MonNum,FactRL) !月积雪数据
            integer:: MaxDepth(FactRL) !季最大积雪深度
            character*(5):: MaxDepthDay(FactRL) !每个台站在MonNum月内的最大积雪日,如果只有1次出现，则为MMDD，否则为9950+n
            integer::MM,DD !月，日
            character*(2)::MMch,DDch !月字符，日字符
            integer::i,j
            integer::n !积雪深度出现的次数
            
            call MaxSnowDepth_Mon(SnowData,MaxDepth,MonNum,FactRL) !求最大积雪深度
            
            do i=1,FactRL
                n=0
                do j=1,MonNum
                        if(MaxDepth(i)==Defaultval) then !若最大积雪深度为缺测值，则日数为缺测值
                            write(MaxDepthDay(i),"(I5)")DefaultVal
                            exit
                    endif
                    if(MaxDepth(i)==MicroscaleVal) then !若最大积雪深度为微量值，则日数为微量值                        
                        write(MaxDepthDay(i),"(I5)")MicroscaleVal
                        exit
                    endif
                    if(SnowData(j,i).MaxDepth==MaxDepth(i))  then
                        if(SnowData(j,i).MaxDay<BaseVal)then !该月只出现一次最大积雪深度日
                            n=n+1
                            if(n==1) then
                                MM=SnowData(j,i).Month
                                DD=SnowData(j,i).MaxDay
                            endif                        
                        else  !该月出现多次最大积雪深度日
                            n=n + SnowData(j,i).MaxDay - BaseVal
                        endif
                    endif
                    if(n==1) MaxDepthDay(i)=MMDDchar(MM,DD) !将月和日转为MMDD型的字符串
                    if(n>1) write(MaxDepthDay(i),"(I5)")BaseVal + n !将形如9952的数字转为字符串并赋值
                enddo
            enddo
            return
        end subroutine MaxSnowDepthDay_Mon
        !---------------------------------------------------------------
        
         !-------------通用符合条件的深度出现日数(输入为月积雪数据)----------------------
        !说明：从多个月积雪数据中获取每个台站>=1cm,5cm,10cm,20cm,30cm的天数
        !SnowData(MonNum,StaNum)存放多月多台站积雪数据，MaxDepthNum(StaNum)存放各个层次的出现日数
        !MonNum为月数，StaNum为台站数
        subroutine GetSnowDepthNumbers_Mon(SnowData,MaxDepthNum,MonNum,StaNum)
            implicit none
            integer :: MonNum !天数
            integer :: StaNum !台站数
            type(MonthSnow):: SnowData(MonNum,StaNum) !MonNum天的日积雪数据，每日的记录条件为StaNum
            integer :: MaxDepthNum(StaNum,5) !各个积雪深度出现的日数
            integer i,j !循环计数
            
            MaxDepthNum(1:StaNum,1:5)=0
            do i=1,StaNum !各个台站
                do j=1,MonNum !各月
                    MaxDepthNum(i,:)=MaxDepthNum(i,:)+SnowData(j,i).DepthDays(:)
                enddo       
            enddo
            return
            
        end subroutine GetSnowDepthNumbers_Mon              
        !---------------------------------------------------------------
       
        !------将月日数字转为字符串----------------------------------------
        character*(5) function MMDDchar(MM,DD)
            integer::MM,DD !月，日
            character*(2)::MMch,DDch !月字符，日字符
            
            write(MMch,"(I2)")MM
            if(MM>0 .and. MM<=9) MMch="0"//adjustl(MMch)
            write(DDch,"(I2)")DD
            if(DD>0 .and. DD<=9) DDch="0"//adjustl(DDch)
            MMDDchar=MMch//DDch//' '
            
        end function MMDDchar
        !------------------------------------------------------------------
        
end module JL_Mod_StaSnow_Season