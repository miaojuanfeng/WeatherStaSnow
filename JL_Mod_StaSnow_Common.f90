module JL_Mod_StaSnow_Common
    implicit none
        integer,parameter:: FileNameMaxLen=80 !文件名最大长度
        integer,parameter:: StaNameMaxLen=20 !台站文件名最大长度
        integer,parameter:: MaxRecordLen=10000 !数据记录最大长度
        integer,parameter:: BaseVal=9950 !最大积雪出现日为多日时所加的基数
        integer,parameter:: DefaultVal = 32766 !积雪深度，积雪日数的缺测值
        integer,parameter:: MicroscaleVal=32744 !积雪深度，积雪日数的微量值
        
        !-------------文件读写返回值含义----------------------------
        type FileReturnInfo
            integer:: index !取值为0，-1，-2，-3，-4
            character*(50)::info !-1为文件打开出错，-2为台站文件为空，无数据,
                                    !0为读取文件记录数结束,-3为读取文件记录时出错，-4为文件不存在
        end type FileReturnInfo
        !------------------------------------------------------------
       
        !------------------------台站基本信息结构--------------------
        type StaInfo
            integer:: StaID !区站号
            character*(StaNameMaxLen)::StaName !区站名称
            real(8)::Longitude !区站纬度
            real(8)::Latitude !区站经度
            real(8)::StaHeight !区站高度
         end type StaInfo
        !------------------------------------------------------------
       
       !----------日积雪深度数据------------------------------------
        type DaySnow !日积雪数据
            integer:: StaID !区站号
            character*(StaNameMaxLen)::StaName !区站名称
            real(8)::Longitude !区站纬度
            real(8)::Latitude !区站经度
            real(8)::StaHeight !区站高度
            integer(4)::Year !年
            integer(4)::Month !月
            integer(2)::Day !日
            integer(4)::Depth !积雪深度，有缺测值和微量值           
        end type DaySnow
       !------------------------------------------------------------  
       
       !------------月积雪数据--------------------------------------
        type MonthSnow !月积雪数据
            integer:: StaID !区站号
            character*(StaNameMaxLen)::StaName !区站名称
            real(8)::Longitude !区站纬度
            real(8)::Latitude !区站经度
            real(8)::StaHeight !区站高度
            integer(4)::Year !年
            integer(4)::Month !月
            integer(2)::SnowDays !积雪日数
            integer(4)::MaxDepth !最大积雪深度
            integer(2)::MaxDay !最大积雪深度出现日
            integer(2),dimension(5)::DepthDays  !积雪深度出现的日数,1cm,5cm,10cm,20cm,30cm
        end type MonthSnow
       !------------------------------------------------------------
       
       !---------季积雪数据-----------------------------------------
        type SeaSnow
            integer:: StaID !区站号
            character*(StaNameMaxLen)::StaName !区站名称
            real(8)::Longitude !区站纬度
            real(8)::Latitude !区站经度
            real(8)::StaHeight !区站高度
            integer(4)::Year !年
            integer(4)::Season !季
            integer(2)::SnowDays !积雪日数
            integer(4)::MaxDepth !最大积雪深度
            character*(5)::MaxDay !最大积雪深度出现日(MMDD)
            integer(2),dimension(5)::DepthDays  !积雪深度出现的日数,1cm,5cm,10cm,20cm,30cm
       end type SeaSnow
       !------------------------------------------------------------
       
       !---------年积雪数据-----------------------------------------
        type YeaSnow
            integer:: StaID !区站号
            character*(StaNameMaxLen)::StaName !区站名称
            real(8)::Longitude !区站纬度
            real(8)::Latitude !区站经度
            real(8)::StaHeight !区站高度
            integer(4)::Year !年
            !integer(4)::Season !季
            integer(2)::SnowDays !积雪日数
            integer(4)::MaxDepth !最大积雪深度
            character*(5)::MaxDay !最大积雪深度出现日(MMDD)
            integer(2),dimension(5)::DepthDays  !积雪深度出现的日数,1cm,5cm,10cm,20cm,30cm
       end type YeaSnow
       
       
       !---------------最大积雪深度/日积雪深度 历史排序数据---------------------
       type MaxDepthHisorder
            integer :: StaID 
            character*(StaNameMaxLen) :: StaName
            integer :: MaxDepth !
            integer :: Year
       end type MaxDepthHisorder       
       !------------------------------------------------------------
        
    contains
    !******************以下为功能函数*******************************
        
        !------------随机产生日积雪数据供测试用----------------------
        subroutine CreateDayData(Year,Month,Day)
        !说明：从台站信息文件中读入真实台站信息，为每个台站生成一条数据，
        !并写入文件.其台站积雪深度随机生成。
        !为得到真实数据，在生成过程中采用如下规则：
        !(1)如果台站纬度超过40度时：3-4,9月值为小雪(3cm以下),5月为微量值
        !    6-8月为缺测值,10-11，2为中雪（3-5cm），12-1月为大雪(超过5cm，上限为50cm)
        !(2)如果纬度为30-40度间时：12-1月为，大雪(5-50cm),2,11为中雪(3-5cm),10为小雪(1-3),
        !   9，3月为微量,其他为缺测值
        !(3)如果纬度低于30：12-1为中雪(3-5cm)，11，1为小雪(1-3cm)，10，2月为微量,其他为缺测        
        !------------------------------------------------------------
            integer::Year,Month,Day !年，月，日
            character*(FileNameMaxLen)::FileName
            type(FileReturnInfo)::FN !用于调用求台站个数函数
            character*(FileNameMaxLen)::StaFileName
            integer::FactRL !台站个数
            type(StaInfo),allocatable::Sta(:) !台站信息动态数组
            type(DaySnow),allocatable::DSnow(:) !日积雪动态数组
            integer::i !循环计数器
            integer::stat
            
            !（1）准备工作：获取台数个数，读台站数据，准备好存储结果文件并创建
            StaFileName="./Data/ResourceData/Stainfo.txt"
            FN=GetStaNumber(StaFileName,FactRL) !获取台站个数
            allocate(Sta(FactRL))
            call ReadStaInfoData(StaFileName,Sta,FactRL) !读台站数据
            call CreateDayFileName(FileName,Year,Month,Day) !创建日积雪数据文件名
            open(10,file=filename,form="formatted",iostat=stat) !创建日积雪文件
            if(stat/=0) then
                write(*,*) "创建日积雪文件错误"
                return
            endif
            
            !(2)根据年，月，日创建积雪深度数据
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
                
                !下面根据实际情况生成模拟积雪深度数值
                !1)北方地区,如果台站纬度超过40度时：3-4,9月值为小雪(5cm以下),5月为微量值
                !    6-8月为缺测值,10-11，2为中雪（5-20cm），12-1月为大雪(超过20cm，上限为100cm)
                if (DSnow(i).longitude>40.0) then
                    if(DSnow(i).Month == 9 .OR. DSnow(i).Month == 3 .OR. DSnow(i).Month == 4 ) then !小雪
                        DSnow(i).Depth = rand(0,5)
                        if(DSnow(i).Depth==0) DSnow(i).Depth=MicroscaleVal
                    endif
                    if(DSnow(i).Month == 10 .OR. DSnow(i).Month == 11 .OR. DSnow(i).Month == 2 )  then !中雪
                        DSnow(i).Depth = rand(5,20)
                    endif
                    if(DSnow(i).Month == 12 .OR. DSnow(i).Month == 1 )  then !大雪
                        DSnow(i).Depth = rand(20,100)
                    endif
                    if(DSnow(i).Month == 5 )  then !微雪
                        DSnow(i).Depth = MicroscaleVal
                    endif              
                    if(DSnow(i).Month >=6 .and. DSnow(i).Month <=8 )  then !无雪
                        DSnow(i).Depth = DefaultVal
                    endif
                endif
                !2)中部地区,如果纬度为30-40度间时：12-1月为大雪(20-100cm),2,11为中雪(5-20cm),10为小雪(0-5),
                !   9，3月为微量,其他为缺测值
                if(DSnow(i).longitude>30.0 .and. DSnow(i).longitude<=40.0) then 
                    if(DSnow(i).Month == 10 ) then !小雪
                        DSnow(i).Depth = rand(0,5)
                        if(DSnow(i).Depth==0) DSnow(i).Depth=MicroscaleVal
                    endif
                    if(DSnow(i).Month == 2 .OR. DSnow(i).Month == 11 )  then !中雪
                        DSnow(i).Depth = rand(5,20)
                    endif
                    if(DSnow(i).Month == 12 .OR. DSnow(i).Month == 1 )  then !大雪
                        DSnow(i).Depth = rand(20,100)
                    endif
                    if(DSnow(i).Month == 3 .OR. DSnow(i).Month == 9)  then !微雪
                        DSnow(i).Depth = MicroscaleVal
                    endif              
                    if(DSnow(i).Month >=4 .and. DSnow(i).Month <=8 )  then !无雪
                        DSnow(i).Depth = DefaultVal
                    endif
                endif
                !3)南方地区,如果纬度低于30：12-1为中雪(3-5cm)，11，2为小雪(0-5cm)，10，3月为微量,其他为缺测  
                if (DSnow(i).longitude<=30.0) then 
                    if(DSnow(i).Month == 11 .OR. DSnow(i).Month == 2  ) then !小雪
                        DSnow(i).Depth = rand(0,5)
                        if(DSnow(i).Depth==0) DSnow(i).Depth=MicroscaleVal
                    endif
                    if(DSnow(i).Month == 12 .OR. DSnow(i).Month == 1 )  then !中雪
                        DSnow(i).Depth = rand(5,20)
                    endif
                    if(DSnow(i).Month == 3 .OR. DSnow(i).Month == 10)  then !微雪
                        DSnow(i).Depth = MicroscaleVal
                    endif              
                    if(DSnow(i).Month >=4 .and. DSnow(i).Month <=9 )  then !无雪
                        DSnow(i).Depth = DefaultVal
                    endif
                endif
                
                !(3)将日积雪所有数据写进文件   
                write(10,*)DSnow(i)
            end do
            deallocate(DSnow) !释放数组
            deallocate(Sta)
            close(10)
            return
        end subroutine CreateDayData
        !------------------------------------------------------------
        
        !----------读日积雪数据---------------------------------------
        subroutine ReadDaySnowData(Year,Month,Day,DayData,FactRL)
        !说明：根据输入的年月日，读该日的日积雪数据，用DayData数组返回,
        !在调用该函数前需要先获取该日数据的记录数FactRL
            implicit none
            integer::Year,Month,Day !年月日
            integer::FactRL !文件中的实际记录条数
            type(DaySnow):: DayData(FactRL)
            character*(FileNameMaxLen)::FileName
            integer::stat
            logical::alive
            integer::i !循环计数器
            !根据年月日生成文件名
            call CreateDayFileName(FileName,Year,Month,Day)
            !从文件读数据，将数据结果存放在DayData数组中
            inquire(file=FileName,exist=alive)
            if(.not. alive) then
                write(*,"(A80,'日积雪文件不存在，程序退出')")filename
                return
            endif
            open(10,file=filename,form="formatted",iostat=stat)
            if(stat/=0) then
                write(*,"(A80,'日积雪文件打开时出错，程序退出')")filename
            endif
            do i=1,FactRL
                read(10,*)DayData(i)
            enddo
            
            close(10)
            return
        end subroutine ReadDaySnowData
        !-------------------------------------------------------------
        
        !--------生成月积雪数据-----------------------------------------
        !说明：由日积雪数据统计分析得到月积雪数据
        subroutine CreateMonData(Year,Month)
            implicit none
            integer::Year,Month !输入的年月
            character*(FileNameMaxLen)::FileName !日积雪文件名
            type(MonthSnow),allocatable:: MonData(:) !动态数组，存储月积雪数据，数组维数记录的条数
            type(StaInfo),allocatable:: StationInfo(:)
            type(DaySnow),allocatable:: SnowData(:,:) !多天多台站的日积雪数据
            integer,allocatable::SnowDayNums(:) !一个月中各台站的积雪日数
            integer,allocatable::MaxDepth(:) !一个月中的所有台站的最大积雪深度值
            integer,allocatable:: MaxDepthDay(:) !一个月中各台站的最大积雪深度日
            integer,allocatable :: MaxDepthNum(:,:) !各个积雪深度出现的日数            
            type(FileReturnInfo):: FN !文件返回值信息
            character*(FileNameMaxLen):: StaFileName !台站信息文件名
            character*(FileNameMaxLen):: DayFileName !日积雪文件名
            integer:: Days !一个月的天数
            integer:: FactRL !日积雪记录个数
            integer::stat
            integer::i,j !循环计数器
            logical::alive
            
            
            !处理过程：（1）准备工作，该月包含的天数，获取日积雪文件的大小,将日积雪数据装入月积雪动态数组中。
            !           (2)计算工作：计算日最大积雪深度，计算最大积雪深度出现日,计算各数值下的出现日数
            
            !准备工作，包括获得该月的天数，动态分配各数组并初始化
            Days=GetMonDays(Year,Month) !根据年月获得该月的天数
            StaFileName="./Data/ResourceData/StaInfo.txt"
            FN = GetStaNumber(StaFileName,FactRL)
            if(FN.index/=0) then 
                write(*,*)"获取台站个数出错，程序将退出"
                return
            endif
            
            allocate(StationInfo(FactRL)) !台站信息数组
            allocate(MonData(FactRL)) !月积雪数组
            allocate(SnowData(Days,FactRL))
            allocate(MaxDepth(FactRL)) !一个月中各台站的最大积雪值
            allocate(MaxDepthDay(FactRL))!一个月中各台站的最大积雪出现日
            allocate(MaxDepthNum(FactRL,5)) !各个台站的各层次上出现日数 
            allocate(SnowDayNums(FactRL)) !一个月中各台站的积雪日数
            
            call ReadStaInfoData(StaFileName,StationInfo,FactRL) !读取台站信息
            !将一个月所有天的日积雪数据装载进SnowData(Days,FactRL)数组
            do i=1,Days !各天的日积雪数据
!                !先得到日积雪数据文件名
!                call CreateDayFileName(FileName,Year,Month,i)
                !读取日积雪数据
                call ReadDaySnowData(Year,Month,i,SnowData(i,:),FactRL)
            enddo
            
            !计算各台站的积雪日数
            call GetSnowDayNubmers_common(SnowData,SnowDayNums,Days,FactRL)
           !  GetSnowDayNubmers_common(SnowData,SnowDayNums,DayNum,StaNum)
           
            !计算最大积雪深度,MaxDepth(FactRL)数组装的是各个台站的最大积雪深度
            call MaxSnowDepth_common(SnowData,MaxDepth,Days,FactRL)
          
            !计算最大积雪深度出现日,由MaxDepthDay(FactRL)返回
            call MaxSnowDepthDay_common(SnowData,MaxDepthDay,Days,FactRL)
          
            !计算各层上的出现日数,由MaxDepthNum返回
            call GetSnowDepthNumbers_common(SnowData,MaxDepthNum,Days,FactRl)
            
            !生成月积雪数据文件名，并创建文件
            call CreateMonthFileName(FileName,Year,Month)
            inquire(file=FileName,exist=alive)
            if(alive) then
                write(*,"(I80,'月文件已存在，不需要再生成')") FileName
                return
            endif
            open(10,File= FileName, form='formatted',iostat=stat)
            if(stat/=0) then 
                write(*,"(I80,'文件创建出借，程序退出')") FileName
            endif
            !将所有数据装载进月数据数组，并将每条记录写进文件
            write(10,*)"区站号 区站名  纬度 经度 高度 年 月 积雪日数 最大深度 出现日 >1 >5 >10 >20 >30"
            do i=1,FactRL
                MonData(i).StaID=StationInfo(i).StaID
                MonData(i).StaName=StationInfo(i).StaName
                MonData(i).Longitude=StationInfo(i).Longitude
                MonData(i).Latitude=StationInfo(i).Latitude
                MonData(i).StaHeight=StationInfo(i).StaHeight
                MonData(i).Year = Year
                MonData(i).Month = Month
                MonData(i).SnowDays = SnowDayNums(i) !积雪日数
                MonData(i).MaxDepth = MaxDepth(i) !最大积雪值
                MonData(i).MaxDay = MaxDepthDay(i) !最大积雪出现日
                MonData(i).DepthDays(1:5) =  MaxDepthNum(i,1:5)
                !write(10,*,iostat=stat) MonData(i)
                
                write(10,100,iostat=stat) MonData(i).StaID,MonData(i).StaName,MonData(i).Longitude,MonData(i).Latitude,&
                                        MonData(i).StaHeight,MonData(i).Year,MonData(i).Month,MonData(i).SnowDays,&
                                        MonData(i).MaxDepth,MonData(i).MaxDay,MonData(i).DepthDays(1:5)
                100 format(I8,' ',A20,F8.2,F8.2,F8.2,I6,I4,I6,I6,I6,I6,I6,I6,I6,I6)
                if(stat/=0) then
                    write(*,*) "月数据写入文件时出错，程序退出"
                    close(10)
                    return
                endif
            enddo
            
            deallocate(StationInfo) !台站信息数组
            deallocate(MonData) !月积雪数组
            deallocate(SnowData)
            deallocate(MaxDepth) !一个月中各台站的最大积雪值
            deallocate(MaxDepthDay)!一个月中各台站的最大积雪出现日
            deallocate(MaxDepthNum) !各个台站的各层次上出现日数 
            deallocate(SnowDayNums) !一个月中各台站的积雪日数
            close(10)
            return
        end subroutine CreateMonData
        !---------------------------------------------------------------
        
        !----------读月积雪数据-----------------------------------------
        subroutine ReadMonSnowData(Year,Month,MonData,FactRL)
            integer::Year,Month,FactRL !年，月，月积雪记录个数
            type(MonthSnow) :: MonData(FactRL) !月积雪数据数组 
            character*(FileNameMaxLen)::FileName
            integer::stat
            logical::alive
            integer::i !循环计数器
            
            !根据年月生成文件名
            call CreateMonthFileName(FileName,Year,Month)
            !从文件读数据，将数据结果存放在DayData数组中
            inquire(file=FileName,exist=alive)
            if(.not. alive) then
                write(*,"(A80,'月积雪文件不存在，程序退出')")filename
                return
            endif
            open(10,file=filename,form="formatted",iostat=stat)
            if(stat/=0) then
                write(*,"(A80,'日积雪文件打开时出错，程序退出')")filename
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
        
        !-------------通用积雪日数(输入为日积雪数据)--------------------------------------
        !说明：求多天内各个台站的积雪日数，通过SnowdayNums返回
        subroutine GetSnowDayNubmers_common(SnowData,SnowDayNums,DayNum,StaNum)
            implicit none
            integer :: DayNum !天数
            integer :: StaNum !台站数
            type(DaySnow):: SnowData(DayNum,StaNum) !DayNum天的日积雪数据，每日的记录条件为StaNum
            integer :: SnowDayNums(StaNum)
            integer :: i,j
            
            SnowDayNums=0 !初始化天数全为0
            
            do i=1,StaNum !各台站
                do j=1,DayNum !各天
                    if(SnowData(j,i).Depth /= DefaultVal .and. SnowData(j,i).depth /= MicroscaleVal) then
                        SnowDayNums(i) = SnowDayNums(i) + 1
                    endif
                enddo
            enddo
            return
        end subroutine GetSnowDayNubmers_common
        !---------------------------------------------------------------
        
        !----------通用最大积雪深度(输入为日积雪数据)-------------------
        !说明：给入日积雪数据，计算出每个台站在某段时间内的最大积雪深度
        !SnowData为DayNum X StaNum 的二维数组，DayNum代表天数，StaNum代表台站数
        !MaxDepth是一维数组，维数为n，该数组返回各个台站的最大积雪深度
        subroutine MaxSnowDepth_common(SnowData,MaxDepth,DayNum,StaNum)
            integer :: DayNum !天数
            integer :: StaNum !台站数
            type(DaySnow):: SnowData(DayNum,StaNum) !DayNum天的日积雪数据，每日的记录条件为StaNum
            integer:: MaxDepth(StaNum) !最大积雪深度，该数组将返回
            integer:: DepthData(DayNum) !某个台在DayNum天数内的积雪深度数据
            integer:: i,j !循环计数
           ! write(*,*)"求最大积雪深度时装载的深度值"
            do i=1,StaNum !每个台站进行计算
                do j=1,DayNum !每个台站的所有日期的积雪深度装入DepthData数组
                    DepthData(j)= SnowData(j,i).Depth                               
                enddo
                !if(i<=10) write(*,*)DepthData   
                MaxDepth(i)=FindMaxDepth(DepthData,DayNum) !查找每个台站的所有日期的最大积雪深度
            enddo           
            
            return
        end subroutine 
        !---------------------------------------------------------------
        
        !--------查找多个积雪深度中的最大积雪深度值(输入为日积雪数据)------------------
        !SnowDepth(n)数组中为多个积雪深度数据，找出最大值，函数返回为该最大积雪深度值
        integer function FindMaxDepth(SnowDepth,n)
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
                    FindMaxDepth=MicroscaleVal
                    return
                else !全为缺测值
                    FindMaxDepth=DefaultVal
                    return
                endif
            endif
            
            FindMaxDepth = max
            return
        end function FindMaxDepth
        !---------------------------------------------------------------
        
        !---------通用求最大积雪深度出现日(输入为日积雪数据)-----------------------
        !说明：给入日积雪数据SnowData（DayNum,StaNum），DayNum表示天数,StaNum表示台站数
        !函数返回值为DayNum中的第几天，或是BaseVal + xx，BaseVal=9950,xx表示出现最大积
        !雪深度日几次,若最大积雪值为缺测值，则返回值也为缺测，若为微量，则返回为微量值
        subroutine MaxSnowDepthDay_common(SnowData,MaxDepthDay,DayNum,StaNum)
            implicit none
            integer :: DayNum !天数
            integer :: StaNum !台站数
            type(DaySnow):: SnowData(DayNum,StaNum) !DayNum天的日积雪数据，每日的记录条件为StaNum
            integer:: MaxDepthDay(StaNum) !最大积雪深度
            integer:: i,j !循环计数
            integer:: n !出现最大积雪深度的次数
            integer:: MaxDepth(StaNum) !各台站最大积雪深度
            integer:: d !记录日期，即该月的第几天
            
            MaxDepthDay=MicroscaleVal
            d=0
            call MaxSnowDepth_common(SnowData,MaxDepth,DayNum,StaNum) !求得各台站的最大积雪深度,数据存放在MaxDepth数组中
           
            do i=1,StaNum !计算每个站点的最大深度出现日
                
                n=0
                do j=1,DayNum !各天
                    if(MaxDepth(i)==Defaultval) then !若最大积雪深度为缺测值，则日数为缺测值
                        MaxDepthDay(i)=DefaultVal
                        exit
                    endif
                    if(MaxDepth(i)==MicroscaleVal) then !若最大积雪深度为微量值，则日数为微量值
                        MaxDepthDay(i)=MicroscaleVal
                        exit
                    endif
                    if(SnowData(j,i).Depth == MaxDepth(i)) then
                        n=n+1
                        if(n==1) d=j
                    endif
                enddo
                if(n==1) MaxDepthDay(i) = d !如果只有一次，则取DayNum中的第几天
                if(n>1) MaxDepthDay(i) = BaseVal + n !如果深度出现日有多次，则取 9950  + n                
            enddo         
            return
        end subroutine MaxSnowDepthDay_common
        !---------------------------------------------------------------
        
        !-------------通用符合条件的深度出现日数(输入为日积雪数据)----------------------
        !说明：从多个日积雪数据中获取每个台站>=1cm,5cm,10cm,20cm,30cm的天数
        !SnowData(DayNum,StaNum)存放多日多台站积雪数据，MaxDepthNum(StaNum)存放各个层次的出现日数
        !DayNum为天数，StaNum为台站数
        subroutine GetSnowDepthNumbers_common(SnowData,MaxDepthNum,DayNum,StaNum)
            implicit none
            integer :: DayNum !天数
            integer :: StaNum !台站数
            type(DaySnow):: SnowData(DayNum,StaNum) !DayNum天的日积雪数据，每日的记录条件为StaNum
            integer :: MaxDepthNum(StaNum,5) !各个积雪深度出现的日数
            integer i,j !循环计数
            logical :: IsDefault1=.true. !开始认为是缺测，一旦有非缺测值出现，则记为false
            logical :: IsDefault5=.true. !开始认为是缺测，一旦有非缺测值出现，则记为false
            logical :: IsDefault10=.true. !开始认为是缺测，一旦有非缺测值出现，则记为false
            logical :: IsDefault20=.true. !开始认为是缺测，一旦有非缺测值出现，则记为false
            logical :: IsDefault30=.true. !开始认为是缺测，一旦有非缺测值出现，则记为false
             
            MaxDepthNum(1:StaNum,1:5)=0
            do i=1,StaNum !各个台站
                do j=1,DayNum !各天
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
                if(IsDefault1) MaxDepthNum(i,1) = DefaultVal !如果全为读入的数据全为缺测值，则最大出现日数也为缺测值，下同
                if(IsDefault5) MaxDepthNum(i,2) = DefaultVal
                if(IsDefault10) MaxDepthNum(i,3) = DefaultVal
                if(IsDefault20) MaxDepthNum(i,4) = DefaultVal
                if(IsDefault30) MaxDepthNum(i,5) = DefaultVal                
            enddo
            return
            
        end subroutine GetSnowDepthNumbers_common              
        !---------------------------------------------------------------
        
        !----------------通用最大积雪深度/日积雪深度排序----------------
        !说明：需要考虑缺测值和微量值，比较中具体值>微量值>缺测值.为解决
        !该问题，采用先将微量值用0，缺测值用-1表示，然后进行排序，排序完
        !以后再将0,-1转换成缺测值和微量值
        subroutine MaxDepthSort(MaxDepth,n) !采用选择法排序
            implicit none
            integer :: n
            type(MaxDepthHisorder) :: MaxDepth(n) !最大积雪深度排序数据数组
            type(MaxDepthHisorder) :: temp
            integer :: i,j,k
            integer :: min
            
            !下面将缺测值用-1,微量值用0表示，对数据进行预处理
            do i=1,n
                if(MaxDepth(i).MaxDepth==DefaultVal) MaxDepth(i).MaxDepth=-1
                if(MaxDepth(i).MaxDepth==MicroscaleVal) MaxDepth(i).MaxDepth=0
            enddo
            
            !下面为选择法排序
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
            
            !下面将数组中的-1和0用缺测值和微量转换回来
            do i=1,n
                if(MaxDepth(i).MaxDepth==-1) MaxDepth(i).MaxDepth=DefaultVal
                if(MaxDepth(i).MaxDepth==0) MaxDepth(i).MaxDepth=MicroscaleVal
            enddo
            
        end subroutine MaxDepthSort
!        !-------求任意时段的日积雪数据----------------------------------
!        subroutine DuringSnowData(SnowData,Year1,Month1,Day1,Year2,Month2,Day2)
!        
!        end subroutine DuringSnowData
!        !---------------------------------------------------------------
        
        
!        !---------计算日积雪数据中的最大积雪深度-----------------------
!        ! !!!!下面的代码有错误！！！！！！！！！！！！！！！！
!        integer function GetDayMaxDepth(Year,Month,Day)
!        !说明：查找所有台站中的最大积雪深度，如果所有台站积雪深度均为缺
!        !      测和微量值时，先考虑微量值，若只有缺测值，记为缺测
!            implicit none
!            integer::Year,Month,Day !年月日
!            character*(FileNameMaxLen)::FileName !日积雪文件名
!            integer::FactRL !日积雪记录个数
!            type(DaySnow),allocatable::DayData(:)
!            integer :: i
!            logical :: t1=.false. !如为Y，则有积雪
!            logical :: t2=.false. !如为Y，则为微量
!            !logical :: t3=.false. !如为Y，则为缺测
!            integer :: max = DefaultVal !默认为缺测值
!            
!            
!            !处理过程：先读取日积雪数据，再到积雪数据中查找最大积雪深度
!            !(1)读取日积雪数据：先生成日积雪文件名，再获取文件记录数，
!            !   再动态生成数组，最后读取日积雪数据
!            
!            call CreateDayFileName(FileName,Year,Month,Day) !产生日积雪文件名filename
!            if(GetDayRL(Year,Month,Day,FactRL)/=0) then !获取日积雪记录个数FactRL
!                write(*,*)"获取日积雪记录个数时出错,程序退出"
!                return
!            endif
!            allocate(DayData(FactRL)) !动态生成数组
!            call ReadDaySnowData(Year,Month,Day,DayData,FactRL) !读日积雪数据DayData
!            
!            !(2)查找最大积雪深度
!            !!!!下面的代码有错误！！！！！！！！！！！！！！！！
!            do i=1,FactRL
!                if(DayData(i).Depth < max .and. t1==.false. .and. (max==DefaultVal .OR. max==MicroscaleVal)) then !未发现积雪，但有微量
!                    max=DayData(i).Depth !取微量值
!                    t2 = .true. !发现了微量
!                endif
!                if((DayData(i).Depth < max) .and. t2== .true. .and. t1== .false.) then !有微量，第一次发现了积雪
!                    max=DayData(i).Depth
!                    t1= .true. !发现了积雪
!                endif
!                if(DayData(i).Depth > max .and. DayData(i).Depth < MicroscaleVal .and. t1== .true.) then !有积雪，找最大值
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
!        !------------计算旬最大积雪深度-------------------------------------
!        subroutine GetTenMaxSnowDepth(Year,Month,Ten)
!        
!        end subroutine GetTenMaxSnowDepth
!        !---------------------------------------------------------------
        
        !------------旬最大积雪深度出现日-------------------------------
        
        !---------------------------------------------------------------
        
        !------------旬最大积雪深度出现日-------------------------------
        
        !---------------------------------------------------------------
        
        
        
!        !----------日积雪符合条件的深度出现日数----------------------------------
!        !说明：计算日积雪数据中各积雪值出现的日数,1cm,5cm,10cm,20cm,30cm
!        !计算中舍弃缺测，若读入数据均为缺测，则结果记为缺测
!        subroutine GetDaySnowNumber(Year,Month,Day,Numbers)
!            implicit none
!            integer :: Year,Month,Day !年月日
!            integer :: Numbers(5) !各积雪值出现的日数，>1cm,5cm,10cm,20cm,30cm
!            character*(FileNameMaxLen):: FileName !日积雪文件名
!            integer :: FactRL  !文件记录数
!            type(DaySnow),allocatable :: DayData(:) !日积雪数据
!            integer :: i
!           
!            
!            !处理过程：读取日积雪文件，再找各个符合条件的天数
!            !(1)准备工作：数组初始化，生成日积雪文件名，获取文件记录数，读文件数据
!            Numbers=0 !数组初始化
!            call CreateDayFileName(FileName,Year,Month,Day) !产生日积雪文件名filename
!            if(GetDayRL(Year,Month,Day,FactRL)/=0) then !获取日积雪记录个数FactRL
!                write(*,*)"获取日积雪记录个数时出错,程序退出"
!                return
!            endif
!            allocate(DayData(FactRL)) !动态生成数组
!            call ReadDaySnowData(Year,Month,Day,DayData,FactRL) !读日积雪数据DayData
!            
!            !(2)找符合条件的天数
!            do i=1,FactRL
!                if(DayData(i).Depth >=1 .and. (DayData(i).Depth/=DefaultVal .and. DayData(i).Depth/=MicroscaleVal)) then
!                    Numbers(1)=Numbers(1)+1  !>=1cm的天数
!                endif
!                if(DayData(i).Depth >=5 .and. (DayData(i).Depth/=DefaultVal .and. DayData(i).Depth/=MicroscaleVal)) then
!                    Numbers(2)=Numbers(2)+1  !>=5cm的天数
!                endif
!                if(DayData(i).Depth >=10 .and. (DayData(i).Depth/=DefaultVal .and. DayData(i).Depth/=MicroscaleVal)) then
!                    Numbers(3)=Numbers(3)+1  !>=10cm的天数
!                endif
!                if(DayData(i).Depth >=20 .and. (DayData(i).Depth/=DefaultVal .and. DayData(i).Depth/=MicroscaleVal)) then
!                    Numbers(4)=Numbers(4)+1  !>=20cm的天数
!                endif
!                if(DayData(i).Depth >=30 .and. (DayData(i).Depth/=DefaultVal .and. DayData(i).Depth/=MicroscaleVal)) then
!                    Numbers(5)=Numbers(5)+1  !>=30cm的天数
!                endif
!            enddo
!            
!            deallocate(DayData)
!            return        
!        end subroutine GetDaySnowNumber
!        !--------------------------------------------------------------
!        
!        !--------计算日积雪最大积雪深度出现日---------------------------
!        !说明：当现象出现日有多次时，将次数加上9950进行表示。
!        !   如：若出现日的值为2,表示在该月2号出现了该现象；若出现日的值为
!        !   9952，表示在该月出现了2次该现象
!        integer function GetMaxDepthDate(Year,Month,Day)
!            implicit none
!            integer:: Year,Month,Day !年月日
!            character*(FileNameMaxLen)::FileName !
!            integer::FactRL
!            type(DaySnow),allocatable:: DayData(:) !动态数组日积雪数据
!            integer::i !循环变量
!            integer::d !出现日
!            integer::n !出现次数
!            integer::max !最大积雪深度
!            
!            !处理过程：先打开该日积雪文件，然后获取该日最大积雪深度，然后计算最大积雪深度出现日
!            !(1)准备工作：打开文件并计算最大积雪深度
!            call CreateDayFileName(FileName,Year,Month,Day) !产生日积雪文件名filename
!            if(GetDayRL(Year,Month,Day,FactRL)/=0) then !获取日积雪记录个数FactRL
!                write(*,*)"获取日积雪记录个数时出错,程序退出"
!                return
!            endif
!            allocate(DayData(FactRL)) !动态生成数组
!            call ReadDaySnowData(Year,Month,Day,DayData,FactRL) !读日积雪数据DayData
!            max=GetDayMaxDepth(Year,Month,Day) 
!            if(max==DefaultVal .OR. max==MicroscaleVal) then !当为微量值时，无积雪，出现日取32744
!                GetMaxDepthDate=MicroscaleVal
!                return
!            endif
!            n=0 !出现次数初始化为0
!                       
!            !(2)找最大积雪深度出现日
!            do i=1,FactRL
!                if(DayData(i).Depth == max)  n=n+1 !出现次数加1天                
!            enddo
!            
!            if(n==1) GetMaxDepthDate=day !只出现一次，则返回该日期
!            if(n>1) GetMaxDepthDate=BaseVal + n !如果再现多日，则用9950+n
!            return            
!        end function GetMaxDepthDate
!        
!        !---------------------------------------------------------------
    
    !*******************功能函数结束********************************
    
    
    
    
    
    
    
    
    
    
    
    
    
    !*******************以下为辅助函数******************************
    
       !------------产生m~n的随机整数---------------------------------------------------------
        integer function Rand(m,n)!注意，在调用此函数前需要产生随机种子，即call random_seed()
             implicit none
             real::t
             integer::m,n
             if(m>=n) then
                  write(*,*)"产生随机数的上界必须大于下界,程序退出"
                  return
             endif
             call random_number(t)
             Rand=m+int(t*(n-m))
             return  
        end function Rand  
       !--------------------------------------------------------
       !-----------判断闰年--------------------------------------
        logical function IsLeapYear(year)
	         integer year
	         IsLeapYear=(mod(year,4)==0 .and. mod(year,100)/=0 ).or. mod(year,400)==0
	    end function IsLeapYear
       !--------------------------------------------------------- 
       
       !------------获取某月所包含的天数---------------------------
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
        
       !----------产生日积雪数据文件名------------------------------
        subroutine CreateDayFileName(FileName,Year,Month,Day)
        !说明：给定输入年，月，日，生成日积雪数据的文件名，通过filename返回
            implicit none
            character*(FileNameMaxLen) :: FileName
            integer::Year,Month,Day
            character*(4)::YearChar
            character*(2)::MonthChar
            character*(2)::DayChar
            integer::stat
        
            write(YearChar,"(I4)",iostat=stat)year !将年份由整型转换成字符串
            if(stat/=0) write(*,*)"年份数据转换出错" 
        
            write(monthChar,"(I2)",iostat=stat)month !将月份由整型转换成字符串
            if(stat/=0) write(*,*)"月份数据转换出错"         
            if(month>0 .and. month<=9) then 
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
            
            write(DayChar,"(I2)",iostat=stat)Day !将日由整型转换成字符串
            if(stat/=0) write(*,*)"月份数据转换出错"         
            if(Day>0 .and. Day<=9) then 
                write(DayChar,"(I2)") Day
                DayChar='0'//adjustl(DayChar)
            endif        
            if(Day>=10 .and. Day<=31) then
                write(DayChar,"(I2)") Day
            endif        
            if(Day<1 .or. Day >31) then
                write(*,*) "输入的月份超出了1-12，程序退出"
                return
            endif
 
            FileName="./Data/ResourceData/Day/snow_day_"//YearChar//MonthChar//DayChar//".txt"  !合成文件名 
           !  write(*,*)filename
            return
        end subroutine CreateDayFileName
       !------------------------------------------------------------
       
       !----------产生月积雪数据文件名------------------------------
        subroutine CreateMonthFileName(FileName,Year,Month)
        !说明：输入年，月，生成月积雪数据文件名，通过FileName返回
            implicit none
            character*(FileNameMaxLen) :: FileName
            integer::Year,Month
            character*(4)::YearChar
            character*(2)::MonthChar
            integer::stat
        
            write(YearChar,"(I4)",iostat=stat)year !将年份由整型转换成字符串
            if(stat/=0) write(*,*)"年份数据转换出错" 
        
            write(monthChar,"(I2)",iostat=stat)month
            if(stat/=0) write(*,*)"月份数据转换出错"         
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
 
            FileName="./Data/ResourceData/Month/snow_mon_"//YearChar//MonthChar//".txt"  !合成文件名 
             !write(*,*)filename
            return
        end subroutine CreateMonthFileName
       !------------------------------------------------------------
       
       !------------获取日积雪数据记录个数--------------------------
       integer function GetDayRL(Year,Month,Day,FactRL)
       !说明：获取某年月日下的日积雪记录个数，FactRL返回该记录数,
       !函数返回值表示各种信息，见FileRetureInfo中的index值
            implicit none
            integer::Year,Month,Day !年月日
            integer::FactRL !日积雪记录数
            integer::cnt !用于计数,记录日积雪记录个数
            character*(FileNameMaxLen)::FileName
            type(DaySnow)::DayData
            logical::alive
            integer::stat
            
            !生成文件名
            call CreateDayFileName(FileName,Year,Month,Day)
            !打开文件并读文件记录，计数
            inquire(file=FileName,exist=alive)
            if(.not. alive) then
                GetDayRL=-4
                write(*,"(A80,'日积雪文件不存在，程序退出')")filename
                return
            endif
            open(10,file=filename,form="formatted",iostat=stat)
            if(stat/=0) then
                write(*,"(A80,'日积雪文件打开时出错，程序退出')")filename
                return
            endif
            cnt=0
            do while(.true.)
                read(10,*,iostat=stat)DayData
                if(stat==0) cnt=cnt+1
                if(stat<0 .and. cnt==1) then
                        GetDayRL=-2 !台站文件为空，无数据                         
					    close(10)
					    return
			    endif
			    if(stat<0 .and. cnt>0) then
					    
					    GetDayRL=0 !读取文件记录数结束
					    FactRL=cnt !返回文件记录数					    
					    close(10)
					    return
                endif
                if(stat>0) then
                        GetDayRL=-3 !读取文件记录时出错
                        close(10)
					    return
                endif                                        
            enddo
       end function GetDayRL
       !------------------------------------------------------------
       
       !--------从台站信息文件中获取台站个数------------------------ 
       type(FileReturnInfo) function GetStaNumber(StaFileName,FactRL)
            implicit none
            character*(FileNameMaxLen)::StaFileName !台站信息文件
            integer::FactRL !返回台站个数
            type(StaInfo):: Sta(10000)!将数据装载到数组中
            integer::cnt !用于临时记录台站个数
            integer::stat
            logical::alive
            character*(60)::c !读文件第1行字符串，该字符串不计算记录数
            
            !查询台站信息文件是否存在，如不存在则退程序
            inquire(file=stafilename,exist=alive)
            if(.not.alive) then
                write(*,"(A60,'台站信息文件不存在')") stafilename            
                GetStaNumber.index=-4
                GetStaNumber.info=StaFileName//"台站信息文件不存在"
                return 
            endif
            
            !台站信息文件存在，打开文件
            open(unit=12,file=StaFileName,form="formatted",iostat=stat)
                if(stat/=0) then
                     GetStaNumber.index=-1
                     GetStaNumber.info=StaFileName//"文件打开出错"                     
                     close(12)
                     return
                endif
            read(12,"(A60)") c !读第1行字符串，该行不占台站个数
            
            cnt=1
            do while(.true.)
                    read(12,"(I5,' ',A20,F8.2,F8.2,F8.2)",iostat=stat)Sta(cnt).StaID,Sta(cnt).StaName,Sta(cnt).longitude,&
                                Sta(cnt).Latitude,Sta(cnt).StaHeight
                    if(stat==0) cnt=cnt+1 !正常读取到数据，台站个数加1
                    if(stat<0 .and. cnt==1) then
                        GetStaNumber.index=-2
                        GetStaNumber.info=StaFileName//"台站文件为空，无数据"                           
					    close(12)
					    return
					endif
					if(stat<0 .and. cnt>1) then
					    GetStaNumber.index=0
                        GetStaNumber.info=StaFileName//"读取文件记录数结束"
					    FactRL=cnt-1 !返回文件记录数
					    close(12)
					    return
                    endif
                    if(stat>0) then
                        GetStaNumber.index=-3
                        GetStaNumber.info=StaFileName//"读取文件记录时出错"
                        close(12)
					    return
                    endif 
            end do
       end function GetStaNumber
    !----------------------------------------------------------------------------
 
    !-----------读台站信息数据---------------------------------------------
    !其中FactRL为台站个数，在调用函数前，需首先读取台站信息文件获取FactRL值 
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
       
    !*******************辅助函数结束********************************
    

end module JL_Mod_StaSnow_Common