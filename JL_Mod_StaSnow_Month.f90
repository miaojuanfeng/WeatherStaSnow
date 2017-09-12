module JL_Mod_StaSnow_Month
    use JL_Mod_StaSnow_Common
    implicit none
    
    contains 
    !-------月最大积雪深度排序----------------------------------------
    !说明：每一站的每个日期最大积雪深度排序资料存为一个数据文件
        subroutine MonSnowDepthSort(StaID,Month,StartYear,EndYear)
            implicit none       
            integer::StaID,Month !台站号，月 
            integer::StartYear,EndYear !起止年
            type(MaxDepthHisorder),allocatable:: MonHisorderData(:) !要排序的数据,数组维数大小由1951到当前的总月数
            type(MonthSnow) :: SnowData !读月最大积雪数据
            type(FileReturnInfo):: FN
            integer :: MonSum !月的总数 
            character*(FileNameMaxLen) :: HisFileName !历史文件名
            character*(FileNameMaxLen) :: MaxFileName !月最大积雪文件名
            character*(FileNameMaxLen) :: StaFileName !台站文件名
            integer :: StaNum !台站个数
            integer :: i,j,k !循环计数器
            integer :: stat
            logical :: alive
            
            
            
            !需要进行的处理包括：生成文件名，读取该台站在历年的数据，然后进行排序
            
            MonSum = EndYear-StartYear + 1  !计算月总数
            allocate(MonHisorderData(MonSum))
            !获取台站数
            StaFileName="./Data/ResourceData/StaInfo.txt"
            FN = GetStaNumber(StaFileName,StaNum)
            if(FN.index /=0) then
                write(*,*)"获取台站个数出错，检查台站信息文件是否存在或数据是否正确"
                return
            endif
            !读月最大积雪数据文件中的月数据
            !产生文件名，打开文件，读每一条记录，
            !判断是否为现在要计算的站:如果不是，则继续读下一条数据;如果是，则读出该站的站号，站名，最大深度，年份
            
            do i=StartYear,EndYear                
                call CreateMonthFileName(MaxFileName,i,Month) !生成月最大积雪文件名
                inquire(file=MaxFileName,exist=alive) !检查文件是否存在
                if(.not. alive) then
                    write(*,"(I80,'文件不存在')")MaxFileName
                    return
                endif
                open(10,file=MaxFileName,iostat=stat) 
                if(stat/=0) then
                    write(*,"('打开月最积雪数据文件',I80,'时出错，检查文件数据是否正确')")MaxFileName
                    return
                endif
                
                do j=1,StaNum !查找StaID，如果找到则将相关信息读进待排序的数组，然后退出循环
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
            
            !排序
            !write(*,*) TenHisorderData(1:tensum)
            call MaxDepthSort(MonHisorderData,MonSum)            
            !存放到文件
            call CreateMonDepthSortFileName(HisFileName,StaID,Month)!生成月最大积雪历史排序文件名
            open(11,file= HisFileName, form="formatted")
            
            write(11,*) "    区站号 区站名                  最大积雪深度     年份"
            do j=1,MonSum
                write(11,*) MonHisorderData(j)
            enddo
            close(11)
            deallocate(MonHisorderData)
        end subroutine MonSnowDepthSort
        
        !---------生成月最大积雪历史排序文件名----------------------------
        subroutine CreateMonDepthSortFileName(HisFileName,StaID,Month)
            character*(FileNameMaxLen):: HisFileName
            integer:: StaID,Month !台站号，月
            character*(5):: StaIDChar !台站号字符
            character*(2):: MonthChar !月字符
            integer :: stat
            
            write(StaIDChar,"(I5)",iostat=stat)StaID !将台站号由整型转换成字符串
                if(stat/=0) write(*,*)"台站号数字转换出错"
            
            write(MonthChar,"(I2)")Month !将月数字转换为字符
            if(month>0 .and. month<=9) MonthChar='0'//adjustl(MonthChar)

            HisFileName="./Data/ResultData/SnowDepthSort/Month/Snow_hisorder_Mon_"//MonthChar//'_'//StaIDChar//".txt"  !合成文件名 
        return
            
        end subroutine CreateMonDepthSortFileName

end module JL_Mod_StaSnow_Month