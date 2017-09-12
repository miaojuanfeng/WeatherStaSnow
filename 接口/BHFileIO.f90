module JL_Mod_FileIO
    use JL_Mod_BHinterface
    contains
    !读取位势高度数据
    !输入
    !   DateS:年月日
    !   HgtS:位势高度
    !   DataFilePath:数据文件所有路径
    !输出
    !   H:某个日期的位势高度数据
    subroutine ReadHgt(DateS,HgtS,DataFilePath,H)
        implicit none
        type(DateStruct)::DateS
        type(HgtStruct)::HgtS
        character(len=200)::DataFilePath
        
        character(len=200)::FileName
        real::val(HgtS%widthIndex,HgtS%heightIndex,HgtS%Layer)
        real::H(HgtS%widthIndex,HgtS%heightIndex)
        integer::stat
        integer::nx,ny,nz,i,j,k,T
        logical:: alive
        
        nx=HgtS%widthIndex
        ny=HgtS%heightIndex
        nz=HgtS%Layer
        T=HowManyDay(DateS%year,DateS%month,DateS%day)       
        call GetFileName(DateS,DataFilePath,FileName)
        inquire(File=FileName,exist=alive)
        if(.not. alive) then
            write(*,*)FileName
            write(*,*)"数据文件不存在!"
            return
        endif
           
!        open(10,file=trim(FileName),form='unformatted',access='direct',recl=nx*ny*nz,& iostat=stat,convert="big_endian")
        open(10,file=trim(FileName),form='unformatted',access='direct',recl=nx*ny*nz*4,iostat=stat)
        if(stat .NE. 0) then
            write(*,*) FileName
            write(*,*)"文件打开不成功，检查文件名及源数据是否存在"
            return
        endif        
        read(10,rec=T) (((val(i,j,k),i=1,nx),j=1,ny),k=1,nz) !读数据        
        H(1:nx,1:ny)=val(1:nx,1:ny,HgtS%CurLayer)        
        close(10)
    end subroutine ReadHgt
    
    !-------生成位势高度数据文件名-------------
    !输入
    !   DateS:日期结构体，包括年月日
    !   DataFilePath:数据文件所在路径
    !输出
    !   FileName:生成的文件名
    subroutine GetFileName(DateS,DataFilePath,FileName)
        type(DateStruct)::DateS
        character(len=200)::DataFilePath,FileName
        character(len=4)::yearCh
        
        write(yearCh,"(I4)")DateS%year
        FileName=trim(DataFilePath)//"hgt.day."//yearCh//".ncep.grd"
        
    end subroutine GetFileName
    !------------------------------------------
    
    !--------生成阻塞高压关键区纬度平均值结果文件名---
    subroutine GetBHLatAvgFileName(DateS,FileName)
        type(DateStruct)::DateS
        character(len=200)::FileName
        character(len=4)::yearCh
        
        write(yearCh,"(I4)")DateS%year
        FileName="blockingpj.day."//yearCh//".ncep.grd"            
    end subroutine  GetBHLatAvgFileName
    !-------------------------------------------------
    
!    !----存储阻塞高压关键区纬度平均值结果到文件-------
!    subroutine SaveLatAvg(FileName,LatAvg)
!        character*(*)::FileName
!        real::LatAvg(:)
!        
!        integer::D,stat
!        logical::alive
!        D=size(LatAvg)!获取数组的大小  
!        
!        inquire(file=FileName,exist=alive)        
!        if(.not. alive)then
!           write(*,*)trim(FileName)
!           write(*,*)"文件第一次创建"   
!           open(11,file=trim(FileName),form='unformatted',status='new',access='direct',recl=D)
!           write(11,iostat=stat)(LatAvg(i),i=1,D)
!           if(stat .EQ. 0) then 
!              write(*,*)trim(FileName)
!              write(*,*)"纬向平均值写数据成功"
!           endif
!           close(11)
!            !read(10,rec=T) (((val(i,j,k),i=1,nx),j=1,ny),k=1,nz) !读数据        
!        else
!            open(11,file=trim(FileName),form='unformatted',status='old',position='append',iostat=stat)
!            if(stat .NE. 0) then
!                write(*,*)trim(FileName)
!                write(*,*)"文件打开错误"
!            endif
!            !backspace(11)
!            write(11,iostat=stat)(LatAvg(i),i=1,D)
!            if(stat .EQ. 0) then 
!              write(*,*)trim(FileName)
!              write(*,*)"纬向平均值写数据成功"
!            endif
!            close(11)
!        endif
!        !open(10,file=trim(FileName),form='unformatted',access='direct',recl=nx*ny*nz,iostat=stat)
!                   
!    end subroutine SaveLatAvg
    !-------------------------------------------------
    
    !-------生成阻塞高压关键区位势梯度结果文件名------
    subroutine GetBHGradient(DateS,FileName)
        type(DateStruct)::DateS
        character(len=200)::FileName
        character(len=4)::yearCh
        
        write(yearCh,"(I4)")DateS%year
        FileName="blockinggradient.day."//yearCh//".ncep.grd"
    end subroutine  GetBHGradient    
    !-------------------------------------------------
    
!    !-----存储位势梯度结果数据到文件-------------------
!    subroutine SaveBHGradient(FileName,G)
!        character*(*)::FileName
!        real::G(:)
!        
!        integer:: D,i,stat
!        logical:: alive 
!        D=size(G)
!        inquire(file=FileName,exist=alive)
!        if(.not. alive)then
!           write(*,*)trim(FileName)
!           write(*,*)"文件第一次创建"   
!           open(11,file=trim(FileName),form='unformatted',status='new',access='direct',recl=D,iostat=stat)
!           
!           write(11,rec=1,iostat=stat)(G(i),i=1,D)
!           write(*,*)stat
!           if(stat .EQ. 0) then 
!              write(*,*)trim(FileName)
!              write(*,*)"位势梯度写数据成功"
!           endif
!           close(11)              
!        else
!            open(11,file=trim(FileName),form='unformatted',status='old',position='append',iostat=stat)
!            if(stat .NE. 0) then
!                write(*,*)trim(FileName)
!                write(*,*)"文件打开错误"
!            endif            
!            write(11,iostat=stat)(G(i),i=1,D)
!            if(stat .EQ. 0) then 
!              write(*,*)trim(FileName)
!              write(*,*)"位势梯度写数据成功"
!            endif
!            close(11)
!        endif        
!    end subroutine SaveBHGradient
!    !--------------------------------------------------
    
    !向目标文件写入一维数组的二进制数据
    subroutine SaveDataArray(FileName,Array,M)
        character*(*)::FileName
        real::Array(:)
        integer(kind=4)::M
        integer(kind=4)::i,stat
        
        open(10,file=trim(FileName),form='unformatted',access='direct',&
            recl=M*4,iostat=stat)
            if(stat .NE. 0)  then
			   write(*,*)trim(FileName) //"文件打开出错 "
			   close(10) 
			   return 
			 end if
	     write(10,rec=1,iostat=stat) (Array(i),i=1,M)
	             if(stat .NE. 0)   then
                        write(*,*)trim(FileName) //"写数据过程中出错"	 
                        close(10) 
                        return 
                end if 
		 close(10) 
         return	     
    end subroutine SaveDataArray
    
    
    !通用向目标文件写入一维数据的二进制数据(以追加的方式)
    !FileName:文件名
    !Arrar[M]：一维数组
    subroutine SaveDataArrayAppend(FileName,Array,M)
        character*(*)::FileName
        real::Array(:)
        integer(kind=4)::M
        integer(kind=4)::i,stat
        logical alive
        
        inquire(file=FileName,exist=alive)
            if(.not. alive) then
                call  SaveDataArray(FileName,Array,M)
            else
                open(10,file=FileName,access='Stream',form='unformatted', position='append',&
                    iostat=stat)
                       if(stat/=0)  then
		                     write(*,*)trim(FileName) //"文件打开错误"
		                     close(10) 
		                     return 
		               end if 
                write(10,iostat=stat) (Array(i),i=1,M)
                        if(stat/=0)   then
                            write(*,*)trim(FileName) //"写数据过程中出错"	 
                            close(10) 
                            return 
                        end if 
		        close(10)
            endif    

    end subroutine SaveDataArrayAppend    
    
    !保存结果数据，根据remark值选择不同的存储方式,0为自动作业，1为用户用业
    subroutine SaveResult(FileName,Array,M,remark)
        character*(*)::FileName
        integer(kind=4)::M,remark
        real::Array(M)
        
        if(remark == 0) then !自动作业
            call SaveDataArrayAppend(FileName,Array,M) !保存纬向平均值
        endif
        if(remark == 1) then !用户作业
            call SaveDataArray(FileName,Array,M)
        endif    
    end subroutine SaveResult
   
    !----------将结果文件名及路径写回到outputPath中--------
    subroutine SaveResultFNtoOutPath(s1,s2,OutPath)
        character*(*)::s1,s2,OutPath        
        open(10,file=OutPath,form='formatted')        
        write(10,*)trim(s1)
        write(10,*)trim(s2)
        close(10)
        
    end subroutine SaveResultFNtoOutPath
    
    
     !以下为辅助函数------------------------------
      !------------计算该日是该年的第几天函数---------------------- 
        integer function HowManyDay(year,month,day)
			implicit none
			integer::year,month,day
		
			integer::bigmonth=31,smallmonth=30,February
			logical::x1 !瀛樺偍鍒ゆ柇鏄惁鏄棸骞村悗杩斿洖鐨勯�昏緫鍊?
			x1=IsLeapYear(year) !鍒ゆ柇鐢ㄦ埛杈撳叆鐨勫勾浠芥槸鍚︽槸闂板勾
			if(x1) then 
				February=29
			else
			    February=28
			end if
			select case(month) !閫氳繃鏈堜唤鍒ゆ柇锛屽苟璁＄畻鍑鸿鏃ユ槸璇ュ勾鐨勭鍑犲ぉ
				case(1)        !涓�鏈堬紝涓嬮潰浠ユ绫绘帹
					howmanyday=day
					return
				case(2)
					howmanyday=bigmonth+day
					return
				case(3)
					howmanyday=bigmonth+February+day
					return
				case(4)
					howmanyday=2*bigmonth+February+day
					return
				case(5)
					howmanyday=2*bigmonth+smallmonth+February+day
					return
				case(6)
					howmanyday=3*bigmonth+smallmonth+February+day
					return
				case(7)
					howmanyday=3*bigmonth+2*smallmonth+February+day
					return
				case(8)
					howmanyday=4*bigmonth+2*smallmonth+February+day
					return
				case(9)
					howmanyday=5*bigmonth+2*smallmonth+February+day
					return
				case(10)
					howmanyday=5*bigmonth+3*smallmonth+February+day
					return
				case(11)
					howmanyday=6*bigmonth+3*smallmonth+February+day
					return
				case(12)
					howmanyday=6*bigmonth+4*smallmonth+February+day
					return
				end select
		end function HowManyDay
    !--------------计算该日是该年的第几天函数------------------------------
     
       !-----------判断闰年--------------------------------------
        logical function IsLeapYear(year)
	         integer year
	         IsLeapYear=(mod(year,4)==0 .and. mod(year,100)/=0 ).or. mod(year,400)==0
	    end function IsLeapYear
    !--------------------------------------------------------- 

    !---------获取某月的起止日期-------------------
    subroutine GetStartEndDay(year,month,d1,d2)
        integer::year,month,d1,d2
        if(month==1 .OR. month==3 .OR. month==5 .OR. month==7 .OR. month==8 .OR. month==10 .OR. month==12) then
            d1=1
            d2=31
        endif
        if(month==4 .OR. month==6 .OR. month==9 .OR. month==11) then 
            d1=1
            d2=30
        endif
        if(month==2) then
            if(IsLeapYear(year)) then
                d1=1
                d2=29
            else
                d1=1
                d2=28
            endif
        endif
    end subroutine GetStartEndDay
   !-------------------------------------------------------

   !----------根据年月日获取文件位置------------------       
    integer function GetFileLac(year,month,day) 
        integer :: year,month,day
        integer:: i,total
        integer:: y        
        total=0
        y=year-1973
        do i=1,y
            if(IsLeapYear(i)) then
                total=total+366
            else
                total=total+365
            endif
        enddo
        
        total=total+HowManyDay(year,month,day)
        GetFileLac=total        
    end function GetFileLac    
    !------------------------------------
    
    
    
end module JL_Mod_FileIO