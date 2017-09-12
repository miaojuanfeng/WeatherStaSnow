module JL_Mod_FileIO
    use JL_Mod_BHinterface
    contains
    !��ȡλ�Ƹ߶�����
    !����
    !   DateS:������
    !   HgtS:λ�Ƹ߶�
    !   DataFilePath:�����ļ�����·��
    !���
    !   H:ĳ�����ڵ�λ�Ƹ߶�����
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
            write(*,*)"�����ļ�������!"
            return
        endif
           
!        open(10,file=trim(FileName),form='unformatted',access='direct',recl=nx*ny*nz,& iostat=stat,convert="big_endian")
        open(10,file=trim(FileName),form='unformatted',access='direct',recl=nx*ny*nz*4,iostat=stat)
        if(stat .NE. 0) then
            write(*,*) FileName
            write(*,*)"�ļ��򿪲��ɹ�������ļ�����Դ�����Ƿ����"
            return
        endif        
        read(10,rec=T) (((val(i,j,k),i=1,nx),j=1,ny),k=1,nz) !������        
        H(1:nx,1:ny)=val(1:nx,1:ny,HgtS%CurLayer)        
        close(10)
    end subroutine ReadHgt
    
    !-------����λ�Ƹ߶������ļ���-------------
    !����
    !   DateS:���ڽṹ�壬����������
    !   DataFilePath:�����ļ�����·��
    !���
    !   FileName:���ɵ��ļ���
    subroutine GetFileName(DateS,DataFilePath,FileName)
        type(DateStruct)::DateS
        character(len=200)::DataFilePath,FileName
        character(len=4)::yearCh
        
        write(yearCh,"(I4)")DateS%year
        FileName=trim(DataFilePath)//"hgt.day."//yearCh//".ncep.grd"
        
    end subroutine GetFileName
    !------------------------------------------
    
    !--------����������ѹ�ؼ���γ��ƽ��ֵ����ļ���---
    subroutine GetBHLatAvgFileName(DateS,FileName)
        type(DateStruct)::DateS
        character(len=200)::FileName
        character(len=4)::yearCh
        
        write(yearCh,"(I4)")DateS%year
        FileName="blockingpj.day."//yearCh//".ncep.grd"            
    end subroutine  GetBHLatAvgFileName
    !-------------------------------------------------
    
!    !----�洢������ѹ�ؼ���γ��ƽ��ֵ������ļ�-------
!    subroutine SaveLatAvg(FileName,LatAvg)
!        character*(*)::FileName
!        real::LatAvg(:)
!        
!        integer::D,stat
!        logical::alive
!        D=size(LatAvg)!��ȡ����Ĵ�С  
!        
!        inquire(file=FileName,exist=alive)        
!        if(.not. alive)then
!           write(*,*)trim(FileName)
!           write(*,*)"�ļ���һ�δ���"   
!           open(11,file=trim(FileName),form='unformatted',status='new',access='direct',recl=D)
!           write(11,iostat=stat)(LatAvg(i),i=1,D)
!           if(stat .EQ. 0) then 
!              write(*,*)trim(FileName)
!              write(*,*)"γ��ƽ��ֵд���ݳɹ�"
!           endif
!           close(11)
!            !read(10,rec=T) (((val(i,j,k),i=1,nx),j=1,ny),k=1,nz) !������        
!        else
!            open(11,file=trim(FileName),form='unformatted',status='old',position='append',iostat=stat)
!            if(stat .NE. 0) then
!                write(*,*)trim(FileName)
!                write(*,*)"�ļ��򿪴���"
!            endif
!            !backspace(11)
!            write(11,iostat=stat)(LatAvg(i),i=1,D)
!            if(stat .EQ. 0) then 
!              write(*,*)trim(FileName)
!              write(*,*)"γ��ƽ��ֵд���ݳɹ�"
!            endif
!            close(11)
!        endif
!        !open(10,file=trim(FileName),form='unformatted',access='direct',recl=nx*ny*nz,iostat=stat)
!                   
!    end subroutine SaveLatAvg
    !-------------------------------------------------
    
    !-------����������ѹ�ؼ���λ���ݶȽ���ļ���------
    subroutine GetBHGradient(DateS,FileName)
        type(DateStruct)::DateS
        character(len=200)::FileName
        character(len=4)::yearCh
        
        write(yearCh,"(I4)")DateS%year
        FileName="blockinggradient.day."//yearCh//".ncep.grd"
    end subroutine  GetBHGradient    
    !-------------------------------------------------
    
!    !-----�洢λ���ݶȽ�����ݵ��ļ�-------------------
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
!           write(*,*)"�ļ���һ�δ���"   
!           open(11,file=trim(FileName),form='unformatted',status='new',access='direct',recl=D,iostat=stat)
!           
!           write(11,rec=1,iostat=stat)(G(i),i=1,D)
!           write(*,*)stat
!           if(stat .EQ. 0) then 
!              write(*,*)trim(FileName)
!              write(*,*)"λ���ݶ�д���ݳɹ�"
!           endif
!           close(11)              
!        else
!            open(11,file=trim(FileName),form='unformatted',status='old',position='append',iostat=stat)
!            if(stat .NE. 0) then
!                write(*,*)trim(FileName)
!                write(*,*)"�ļ��򿪴���"
!            endif            
!            write(11,iostat=stat)(G(i),i=1,D)
!            if(stat .EQ. 0) then 
!              write(*,*)trim(FileName)
!              write(*,*)"λ���ݶ�д���ݳɹ�"
!            endif
!            close(11)
!        endif        
!    end subroutine SaveBHGradient
!    !--------------------------------------------------
    
    !��Ŀ���ļ�д��һά����Ķ���������
    subroutine SaveDataArray(FileName,Array,M)
        character*(*)::FileName
        real::Array(:)
        integer(kind=4)::M
        integer(kind=4)::i,stat
        
        open(10,file=trim(FileName),form='unformatted',access='direct',&
            recl=M*4,iostat=stat)
            if(stat .NE. 0)  then
			   write(*,*)trim(FileName) //"�ļ��򿪳��� "
			   close(10) 
			   return 
			 end if
	     write(10,rec=1,iostat=stat) (Array(i),i=1,M)
	             if(stat .NE. 0)   then
                        write(*,*)trim(FileName) //"д���ݹ����г���"	 
                        close(10) 
                        return 
                end if 
		 close(10) 
         return	     
    end subroutine SaveDataArray
    
    
    !ͨ����Ŀ���ļ�д��һά���ݵĶ���������(��׷�ӵķ�ʽ)
    !FileName:�ļ���
    !Arrar[M]��һά����
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
		                     write(*,*)trim(FileName) //"�ļ��򿪴���"
		                     close(10) 
		                     return 
		               end if 
                write(10,iostat=stat) (Array(i),i=1,M)
                        if(stat/=0)   then
                            write(*,*)trim(FileName) //"д���ݹ����г���"	 
                            close(10) 
                            return 
                        end if 
		        close(10)
            endif    

    end subroutine SaveDataArrayAppend    
    
    !���������ݣ�����remarkֵѡ��ͬ�Ĵ洢��ʽ,0Ϊ�Զ���ҵ��1Ϊ�û���ҵ
    subroutine SaveResult(FileName,Array,M,remark)
        character*(*)::FileName
        integer(kind=4)::M,remark
        real::Array(M)
        
        if(remark == 0) then !�Զ���ҵ
            call SaveDataArrayAppend(FileName,Array,M) !����γ��ƽ��ֵ
        endif
        if(remark == 1) then !�û���ҵ
            call SaveDataArray(FileName,Array,M)
        endif    
    end subroutine SaveResult
   
    !----------������ļ�����·��д�ص�outputPath��--------
    subroutine SaveResultFNtoOutPath(s1,s2,OutPath)
        character*(*)::s1,s2,OutPath        
        open(10,file=OutPath,form='formatted')        
        write(10,*)trim(s1)
        write(10,*)trim(s2)
        close(10)
        
    end subroutine SaveResultFNtoOutPath
    
    
     !����Ϊ��������------------------------------
      !------------��������Ǹ���ĵڼ��캯��---------------------- 
        integer function HowManyDay(year,month,day)
			implicit none
			integer::year,month,day
		
			integer::bigmonth=31,smallmonth=30,February
			logical::x1 !存储判断是否是闰年后返回的逻辑�?
			x1=IsLeapYear(year) !判断用户输入的年份是否是闰年
			if(x1) then 
				February=29
			else
			    February=28
			end if
			select case(month) !通过月份判断，并计算出该日是该年的第几天
				case(1)        !一月，下面以此类推
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
    !--------------��������Ǹ���ĵڼ��캯��------------------------------
     
       !-----------�ж�����--------------------------------------
        logical function IsLeapYear(year)
	         integer year
	         IsLeapYear=(mod(year,4)==0 .and. mod(year,100)/=0 ).or. mod(year,400)==0
	    end function IsLeapYear
    !--------------------------------------------------------- 

    !---------��ȡĳ�µ���ֹ����-------------------
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

   !----------���������ջ�ȡ�ļ�λ��------------------       
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