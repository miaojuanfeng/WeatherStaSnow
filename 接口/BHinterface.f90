module JL_Mod_BHinterface  
      
    type InterStruct !�ӿڲ���·���ṹ��
        character(len=200):: RunPara !����ʱ�ɱ�����ļ���·��
        character(len=200):: FixedPara !�̶������ļ���·��
        character(len=200):: InputPath !����Դ���ݵ�·���ļ���
        character(len=200):: OutPath !�������ļ���·���ļ���    
    end type InterStruct
    
    type DateStruct !���ڽṹ��
        integer::year
        integer::month
        integer::day
    end type DateStruct
    
    type HgtStruct !λ�Ƹ߶Ȼ����ṹ��
        real::height !����ֱ���
        real::width !γ��ֱ���
        integer:: heightIndex !������������±�
        integer:: widthIndex !γ����������±�
        integer:: Layer !GRD�ļ��еĲ���
        integer:: CurLayer !GRD�ļ���Ҫ��ȡ�Ĳ���
    end type HgtStruct
    
    type LatAvgAreaStruct !γ��ƽ��ֵ����ʱ������������Χ
        real::Ural_x1 !����������
        real::Ural_x2
        real::Beger_x1 !���Ӷ�������
        real::Beger_x2 
        real::Ehercikl_x1 !�����Ŀ˺�����
        real::Ehercikl_x2
    end type LatAvgAreaStruct
    
    type BHGradientStruct !����������ѹλ���ݶ�ʱ�Ĳ���,��γ��Χ(20-160E),(35-45N)
        real::x1,x2
        real::y1,y2
        real::step !γ�ȼ���ƫ��
        integer::m !γ���±���
        real::s_min !GHGS�½�
        real::n_max !GHGN�Ͻ�
    end type BHGradientStruct    
      
    contains
    
    !  ���ݽӿڲ���inputstr,outputstr����ȡ��Ҫ����Ϣ����ֵ��interPara�ṹ�����    
    subroutine readIOPara( inputstr,outputstr,interPara )  
        implicit none
        character*(*) inputstr,outputstr
        type (InterStruct)::interPara 
        character(len=200) dataobj(4,1)
        integer(kind=4) ::pos,endpos
        call ReadIOParaFile(trim(inputstr),dataobj,4)
  
        interPara%RunPara=trim(dataobj(1,1))
        interPara%FixedPara=trim(dataobj(2,1))
        interPara%InputPath=trim(dataobj(3,1))
        interPara%OutPath=trim(dataobj(4,1))	 
    end subroutine  readIOPara
    
    !   �������ܣ���ȡ�ļ��е����ݣ����ַ�����ķ�ʽ���ؽ��
!    parameterfile:character*(*),�ļ�����
!    dataobj��character(len=200) dataobj(:,1)����ά�ַ����飬������
!     num��ȡ���ַ�������
    subroutine ReadIOParaFile(parameterfile,dataobj,num)
        implicit none
	    character(len=200) dataobj(num,1)
	    character*(*) parameterfile
	    integer(kind=4) irow,icolumn,num,error
	    integer(kind=4),parameter:: field=11
	    open(field,file=parameterfile)
	    do irow=1,num
		    read(field,'(a)',iostat=error)(dataobj(irow,icolumn),icolumn=1,1)
		    !(dataobj(irow,icolumn),icolumn=1,1)=adjustr(adjustl(dataobj(irow,1)))
		    dataobj=adjustl(adjustr(dataobj))
		    if(error/=0) then
		         ! write(*,*)"ReadParameterFile",parameterfile
		          exit
		    endif
	    end do
	    close(field)
    end subroutine   ReadIOParaFile
    
    !���û����ò����ļ�
    subroutine  ReadUserParaFile(parameterfile,DateS)  
       character*(*)  parameterfile
       type (DateStruct)::DateS
       
       character(len=150) dataobj(100,1)!����ά�ַ����飬������
        integer(kind=4) ::num   !,�����ļ�������
       call readParameterFile(parameterfile,dataobj,num)
       !write(*,*)dataobj(1:num,1)
       if(num .EQ. 3) then
             read(dataobj(1,1),"(i4)",blank='null')DateS%year
             read(dataobj(2,1),"(i2)",blank='null')DateS%month
             read(dataobj(3,1),"(i2)",blank='null')DateS%day
       else
            write(*,*)trim(parameterfile)
            write(*,*)"�û������ļ��еĲ�����������3��"
            return
       endif
       
    end subroutine  ReadUserParaFile
    
    !���̶������ļ�
    subroutine  ReadFixedParaFile(parameterfile,HgtS,LatAvgAreaS,BHGradientS) 
       implicit none
       character*(*)  parameterfile
       type(HgtStruct)::HgtS
       type(LatAvgAreaStruct)::LatAvgAreaS
       type(BHGradientStruct)::BHGradientS
       
       character(len=150) dataobj(100,1)!����ά�ַ����飬������
        integer(kind=4) ::num   !,�����ļ�������
       call readParameterFile(parameterfile,dataobj,num)
      
       if(num .NE. 20) then
            write(*,*)trim(parameterfile)
            write(*,*)"�̶������ļ��еĲ�����������20��"
            return
       endif
      
     
       read(dataobj(1,1),"(F4.1)")HgtS%height
       read(dataobj(2,1),"(F4.1)")HgtS%width
       read(dataobj(3,1),"(I4)",blank='null')HgtS%heightIndex       
       read(dataobj(4,1),"(I4)",blank='null')HgtS%widthIndex       
       read(dataobj(5,1),"(I4)",blank='null')HgtS%Layer
       read(dataobj(6,1),"(I4)",blank='null')HgtS%CurLayer
       
       read(dataobj(7,1),"(F5.1)")LatAvgAreaS%Ural_x1
       read(dataobj(8,1),"(F5.1)")LatAvgAreaS%Ural_x2       
       read(dataobj(9,1),"(F5.1)")LatAvgAreaS%Beger_x1
       read(dataobj(10,1),"(F5.1)")LatAvgAreaS%Beger_x2
       read(dataobj(11,1),"(F5.1)")LatAvgAreaS%Ehercikl_x1
       read(dataobj(12,1),"(F5.1)")LatAvgAreaS%Ehercikl_x2
       
       read(dataobj(13,1),"(F5.1)")BHGradientS%x1
       read(dataobj(14,1),"(F5.1)")BHGradientS%x2
       read(dataobj(15,1),"(F5.1)")BHGradientS%y1
       read(dataobj(16,1),"(F5.1)")BHGradientS%y2
       read(dataobj(17,1),"(F4.1)")BHGradientS%step
       read(dataobj(18,1),"(I4)")BHGradientS%m
       read(dataobj(19,1),"(F4.1)")BHGradientS%s_min
       read(dataobj(20,1),"(F4.1)")BHGradientS%n_max
     
        
    end subroutine
    
!    subroutine reaParameterFile(parameterfile,dataobj)�������ܣ���ȡ�ļ��е����ݣ����ַ�����ķ�ʽ���ؽ��
!    parameterfile:character*(*),�ļ�����
!    dataobj��character(len=150) dataobj(100,1)����ά�ַ����飬������
!     num:integer,�����ļ�������
    subroutine ReadParameterFile(parameterfile,dataobj,num)
        implicit none
	    character(len=150) dataobj(100,1),tempstr(100,1)
	    character (len=150) temp
	    character*(*) parameterfile
	    integer(kind=4) irow,icolumn
	    integer(kind=4) num,row
	    integer(kind=4):: error=0
	    integer(kind=4),parameter :: column=1
	    logical alive
	    integer(kind=4),parameter:: field=11
	    num=txtRow(parameterfile)
	    open(field,file=parameterfile,blank='null')
	    do irow=1,num
		    read(field,'(a)',iostat=error)(tempstr(irow,icolumn),icolumn=1,1)
		    if(error/=0) then
		          !write(*,*)"Procedure of ReadParameterFile which reads file occurs error!",trim(parameterfile)
		          exit
		    endif
	    end do
	    if(num>=1) then
	           row=0
	        do irow=1,num
	             temp=tempstr(irow,1)
	            if(temp(1:1)/='#') then
	                  row=row+1
	                  dataobj(row,1)=tempstr(irow,1)
	             end if
	         end do    
	                num=row
	    end if
	        
!	do irow=1,row
!	    print *, dataobj(irow,1)
!	end do
	    close(field)
    end subroutine  ReadParameterFile
    
    
!    function txtRow(parameterfile)�������ܣ�ͳ���ļ�������,integer(kind=4)
!    parameterfile:character*(*),�ļ�����    
    integer(kind=4) function txtRow(parameterfile)
        implicit none
	    integer(kind=4) irow,icolumn,i
	    character(len=150) dataobj(100,1)
	    integer(kind=4),parameter :: field=11
	    integer(kind=4):: error=0
	    character*(*) parameterfile
        logical alive
	    inquire(file=parameterfile,exist=alive)
	    if(.not. alive) then
		    write(*,*) trim(parameterfile), " file not exists!"
		    return
	    end if
	    open(field,file=parameterfile)
	    irow=1
	    do while(.true.)
		    read(field,*,iostat=error)(dataobj(irow,icolumn),icolumn=1,1)
		    if(error/=0) then
 		      exit
		    endif
		    irow=irow+1
	    end do
	    txtRow=irow-1
	    close(field)
    end function txtRow	
    
 end module JL_Mod_BHinterface