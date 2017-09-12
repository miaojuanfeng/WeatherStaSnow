!module C_Function
!    contains
    
integer(kind=4) function output(srcpath, srclen, descpath, desclen, remark) &
        bind(c, name='BlockHigh')
   use iso_c_binding
   use JL_Mod_BHinterface
   use JL_Mod_FileIO
   use JL_Mod_BHCommon
   use JL_BHFunction   
   
   
   implicit none
   integer(kind=4), value:: srclen, desclen
   character(len=1), dimension(srclen) :: srcpath
   character(len=1), dimension(desclen) :: descpath
   integer(kind=4), value :: remark
   
   integer(kind=4)::i,M,N     
   character(len=srclen) :: inputfile
   character(len=desclen):: outputfile
   
   type (InterStruct) ::interPara 
   type(DateStruct)::DateS
   type(HgtStruct)::HgtS
   type(LatAvgAreaStruct) ::LatAvgAreaS
   type(BHGradientStruct)::BHGradientS
   type(hgt_t)::Hgt
   
   character(len=200)::FileName,s1,s2
   real,allocatable::H(:,:)
   real,allocatable::LatAvg(:)
   real,allocatable::G(:)
   real,allocatable::SHAvg(:,:)
    
    write(*,*)"开始计算阻塞高压"
   output=-1
   inputfile=""
   do i=1, srclen
      inputfile=trim(inputfile)//srcpath(i)
   enddo

   outputfile=""
   do i=1, desclen
      outputfile=trim(outputfile)//descpath(i)
   enddo
!   write(*,*)inputfile
!   write(*,*)outputfile

     !获取接口文件中的路径，路径放在结构体interPara中
   call readIOPara(inputfile,outputfile,interPara)
!  write(*,*)trim(interPara%FixedPara)
!  write(*,*)trim(interPara%RunPara)
!  write(*,*)trim(interPara%InputPath)
!  write(*,*)trim(interPara%OutPath)
!!  
!   !读取用户输入的年月日参数文件
   call ReadUserParaFile(interPara%RunPara,DateS)
!   write(*,*)DateS%year
!   write(*,*)DateS%month
!   write(*,*)DateS%day  

   !读固定参数文件
   call ReadFixedParaFile(interPara%FixedPara,HgtS,LatAvgAreaS,BHGradientS)  
!   write(*,*)HgtS
!   write(*,*)LatAvgAreaS
!   write(*,*)BHGradientS

    !读取某个日期的位势高度数据
    allocate(H(HgtS%widthIndex,HgtS%heightIndex))    
    call ReadHgt(DateS,HgtS,interPara%InputPath,H)
! write(*,*)H(1:10,1)

!计算纬向平均
    allocate(LatAvg(HgtS%widthIndex))   
        !初始化位势高度结构体hgt       
    call hgt_create(hgt, H, HgtS%width, HgtS%height)
    !生成纬向平均结果文件名
    call GetBHLatAvgFileName(DateS,FileName)
    FileName=trim(interPara%OutPath)//trim(FileName)
   
    !计算乌拉尔地区的纬向平均值
    call calc_latitude_avg(hgt,LatAvgAreaS%Ural_x1,LatAvgAreaS%Ural_x2,LatAvg)    
    call SaveResult(FileName,LatAvg,HgtS%widthIndex,remark)
 
    
    !计算贝加尔湖地区地区的纬向平均值
    call calc_latitude_avg(hgt,LatAvgAreaS%Beger_x1,LatAvgAreaS%Beger_x2,LatAvg)   
    call SaveResult(FileName,LatAvg,HgtS%widthIndex,remark)
 
    !计算鄂霍茨克海地区地区地区的纬向平均值
    call calc_latitude_avg(hgt,LatAvgAreaS%Ehercikl_x1,LatAvgAreaS%Ehercikl_x2,LatAvg)   
    call SaveResult(FileName,LatAvg,HgtS%widthIndex,remark)
 
    s1=trim(FileName)
    
    M=hgt_longitude_index(hgt,BHGradientS%x2)
    N=hgt_longitude_index(hgt,BHGradientS%x1)
    allocate(G(M-N+1))
    allocate(SHAvg(HgtS%widthIndex,HgtS%heightIndex))
        !计算滑动平均
    call Slide_Avg(DateS,HgtS,interPara%InputPath,SHAvg)
    
    call calc_Block_Gradient(hgt,DateS%year,DateS%month,DateS%day,BHGradientS%x1,&
        BHGradientS%x2,BHGradientS%y1,BHGradientS%y2,BHGradientS%step, BHGradientS%m, &
        BHGradientS%s_min, BHGradientS%n_max,SHAvg,G)
!        write(*,*)G
    call GetBHGradient(DateS,FileName)
    FileName=trim(interPara%OutPath)//trim(FileName)    
    call SaveResult(FileName,G,M-N+1,remark)
     !将结果文件名写到outPath中
     s2=trim(FileName)
   ! call SaveResultFNtoOutPath(s1,s2,outputfile)    
  
   output=0 
   write(*,*)"计算阻塞高压结束"
end function output
!end module C_Function

!program main
!
!    use C_Function
!    integer(kind=4)::R
!!    R=output("F:\BlockHigh\BlockHigh\BHinterface.txt", len("F:\BlockHigh\BlockHigh\BHinterface.txt"),&
!!     "F:\BlockHigh\BlockHigh\outPath.txt", len("F:\BlockHigh\BlockHigh\outPath.txt"), 0)
!    
!    R=output("/gpfs/home/ncc/workspace/algorithms/JL/TBlockHigh/BHinterface.txt", &
!        len("/gpfs/home/ncc/workspace/algorithms/JL/TBlockHigh/BHinterface.txt"),&
!     "/gpfs/home/ncc/workspace/algorithms/JL/TBlockHigh/outPath.txt", &
!     len("/gpfs/home/ncc/workspace/algorithms/JL/TBlockHigh/OutPath.txt"), 0)
!     
!
!end program main