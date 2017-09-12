module JL_BHFunction
    use JL_Mod_BHCommon
    use JL_Mod_FileIO
    contains
    
    !计算关键区为纬度(y1,y2)上的经向平均值
    !输入：
    !  hgt:位势高度结构体
    !  y1,y2：纬度区域
    !输出：
    !   LongAvg:LongAvg(144)数组(其中144由分辨率确定的值360/2.5),经向平均值
    subroutine calc_longitude_avg(hgt,y1,y2,LongAvg)
        type(hgt_t)::hgt
        real::y1,y2
        real::LongAvg(int(longitude_max/hgt%block_height))
        integer::iy1,iy2
        integer::i,j
        
        iy1=hgt_latitude_index(hgt,y1)
        iy2=hgt_latitude_index(hgt,y2)
        LongAvg=0.0
        
        do j=iy1,iy2
            do i=1,longitude_max/hgt%block_height
                LongAvg(i)=LongAvg(i)+hgt%H(j,i)
            end do
        end do
        LongAvg=LongAvg/(iy2-iy1+1)
    end subroutine
    
    !计算关键区为经度(x1,x2)上的纬向平均值
    !输入：
    !  hgt:位势高度结构体
    !  x1,x2：经度区域
    !输出：
    !   LatAvg:LatAvg(73)数组(其中144由分辨率确定的值180/2.5+1),纬向平均值
    subroutine calc_latitude_avg(hgt,x1,x2,LatAvg)
        type(hgt_t)::hgt
        real::x1,x2
        real::LatAvg(int(latitude_max/hgt%block_width+1))
        integer::ix1,ix2
        integer::i,j
        
        ix1=hgt_longitude_index(hgt,x1)
        ix2=hgt_longitude_index(hgt,x2)
        LatAvg=0.0
        
        do j=1,int(latitude_max/hgt%block_width+1)
            do i=ix1,ix2
                LatAvg(j)=LatAvg(j)+hgt%H(j,i)
            enddo
            LatAvg(j)=LatAvg(j)/(ix2-ix1+1)            
        enddo
    end subroutine
    
    !计算某一个日期的滑动平均值
    !输入：
        !year,month,day:年月日
        !hgt,位势高度结构体
    !输出：
        !SHAvg(73,144):滑动平均值，通过形参带回。数组坐标值与源数据分辨有关，此处为2.5*2.5。
    !subroutine Slide_Avg(hgt,year,month,day,SHAvg)
    subroutine Slide_Avg(DateS,HgtS,DataFilePath,SHAvg)
        !integer::year,month,day
        !type(hgt_t)::hgt
        type(DateStruct)::DateS
        type(HgtStruct)::Hgts
        character(len=200)::DataFilePath
        real::SHAvg(HgtS%widthIndex,HgtS%heightIndex)
        
        !real::SHAvg(latitude_max/hgt%block_width+1,longitude_max/hgt%block_height)
        real::H_temp(HgtS%widthIndex,HgtS%heightIndex)        
        integer::y_t,m_t,d_t !临时用年月日
        integer::i,j,k
        integer::ix,iy
        iy=HgtS%widthIndex
        ix=HgtS%heightIndex
        SHAvg=0.0
      
        call Prev_day(DateS%year,DateS%month,DateS%day,y_t,m_t,d_t)
        call Prev_day(y_t,m_t,d_t,y_t,m_t,d_t)
        !ReadHgt(DateS,HgtS,DataFilePath,H)
        do i=1,5
            !call ReadHgt(H_temp,y_t,m_t,d_t)            
            call ReadHgt(DateS,HgtS,DataFilePath,H_temp)
            SHAvg(1:iy,1:ix)=SHAvg(1:iy,1:ix)+H_temp(1:iy,1:ix)
            call Next_day(y_t,m_t,d_t,y_t,m_t,d_t)
        enddo
        SHAvg=SHAvg/5
              
    end subroutine Slide_Avg
    
    !计算阻塞高压位势梯度
    !输入：
    !   hgt:位势结构体
    !   x1,x2,y1,y2：经纬度范围
    !   year,month,day：年月日
    !   step:纬度计数偏移(计算大陆高压时默认2.5 阻塞默认5.0)
    !   m:	纬度下标差距(计算大陆高压时默认4 阻塞默认8)
    !   s_min:GHGS下界
    !   n_max:GHGN上界
    !输出:G,位势梯度
    subroutine calc_Block_Gradient(hgt,year,month,day,x1,x2,y1,y2,step, m, s_min, n_max,SHAvg,G)
        implicit none    
        type(hgt_t)::hgt
        integer::year,month,day
        real::x1,x2,y1,y2
        real::step,s_min,n_max
        integer::m
        real::G(:),SHAvg(:,:)
        
        real::GHGS,GHGN !南方，北方位势高度梯度
        integer::ix1,ix2,iy1,iy2,i,j
!        real::SHAvg(latitude_max/hgt%block_width+1,longitude_max/hgt%block_height)
        
        !将经纬度转换为数组下标
        ix1=hgt_longitude_index(hgt, x1)
        ix2=hgt_longitude_index(hgt, x2)
        iy1=hgt_latitude_index(hgt,y1)
        iy2=hgt_latitude_index(hgt,y2)
        
        G=32767.0
       
        !call Slide_Avg(hgt,year,month,day,SHAvg)!计算滑动平均值
        do i=ix1,ix2
            do j=iy1,iy2,int(step/hgt%block_width)
                if(j+2*m>latitude_max/hgt%block_width+1)then
                    write(*,*)"计算阻塞高压位势梯度时纬度越界，纬度值不能大于50N"
                    exit
                endif
                GHGS=(SHAvg(j+m,i)-SHAvg(j,i))/(m*hgt%block_height)
                GHGN=(SHAvg(j+2*m,i)-SHAvg(j+m,i))/(m*hgt%block_height)
                !write(*,*)GHGS,GHGN
                if(GHGS>s_min .and. GHGN< n_max) then
                    if(G(i-ix1+1) .EQ. 32767) then
                        G(i-ix1+1)=GHGS                    
                    else
                        G(i-ix1+1)=max(GHGS,G(i-ix1+1))
                    endif
                endif
            enddo
        enddo       
    end subroutine

end module JL_BHFunction